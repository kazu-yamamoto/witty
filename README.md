# Witty

`witty` is a network server to show bottlenecks of GHC (Glasgow
Haskell Compiler).

It repeats receive/send actions without analyzing application
protocols. Reply messages contain fixed-size HTTP reply messages.
This server can be considered as a web server if benchmark tools
analyze reply messages. Also, it can be considered as an ping-pong
server if benchmark tools ignore the reply messages.

I recommend to use
[`weighttp`](http://redmine.lighttpd.net/projects/weighttp/wiki) and
[`echo-client`](https://github.com/kazu-yamamoto/latency-bench) to
measure throughput and latency (response time), respectively.

`witty` has one and only argument - port number:

    % witty 8000

## Options

Several options are prepared to show bottlenecks of GHC.

### The '-a' option

Without this option, the `accept` loop is in the main native thread.
So, a new connection is accepted by the main native thread
and a spawn Haskell goes to a worker native thread on a HEC.

    23960 accept4(10, ...)
    23961 recvfrom(11, ...)
    23961 sendto(11, ...)

This context switches are unnecessary overhead. With this option, the
`accept` loop is in a worker native thread thanks to
`runInUnboundThread`.

    23961 accept4(10, ...)
    23961 recvfrom(11, ...)
    23961 sendto(11, ...)

This technique improves latency.

### The '-y' option

GHC's I/O functions are optimistic. Let's consider `recv`.
It first tries to read incoming data anyway.
If the data are available, `recv` succeeds.
Otherwise, `EAGAIN` is returned.
In this case, `recv` asks the IO manager to notify when the data are available.

If a network server repeats receive/send actions,
`recv` just after `send` probably fails because
there is a time lag for the next request from the client.
Thus the IO manager works frequently.

    recvfrom(13, )                -- Haskell thread A
    sendto(13, )                  -- Haskell thread A
    recvfrom(13, ) = -1 EAGAIN    -- Haskell thread A
    epoll_ctl(3, )                -- Haskell thread A (a job for IO manager)
    recvfrom(14, )                -- Haskell thread B
    sendto(14, )                  -- Haskell thread B
    recvfrom(14, ) = -1 EAGAIN    -- Haskell thread B
    epoll_ctl(3, )                -- Haskell thread B (a job for IO manager)

With the '-y' option, `witty` calls `yield` after `send`.
`yield` pushes its Haskell thread onto the end of thread queue. So,
another thread can work. During the work of other threads, a request
message would arrive.

    recvfrom(13, )                -- Haskell thread A
    sendto(13, )                  -- Haskell thread A
    recvfrom(14, )                -- Haskell thread B
    sendto(14, )                  -- Haskell thread B
    recvfrom(13, )                -- Haskell thread A
    sendto(13, )                  -- Haskell thread A

In other words, `yield` makes the IO manager work less frequently.
This magically improves throughput.

The IO manager uses `MVar` to notify data availability to Haskell threads.
Since `MVar` is a lock, it may be slow.
Or, allocation of `MVar` may be slow.

Note that typical real servers record log messages after `send`.
So, `yield` may not improve the throughput of the servers magically.

### The '-s' option

`witty` uses `sendAll` of `Network.Socket.ByteString` by default.
If the '-s' option, it uses the original `sendAll`
which directly manipulates a buffer.
I think there is no significant overhead in
`sendAll` of `Network.Socket.ByteString`.

### The '-r' option

`witty` uses `recv` of `Network.Socket.ByteString` by default.
If the '-r' option, it uses the original `recv`
which directly manipulates a buffer.

I think that
`recv` of `Network.Socket.ByteString` has significant overhead.
It uses `createAndTrim` in `Data.ByteString.Internal`.
`createAndTrim` first allocates a `ByteString` of the size specified
to `recv`.
After receiving data, another `ByteString` is allocated because of trimming.

`ByteString` is categorized into small and large:

- On 64 bit machines, large if the size >= 3272 (409 words), otherwise small.
- On 32 bit machines, large if the size >= 3276 (819 words), otherwise small.

GHC 7.6.3 or earlier, `SM_LOCK` is used to allocate any `ByteString`s
because the global area is used.
GHC 7.7 or later, `SM_LOCK` is used to allocate large `ByteString`s
while small `ByteString`s are allocated from a local nursery without a lock.

`witty` specifies 4096 to `recv` of `Network.Socket.ByteString`.

I think that `recv` should not trim `ByteString` if it receives
considerably large data.
Moreover, large `ByteString`s should also be allocated from
a local nursery without a lock.

`recv` of `Network.Socket.ByteString` allocates one or two `ByteString`
everytime when it is called.
Our `recv` allocates only one buffer for each HTTP connection
even if multiple requests are transferred throught the connection.

### The '-p' option

If this option is specified, receiving buffers are prepared in advance.
This option must be used with the '-r' option.

### The '-m' option

If this option is specified, receiving buffers are prepared by
malloc(3). GHC's GC does not take care of them.  Sophisticated malloc(3)
has an arena for each core. So, we don't have to manage arenas by
ourselves (like '-p' option).

This option must not be used with the '-r' option and '-p' option.

### The '-n' option

This option takes the number of processes. Instead of `+RTS -Nx` option,
this option creates N processes to utilize cores.
