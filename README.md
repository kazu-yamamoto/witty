# Witty

`witty` is a network server to show bottlenecks of GHC (Glasgow
Haskell Compiler).

It repeats receive/send actions without analyzing application
protocols. Reply messages contain fixed-size HTTP reply messages.  This
server can be considered as a web server if benchmark tools analyze
reply messages. Also, it can be considered as a echo server if
benchmark tools ignore the reply messages.

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
It first tries to read incomming data anyway.
If the data is available, `recv` succeeds.
Otherwise, `EAGAIN` is returned.
In this case, `recv` asks the IO manager to notify when the data is available.

If a network server repeats receive/send actions,
`recv` just after `send` probably fails because
there is time lag for the next request from the client.
Thus the IO manager works frequently.

    recvfrom(13, )                -- Haskell thread A
    sendto(13, )                  -- Haskell thread A
    recvfrom(13, ) = -1 EAGAIN    -- Haskell thread A
    epoll_ctl(3, )                -- IO manager
    recvfrom(14, )                -- Haskell thread B
    sendto(14, )                  -- Haskell thread B
    recvfrom(14, ) = -1 EAGAIN    -- Haskell thread B
    epoll_ctl(3, )                -- IO manager

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

In other words, `yield` makes IO manager work less frequently.
This magically improves throughput.

The IO manager use `MVar` to notify data availability to Haskell threads.
Since `MVar` is a lock, it may be slow.
Or, allocation of `MVar` may be slow.

Note that typical real servers record log messages after `send`.
So, `yield` may not improve the throughput of the servers magically.

### The '-s' option

### The '-r' option

### The '-p' option

