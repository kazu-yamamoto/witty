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

This is unnecessary overhead. With this option, the `accept` loop
is in a worker native thread thanks to `runInUnboundThread`.

    23961 accept4(10, ...)
    23961 recvfrom(11, ...)
    23961 sendto(11, ...)

This technique improves latency.

### The '-y' option

### The '-s' option

### The '-r' option

### The '-p' option

