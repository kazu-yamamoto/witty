# Witty

`witty` is a network server to show bottlenecks of GHC (Glasgow
Haskell Compiler).

It repeats receive/send actions without analyzing application
protocol. Reply messages contain fixed-size HTTP reply messages.  This
server can be considered as a web server if benchmark tools analyze
reply messages. Alos, it can be considered as a echo server if
benchmark tools ignore the reply messages.

I recommend to use
[`weighttp`](http://redmine.lighttpd.net/projects/weighttp/wiki) and
[`echo-client`](https://github.com/kazu-yamamoto/latency-bench) to
measure throughput and latency (response time), respectively.

`witty` has one and only argument, port number:

    % witty 8000

Several options are prepared to show bottlenecks of GHC.

## Options
