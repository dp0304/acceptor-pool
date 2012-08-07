acceptor-pool
=============

不是什么高深的东西，只是写出来个通用的socket接入池，以后自己可以复用~~

====================
参考cowboy写的，觉得cowboy的2秒acceptor，然后监测配置更新很浪费。就去除了全部的更新配置操作。干净的acceptor pool


====================

使用方法：
application:start(daccpol).



daccpol:start_listener(echo1,1000,daccpol_tcp_transport,[{port,8080}],d_self_protocol,[]).





