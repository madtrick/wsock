#Change Log

### 1.1.2
* Handle fragmented HTTP messages. In previous version if you tried to decode a fragmented HTTP message with ```wsock_http:decode``` you will run into an error. Now, using [erlang:decode_packet](http://www.erlang.org/doc/man/erlang.html#decode_packet-3), ```wsock_http:decode``` can work around this issue. If you pass a fragmented http message it will return the atom ```fragmendted_http_message```.

### 1.1.1
* Fix a bugs in type specs.
