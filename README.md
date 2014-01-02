[![Analytics](https://ga-beacon.appspot.com/UA-46795389-1/wsock/README)](https://github.com/igrigorik/ga-beacon)

WSOCK
-----

Library for building [WebSocket](http://tools.ietf.org/html/rfc6455) clients and servers in Erlang.

**Still in beta state** but you can see an example at [wsserver](https://github.com/madtrick/wsserver)
WSOCK
=====

* [About](#about)
* [Writing clients](#usage_clients)
  * [Openning](#openning_client)
  * [Sending data](#sending_client)
  * [Receiving data](#receiving_client)
  * [Closing](#closing_client)
* [Writing servers](#usage_servers)
  * [Openning](#openning_server)
  * [Sending data](#sending_server)
  * [Receiving data](#receiving_server)
  * [Closing](#closing_server)
* [Tests](#tests)
* [Contributing](#contributing)
* [Author](#author)
* [License](#license)


## About <a name="about"></a>

Wsock are a set of modules that can be used to build Websockets ([RFC 6455](http://tools.ietf.org/html/rfc6455#section-5.3)) clients an servers.

It's still in beta state (not valid for something serious. Yet)



## Writing clients <a name="usge_clients"></a>
There's an example of a client wrote using wsock at [https://github.com/madtrick/wsecli](https://github.com/madtrick/wsecli)

Don't forguet to include the wsock headers file:

  ```erlang
  -include_lib("wsock/include/wsock.hrl").
  ```
#### Opening a connection <a name="openning_client"></a>

Build a handshake request:

  ```erlang
  HandshakeRequest = wsock_handshake:open(Resource, Host, Port)
  ```

And encode it to send it through the wire:

  ```erlang
  BinaryData = wsock_http:encode(HandshakeRequest#handshake.message)
  ```

Receive and validate handshake response:

  ```erlang
  {ok, HandshakeResponse} = wsock_http:decode(Data, response)
  wsock_handshake:handle_response(HandshakeResponse, HandshakeRequest)
  ```

#### Sending data <a name="client_sending"></a>
Once the connection has been stablished you can send data through it:
  
  ```erlang
  Message = wsock_message:encode(Data, [mask, text]) %text data
  Message = wsock_message:encode(Data, [mask, binary]) %binary data
  ```

#### Receiving data <a name="client_receiving"></a>

If there is no previous fragmented message:

   ```erlang
   ListOfMessages = wsock_message:decode(Data, [])
   ```

If the previously received message was fragmented pass it as parameter:

   ```erlang
   ListOfMessages = wsock_message:decode(Data, FragmentedMessage, [])
   ```

Check wsock.hrl for a description of the message record.

#### Closing the connection <a name="client_closing"></a>
Once you are done close the connection following the closing handshake.

Init closing:

  ```erlang
  Message = wsock_message:encode([], [mask, close])
  ```

Then you'll have to wait for a close message and a the closing of the server socket before closing your socket (read the RFC).

## Writing servers <a name="usage_servers"></a>
There's an example of a dummy-server using wsock at [https://github.com/madtrick/wsserver](https://github.com/madtrick/wsserver)

Don't forget to include the wsock headers file:

  ```erlang
  -include_lib("wsock/include/wsock.hrl").
  ```

### Accepting <a name="openning_server"></a>
Accept openning handshakes from your clients (gen_tcp).

Decode the http-handshake request:

  ```erlang
  {ok, OpenHttpMessage}   = wsock_http:decode(Data, request),
  {ok, OpenHandshake}     = wsock_handshake:handle_open(OpenHttpMessage)
  ```

Get handshake key to generate a handshake response:
  
  ```erlang
  ClientWSKey             = wsock_http:get_header_value("sec-websocket-key", OpenHandshake#handshake.message),
  {ok, HandshakeResponse} = wsock_handshake:response(ClientWSKey)
  ```
  
Encode the http-handshake response:
  
  ```erlang
  ResponseHttpMessage     = wsock_http:encode(HandshakeResponse#handshake.message)
  ```

Now you all have to do is send it over the wire.

### Receiving data <a name="receiving_server"></a>

  ```erlang
  ListOfMessages = wsock_message:decode(Data, [masked]) % messages from clients are masked
  ```

### Sending data <a name="sending_server"></a>

  ```erlang
  ListOfMessages = wsock_message:encode(Data, [text]) % text data, servers don't mask data
  ListOfMessages = wsock_message:encode(Data, [binary]) % binary data
  ```

### Closing the connection <a name="closing_server"></a>

  ```erlang
  CloseMessage = wsock_message:encode(Reason, [close])
  ```

## Tests <a name="tests"> ##
Unit test where done with the library espec by lucaspiller.

To run them

  rake spec
or, in case you don't have rake installed,

  rebar compile && ERL_LIBS='deps/' ./espec test/spec/

## Contribute <a name="contributing">

If you find or think that something isn't working properly, just open an issue.

Pull requests and patches (with tests) are welcome.

## Author <a name="author"> ##

This stuff has been writen by Farruco sanjurjo

  * [@madtrick](https://twitter.com/madtrick) at twitter
  * Blog at [blog.tenako.com](http://blog.tenako.com)

## License <a name="license"> ##
Copyright [2012] [Farruco Sanjurjo Arcay]

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

