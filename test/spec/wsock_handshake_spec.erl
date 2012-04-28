%Copyright [2012] [Farruco Sanjurjo Arcay]

%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at

%       http://www.apache.org/licenses/LICENSE-2.0

%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.

-module(wsock_handshake_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").

-include("wsock.hrl").

spec() ->
  describe("wsock_handshake", fun() ->
        describe("handle_open", fun() ->
              it("should handle a open-handshake request from the client", fun() ->
                    BinRequest = list_to_binary(["GET / HTTP/1.1\r\n
                        Host : server.example.org\r\n
                        Upgrade : websocket\r\n
                        Connection : Upgrade\r\n
                        Sec-WebSocket-Key : AQIDBAUGBwgJCgsMDQ4PEA==\r\n
                        Sec-WebSocket-Version : 13\r\n\r\n
                        "]),
                    {ok, Message} = wsock_http:decode(BinRequest, request),
                    {ok,Response} = wsock_handshake:handle_open(Message),

                    assert_that(is_record(Response, handshake), is(true)),
                    assert_that(Response#handshake.type, is(handle_open))
                end),
              it("should return an error if the request isn't valid", fun() ->
                    %Missing sec-websocket-key header
                    BinRequest = list_to_binary(["GET / HTTP/1.1\r\n
                        Host : server.example.org\r\n
                        Upgrade : websocket\r\n
                        Connection : Upgrade\r\n
                        Sec-WebSocket-Version : 13\r\n\r\n
                        "]),
                    {ok, Message} = wsock_http:decode(BinRequest, request),
                    {error, invalid_handshake_opening} = wsock_handshake:handle_open(Message)
                end)
          end),
      describe("response", fun() ->
                     it("should return a valid handshake response", fun() ->
                {ok, Response} = wsock_handshake:response([{"sec-websocket-key", "AQIDBAUGBwgJCgsMDQ4PEA=="}]),

                assert_that(is_record(Response, handshake), is(true)),
                assert_that(Response#handshake.type, is(response)),

                Message = Response#handshake.message,
                assert_that(wsock_http:get_start_line_value(version, Message), is("1.1")),
                assert_that(wsock_http:get_start_line_value(status, Message), is("101")),
                assert_that(wsock_http:get_start_line_value(reason, Message), is("Switching protocols")),

                assert_that(wsock_http:get_header_value("upgrade", Message), is("Websocket")),
                assert_that(wsock_http:get_header_value("connection", Message), is("Upgrade")),
                assert_that(wsock_http:get_header_value("sec-websocket-accept", Message), is(fake_sec_websocket_accept("AQIDBAUGBwgJCgsMDQ4PEA==")))
            end),
          it("should return an error if some of the required fields is missing")
        end),
        describe("open", fun() ->
        it("should return a valid handshake request", fun() ->
              Resource  = "/",
              Host     = "localhost",
              Port      = 8080,

              HandShake = wsock_handshake:open(Resource, Host, Port),
              assert_that(HandShake#handshake.version, is(13)),

              HttpMessage = HandShake#handshake.message,
              assert_that(wsock_http:get_start_line_value(method, HttpMessage), is("GET")),
              assert_that(wsock_http:get_start_line_value(version, HttpMessage), is("1.1")),
              assert_that(wsock_http:get_start_line_value(resource, HttpMessage), is("/")),

              assert_that(wsock_http:get_header_value("Host", HttpMessage), is(Host ++ ":" ++ integer_to_list(Port))),
              assert_that(wsock_http:get_header_value("Upgrade", HttpMessage), is("websocket")),
              assert_that(wsock_http:get_header_value("Connection", HttpMessage), is("upgrade")),
              assert_that(wsock_http:get_header_value("Sec-Websocket-Key", HttpMessage), is_not(undefined)),
              assert_that(wsock_http:get_header_value("Sec-Websocket-Version", HttpMessage), is("13"))
          end)
          end),
        describe("handle_response", fun() ->
              it("should handle handshake response from a server", fun() ->
              Resource = "/",
              Host = "localhost",
              Port = 8080,

              HandShake = wsock_handshake:open(Resource, Host, Port),
              Key = wsock_http:get_header_value("sec-websocket-key", HandShake#handshake.message),

              BinResponse = list_to_binary(["HTTP/1.1 101 Switch Protocols\r\n
              Upgrade: websocket\r\n
              Connection: upgrade\r\n
              Sec-Websocket-Accept: ", fake_sec_websocket_accept(Key), "\r\n",
              "Header-A: A\r\n
              Header-C: 123123\r\n
              Header-D: D\r\n\r\n"]),
              {ok, Response} = wsock_http:decode(BinResponse, response),

              assert_that(wsock_handshake:handle_response(Response, HandShake),is({ok, Response}))
          end)
          end)
    end).

fake_sec_websocket_accept(Key) ->
  BinaryKey = list_to_binary(Key),
  base64:encode_to_string(crypto:sha(<<BinaryKey/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>)).
