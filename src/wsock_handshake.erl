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

%% @hidden

-module(wsock_handshake).
-include("wsock.hrl").

-export([build/3, validate/2]).
-export([handle_open/1]).

-define(VERSION, 13).
-define(GUID, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").

-spec handle_open(Message::#http_message{}) -> {ok, #handshake{}} | {error, atom()}.
handle_open(Message) ->
  StartLine = Message#http_message.start_line,
  Headers = Message#http_message.headers,

  case validate_startline(StartLine) andalso validate_headers(Headers) of
    true ->
      {ok , #handshake{ type = handle_open}};
    false ->
      {error, fuuu}
  end.

-spec validate_startline(StartLine::list({atom(), term()})) -> true | false.
validate_startline(StartLine) ->
  Matchers = [{method, "GET"}, {version, "1\.1"}],
  lists:all(fun({Key, Value}) ->
        match == re:run(proplists:get_value(Key, StartLine), Value, [caseless, {capture, none}])
    end, Matchers).

validate_headers(Headers) ->
  Matchers = [
    {"host", ".+"},
    {"upgrade", "websocket"},
  {"connection", "upgrade"},
  {"sec-websocket-version", "13"}],

  lists:all(fun({HeaderName, HeaderValue}) ->
        case get_value_insensitive(HeaderName, Headers) of
          Value ->
            match == re:run(Value, HeaderValue, [caseless, {capture, none}]);
          undefined ->
            false
        end
        %match == re:run(proplists:get_value(Key, Headers), Value, [caseless, {capture, none}])
    end, Matchers).

get_value_insensitive(Key, [{Name, Value} | Tail]) ->
  case re:run(Name, "^" ++ Key ++ "$", [caseless, {capture, first, list}]) of
    {match, _} ->
      Value;
    nomatch ->
        get_value_insensitive(Key, Tail)
    end;

get_value_insensitive(_, []) ->
  undefined.

-spec build(Resource ::string(), Host ::string(), Port::integer()) -> #handshake{}.
build(Resource, Host, Port) ->
  RequestLine = [
    {method, "GET"},
    {version, "1.1"},
    {resource, Resource}
  ],

  Headers =[
    {"Host", Host ++ ":" ++ integer_to_list(Port)},
    {"Upgrade", "websocket"},
    {"Connection", "upgrade"},
    {"Sec-Websocket-Key", wsock_key:generate()},
    {"Sec-Websocket-Version", integer_to_list(?VERSION)}
  ],

  Message = wsock_http:build(request, RequestLine, Headers),
  #handshake{ version = ?VERSION, message = Message}.

-spec validate(Response::#http_message{}, Handshake::#handshake{}) -> boolean().
validate(Response, Handshake) ->
  validate_http_status(Response)
  and
  validate_upgrade_header(Response)
  and
  validate_connection_header(Response)
  and
  validate_sec_websocket_accept_header(Response, Handshake).


-spec validate_http_status(Response::#http_message{}) -> boolean().
validate_http_status(Response) ->
  "101" == wsock_http:get_start_line_value(status, Response).

-spec validate_upgrade_header(Response ::#http_message{}) -> boolean().
validate_upgrade_header(Response) ->
  "websocket" == string:to_lower(wsock_http:get_header_value("upgrade", Response)).

-spec validate_connection_header(Response ::#http_message{}) -> boolean().
validate_connection_header(Response) ->
  "upgrade" == string:to_lower(wsock_http:get_header_value("connection", Response)).

-spec validate_sec_websocket_accept_header(Response::#http_message{}, Handshake::#handshake{}) -> boolean().
validate_sec_websocket_accept_header(Response, Handshake) ->
  ClientKey         = wsock_http:get_header_value("sec-websocket-key", Handshake#handshake.message),
  BinaryClientKey   = list_to_binary(ClientKey),
  ExpectedHashedKey = base64:encode_to_string(crypto:sha(<<BinaryClientKey/binary, ?GUID>>)),
  HashedKey         = wsock_http:get_header_value("sec-websocket-accept", Response),

  ExpectedHashedKey == HashedKey.
