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

-module(wsock_http).
-include("wsock.hrl").

-export([build/3, to_request/1, get_start_line_value/2, get_header_value/2]).
-export([decode/2]).

-define(CTRL, "\r\n").

-spec decode(Data::binary(), Type::request | response) -> #http_message{}.
decode(Data, Type) ->
  [StartLine | Headers] = split(Data),
  StartLineList = process_startline(StartLine, Type),
  HeaderList = lists:foldr(fun(Element, Acc) ->
        {match, [_Match, HeaderName, HeaderValue]} = re:run(Element, "(\.+):\s+(\.+)", [{capture, all, list}]),
        [{string:strip(HeaderName), HeaderValue} | Acc]
    end, [], Headers),
  wsock_http:build(Type, StartLineList, HeaderList).


-spec build(Type::atom(), StartLine::list({atom(), string()}), Headers::list({string(), string()})) -> list(string()).
build(Type, StartLine, Headers) ->
  #http_message{type = Type, start_line = StartLine, headers = Headers}.

-spec to_request(Message::#http_message{}) -> list(string()).
to_request(Message) ->
  build_request_line(
    Message#http_message.start_line,
    build_headers(Message#http_message.headers, ["\r\n"])
  ).

-spec build_headers(list({HeaderName::string(), HeaderValue::string()}), list(string())) -> list(string()).
build_headers(Headers, Acc) ->
  lists:foldr(fun({Key, Value}, AccIn) ->
        [ Key ++ ": " ++ Value ++ "\r\n" | AccIn]
    end, Acc, Headers).

-spec build_request_line(list({Name::atom(), Value::string()}), list(string())) -> list(string()).
build_request_line(RequestLine, Acc) ->
  Method   = proplists:get_value(method, RequestLine),
  Version  = proplists:get_value(version, RequestLine),
  Resource = proplists:get_value(resource, RequestLine),

  [Method ++ " " ++ Resource ++ " " ++ "HTTP/" ++ Version ++ "\r\n" | Acc].

-spec get_start_line_value(Key::atom(), Message::#http_message{}) -> string().
get_start_line_value(Key, Message) ->
  proplists:get_value(Key, Message#http_message.start_line).

-spec get_header_value(Key::string(), Message::#http_message{}) -> string().
get_header_value(Key, Message) ->
  LowerCasedKey = string:to_lower(Key),
  get_header_value_case_insensitive(LowerCasedKey, Message#http_message.headers).

-spec get_header_value_case_insensitive(Key::string(), list()) ->  undefined;
                                        (Key::string(), list()) -> string().
get_header_value_case_insensitive(_, []) ->
  undefined;

get_header_value_case_insensitive(Key, [{Name, Value} | Tail]) ->
  LowerCaseName = string:to_lower(Name),
  case Key == LowerCaseName of
    true ->
      Value;
    false ->
      get_header_value_case_insensitive(Key, Tail)
  end.

%=============
% Helpers
%=============
-spec split(Data::binary()) -> list(binary()).
split(Data)->
  binary:split(Data, <<?CTRL>>, [trim, global]).

-spec process_startline(StartLine::binary(), Type:: request | response) -> list().
process_startline(StartLine, request) ->
  {match, [_, Method, RequestURI, Version]} = regexp_run("(GET)\s+([\S/])\s+HTTP\/([0-9]\.[0-9])", StartLine),
  [{method, Method}, {resource, RequestURI}, {version, Version}];

process_startline(StartLine, response) ->
  {match, [_, Version, Status, Reason]} = regexp_run("HTTP/([0-9]\.[0-9])\s([0-9]{3,3})\s([a-zA-z0-9 ]+)", StartLine),
  [{version, Version}, {status, Status}, {reason, Reason}].

-spec regexp_run(Regexp::list(), String::binary()) -> {match, list()}.
regexp_run(Regexp, String) ->
  re:run(String, Regexp, [{capture, all, list}, caseless]).
