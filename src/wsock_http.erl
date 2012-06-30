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

-export([build/3, get_start_line_value/2, get_header_value/2]).
-export([decode/2, encode/1]).

-define(CTRL, "\r\n").

-spec decode(Data::binary(), Type::request | response) -> {ok,#http_message{}} | {error, malformed_request}.
decode(Data, Type) ->
  [StartLine | Headers] = split(Data),
  StartLineProcessed = process_startline(StartLine, Type),
  HeadersProcessed = process_headers(Headers),

  case {StartLineProcessed, HeadersProcessed} of
    {{error, _}, _} ->
      {error, malformed_request};
    {_, {error, _}} ->
      {error, malformed_request};
    {{ok, StartLineList}, {ok, HeaderList}} ->
          {ok, wsock_http:build(Type, StartLineList, HeaderList)}
  end.


-spec build(Type::atom(), StartLine::list({atom(), string()}), Headers::list({string(), string()})) -> #http_message{}.
build(Type, StartLine, Headers) ->
  #http_message{type = Type, start_line = StartLine, headers = Headers}.

-spec encode(Message::#http_message{}) -> list(string()).
encode(Message) ->
  Startline = Message#http_message.start_line,
  Headers = Message#http_message.headers,
  encode(Startline, Headers, Message#http_message.type).

-spec encode(Startline::list({atom(), string()}), Headers::list({string(), string()}), Type:: request | response) -> list(string()).
encode(Startline, Headers, request) ->
  encode_message("{{method}} {{resource}} HTTP/{{version}}", Startline, Headers);

encode(Startline, Headers, response) ->
  encode_message("HTTP/{{version}} {{status}} {{reason}}", Startline, Headers).


-spec get_start_line_value(Key::atom(), Message::#http_message{}) -> string().
get_start_line_value(Key, Message) ->
  proplists:get_value(Key, Message#http_message.start_line).

-spec get_header_value(Key::string(), Message::#http_message{}) -> string().
get_header_value(Key, Message) ->
  LowerCasedKey = string:to_lower(Key),
  get_header_value_case_insensitive(LowerCasedKey, Message#http_message.headers).

-spec get_header_value_case_insensitive(Key::string(), list()) ->  undefined | string().
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
  Fragments =   lists:map(fun(Element) ->
        re:replace(Element, <<"^\n*\s*|\n*\s*$">>, <<"">>, [global, {return, binary}])
    end, binary:split(Data, <<?CTRL>>, [trim, global])),

  lists:filter(fun(Element) ->
        <<>> =/= Element
    end, Fragments).

-spec process_startline(StartLine::binary(), Type:: request | response) -> {ok, list({atom(), string()})} | {error, nomatch}.
process_startline(StartLine, request) ->
  process_startline(StartLine, "(GET)\s+([\S/])\s+HTTP\/([0-9]\.[0-9])", [method, resource, version]);

process_startline(StartLine, response) ->
  process_startline(StartLine, "HTTP/([0-9]\.[0-9])\s([0-9]{3,3})\s([a-zA-z0-9 ]+)", [version, status, reason]).

-spec process_startline(StartLine::binary(), Regexp::list(), Keys::list(atom())) -> {ok, list({atom(), string()})} | {error, nomatch}.
process_startline(StartLine, Regexp, Keys) ->
  case regexp_run(Regexp, StartLine) of
    {match, [_ | Matchs]} ->
      {ok , lists:zip(Keys, Matchs)};
    nomatch -> {error, nomatch}
  end.

-spec regexp_run(Regexp::list(), String::binary()) -> {match, list()} | nomatch.
regexp_run(Regexp, String) ->
  re:run(String, Regexp, [{capture, all, list}, caseless]).


-spec process_headers(Headers::list(binary())) -> {ok, list({string(), string()})} | {error, nomatch}.
process_headers(Headers) ->
  process_headers(Headers, []).

-spec process_headers(Headers::list(binary()), Acc::list({list(), list()})) -> {ok, list({string(), string()})} | {error, nomatch}.
process_headers([Header | Tail], Acc) ->
  case regexp_run("([!-9;-~]+)\s*:\s*(.+)", Header) of
    {match, [_Match, HeaderName, HeaderValue]} -> 
      process_headers(Tail, [{string:strip(HeaderName), HeaderValue} | Acc]);
    nomatch ->
      {error, nomatch}
  end;

process_headers([], Acc) ->
  {ok, Acc}.

encode_message(StartlineExpr, StartlineFields, Headers) ->
  SL = build_start_line(StartlineExpr, StartlineFields),
  H= build_headers(Headers),

  lists:foldr(fun(El, Acc) ->
        [El++"\r\n" | Acc]
    end, ["\r\n"], [SL | H]).

build_start_line(StartlineExpr, StartlineFields) ->
  lists:foldr(fun({Key, Value}, Acc) ->
        re:replace(Acc, "{{" ++ atom_to_list(Key) ++ "}}", Value, [{return, list}])
    end, StartlineExpr, StartlineFields).

-spec build_headers(list({HeaderName::string(), HeaderValue::string()})) -> list(string()).
build_headers(Headers) ->
  lists:map(fun({Key, Value}) ->
        Key ++ ": " ++ Value
    end, Headers).
