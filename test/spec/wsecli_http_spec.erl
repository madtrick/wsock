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

-module(wsecli_http_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include("wsecli.hrl").

spec() ->
  describe("wsecli_http", fun() ->
        it("should build proper HTTP messages", fun() ->
          RequestLine = [
            {method, "GET"},
            {version, "1.1"},
            {resource, "/"}
          ],

          Headers = [
            {"Header-A", "A"},
            {"Header-B", "B"}
          ],

          Message = wsecli_http:build(request, RequestLine, Headers),

          assert_that(Message#http_message.type, is(request)),

          assert_that(proplists:get_value(method, Message#http_message.start_line), is("GET")),
          assert_that(proplists:get_value(version, Message#http_message.start_line), is("1.1")),
          assert_that(proplists:get_value(resource, Message#http_message.start_line), is("/")),
          assert_that(proplists:get_value("Header-A", Message#http_message.headers), is("A")),
          assert_that(proplists:get_value("Header-B", Message#http_message.headers), is("B"))
          end),
        it("should build proper HTTP request strings", fun() ->
          RequestLine = [
            {method, "GET"},
            {version, "1.1"},
            {resource, "/"}
          ],

          Headers = [
            {"Header-A", "A"},
            {"Header-B", "B"}
          ],

          Message = wsecli_http:build(request, RequestLine, Headers),
          Request = wsecli_http:to_request(Message),

          assert_that(Request, is([
                "GET / HTTP/1.1\r\n",
                "Header-A: A\r\n",
                "Header-B: B\r\n",
                "\r\n"
              ]))
      end),
    it("should build a proper HTTP response from binary message", fun() ->
          Data = <<"HTTP/1.1 205 Reset Content\r\n
          Header-A: A\r\n
          Header-C: dGhlIHNhbXBsZSBub25jZQ==\r\n
          Header-D: D\r\n\r\n">>,

          StatusLine = [
            {version, "1.1"},
            {status, "205"},
            {reason, "Reset Content"}
          ],

          Headers = [
            {"Header-A", "A"},
            {"Header-C", "dGhlIHNhbXBsZSBub25jZQ=="},
            {"Header-D", "D"}
          ],

          ExpectedResponse = #http_message{type = response, start_line = StatusLine, headers = Headers},
          Response = wsecli_http:from_response(Data),

          assert_that(Response, is(ExpectedResponse))
      end),
    it("should return http_message start_line values if present", fun() ->
          Message = #http_message{
            type = request,
            start_line = [
              {method, "GET"},
              {version, "1.1"},
              {resource, "/"}
            ],
            headers = [
              {"header-a", "A"},
              {"header-b", "b"}
            ]
          },

          assert_that(wsecli_http:get_start_line_value(version, Message), is("1.1")),
          assert_that(wsecli_http:get_start_line_value(method, Message), is("GET")),
          assert_that(wsecli_http:get_start_line_value(resource, Message), is("/"))
      end),
    it("should return http_message header values if present", fun() ->
          Message = #http_message{
            type = request,
            start_line = [
              {method, "GET"},
              {version, "1.1"},
              {resource, "/"}
            ],
            headers = [
              {"Header-a", "A"},
              {"header-B", "b"}
            ]
          },

          assert_that(wsecli_http:get_header_value("header-a", Message), is("A")),
          assert_that(wsecli_http:get_header_value("header-b", Message), is("b"))
      end)
    end).
