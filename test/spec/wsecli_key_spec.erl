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

-module(wsecli_key_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").

spec() ->
  describe("wsecli_key", fun() ->
        it("should return a valid Sec-WebSocket-Key", fun() ->
              %Meck crashes if we try to mock crypto module
              %meck:new(crypto, [passthrough]),
              meck:new(base64, [unstick, passthrough]),

              Key = wsecli_key:generate(),

              assert_that(length(Key), is(24)),
              %assert_that(meck:called(crypto, rand_bytes, 16), is(true)),
              assert_that(meck:called(base64, encode, '_'), is(true)),
              meck:unload(base64)
          end)
    end).
