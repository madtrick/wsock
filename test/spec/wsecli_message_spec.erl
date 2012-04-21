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

-module(wsecli_message_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include("wsecli.hrl").

-define(FRAGMENT_SIZE, 4096).

spec() ->
  describe("encode", fun() ->
        before_all(fun()->
              meck:new(wsecli_framing, [passthrough])
          end),

        after_all(fun()->
              meck:unload(wsecli_framing)
          end),

        it("should set opcode to 'text' if type is text", fun() ->
              wsecli_message:encode("asadsd", text),
              assert_that(meck:called(wsecli_framing, frame, ['_', [fin, {opcode, text}]]), is(true))
          end),
        it("should set opcode to 'binary' if type is binary", fun() ->
              wsecli_message:encode(<<"asdasd">>, binary),
              assert_that(meck:called(wsecli_framing, frame, ['_', [fin, {opcode, binary}]]), is(true))
          end),
        describe("when payload size is <= fragment size", fun()->
              it("should return a list with only one binary fragment", fun()->
                    Data = "Foo bar",
                    [BinFrame | []] = wsecli_message:encode(Data, text),
                    assert_that(byte_size(list_to_binary(Data)),is(less_than(?FRAGMENT_SIZE))),
                    assert_that(is_binary(BinFrame), is(true)),
                    assert_that(meck:called(wsecli_framing, to_binary, '_'), is(true)),
                    assert_that(meck:called(wsecli_framing, frame, '_'), is(true))
                end),
              it("should set opcode to 'type'", fun() ->
                    Data = "Foo bar",
                    [Frame] = wsecli_message:encode(Data, text),

                    <<_:4, Opcode:4, _/binary>> = Frame,

                    assert_that(Opcode, is(1))
                end),
              it("should set fin", fun()->
                    Data = "Foo bar",
                    [Frame] = wsecli_message:encode(Data, text),

                    <<Fin:1, _/bits>> = Frame,

                    assert_that(Fin, is(1))
                end)
          end),
        describe("when payload size is > fragment size", fun() ->
              it("should return a list of binary fragments", fun()->
                    Data = crypto:rand_bytes(5000),
                    Frames = wsecli_message:encode(Data, binary),
                    assert_that(meck:called(wsecli_framing, to_binary, '_'), is(true)),
                    assert_that(meck:called(wsecli_framing, frame, '_'), is(true)),
                    assert_that(length(Frames), is(2))
                end),
              it("should set a payload of 4096 bytes or less on each fragment", fun() ->
                    Data = crypto:rand_bytes(12288),
                    Frames = wsecli_message:encode(Data, binary),

                    [Frame1, Frame2, Frame3] = Frames,

                    <<_:64, Payload1/binary>> = Frame1,
                    <<_:64, Payload2/binary>> = Frame2,
                    <<_:64, Payload3/binary>> = Frame3,

                    assert_that(byte_size(Payload1), is(4096)),
                    assert_that(byte_size(Payload2), is(4096)),
                    assert_that(byte_size(Payload3), is(4096))
                end),
              it("should set opcode to 'type' on the first fragment", fun()->
                    Data = crypto:rand_bytes(5000),
                    Frames = wsecli_message:encode(Data, binary),

                    [FirstFragment | _ ] = Frames,

                    <<_:4, Opcode:4, _/binary>> = FirstFragment,

                    assert_that(Opcode, is(2))
                end),
              it("should unset fin on all fragments but last", fun() ->
                    Data = crypto:rand_bytes(12288), %4096 * 3
                    Frames = wsecli_message:encode(Data, binary),

                    [Frame1, Frame2, Frame3] = Frames,

                    <<Fin1:1, _/bits>> = Frame1,
                    <<Fin2:1, _/bits>> = Frame2,
                    <<Fin3:1, _/bits>> = Frame3,

                    assert_that(Fin1, is(0)),
                    assert_that(Fin2, is(0)),
                    assert_that(Fin3, is(1))
                end),
              it("should set opcode to 'continuation' on all fragments but first", fun() ->
                    Data = crypto:rand_bytes(12288), %4096 * 3
                    Frames = wsecli_message:encode(Data, binary),

                    [Frame1, Frame2, Frame3] = Frames,

                    <<_:4, Opcode1:4, _/binary>> = Frame1,
                    <<_:4, Opcode2:4, _/binary>> = Frame2,
                    <<_:4, Opcode3:4, _/binary>> = Frame3,

                    assert_that(Opcode1, is(2)),
                    assert_that(Opcode2, is(0)),
                    assert_that(Opcode3, is(0))
                end)
          end),
        describe("control messages", fun() ->
              describe("close", fun() ->
                    it("should return a list of one frame", fun() ->
                          [_Frame] = wsecli_message:encode([], close)
                      end),
                    it("should return a close frame", fun() ->
                          [Frame] = wsecli_message:encode([], close),

                          <<Fin:1, Rsv:3, Opcode:4, _/binary>> = Frame,

                          assert_that(Fin, is(1)),
                          assert_that(Rsv, is(0)),
                          assert_that(Opcode, is(8))
                      end),
                    it("should attach application payload", fun() ->
                          [Frame] = wsecli_message:encode({1004, "Chapando el garito"}, close),

                          <<_Fin:1, _Rsv:3, _Opcode:4, 1:1, _PayloadLen:7, _Mask:32, _Payload/binary>> = Frame
                      end)
                end),
              describe("ping", fun() ->
                    it("should return a list of one frame", fun() ->
                          [_Frame] = wsecli_message:encode([], ping)
                      end),
                    it("should return a ping frame", fun() ->
                          [Frame] = wsecli_message:encode([], ping),

                          <<Fin:1, Rsv:3, Opcode:4, _/binary>> = Frame,

                          assert_that(Fin, is(1)),
                          assert_that(Rsv, is(0)),
                          assert_that(Opcode, is(9))
                      end),
                    it("should attach application payload", fun() ->
                          [Frame] = wsecli_message:encode("1234", ping),

                          <<_Fin:1, _Rsv:3, _Opcode:4, 1:1, 4:7, _Mask:32, _Payload:4/binary>> = Frame
                      end)
                end),
              describe("pong", fun() ->
                    it("should return a list of one frame", fun() ->
                          [_Frame] = wsecli_message:encode([], pong)
                      end),
                    it("should return a ping frame", fun() ->
                          [Frame] = wsecli_message:encode([], pong),

                          <<Fin:1, Rsv:3, Opcode:4, _/binary>> = Frame,

                          assert_that(Fin, is(1)),
                          assert_that(Rsv, is(0)),
                          assert_that(Opcode, is(10))
                      end),
                    it("should attach application payload", fun() ->
                          [Frame] = wsecli_message:encode("1234", pong),

                          <<_Fin:1, _Rsv:3, _Opcode:4, 1:1, 4:7, _Mask:32, _Payload:4/binary>> = Frame
                      end)
                end)
          end)
    end),
  describe("decode", fun()->
        describe("fragmented messages", fun() ->
              it("should complain when control messages are fragmented"),
              it("should return a fragmented message with undefined payload when message is not complete", fun() ->
                    Payload = crypto:rand_bytes(20),
                    <<
                    Payload1:10/binary,
                    Payload2:5/binary,
                    _Payload3/binary
                    >> = Payload,

                    FakeFragment1 = get_binary_frame(0, 0, 0, 0, 2, 0, 10, 0, Payload1),
                    FakeFragment2 = get_binary_frame(0, 0, 0, 0, 0, 0, 5, 0, Payload2),

                    Data = <<FakeFragment1/binary, FakeFragment2/binary>>,

                    [Message] = wsecli_message:decode(Data),

                    assert_that(Message#message.type, is(fragmented)),
                    assert_that(length(Message#message.frames), is(2))
                end),
              it("should decode data containing a complete fragmented binary message", fun() ->
                    Payload = crypto:rand_bytes(40),
                    <<
                    Payload1:10/binary,
                    Payload2:10/binary,
                    Payload3:10/binary,
                    Payload4:10/binary
                    >> = Payload,

                    FakeFragment1 = get_binary_frame(0, 0, 0, 0, 2, 0, 10, 0, Payload1),
                    FakeFragment2 = get_binary_frame(0, 0, 0, 0, 0, 0, 10, 0, Payload2),
                    FakeFragment3 = get_binary_frame(0, 0, 0, 0, 0, 0, 10, 0, Payload3),
                    FakeFragment4 = get_binary_frame(1, 0, 0, 0, 0, 0, 10, 0, Payload4),

                    Data = << FakeFragment1/binary, FakeFragment2/binary, FakeFragment3/binary, FakeFragment4/binary>>,

                    [Message] = wsecli_message:decode(Data),

                    assert_that(Message#message.type, is(binary)),
                    assert_that(Message#message.payload, is(Payload))
                end),
              it("should decode data containing a complete fragmented text message", fun() ->
                    Text = "asasdasdasdasdasdasdasdasdasdasdasdasdasdasdasd",
                    Payload = list_to_binary(Text),
                    <<
                    Payload1:5/binary,
                    Payload2:2/binary,
                    Payload3/binary
                    >> = Payload,

                    FakeFragment1 = get_binary_frame(0, 0, 0, 0, 1, 0, byte_size(Payload1), 0, Payload1),
                    FakeFragment2 = get_binary_frame(0, 0, 0, 0, 0, 0, byte_size(Payload2), 0, Payload2),
                    FakeFragment3 = get_binary_frame(1, 0, 0, 0, 0, 0, byte_size(Payload3), 0, Payload3),

                    Data = << FakeFragment1/binary, FakeFragment2/binary, FakeFragment3/binary>>,

                    [Message] = wsecli_message:decode(Data),

                    assert_that(Message#message.type, is(text)),
                    assert_that(Message#message.payload, is(Text))
                end),
              it("should complete a fragmented message", fun() ->
                    Payload = crypto:rand_bytes(20),
                    <<
                    Payload1:10/binary,
                    Payload2:5/binary,
                    Payload3/binary
                    >> = Payload,

                    FakeFragment1 = get_binary_frame(0, 0, 0, 0, 2, 0, 10, 0, Payload1),
                    FakeFragment2 = get_binary_frame(0, 0, 0, 0, 0, 0, 5, 0, Payload2),
                    FakeFragment3 = get_binary_frame(1, 0, 0, 0, 0, 0, 5, 0, Payload3),


                    Data1 = <<FakeFragment1/binary, FakeFragment2/binary>>,
                    Data2 = <<FakeFragment3/binary>>,

                    [Message1] = wsecli_message:decode(Data1),
                    [Message2] = wsecli_message:decode(Data2, Message1),

                    assert_that(Message1#message.type, is(fragmented)),
                    assert_that(Message2#message.type, is(binary)),
                    assert_that(Message2#message.payload, is(Payload))
                end),
              it("should decode data with complete fragmented messages and part of fragmented one", fun() ->
                    BinPayload1 = crypto:rand_bytes(30),
                    <<
                    Payload1:10/binary,
                    Payload2:10/binary,
                    Payload3/binary
                    >> = BinPayload1,

                    FakeFragment1 = get_binary_frame(0, 0, 0, 0, 2, 0, 10, 0, Payload1),
                    FakeFragment2 = get_binary_frame(0, 0, 0, 0, 0, 0, 10, 0, Payload2),
                    FakeFragment3 = get_binary_frame(1, 0, 0, 0, 0, 0, 10, 0, Payload3),

                    BinPayload2 = crypto:rand_bytes(10),
                    <<
                    Payload4:10/binary,
                    _/binary
                    >> = BinPayload2,
                    FakeFragment4 = get_binary_frame(0, 0, 0, 0, 2, 0, 10, 0, Payload4),

                    Data = << FakeFragment1/binary, FakeFragment2/binary, FakeFragment3/binary, FakeFragment4/binary>>,

                    [Message1, Message2] = wsecli_message:decode(Data),

                    assert_that(Message1#message.type, is(binary)),
                    assert_that(Message1#message.payload, is(BinPayload1)),
                    assert_that(Message2#message.type, is(fragmented)),
                    assert_that(length(Message2#message.frames), is(1))
                end)

          end),
        describe("unfragmented messages", fun()->
              it("should decode data containing various text messages", fun()->
                  Text1 = "Churras churras",
                  Payload1 = list_to_binary(Text1),
                  PayloadLength1 = byte_size(Payload1),

                  Text2 = "Pitas pitas",
                  Payload2 = list_to_binary(Text2),
                  PayloadLength2 = byte_size(Payload2),

                  Text3 = "Pero que jallo eh",
                  Payload3 = list_to_binary(Text3),
                  PayloadLength3 = byte_size(Payload3),

                  FakeMessage1 = get_binary_frame(1, 0, 0, 0, 1, 0, PayloadLength1, 0, Payload1),
                  FakeMessage2 = get_binary_frame(1, 0, 0, 0, 1, 0, PayloadLength2, 0, Payload2),
                  FakeMessage3 = get_binary_frame(1, 0, 0, 0, 1, 0, PayloadLength3, 0, Payload3),

                  Data = << FakeMessage1/binary, FakeMessage2/binary, FakeMessage3/binary>>,

                  [Message1, Message2, Message3] = wsecli_message:decode(Data),

                  assert_that(Message1#message.type, is(text)),
                  assert_that(Message1#message.payload, is(Text1)),
                  assert_that(Message2#message.type, is(text)),
                  assert_that(Message2#message.payload, is(Text2)),
                  assert_that(Message3#message.type, is(text)),
                  assert_that(Message3#message.payload, is(Text3))
                end),
              it("should decode data containing text and binary messages", fun()->
                  Text1 = "Churras churras",
                  Payload1 = list_to_binary(Text1),
                  PayloadLength1 = byte_size(Payload1),

                  Payload2 = crypto:rand_bytes(20),
                  PayloadLength2 = 20,

                  Text3 = "Pero que jallo eh",
                  Payload3 = list_to_binary(Text3),
                  PayloadLength3 = byte_size(Payload3),

                  FakeMessage1 = get_binary_frame(1, 0, 0, 0, 1, 0, PayloadLength1, 0, Payload1),
                  FakeMessage2 = get_binary_frame(1, 0, 0, 0, 2, 0, PayloadLength2, 0, Payload2),
                  FakeMessage3 = get_binary_frame(1, 0, 0, 0, 1, 0, PayloadLength3, 0, Payload3),

                  Data = << FakeMessage1/binary, FakeMessage2/binary, FakeMessage3/binary>>,

                  [Message1, Message2, Message3] = wsecli_message:decode(Data),

                  assert_that(Message1#message.type, is(text)),
                  assert_that(Message1#message.payload, is(Text1)),
                  assert_that(Message2#message.type, is(binary)),
                  assert_that(Message2#message.payload, is(Payload2)),
                  assert_that(Message3#message.type, is(text)),
                  assert_that(Message3#message.payload, is(Text3))
                end),
              it("should decode data containing all message types"),
              it("should decode data containing a binary message", fun() ->
                    Payload = crypto:rand_bytes(45),
                    %")
                    FakeMessage = get_binary_frame(1, 0, 0, 0, 2, 0, 45, 0, Payload),
                    [Message] = wsecli_message:decode(FakeMessage),

                    assert_that( Message#message.payload, is(Payload))
                end),
              it("should decode data containing a text message", fun() ->
                    Payload = "Iepa yei!",
                    PayloadLength = length(Payload),
                    PayloadData = list_to_binary(Payload),

                    FakeMessage = get_binary_frame(1, 0, 0, 0, 1, 0, PayloadLength, 0, PayloadData),
                    [Message] = wsecli_message:decode(FakeMessage),

                    assert_that( Message#message.payload, is(Payload))
                end),
              describe("control frames", fun() ->
                    describe("ping", fun() ->
                          it("should return a message with type ping", fun() ->
                                FakeMessage = get_binary_frame(1, 0, 0, 0, 9, 0, 0, 0, <<>>),

                                [Message] = wsecli_message:decode(FakeMessage),

                                assert_that(Message#message.type, is(ping))
                            end)
                      end),
                    describe("pong", fun() ->
                          it("should return a message with type pong", fun() ->
                                FakeMessage = get_binary_frame(1, 0, 0, 0, 10, 0, 0, 0, <<>>),

                                [Message] = wsecli_message:decode(FakeMessage),

                                assert_that(Message#message.type, is(pong))
                            end)
                      end),
                    describe("close", fun() ->
                          it("should return a message with type close", fun() ->
                                FakeMessage = get_binary_frame(1, 0, 0, 0, 8, 0, 0, 0, <<>>),

                                [Message] = wsecli_message:decode(FakeMessage),

                                assert_that(Message#message.type, is(close))
                            end),
                          describe("with payload", fun() ->
                                it("should return the payload a a tuple {Status, Reason}", fun()->
                                      Status = 1004,
                                      Reason = list_to_binary("A tomar por saco"),
                                      Payload = <<Status:16, Reason/binary>>,
                                      PayloadLen = byte_size(Payload),
                                      FakeMessage = get_binary_frame(1, 0, 0, 0, 8, 0, PayloadLen, 0, Payload),

                                      [Message] = wsecli_message:decode(FakeMessage),
                                      {St, Re} = Message#message.payload,

                                      assert_that(St, is(Status)),
                                      assert_that(Re, is("A tomar por saco"))
                                  end)
                            end),
                          describe("without payload", fun() ->
                                it("should return the payload as a tuple {undefined, undefined}", fun() ->
                                      FakeMessage = get_binary_frame(1, 0, 0, 0, 8, 0, 0, 0, <<>>),

                                      [Message] = wsecli_message:decode(FakeMessage),
                                      {Status, Reason} = Message#message.payload,

                                      assert_that(Status, is(undefined)),
                                      assert_that(Reason, is(undefined))
                                  end)
                            end)
                      end)
                end)
          end)
      end).

get_binary_frame(Fin, Rsv1, Rsv2, Rsv3, Opcode, Mask, Length, ExtendedPayloadLength, Payload) ->
  Head = <<Fin:1, Rsv1:1, Rsv2:1, Rsv3:1, Opcode:4, Mask:1, Length:7>>,

  case Length of
    126 ->
      <<Head/binary, ExtendedPayloadLength:16, Payload/binary>>;
    127 ->
      <<Head/binary, ExtendedPayloadLength:64, Payload/binary>>;
    _ ->
      <<Head/binary, Payload/binary>>
  end.
