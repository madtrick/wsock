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

-module(wsock_message_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include("wsock.hrl").

-define(FRAGMENT_SIZE, 4096).

spec() ->
  describe("encode", fun() ->
        before_each(fun()->
              meck:new(wsock_framing, [passthrough])
          end),

        after_each(fun()->
              meck:unload(wsock_framing)
          end),

        it("should return an error if no datatype option is given", fun() ->
              Return = wsock_message:encode("motosicleta man", []),
              assert_that(Return, is({error, missing_datatype}))
          end),
        it("should mask data if 'mask' option is present", fun() ->
              wsock_message:encode("asdasda", [text ,mask]),
              [_Data, Options] = meck_arguments(wsock_framing, frame),
              assert_that(proplists:get_value(mask, Options), is(true))
          end),
        it("should not mask data if 'mask' option is not present", fun() ->
              wsock_message:encode("frotisfrotis", [text]),
              [_Data, Options] = meck_arguments(wsock_framing, frame),
              assert_that(proplists:get_value(mask, Options), is(undefined))
          end),
        it("should set opcode to 'text' if type is text", fun() ->
              wsock_message:encode("asadsd", [text]),
              [_Data, Options] = meck_arguments(wsock_framing, frame),
              assert_that(proplists:get_value(opcode, Options), is(text))
          end),
        it("should set opcode to 'binary' if type is binary", fun() ->
              wsock_message:encode(<<"asdasd">>, [binary]),
              [_Data, Options] = meck_arguments(wsock_framing, frame),
              assert_that(proplists:get_value(opcode, Options), is(binary))
          end),
        describe("when payload size is <= fragment size", fun()->
              it("should return a list with only one binary fragment", fun()->
                    Data = "Foo bar",
                    [BinFrame | []] = wsock_message:encode(Data, [text]),
                    assert_that(byte_size(list_to_binary(Data)),is(less_than(?FRAGMENT_SIZE))),
                    assert_that(is_binary(BinFrame), is(true)),
                    assert_that(meck:called(wsock_framing, to_binary, '_'), is(true)),
                    assert_that(meck:called(wsock_framing, frame, '_'), is(true))
                end),
              it("should set opcode to 'type'", fun() ->
                    Data = "Foo bar",
                    [Frame] = wsock_message:encode(Data, [text]),

                    <<_:4, Opcode:4, _/binary>> = Frame,

                    assert_that(Opcode, is(1))
                end),
              it("should set fin", fun()->
                    Data = "Foo bar",
                    [Frame] = wsock_message:encode(Data, [text]),

                    <<Fin:1, _/bits>> = Frame,

                    assert_that(Fin, is(1))
                end)
          end),
        describe("when payload size is > fragment size", fun() ->
              it("should return a list of binary fragments", fun()->
                    Data = crypto:rand_bytes(5000),
                    Frames = wsock_message:encode(Data, [binary]),
                    assert_that(meck:called(wsock_framing, to_binary, '_'), is(true)),
                    assert_that(meck:called(wsock_framing, frame, '_'), is(true)),
                    assert_that(length(Frames), is(2))
                end),
              it("should set a payload of 4096 bytes or less on each fragment", fun() ->
                    Data = crypto:rand_bytes(?FRAGMENT_SIZE*3),
                    Frames = wsock_message:encode(Data, [binary]),

                    [Frame1, Frame2, Frame3] = Frames,

                    <<_:32, Payload1/binary>> = Frame1,
                    <<_:32, Payload2/binary>> = Frame2,
                    <<_:32, Payload3/binary>> = Frame3,

                    assert_that(byte_size(Payload1), is(?FRAGMENT_SIZE)),
                    assert_that(byte_size(Payload2), is(?FRAGMENT_SIZE)),
                    assert_that(byte_size(Payload3), is(?FRAGMENT_SIZE))
                end),
              it("should set opcode to 'type' on the first fragment", fun()->
                    Data = crypto:rand_bytes(5000),
                    Frames = wsock_message:encode(Data, [binary]),

                    [FirstFragment | _ ] = Frames,

                    <<_:4, Opcode:4, _/binary>> = FirstFragment,

                    assert_that(Opcode, is(2))
                end),
              it("should unset fin on all fragments but last", fun() ->
                    Data = crypto:rand_bytes(12288), %4096 * 3
                    Frames = wsock_message:encode(Data, [binary]),

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
                    Frames = wsock_message:encode(Data, [binary]),

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
                          [_Frame] = wsock_message:encode([], [close])
                      end),
                    it("should return a close frame", fun() ->
                          [Frame] = wsock_message:encode([], [close]),

                          <<Fin:1, Rsv:3, Opcode:4, _/binary>> = Frame,

                          assert_that(Fin, is(1)),
                          assert_that(Rsv, is(0)),
                          assert_that(Opcode, is(8))
                      end),
                    it("should attach application payload", fun() ->
                          [Frame] = wsock_message:encode({1004, "Chapando el garito"}, [mask, close]),

                          <<_Fin:1, _Rsv:3, _Opcode:4, 1:1, _PayloadLen:7, _Mask:32, _Payload/binary>> = Frame
                      end)
                end),
              describe("ping", fun() ->
                    it("should return a list of one frame", fun() ->
                          [_Frame] = wsock_message:encode([], [ping])
                      end),
                    it("should return a ping frame", fun() ->
                          [Frame] = wsock_message:encode([], [ping]),

                          <<Fin:1, Rsv:3, Opcode:4, _/binary>> = Frame,

                          assert_that(Fin, is(1)),
                          assert_that(Rsv, is(0)),
                          assert_that(Opcode, is(9))
                      end),
                    it("should attach application payload", fun() ->
                          [Frame] = wsock_message:encode("1234", [mask, ping]),

                          <<_Fin:1, _Rsv:3, _Opcode:4, 1:1, 4:7, _Mask:32, _Payload:4/binary>> = Frame
                      end)
                end),
              describe("pong", fun() ->
                    it("should return a list of one frame", fun() ->
                          [_Frame] = wsock_message:encode([], [pong])
                      end),
                    it("should return a ping frame", fun() ->
                          [Frame] = wsock_message:encode([], [pong]),

                          <<Fin:1, Rsv:3, Opcode:4, _/binary>> = Frame,

                          assert_that(Fin, is(1)),
                          assert_that(Rsv, is(0)),
                          assert_that(Opcode, is(10))
                      end),
                    it("should attach application payload", fun() ->
                          [Frame] = wsock_message:encode("1234", [mask, pong]),

                          <<_Fin:1, _Rsv:3, _Opcode:4, 1:1, 4:7, _Mask:32, _Payload:4/binary>> = Frame
                      end)
                end)
          end)
    end),
  describe("decode", fun()->
        it("should return an error if unexpected masking", fun() ->
              Payload = crypto:rand_bytes(20),
              Frame = get_binary_frame(0, 0, 0, 0, 2, 1, 20, 0, Payload),

              Response = wsock_message:decode(Frame, []),

              assert_that(Response, is({error, frames_masked}))
          end),
        it("should return an error if expected masking", fun() ->
              Payload = crypto:rand_bytes(20),
              Frame = get_binary_frame(0, 0, 0, 0, 2, 0, 20, 0, Payload),

              Response = wsock_message:decode(Frame, [masked]),

              assert_that(Response, is({error, frames_unmasked}))
          end),
        it("should decode masked messages", fun() ->
            Payload = crypto:rand_bytes(20),
            Fragment = get_binary_frame(0, 0, 0, 0, 2, 1, 20, 0, Payload),

            [_Message] = wsock_message:decode(Fragment, [masked])
          end),
        it("should decode unmasked messages", fun() ->
              Payload = crypto:rand_bytes(20),
              Frame = get_binary_frame(0, 0, 0, 0, 2, 0, 20, 0, Payload),

              [_Message] = wsock_message:decode(Frame, [])
          end),
        describe("fragmented messages", fun() ->
              %describe("when they are control messages", )
              it("should complain when control messages are fragmented", fun() ->
                    Data = crypto:rand_bytes(10),
                    Frame1 = get_binary_frame(0, 0, 0, 0, 8, 0, 10, 0, Data),
                    Frame2 = get_binary_frame(1, 0, 0, 0, 0, 0, 10, 0, Data),

                    Message = <<Frame1/binary, Frame2/binary>>,

                    Return = wsock_message:decode(Message, []),

                    assert_that(Return, is({error, fragmented_control_message}))
                end),
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

                    [Message] = wsock_message:decode(Data, []),

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

                    [Message] = wsock_message:decode(Data, []),

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

                    [Message] = wsock_message:decode(Data, []),

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

                    [Message1] = wsock_message:decode(Data1, []),
                    [Message2] = wsock_message:decode(Data2, Message1, []),

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
                    FakeFragment4 = get_binary_frame(0, 0, 0, 0, 2, 0, 10, 0, BinPayload2),

                    Data = << FakeFragment1/binary, FakeFragment2/binary, FakeFragment3/binary, FakeFragment4/binary>>,

                    [Message1, Message2] = wsock_message:decode(Data, []),

                    assert_that(Message1#message.type, is(binary)),
                    assert_that(Message1#message.payload, is(BinPayload1)),
                    assert_that(length(Message1#message.frames), is(3)),
                    assert_that(Message2#message.type, is(fragmented)),
                    assert_that(length(Message2#message.frames), is(1))
                end),
              describe("fragmented frames", fun() ->
                    it("should return a fragmented message", fun() ->
                          FakeFrame = get_binary_frame(0, 0, 0, 0, 2, 0, 10, 0, crypto:rand_bytes(10)),

                          <<Data:1/binary, _/binary>> = FakeFrame,
                          [Message] = wsock_message:decode(Data, []),

                          assert_that(Message#message.type, is(fragmented)),
                          assert_that(length(Message#message.frames), is(1))
                      end),
                    it("should return a fragmented message that is made up of more that one frame", fun() ->
                          Payload = crypto:rand_bytes(10),
                          FakeFrame = get_binary_frame(0, 0, 0, 0, 2, 0, 10, 0, Payload),

                          <<FirstFragment:1/binary, SecondFragment/binary>> = FakeFrame,
                          [FragmentedMessage] = wsock_message:decode(FirstFragment, []),
                          [Message] = wsock_message:decode(SecondFragment, FragmentedMessage, []),

                          assert_that(Message#message.type, is(fragmented)),
                          assert_that(length(Message#message.frames), is(1))
                      end),
                    it("should complete a fragmented message that is made up of one frame", fun() ->
                          Payload = crypto:rand_bytes(10),
                          FakeFrame = get_binary_frame(1, 0, 0, 0, 2, 0, 10, 0, Payload),

                          <<FirstFragment:1/binary, SecondFragment/binary>> = FakeFrame,
                          [FragmentedMessage] = wsock_message:decode(FirstFragment, []),
                          [Message] = wsock_message:decode(SecondFragment, FragmentedMessage, []),

                          assert_that(Message#message.type, is(binary)),
                          assert_that(length(Message#message.frames), is(1)),
                          assert_that(Message#message.payload, is(Payload))
                      end),
                    it("should complete a fragmented message that is made up of more than one frame", fun() ->
                          Data = crypto:rand_bytes(20),
                          <<DataFrame1:10/binary, DataFrame2/binary>> = Data,
                          FakeFrame1 = get_binary_frame(0, 0, 0, 0, 2, 0, 10, 0, DataFrame1),
                          FakeFrame2 = get_binary_frame(1, 0, 0, 0, 0, 0, 10, 0, DataFrame2),

                          <<FakeFrameFragment1:3/binary, FakeFrameFragment2/binary>> = FakeFrame2,


                          InputData = <<FakeFrame1/binary, FakeFrameFragment1/binary>>,
                          [FragmentedMessage] = wsock_message:decode(InputData, []),
                          [Message] = wsock_message:decode(FakeFrameFragment2, FragmentedMessage, []),

                          assert_that(Message#message.type, is(binary)),
                          assert_that(length(Message#message.frames), is(2)),
                          assert_that(Message#message.payload, is(Data))
                      end)
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

                  [Message1, Message2, Message3] = wsock_message:decode(Data, []),

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

                  [Message1, Message2, Message3] = wsock_message:decode(Data, []),

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
                    [Message] = wsock_message:decode(FakeMessage, []),

                    assert_that( Message#message.payload, is(Payload))
                end),
              it("should decode data containing a text message", fun() ->
                    Payload = "Iepa yei!",
                    PayloadLength = length(Payload),
                    PayloadData = list_to_binary(Payload),

                    FakeMessage = get_binary_frame(1, 0, 0, 0, 1, 0, PayloadLength, 0, PayloadData),
                    [Message] = wsock_message:decode(FakeMessage, []),

                    assert_that( Message#message.payload, is(Payload))
                end),
              describe("control frames", fun() ->
                    describe("ping", fun() ->
                          it("should return a message with type ping", fun() ->
                                FakeMessage = get_binary_frame(1, 0, 0, 0, 9, 0, 0, 0, <<>>),

                                [Message] = wsock_message:decode(FakeMessage, []),

                                assert_that(Message#message.type, is(ping))
                            end)
                      end),
                    describe("pong", fun() ->
                          it("should return a message with type pong", fun() ->
                                FakeMessage = get_binary_frame(1, 0, 0, 0, 10, 0, 0, 0, <<>>),

                                [Message] = wsock_message:decode(FakeMessage, []),

                                assert_that(Message#message.type, is(pong))
                            end)
                      end),
                    describe("close", fun() ->
                          it("should return a message with type close", fun() ->
                                FakeMessage = get_binary_frame(1, 0, 0, 0, 8, 0, 0, 0, <<>>),

                                [Message] = wsock_message:decode(FakeMessage, []),

                                assert_that(Message#message.type, is(close))
                            end),
                          describe("with payload", fun() ->
                                it("should return the payload a a tuple {Status, Reason}", fun()->
                                      Status = 1004,
                                      Reason = list_to_binary("A tomar por saco"),
                                      Payload = <<Status:16, Reason/binary>>,
                                      PayloadLen = byte_size(Payload),
                                      FakeMessage = get_binary_frame(1, 0, 0, 0, 8, 0, PayloadLen, 0, Payload),

                                      [Message] = wsock_message:decode(FakeMessage, []),
                                      {St, Re} = Message#message.payload,

                                      assert_that(St, is(Status)),
                                      assert_that(Re, is("A tomar por saco"))
                                  end)
                            end),
                          describe("without payload", fun() ->
                                it("should return the payload as a tuple {undefined, undefined}", fun() ->
                                      FakeMessage = get_binary_frame(1, 0, 0, 0, 8, 0, 0, 0, <<>>),

                                      [Message] = wsock_message:decode(FakeMessage, []),
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
  TempBin =  case Length of
    126 ->
      <<Head/binary, ExtendedPayloadLength:16>>;
    127 ->
      <<Head/binary, ExtendedPayloadLength:64>>;
    _ ->
      <<Head/binary>>
  end,

  case Mask of
    0 ->
      <<TempBin/binary, Payload/binary>>;
    1 ->
      <<Mk:32>> = crypto:rand_bytes(4),
      MaskedPayload =  mask(Payload, Mk, <<>>),
      <<TempBin/binary, Mk:32, MaskedPayload/binary>>
  end.

mask(<<Data:32, Rest/bits>>, MaskKey, Acc) ->
  T = Data bxor MaskKey,
  mask(Rest, MaskKey, <<Acc/binary, T:32>>);

mask(<< Data:24>>, MaskKey, Acc) ->
  <<MaskKey2:24, _/bits>> = <<MaskKey:32>>,
  T = Data bxor MaskKey2,
  <<Acc/binary, T:24>>;

mask(<< Data:16>>, MaskKey, Acc) ->
  <<MaskKey2:16, _/bits>> = <<MaskKey:32>>,
  T = Data bxor MaskKey2,
  <<Acc/binary, T:16>>;

mask(<< Data:8>>, MaskKey, Acc) ->
  <<MaskKey2:8, _/bits>> = <<MaskKey:32>>,
  T = Data bxor MaskKey2,
  <<Acc/binary, T:8>>;

mask(<<>>, _, Acc) ->
  Acc.
meck_arguments(Module, Function) ->
  History = meck:history(Module),

  [Args] = [ X || {_, {_, F, X}, _} <- History, F == Function],
  Args.
