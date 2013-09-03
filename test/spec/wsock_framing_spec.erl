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

-module(wsock_framing_spec).
-include_lib("espec/include/espec.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include("wsock.hrl").
%-compile([export_all]).

-define(OP_CODE_CONT, 0).
-define(OP_CODE_TEXT, 1).
-define(OP_CODE_BIN, 2).
-define(OP_CODE_CLOSE, 8).
-define(OP_CODE_PING, 9).
-define(OP_CODE_PONG, 10).

spec() ->
  describe("to_binary", fun() ->
        describe("not payload fields", fun() ->
              describe("opcode", fun() ->
                    before_all(fun() ->
                          spec_set(validator, fun(OpcodeType, OpcodeBinType) ->
                                Frame = wsock_framing:frame("aas", [{opcode, OpcodeType}]),
                                <<_:4, Opcode:4, _/binary>> = wsock_framing:to_binary(Frame),
                                assert_that(Opcode, is(OpcodeBinType))
                          end)
                      end),
                    it("should set 'opcode' to close if close frame", fun() ->
                          (spec_get(validator))(close, ?OP_CODE_CLOSE)
                      end),
                    it("should set 'opcode' to pong if pong frame", fun() ->
                          (spec_get(validator))(pong, ?OP_CODE_PONG)
                      end),
                    it("should set 'opcode' to ping if ping frame", fun() ->
                          (spec_get(validator))(ping, ?OP_CODE_PING)
                      end),
                    it("should set 'opcode' to continuation if continuation frame", fun() ->
                          (spec_get(validator))(continuation, ?OP_CODE_CONT)
                      end),
                    it("should set 'opcode' to binary if binary frame", fun() ->
                          (spec_get(validator))(binary, ?OP_CODE_BIN)
                      end),
                    it("should set 'opcode' to text if text frame", fun() ->
                          (spec_get(validator))(text, ?OP_CODE_TEXT)
                      end)
                end),
              describe("mask", fun() ->
                    before_all(fun() ->
                          spec_set(validator, fun(Options, ExpectedMask) ->
                                Frame = wsock_framing:frame("asas", Options),
                                BinFrame = wsock_framing:to_binary(Frame),

                                <<_:8, Mask:1, _:7, _/binary>> = BinFrame,

                                assert_that(Mask, is(ExpectedMask))
                            end)
                      end),
                    it("should set 'mask' to 0 if unmasked data", fun() ->
                          (spec_get(validator))([{opcode, text}], 0)
                      end),
                    it("should set 'mask' to 1 if masked data", fun() ->
                          (spec_get(validator))([{opcode, text}, mask], 1)
                      end)
                end),
              describe("mask key", fun() ->
                    it("should include masking key when 'mask' option is set", fun() ->
                          Frame = wsock_framing:frame("pesto", [mask, {opcode, text}]),
                          BinFrame = wsock_framing:to_binary(Frame),

                          <<_:16, MaskKey:32, _/binary>> = BinFrame,

                          assert_that(MaskKey, is(Frame#frame.masking_key))
                      end),
                    it("should not include masking key when 'mask' option is not set", fun() ->
                          Frame = wsock_framing:frame("conasa", [{opcode, text}]),
                          BinFrame = wsock_framing:to_binary(Frame),

                          <<_:16, Payload/binary>> = BinFrame,

                          assert_that(Payload, is(Frame#frame.payload))
                      end)
                end),
              describe("fin", fun() ->
                    before_all(fun() ->
                          spec_set(validator, fun(Options, Expected) ->
                                Frame = wsock_framing:frame("asas", Options),
                                BinFrame = wsock_framing:to_binary(Frame),

                                <<Fin:1, _/bits>> = BinFrame,

                                assert_that(Fin, is(Expected))
                            end)
                      end),
                    it("should unset 'fin' bit if 'fin' option is  not set", fun() ->
                          (spec_get(validator))([{opcode, text}], 0)
                      end),
                    it("should set 'fin' bit if 'fin' option is set", fun() ->
                          (spec_get(validator))([mask, {opcode, text}], 0)
                      end)
              end),
            describe("rsv", fun() ->
                  it("should set all 3 rsv bits to 0", fun() ->
                        Frame = wsock_framing:frame("asasda", [{opcode, text}]),
                        BinFrame = wsock_framing:to_binary(Frame),

                        <<_:1, Rsv:3, _/bits>> = BinFrame,

                        assert_that(Rsv, is(0))
                    end)
              end),
            describe("payload length", fun()->
                  it("should set payload length of data with <= 125 bytes", fun() ->
                        Frame = wsock_framing:frame("asdasd", [{opcode, text}]),
                        BinFrame = wsock_framing:to_binary(Frame),

                        <<_:9, PayloadLen:7, _/binary>> = BinFrame,

                        assert_that(PayloadLen, is(Frame#frame.payload_len))
                    end),
                  it("should set extended payload length of data with > 125 and <= 65536 bytes", fun() ->
                        Frame = wsock_framing:frame(crypto:rand_bytes(300), [{opcode, binary}]),
                        BinFrame = wsock_framing:to_binary(Frame),

                        <<_:9, PayloadLen:7, ExtendedPLen:16, _/binary>> = BinFrame,

                        assert_that(PayloadLen, is(Frame#frame.payload_len)),
                        assert_that(ExtendedPLen, is(Frame#frame.extended_payload_len))
                    end),
                  it("should set extended payload length cont. of data with > 65536 bytes ", fun() ->
                        Frame = wsock_framing:frame(crypto:rand_bytes(70000), [{opcode, binary}]),
                        BinFrame = wsock_framing:to_binary(Frame),

                        <<_:9, PayloadLen:7, ExtendedPLen:64, _/binary>> = BinFrame,

                        assert_that(PayloadLen, is(Frame#frame.payload_len)),
                        assert_that(ExtendedPLen, is(Frame#frame.extended_payload_len_cont))
                    end)
              end)
          end),
            describe("payload", fun()->
                  before_all(fun() ->
                      spec_set(validator, fun(Size, Options, Callback) ->
                        Frame = wsock_framing:frame(crypto:rand_bytes(Size), Options),
                        BinFrame = wsock_framing:to_binary(Frame),
                        Payload = Callback(BinFrame),
                        assert_that(Payload, is(Frame#frame.payload))
                        end)
                    end),
                  describe("length <= 125 bytes", fun() ->
                        it("should set unmasked data", fun() ->
                              (spec_get(validator))(100, [{opcode, text}], fun(<<_:16, Payload/binary>>) ->
                                    Payload
                                end)
                          end),
                        it("should set masked data", fun()->
                              (spec_get(validator))(100, [mask, {opcode, text}], fun(<<_:48, Payload/binary>>) ->
                                    Payload
                                end)
                          end)
                    end),
                  describe("length 125 > and <= 65536 bytes", fun() ->
                        it("should set unmasked data", fun() ->
                              (spec_get(validator))(300, [{opcode, text}], fun(<<_:32, Payload/binary>>) ->
                                    Payload
                                end)
                          end),
                        it("should set masked data", fun()->
                              (spec_get(validator))(300, [mask, {opcode, text}], fun(<<_:64, Payload/binary>>) ->
                                    Payload
                                end)
                          end)
                    end),
                  describe("length > 65536 bytes", fun() ->
                        it("should set unmasked data", fun() ->
                              (spec_get(validator))(70000, [{opcode, text}], fun(<<_:80, Payload/binary>>) ->
                                    Payload
                                end)
                          end),
                        it("should set masked data", fun()->
                              (spec_get(validator))(70000, [mask, {opcode, text}], fun(<<_:112, Payload/binary>>) ->
                                    Payload
                                end)
                          end)
                    end)
              end)
    end),
  describe("from_binary", fun() ->
        before_all(fun() ->
              spec_set(frame_builder, fun(Fin, Rsv, Opcode, Mask, Data) ->
                    ByteSize =  byte_size(Data),
                    DataLen = case ByteSize of
                      X when X =< 125 -> X;
                      X when X =< 65536  -> 126;
                      X when X > 65536 -> 127
                    end,
                    BinFrame = get_binary_frame(Fin, Rsv, Rsv, Rsv, Opcode, Mask, DataLen, ByteSize, Data),
                    [Frame]  = wsock_framing:from_binary(BinFrame),
                    {BinFrame, Frame}
                end)
          end),
        describe("non payload fields", fun() ->
              describe("fin", fun() ->
                    it("should set fin property to fin bit", fun() ->
                          Data = crypto:rand_bytes(20),
                          DataLen = byte_size(Data),


                          BinFrame = get_binary_frame(1, 0, 0, 0, 1, 0, DataLen, 0, Data),
                          [Frame] = wsock_framing:from_binary(BinFrame),
                          assert_that(Frame#frame.fin, is(1))
                      end)
                end),
              describe("rsv", fun() ->
                    before_all(fun() ->
                          spec_set(generator, fun(Rsv1, Rsv2, Rsv3)->
                                Data = crypto:rand_bytes(20),
                                DataLen = byte_size(Data),

                                BinFrame = get_binary_frame(1, Rsv1, Rsv2, Rsv3, 1, 0, DataLen, 0, Data),
                                [Frame] = wsock_framing:from_binary(BinFrame),
                                Frame
                            end)
                      end),
                    it("should set rsv1 to rsv1 bit", fun() ->
                          Frame = (spec_get(generator))(0, 0, 0),
                          assert_that(Frame#frame.rsv1, is(0))
                      end),
                    it("should set rsv2 to rsv2 bit", fun() ->
                          Frame = (spec_get(generator))(0, 0, 0),
                          assert_that(Frame#frame.rsv2, is(0))
                      end),
                    it("should set rsv3 to rsv3 bit", fun() ->
                          Frame = (spec_get(generator))(0, 0, 0),
                          assert_that(Frame#frame.rsv3, is(0))
                      end)
                end),
              describe("opcode", fun() ->
                    before_all(fun() ->
                          spec_set(validator, fun(OpCode) ->
                                Data = crypto:rand_bytes(20),
                                DataLen = byte_size(Data),
                                BinFrame = get_binary_frame(1, 0, 0, 0, OpCode, 0, DataLen, 0, Data),
                                [Frame] = wsock_framing:from_binary(BinFrame),

                                assert_that(Frame#frame.opcode, is(OpCode))
                            end)
                      end),
                    it("should set opcode to continuation if continuation frame", fun() ->
                          (spec_get(validator))(?OP_CODE_CONT)
                      end),
                    it("should set opcode to text if text frame", fun() ->
                          (spec_get(validator))(?OP_CODE_TEXT)
                      end),
                    it("should set opcode to binary if binary frame", fun() ->
                          (spec_get(validator))(?OP_CODE_BIN)
                      end),
                    it("should set opcode to ping if ping frame", fun() ->
                          (spec_get(validator))(?OP_CODE_PING)
                      end),
                    it("should set opcode to pong if pong frame", fun() ->
                          (spec_get(validator))(?OP_CODE_PONG)
                      end),
                    it("should set opcode to close if close frame", fun() ->
                          (spec_get(validator))(?OP_CODE_CLOSE)
                      end)
                end),
              describe("mask", fun() ->
                    before_all(fun() ->
                          spec_set(validator, fun(Mask) ->
                                {_BinFrame, Frame} = (spec_get(frame_builder))(0, 0, 1, Mask, crypto:rand_bytes(20)),
                                assert_that(Frame#frame.mask, is(Mask))
                            end)
                      end),
                    it("should set mask if masked data", fun() ->
                          (spec_get(validator))(1)
                      end),
                    it("should  not set mask if unmasked data", fun() ->
                          (spec_get(validator))(0)
                      end)
                end),
              describe("payoad lenght", fun()->
                    before_all(fun() ->
                          spec_set(frame, fun(Size) ->
                                {_BinFrame, Frame} = (spec_get(frame_builder))(0, 0, 2, 0, crypto:rand_bytes(Size)),
                                Frame
                            end)
                      end),
                    it("set payload length of data with <= 125 bytes", fun() ->
                          Frame = (spec_get(frame))(100),

                          assert_that(Frame#frame.payload_len, is(100))
                      end),
                    it("set payload length of data with > 125 <= 65536 bytes", fun() ->
                          Frame = (spec_get(frame))(200),

                          assert_that(Frame#frame.payload_len, is(126)),
                          assert_that(Frame#frame.extended_payload_len, is(200))
                      end),
                    it("set payload length of data with > 65536 bytes", fun() ->
                          Frame = (spec_get(frame))(70000),

                          assert_that(Frame#frame.payload_len, is(127)),
                          assert_that(Frame#frame.extended_payload_len_cont, is(70000))
                      end)
                end),
              describe("masking key", fun() ->
                    before_all(fun() ->
                          spec_set(frame, fun(Mask) ->
                                (spec_get(frame_builder))(0, 0, 2, Mask, crypto:rand_bytes(20))
                            end)
                      end),
                    it("should be undefined if data is unmasked", fun() ->
                          {_, Frame} = (spec_get(frame))(0),

                          assert_that(Frame#frame.masking_key, is(undefined))
                      end),
                    it("should be set if data is masked", fun() ->
                          {BinFrame, Frame} = (spec_get(frame))(1),
                          <<_:2/binary, MK:32/integer, _/binary>> = BinFrame,

                          assert_that(Frame#frame.masking_key, is_not(undefined)),
                          assert_that(Frame#frame.masking_key, is(MK))
                      end)
              end)
        end),
        describe("payload", fun() ->
              before_all(fun() ->
                    spec_set(validator, fun(Mask, Size) ->
                          Data = crypto:rand_bytes(Size),
                          {_BinFrame, Frame} = (spec_get(frame_builder))(0, 0, 2, Mask, Data),

                          assert_that(Frame#frame.payload, is(Data))
                      end)
                end),
              describe("when payload length is 0", fun() ->
                    it("should set fragmented to true", fun() ->
                          {_, Frame} = (spec_get(frame_builder))(0, 0, 0, 0, <<>>),

                          assert_that(Frame#frame.fragmented, is(false))
                      end)
                end),
              describe("when payload length <= 125", fun()->
                    it("should set unmasked data", fun()->
                          (spec_get(validator))(0 ,100)
                      end),
                    it("should unmask masked data", fun() ->
                          (spec_get(validator))(1 ,100)
                      end)
                end),
              describe("when payload length > 125 and <= 65536 bytes", fun()->
                    it("should set unmasked data", fun()->
                          (spec_get(validator))(0 , 300)
                      end),
                    it("should unmask masked data", fun() ->
                          (spec_get(validator))(1 , 300)
                      end)
                end),
              describe("when payload length > 65536 bytes", fun()->
                    it("should set unmasked data", fun()->
                          (spec_get(validator))(0 , 70000)
                      end),
                    it("should unmask masked data", fun() ->
                          (spec_get(validator))(1 , 70000)
                      end)
                end)
          end),
        describe("when binary is composed from various frames", fun() ->
              it("should return a list of frame records", fun() ->
                    Text1 = "Jankle jankle",
                    Payload1 = list_to_binary(Text1),
                    PayloadLen1 = byte_size(Payload1),

                    Text2 = "Pasa pra casa",
                    Payload2 = list_to_binary(Text2),
                    PayloadLen2 = byte_size(Payload2),

                    BinFrame1 = get_binary_frame(0, 0, 0, 0, 1, 0, PayloadLen1, 0, Payload1),
                    BinFrame2 = get_binary_frame(1, 0, 0, 0, 0, 0, PayloadLen2, 0, Payload2),

                    BinFrames = <<BinFrame1/binary, BinFrame2/binary>>,

                    [Frame1, Frame2] = wsock_framing:from_binary(BinFrames),

                    assert_that(Frame1#frame.fin, is(0)),
                    assert_that(Frame1#frame.rsv1, is(0)),
                    assert_that(Frame1#frame.rsv2, is(0)),
                    assert_that(Frame1#frame.rsv3, is(0)),
                    assert_that(Frame1#frame.opcode, is(1)),
                    assert_that(Frame1#frame.mask, is(0)),
                    assert_that(Frame1#frame.payload_len, is(PayloadLen1)),
                    assert_that(Frame1#frame.payload, is(Payload1)),

                    assert_that(Frame2#frame.fin, is(1)),
                    assert_that(Frame2#frame.rsv1, is(0)),
                    assert_that(Frame2#frame.rsv2, is(0)),
                    assert_that(Frame2#frame.rsv3, is(0)),
                    assert_that(Frame2#frame.opcode, is(0)),
                    assert_that(Frame2#frame.mask, is(0)),
                    assert_that(Frame2#frame.payload_len, is(PayloadLen2)),
                    assert_that(Frame2#frame.payload, is(Payload2))
                end)
          end),
        describe("when input data is fragmented", fun() ->
              describe("when there's only 8 bits of data", fun() ->
                    it("should return a fragmented frame", fun() ->
                          Data = crypto:rand_bytes(20),
                          DataLen = byte_size(Data),
                          BinFrame = get_binary_frame(1, 0, 0, 0, 1, 0, DataLen, 0, Data),
                          <<Fragment:1/binary, _/binary>> = BinFrame,

                          [Frame] = wsock_framing:from_binary(Fragment),

                          assert_that(Frame#frame.fragmented, is(true)),
                          assert_that(Frame#frame.fin, is(1)),
                          assert_that(Frame#frame.rsv1, is(0)),
                          assert_that(Frame#frame.rsv2, is(0)),
                          assert_that(Frame#frame.rsv3, is(0)),
                          assert_that(Frame#frame.opcode, is(1)),
                          assert_that(Frame#frame.raw, is(<<>>))
                      end)
                end),
              describe("when there's only 16 bits of data", fun() ->
                    it("shoudl return a fragmented frame", fun() ->
                          Data = crypto:rand_bytes(20),
                          DataLen = byte_size(Data),
                          BinFrame = get_binary_frame(1, 0, 0, 0, 1, 0, DataLen, 0, Data),
                          <<Fragment:2/binary, _/binary>> = BinFrame,

                          [Frame] = wsock_framing:from_binary(Fragment),

                          assert_that(Frame#frame.fragmented, is(true)),
                          assert_that(Frame#frame.mask, is(0)),
                          assert_that(Frame#frame.payload_len, is(DataLen)),
                          assert_that(Frame#frame.raw, is(<<>>))
                      end)
                end),
              describe("when there's only 24 bits of data", fun() ->
                    describe("when payload length is extended", fun() ->
                          it("should not set the extended payload length", fun() ->
                            Data = crypto:rand_bytes(140),
                            DataLen = byte_size(Data),
                            BinFrame = get_binary_frame(1, 0, 0, 0, 1, 0, 126, DataLen, Data),
                            <<Fragment:3/binary, _/binary>> = BinFrame,
                            <<_:2/binary, LastFragment/binary>> = Fragment,

                            [Frame] = wsock_framing:from_binary(Fragment),

                            assert_that(Frame#frame.fragmented, is(true)),
                            assert_that(Frame#frame.payload_len, is(126)),
                            assert_that(Frame#frame.extended_payload_len, is(undefined)),
                            assert_that(Frame#frame.raw, is(LastFragment))
                            end)
                      end)
                end),
              describe("when new data is received", fun() ->
                    it("should complete the fragmented frame", fun() ->
                            Data = crypto:rand_bytes(140),
                            DataLen = byte_size(Data),
                            BinFrame = get_binary_frame(1, 0, 0, 0, 1, 0, 126, DataLen, Data),
                            <<FirstFragment:3/binary, SecondFragment/binary>> = BinFrame,

                            [FragmentedFrame] = wsock_framing:from_binary(FirstFragment),
                            [Frame] = wsock_framing:from_binary(SecondFragment, FragmentedFrame),

                            assert_that(Frame#frame.fragmented, is(false)),
                            assert_that(Frame#frame.payload, is(Data)),
                            assert_that(Frame#frame.raw, is(<<>>))
                      end)
                end)
          end)
    end),
  describe("frame", fun() ->
        describe("when no options are passed", fun() ->
              it("should unset fin", fun() ->
                    Frame = wsock_framing:frame("Foo bar"),
                    assert_that(Frame#frame.fin, is(0))
                end),
              it("should set opcode to text on text data", fun()->
                    Frame = wsock_framing:frame("Foo bar"),
                    assert_that(Frame#frame.opcode, is(1))
                end),
              it("should set opcode to binary on binary data", fun()->
                    Frame = wsock_framing:frame(<<"Foo bar">>),
                    assert_that(Frame#frame.opcode, is(2))
                end),
              it("should leave data unmasked", fun() ->
                    Data = "Fofito",
                    Frame = wsock_framing:frame(Data),
                    assert_that(Frame#frame.mask, is(0)),
                    assert_that(Frame#frame.payload, is(list_to_binary(Data))),
                    assert_that(Frame#frame.masking_key, is(undefined))
                end)
          end),
        describe("not payload fields", fun()->
              describe("fin", fun() ->
                    it("should set fin if fin option is present", fun()->
                          Frame = wsock_framing:frame("Foo bar", [fin]),
                          assert_that(Frame#frame.fin, is(1))
                      end),
                    it("should unset fin if fin option is not present", fun() ->
                          Frame = wsock_framing:frame("asdasda", []),
                          assert_that(Frame#frame.fin, is(0))
                      end)
                end),
              describe("rsv", fun() ->
                    it("should set all 3 'rsv' to 0", fun()->
                          Data = "Foo bar",
                          Frame = wsock_framing:frame(Data),
                          assert_that(Frame#frame.rsv1, is(0)),
                          assert_that(Frame#frame.rsv2, is(0)),
                          assert_that(Frame#frame.rsv3, is(0))
                      end)
                end),
              describe("opcode", fun() ->
                    before_all(fun() ->
                          spec_set(validator, fun(Options, Expected) ->
                                Frame = wsock_framing:frame("Foo bar", Options),
                                assert_that(Frame#frame.opcode, is(Expected))
                            end)
                      end),
                    it("should set opcode to text if opcode option is text", fun()->
                          (spec_get(validator))([{opcode, text}], 1)
                      end),
                    it("should set opcode to binary if opcode option is binary", fun()->
                          (spec_get(validator))([{opcode, binary}], 2)
                      end),
                    it("should set opcode to ping if opcode option is ping", fun()->
                          (spec_get(validator))([{opcode, ping}], 9)
                      end),
                    it("should set opcode to pong if opcode option is pong", fun() ->
                          (spec_get(validator))([{opcode, pong}], 10)
                      end),
                    it("should set opcode to close if opcode option is close", fun() ->
                          (spec_get(validator))([{opcode, close}], 8)
                      end),
                    it("should set opcode to continuation if opcode option is continuation", fun()->
                          (spec_get(validator))([{opcode, continuation}], 0)
                      end)
                end),
              describe("mask", fun() ->
                    it("should be unset if mask option is not present", fun() ->
                          Frame = wsock_framing:frame("asdasd", []),
                          assert_that(Frame#frame.mask, is(0))
                      end),
                    it("should set mask if mask option is present", fun() ->
                          Frame = wsock_framing:frame("assd", [mask]),
                          assert_that(Frame#frame.mask, is(1))
                      end)
                end),
              describe("masking key", fun() ->
                    it("should be unset if mask option is not present", fun() ->
                          Frame = wsock_framing:frame("asda", []),
                          assert_that(Frame#frame.masking_key, is(undefined))
                      end),
                    it("should be set if mask option is present", fun() ->
                          Frame = wsock_framing:frame("asads", [mask]),
                          assert_that(Frame#frame.masking_key, is_not(undefined))
                      end)
                end),
              describe("payload length", fun() ->
                    before_all(fun() ->
                        spec_set(validator, fun(Size, PL, EPL, EPLC) ->
                              Frame = wsock_framing:frame(crypto:rand_bytes(Size), []),
                              assert_that(Frame#frame.payload_len, is(PL)),
                              assert_that(Frame#frame.extended_payload_len, is(EPL)),
                              assert_that(Frame#frame.extended_payload_len_cont, is(EPLC))
                          end)
                      end),
                    it("should set payload length of data <= 125 bytes", fun() ->
                          (spec_get(validator))(100, 100, 0, 0)
                      end),
                    it("should set payload length of data > 125 and <= 65536 bytes", fun() ->
                          (spec_get(validator))(1000, 126, 1000, 0)
                      end),
                    it("should set payload length of data > 65536 bytes", fun() ->
                          (spec_get(validator))(70000, 127, 0, 70000)
                      end)
                end)
          end),
        describe("payload", fun()->
              it("should set unmasked payload", fun() ->
                    Data = crypto:rand_bytes(100),
                    Frame = wsock_framing:frame(Data, []),

                    assert_that(Frame#frame.payload, is(Data))
                end),
              it("should set masked payload", fun() ->
                    Data = crypto:rand_bytes(100),
                    Frame = wsock_framing:frame(Data, [mask]),

                    MaskedData = mask(Data, Frame#frame.masking_key, <<>>),

                    assert_that(Frame#frame.payload, is(MaskedData))
                end)
          end),
        describe("control frames", fun()->
              describe("close", fun() ->
                    it("should frame closes without payload", fun() ->
                          Frame = wsock_framing:frame([], [fin, {opcode, close}]),

                          assert_that(Frame#frame.fin, is(1)),
                          assert_that(Frame#frame.rsv1, is(0)),
                          assert_that(Frame#frame.rsv2, is(0)),
                          assert_that(Frame#frame.rsv3, is(0)),
                          assert_that(Frame#frame.opcode, is(8)),
                          assert_that(Frame#frame.mask, is(0))
                      end),
                    it("should frames closes with payload", fun() ->
                          Frame = wsock_framing:frame({1000, "Closing this shit"}, [mask, fin, {opcode, close}]),
                          %mask function also unmask the data
                          <<Code:16, Reason/binary>> = mask(
                            Frame#frame.payload,
                            Frame#frame.masking_key,
                            <<>>),

                          assert_that(Frame#frame.fin, is(1)),
                          assert_that(Frame#frame.rsv1, is(0)),
                          assert_that(Frame#frame.rsv2, is(0)),
                          assert_that(Frame#frame.rsv3, is(0)),
                          assert_that(Frame#frame.opcode, is(8)),
                          assert_that(Code, is(1000)),
                          assert_that(Frame#frame.mask, is(1)),
                          assert_that(binary_to_list(Reason), is("Closing this shit"))
                      end)
                end),
              it("should not allow payload size over 125 bytes")
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

%get_random_string(Length) ->
%  AllowedChars = "qwertyQWERTY1234567890",
%  lists:foldl(fun(_, Acc) ->
%        [lists:nth(random:uniform(length(AllowedChars)),
%            AllowedChars)]
%        ++ Acc
%    end, [], lists:seq(1, Length)).

%mask(Bin, MaskKey, Acc) ->
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
