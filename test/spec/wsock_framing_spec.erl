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
        describe("when payload length <= 125", fun()->
              it("should return a frame record", fun() ->
                    Text = "Jankle jankle",
                    Payload = list_to_binary(Text),
                    PayloadLen = byte_size(Payload),

                    BinFrame = get_binary_frame(1, 0, 0, 0, 1, 0, PayloadLen, 0, Payload),

                    [Frame] = wsock_framing:from_binary(BinFrame),

                    assert_that(Frame#frame.fin, is(1)),
                    assert_that(Frame#frame.rsv1, is(0)),
                    assert_that(Frame#frame.rsv2, is(0)),
                    assert_that(Frame#frame.rsv3, is(0)),
                    assert_that(Frame#frame.opcode, is(1)),
                    assert_that(Frame#frame.mask, is(0)),
                    assert_that(Frame#frame.payload_len, is(PayloadLen)),
                    assert_that(Frame#frame.payload, is(Payload))
                end)
          end),
        describe("when payload length > 125 and <= 65536", fun()->
              it("should return a frame record", fun() ->
                    Data = get_random_string(4096),
                    Payload = list_to_binary(Data),
                    PayloadLen = 126,
                    ExtendedPayloadLen = byte_size(Payload),

                    BinFrame = get_binary_frame(1, 0, 0, 0, 1, 0, PayloadLen, ExtendedPayloadLen, Payload),

                    [Frame] = wsock_framing:from_binary(BinFrame),

                    assert_that(Frame#frame.fin, is(1)),
                    assert_that(Frame#frame.rsv1, is(0)),
                    assert_that(Frame#frame.rsv2, is(0)),
                    assert_that(Frame#frame.rsv3, is(0)),
                    assert_that(Frame#frame.opcode, is(1)),
                    assert_that(Frame#frame.mask, is(0)),
                    assert_that(Frame#frame.payload_len, is(126)),
                    assert_that(Frame#frame.extended_payload_len, is(ExtendedPayloadLen)),
                    assert_that(Frame#frame.payload, is(Payload))
                end)
          end),
        describe("when payload length > 65536", fun()->
              it("should return a frame record", fun() ->
                    Data = get_random_string(70000),
                    Payload = list_to_binary(Data),
                    PayloadLen = 127,
                    ExtendedPayloadLenCont = byte_size(Payload),

                    BinFrame = get_binary_frame(1, 0, 0, 0, 1, 0, PayloadLen, ExtendedPayloadLenCont, Payload),

                    [Frame] = wsock_framing:from_binary(BinFrame),

                    assert_that(Frame#frame.fin, is(1)),
                    assert_that(Frame#frame.rsv1, is(0)),
                    assert_that(Frame#frame.rsv2, is(0)),
                    assert_that(Frame#frame.rsv3, is(0)),
                    assert_that(Frame#frame.opcode, is(1)),
                    assert_that(Frame#frame.mask, is(0)),
                    assert_that(Frame#frame.payload_len, is(127)),
                    assert_that(Frame#frame.extended_payload_len_cont, is(ExtendedPayloadLenCont)),
                    assert_that(Frame#frame.payload, is(Payload))
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
                    %it("should mask data if 'mask' option is present", fun() ->
                          Frame = wsock_framing:frame("assd", [mask]),
                          assert_that(Frame#frame.mask, is(1))
                          %Data = list_to_binary("Era unha jrota"),
                          %Frame = wsock_framing:frame(Data, [mask]),
                          %assert_that(Frame#frame.mask, is(1)),
                          %assert_that(Frame#frame.payload, is(mask(Data, Frame#frame.masking_key, <<>>)))
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
              %it("should set the FIN bit when the message is not fragmented", fun()->
              %      Data = "Foo bar",

              %      Frame = wsock_framing:frame(Data),
              %      assert_that(Frame#frame.fin, is(1))
              %  end),
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
              %it("should set opcode to TEXT", fun() ->
              %      Data = "Foo bar",

              %      Frame = wsock_framing:frame(Data),
              %      assert_that(Frame#frame.opcode, is(1))
              %  end),
              %describe("data length <= 125", fun() ->
              %      it("should set data length in payload length", fun() ->
              %            Data = "Foo bar",
              %            Frame = wsock_framing:frame(Data),
              %            assert_that(Frame#frame.payload_len, is(string:len(Data)))
              %        end),
              %      it("should set 0 in extended payload length", fun()->
              %            Data = "Foo bar",
              %            Frame = wsock_framing:frame(Data),
              %            assert_that(Frame#frame.extended_payload_len, is(0))
              %          %)                     end),
              %      end),
              %      it("should set 0 in extended payload length cont.", fun()->
              %            Data = "Foo bar",
              %            Frame = wsock_framing:frame(Data),
              %            assert_that(Frame#frame.extended_payload_len_cont, is(0))
              %        end)
              %  end),
              %describe("data length > 125", fun() ->
              %      it("should set in payload_len the value 126", fun() ->
              %            Data = get_random_string(320),
              %            Frame = wsock_framing:frame(Data),
              %            assert_that(Frame#frame.payload_len, is(126))
              %          )      end),
              %      it("should set data length in extended payload length", fun()->
              %            Data = get_random_string(455),
              %            Frame = wsock_framing:frame(Data),
              %            assert_that(Frame#frame.extended_payload_len, is(455))
              %        end),
              %      it("should set 0 in extended payload length cont.", fun()->
              %            Data = "Foo bar",
              %            Frame = wsock_framing:frame(Data),
              %            assert_that(Frame#frame.extended_payload_len_cont, is(0))
              %        end)
              %  end),
              %describe("data length > 65536", fun() ->
              %      it("should set in payload_len the value 127", fun() ->
              %            Data = get_random_string(70000),
              %            Frame = wsock_framing:frame(Data),
              %            assert_that(Frame#frame.payload_len, is(127))
              %        end),
              %      it("should set 0 in extended payload length", fun()->
              %            Data = get_random_string(68000),
              %            Frame = wsock_framing:frame(Data),
              %            assert_that(Frame#frame.extended_payload_len, is(0))
              %        end),
              %      it("should set data length in extended payload length cont.", fun()->
              %            Data = get_random_string(75000),
              %            Frame = wsock_framing:frame(Data),
              %            assert_that(Frame#frame.extended_payload_len_cont, is(75000))
              %        end)
              %  end),
              %describe("masking", fun() ->
              %      it("should set MASK", fun() ->
              %            Data = "Foo bar",
              %            Frame = wsock_framing:frame(Data, [mask]),
              %            assert_that(Frame#frame.mask,is(1))
              %        end),
              %      it("should mask the payload", fun() ->
              %            Data = "Foo bar",
              %            BinData = list_to_binary(Data),
              %            Frame = wsock_framing:frame(Data, [mask]),
              %            MaskKey = Frame#frame.masking_key,
              %            assert_that(Frame#frame.payload, is(mask(BinData, MaskKey, <<>>)))
              %        end),
              %      it("shouldn't affect payload length", fun() ->
              %            Data = "Foo Bar",
              %            BinData = list_to_binary(Data),
              %            Frame = wsock_framing:frame(Data, [mask]),
              %            assert_that(byte_size(Frame#frame.payload), is(byte_size(BinData)))
              %        end)
              %  end)
          end),
        %it("should set opcode to BINARY when data is binary", fun() ->
        %      Data = crypto:rand_bytes(64),

        %      Frame = wsock_framing:frame(Data),
        %      assert_that(Frame#frame.opcode, is(2))
        %  end),
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
              %describe("ping", fun() ->
              %      it("should frame pings without payload", fun() ->
              %            Frame = wsock_framing:frame([], [fin, {opcode, ping}]),

              %            assert_that(Frame#frame.fin, is(1)),
              %            assert_that(Frame#frame.rsv1, is(0)),
              %            assert_that(Frame#frame.rsv2, is(0)),
              %            assert_that(Frame#frame.rsv3, is(0)),
              %            assert_that(Frame#frame.opcode, is(9)),
              %            assert_that(Frame#frame.mask, is(0)),
              %            %assert_that(Frame#frame.payload, is(undefined))
              %            assert_that(Frame#frame.payload, is(<<>>))
              %        end),
              %      it("should frame pings with payload", fun() ->
              %            Frame = wsock_framing:frame("Andale", [mask, fin, {opcode, ping}]),

              %            MaskedData = mask(
              %              list_to_binary("Andale"),
              %              Frame#frame.masking_key,
              %              <<>>),

              %            assert_that(Frame#frame.fin, is(1)),
              %            assert_that(Frame#frame.rsv1, is(0)),
              %            assert_that(Frame#frame.rsv2, is(0)),
              %            assert_that(Frame#frame.rsv3, is(0)),
              %            assert_that(Frame#frame.opcode, is(9)),
              %            assert_that(Frame#frame.mask, is(1)),
              %            assert_that(Frame#frame.payload, is(MaskedData))
              %        end)
              %  end),
              %describe("pong", fun() ->
              %      it("should frame pong without payload", fun() ->
              %            Frame = wsock_framing:frame([], [fin, {opcode, pong}]),

              %            assert_that(Frame#frame.fin, is(1)),
              %            assert_that(Frame#frame.rsv1, is(0)),
              %            assert_that(Frame#frame.rsv2, is(0)),
              %            assert_that(Frame#frame.rsv3, is(0)),
              %            assert_that(Frame#frame.opcode, is(10)),
              %            assert_that(Frame#frame.mask, is(0)),
              %            assert_that(Frame#frame.payload, is(<<>>))
              %            %assert_that(Frame#frame.payload, is(undefined))
              %        end),
              %      it("should fram pongs with payload", fun() ->
              %            Frame = wsock_framing:frame("Andale", [mask, fin, {opcode, pong}]),

              %            MaskedData = mask(
              %              list_to_binary("Andale"),
              %              Frame#frame.masking_key,
              %              <<>>),

              %            assert_that(Frame#frame.fin, is(1)),
              %            assert_that(Frame#frame.rsv1, is(0)),
              %            assert_that(Frame#frame.rsv2, is(0)),
              %            assert_that(Frame#frame.rsv3, is(0)),
              %            assert_that(Frame#frame.opcode, is(10)),
              %            assert_that(Frame#frame.mask, is(1)),
              %            assert_that(Frame#frame.payload, is(MaskedData))
              %        end)
              %  end),
              it("should not allow payload size over 125 bytes")
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

get_random_string(Length) ->
  AllowedChars = "qwertyQWERTY1234567890",
  lists:foldl(fun(_, Acc) ->
        [lists:nth(random:uniform(length(AllowedChars)),
            AllowedChars)]
        ++ Acc
    end, [], lists:seq(1, Length)).

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
