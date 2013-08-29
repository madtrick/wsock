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

-module(wsock_message).
-include("wsock.hrl").

-export([encode/2, decode/2, decode/3]).
%-export([encode/3]).

-define(FRAGMENT_SIZE, 4096).
-type message_type() :: begin_message | continue_message.

-spec encode(Data::string() | binary(), Options::list()) -> [binary()] | {error, missing_datatype}.
encode(Data, Options) when is_list(Data) ->
  encode(list_to_binary(Data), Options);

encode(Data, Options) ->
  case extract_type(Options) of
    error ->
      {error, missing_datatype};
    {Type, BaseOptions} ->
      lists:reverse(encode(Data, Type, BaseOptions, []))
  end.


-spec decode(Data::binary(), Options::list()) -> list(#message{}).
decode(Data, Options) ->
  Masked = proplists:get_value(masked, Options, false),
  decode(Data, begin_message, #message{}, Masked).

-spec decode(Data::binary(), Message::#message{}, Options::list()) -> list(#message{}).
decode(Data, Message, Options) ->
  Masked = proplists:get_value(masked, Options, false),
  decode(Data, continue_message, Message, Masked).

%===================
% Internal
%===================

extract_type(Options) ->
  Types = [text, binary, close, ping, pong],
  Type = lists:filter(fun(E) ->
        true == proplists:get_value(E, Options)
    end, Types),

  case Type of
    [] -> error;
    [T] ->
      OptionsWithoutType = proplists:delete(T, Options),
      {T, OptionsWithoutType}
  end.

encode(Data, Type, BaseOptions, _Acc) when Type =:= ping ; Type =:= pong ; Type =:= close->
  [frame(Data, [ fin, {opcode, Type} | BaseOptions])];

encode(<<Data:?FRAGMENT_SIZE/binary>>, Type, BaseOptions, Acc) ->
  [frame(Data, [fin, {opcode, Type} | BaseOptions]) | Acc];

encode(<<Data:?FRAGMENT_SIZE/binary, Rest/binary>>, Type, BaseOptions, []) ->
  encode(Rest, continuation, BaseOptions, [frame(Data, [{opcode, Type} | BaseOptions]) | []]);

encode(<<Data:?FRAGMENT_SIZE/binary, Rest/binary>>, Type, BaseOptions, Acc) ->
  encode(Rest, Type, BaseOptions, [frame(Data, [{opcode, Type} | BaseOptions]) | Acc]);

encode(<<>>, _Type, _Options, Acc) ->
  Acc;

encode(<<Data/binary>>, Type, BaseOptions, Acc) ->
  [frame(Data, [fin, {opcode, Type} | BaseOptions]) | Acc].

-spec frame(Data::binary(), Options::list()) -> binary().
frame(Data, Options) ->
  Frame = wsock_framing:frame(Data, Options),
  wsock_framing:to_binary(Frame).

-spec decode(Data::binary(), Type :: message_type(), Message::#message{}, Masked::boolean()) -> list(#message{}) | {error, frames_unmasked | fragmented_control_message}.
decode(Data, begin_message, _Message, Masked) ->
  do_decode(Data, begin_message, [], Masked);

decode(Data, continue_message, Message, Masked) ->
  do_decode(Data, continue_message, [Message | []], Masked).

-spec do_decode(Data::binary(), Type:: message_type(), Acc::list(), Masked::boolean()) -> list(#message{}) | {error, frames_unmasked | fragmented_control_message}.
do_decode(Data, Type, Acc, Masked) ->
  Frames = wsock_framing:from_binary(Data),
  do_decode_frames(Masked, Type, Frames, Acc).

-spec do_decode_frames(Masked :: boolean(), Type :: message_type(), Frames :: list(#frame{}), Acc :: list()) -> list(#message{}) | {error, frames_unmasked | fragmented_control_message}.
do_decode_frames(_Masked = true, Type, Frames, Acc) ->
  do_decode_masked_frames(ensure_all_frames_mask_value(Frames, 1), Type, Frames, Acc);

do_decode_frames(_Masked = false, Type, Frames, Acc) ->
  do_decode_unmasked_frames(ensure_all_frames_mask_value(Frames, 0), Type, Frames, Acc).

-spec do_decode_masked_frames(AllFramesMasked :: boolean(), Type :: message_type(), Frames :: list(#frame{}), Acc :: list()) -> list(#message{}) | {error, frames_unmasked | fragmented_control_message}.
do_decode_masked_frames(_AllFramesMasked = true, Type, Frames, Acc) ->
  transform_frames_into_messages(Type, Frames, Acc);
do_decode_masked_frames(_AllFramesMasked = false, _, _, _)  ->
  {error, frames_unmasked}.

-spec do_decode_unmasked_frames(AllFramesUnmasked :: boolean(), Type :: message_type(), Frames :: list(#frame{}), Acc :: list()) -> list(#message{}) | {error, frames_unmasked | fragmented_control_message}.
do_decode_unmasked_frames(_AllFramesUnmasked = true, Type, Frames, Acc) ->
  transform_frames_into_messages(Type, Frames, Acc);
do_decode_unmasked_frames(_AllFramesUnmasked = false, _, _, _) ->
  {error, frames_masked}.

-spec ensure_all_frames_mask_value(Frames :: list(#frame{}), Value :: integer()) -> true | false.
ensure_all_frames_mask_value(Frames, Value) ->
  lists:all(fun(F) -> F#frame.mask == Value end, Frames).

-spec transform_frames_into_messages(Type :: message_type(), Frames :: list(#frame{}), Acc :: list(#message{})) -> list(#message{}) | {error, fragmented_control_message}.
transform_frames_into_messages(Type, Frames, Acc) ->
  case process_frames(Type, Frames, Acc) of
    {error, Reason} ->
      {error, Reason};
    Messages ->
      lists:reverse(Messages)
  end.

-spec process_frames(Type:: message_type(), Frames :: list(#frame{}), Messages :: list(#message{})) -> list(#message{}) | {error, fragmented_control_message}.
process_frames(_, [], Acc) ->
  Acc;
process_frames(begin_message, Frames, Acc) ->
  wtf(Frames, begin_message, #message{}, Acc);

process_frames(continue_message, Frames, [FramgmentedMessage | Acc]) ->
  wtf(Frames, continue_message, FramgmentedMessage, Acc).

wtf([Frame | Frames], Type, XMessage, Acc) ->
  case process_frame(Frame, Type, XMessage) of
    {error, Reason} ->
      {error, Reason};
    {fragmented, Message} ->
      process_frames(continue_message, Frames, [Message#message{type = fragmented} | Acc]);
    {completed, Message} ->
      process_frames(begin_message, Frames, [Message | Acc])
  end.

-spec process_frame(Frame :: #frame{}, MessageType :: message_type(), Message :: #message{})-> {fragmented | completed, #message{}} | {error, fragmented_control_message}.
process_frame(Frame, MessageType, Message) ->
  process_frame(contextualize_frame(Frame), MessageType, Frame, Message).

-spec process_frame(FrameType :: atom(), MessageType :: message_type(), Frame :: #frame{}, Message :: #message{}) -> {frame | completed, #message{}} | {error, fragmented_control_message}.
process_frame(control_fragment, _ ,_, _) ->
  {error, fragmented_control_message};
process_frame(open_close, begin_message, Frame, Message) ->
  frame_to_complete_message(Frame, Message);
process_frame(open_continue, begin_message, Frame, Message) ->
  frame_for_fragmented_message(Frame, Message);
process_frame(continue, continue_message, Frame, Message) ->
  frame_for_fragmented_message(Frame, Message);
process_frame(continue_close, continue_message, Frame, Message) ->
  frame_to_complete_message(Frame, Message).

frame_to_complete_message(Frame, Message) ->
  BuiltMessage = build_message(Message, lists:reverse([Frame | Message#message.frames])),
  {completed, BuiltMessage}.
frame_for_fragmented_message(Frame, Message) ->
  {fragmented, append_frame_to_fragmented_message(Frame, Message)}.

append_frame_to_fragmented_message(Frame, Message) ->
  Frames = Message#message.frames,
  Message#message{frames = [Frame | Frames]}.

-spec contextualize_frame(Frame :: #frame{}) -> continue_close | open_continue | continue | open_close | control_fragment.
contextualize_frame(Frame) ->
  case {Frame#frame.fin, Frame#frame.opcode} of
    {1, 0} -> continue_close;
    {0, 0} -> continue;
    {0, Opcode} when Opcode == 8; Opcode == 9; Opcode == 10 -> control_fragment;
    {1, _} -> open_close;
    {0, _} -> open_continue
  end.

build_message(Message, Frames) ->
  [HeadFrame | _] = Frames,

  case HeadFrame#frame.opcode of
    1 ->
      Payload = build_payload_from_frames(text, Frames),
      Message#message{type = text, payload = Payload};
    2 ->
      Payload = build_payload_from_frames(binary, Frames),
      Message#message{type = binary, payload = Payload};
    8 ->
      Payload = build_payload_from_frames(close, Frames),
      Message#message{type = close, payload = Payload};
    9 ->
      Payload = build_payload_from_frames(text, Frames),
      Message#message{type = ping, payload = Payload};
    10 ->
      Payload = build_payload_from_frames(text, Frames),
      Message#message{type = pong, payload = Payload}
  end.

build_payload_from_frames(close, [Frame]) ->
  case Frame#frame.payload of
    <<>> -> {undefined, undefined};
    <<Status:16, Reason/binary>> -> {Status, binary_to_list(Reason)}
  end;

build_payload_from_frames(binary, Frames) ->
  concatenate_payload_from_frames(Frames);

build_payload_from_frames(text, Frames) ->
  Payload = concatenate_payload_from_frames(Frames),
  binary_to_list(Payload).

concatenate_payload_from_frames(Frames) ->
  concatenate_payload_from_frames(Frames, <<>>).

concatenate_payload_from_frames([], Acc) ->
  Acc;
concatenate_payload_from_frames([Frame | Rest], Acc) ->
  concatenate_payload_from_frames(Rest, <<Acc/binary, (Frame#frame.payload)/binary>>).

