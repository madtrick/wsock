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
  case Masked of
    true ->
      All = lists:all(fun(F)-> F#frame.mask == 1 end, Frames),
      case All of
        true ->
          case process_frames(Type, Frames, Acc) of
            {error, Reason} ->
              {error, Reason};
            Messages ->
              lists:reverse(Messages)
          end;
          %lists:reverse(process_frames(Type, Frames, Acc));
        false ->
          {error, frames_unmasked}
      end;
    false ->
      Any = lists:any(fun(F) -> F#frame.mask == 1 end, Frames),
      case Any of
        true ->
          {error, frames_masked};
        false ->
          case process_frames(Type, Frames, Acc) of
            {error, Reason} ->
              {error, Reason};
            Messages ->
              lists:reverse(Messages)
          end
          %lists:reverse(process_frames(Type, Frames, Acc))
      end
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
process_frame(Frame, begin_message, Message) ->
  case contextualize_frame(Frame) of
    control_fragment ->
      {error, fragmented_control_message};
    open_close ->
      BuiltMessage = build_message(Message, [Frame]),
      {completed, BuiltMessage};
    open_continue ->
      Frames = Message#message.frames,
      {fragmented, Message#message{frames = [Frame | Frames]}}
  end;

process_frame(Frame, continue_message, Message) ->
  case contextualize_frame(Frame) of
    control_fragment ->
      {error, fragmented_control_message};
    continue ->
      Frames = Message#message.frames,
      {fragmented, Message#message{frames = [Frame | Frames]}};
    continue_close ->
      BuiltMessage = build_message(Message, lists:reverse([Frame | Message#message.frames])),
      {completed, BuiltMessage}
  end.

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

