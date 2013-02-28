%%-------------------------------------------------------------------
%%% Copyright 2010 Cleartext
%%% Author  : Boris Okner <boris.okner@gmail.com>
%%% Description : JSON support
%%% The code was derived from mochiweb library,
%%% with the goal to have a single file with JSON utilities
%%% 	
%%% Created : 14 Aug 2010
%%%-------------------------------------------------------------------
-module(json_utils).

-export([encode/1, encoder/1]).

-export([decode/1, decoder/1]).

-export([pretty_print/1, pretty_print/2]).

-export([test/0]).

-compile(export_all).

% This is a macro to placate syntax highlighters..
-define(Q, $").

-define(ADV_COL(S, N),
	S#decoder{column = N + S#decoder.column}).

-define(INC_COL(S),
	S#decoder{column = 1 + S#decoder.column}).

-define(INC_LINE(S),
	S#decoder{column = 1, line = 1 + S#decoder.line}).

% from mochinum
%% IEEE 754 Float exponent bias
-define(FLOAT_BIAS, 1022).

-define(MIN_EXP, -1074).

-define(BIG_POW, 4503599627370496).

%% @type iolist() = [char() | binary() | iolist()]
%% @type iodata() = iolist() | binary()
%% @type json_string() = atom | string() | binary()
%% @type json_number() = integer() | float()
%% @type json_array() = {array, [json_term()]}
%% @type json_object() = {struct, [{json_string(), json_term()}]}
%% @type json_term() = json_string() | json_number() | json_array() |
%%                     json_object()
%% @type encoding() = utf8 | unicode
%% @type encoder_option() = {input_encoding, encoding()} |
%%                          {handler, function()}
%% @type decoder_option() = {input_encoding, encoding()} |
%%                          {object_hook, function()}
%% @type bjson_string() = binary()
%% @type bjson_number() = integer() | float()
%% @type bjson_array() = [bjson_term()]
%% @type bjson_object() = {struct, [{bjson_string(), bjson_term()}]}
%% @type bjson_term() = bjson_string() | bjson_number() | bjson_array() |
%%                      bjson_object()
%% @type binary_encoder_option() = {handler, function()}
%% @type binary_decoder_option() = {object_hook, function()}

-record(encoder,
	{input_encoding = unicode, handler = null}).

-record(decoder,
	{input_encoding = utf8, object_hook = null, line = 1,
	 column = 1, state = null}).

%% @spec encoder([encoder_option()]) -> function()
%% @doc Create an encoder/1 with the given options.
encoder(Options) ->
    State = parse_encoder_options(Options, #encoder{}),
    fun (O) -> json_encode(O, State) end.

%% @spec encode(json_term()) -> iolist()
%% @doc Encode the given as JSON to an iolist.
encode(Any) -> json_encode(Any, #encoder{}).

%% @spec decoder([decoder_option()]) -> function()
%% @doc Create a decoder/1 with the given options.
decoder(Options) ->
    State = parse_decoder_options(Options, #decoder{}),
    fun (O) -> json_decode(O, State) end.

%% @spec decode(iolist()) -> json_term()
%% @doc Decode the given iolist to Erlang terms.
decode(S) -> json_decode(S, #decoder{}).

test() -> test_all().

%% Internal API

parse_encoder_options([], State) -> State;
parse_encoder_options([{input_encoding, Encoding}
		       | Rest],
		      State) ->
    parse_encoder_options(Rest,
			  State#encoder{input_encoding = Encoding});
parse_encoder_options([{handler, Handler} | Rest],
		      State) ->
    parse_encoder_options(Rest,
			  State#encoder{handler = Handler}).

parse_decoder_options([], State) -> State;
parse_decoder_options([{input_encoding, Encoding}
		       | Rest],
		      State) ->
    parse_decoder_options(Rest,
			  State#decoder{input_encoding = Encoding});
parse_decoder_options([{object_hook, Hook} | Rest],
		      State) ->
    parse_decoder_options(Rest,
			  State#decoder{object_hook = Hook}).

json_encode(true, _State) -> "true";
json_encode(false, _State) -> "false";
json_encode(null, _State) -> "null";
json_encode(I, _State) when is_integer(I) ->
    integer_to_list(I);
json_encode(F, _State) when is_float(F) -> digits(F);
json_encode(L, State)
    when is_list(L); is_binary(L); is_atom(L) ->
    json_encode_string(L, State);
json_encode({array, Props}, State)
    when is_list(Props) ->
    json_encode_array(Props, State);
json_encode({struct, Props}, State)
    when is_list(Props) ->
    json_encode_proplist(Props, State);
json_encode(Bad, #encoder{handler = null}) ->
    exit({json_encode, {bad_term, Bad}});
json_encode(Bad, State = #encoder{handler = Handler}) ->
    json_encode(Handler(Bad), State).

json_encode_array([], _State) -> "[]";
json_encode_array(L, State) ->
    F = fun (O, Acc) -> [$,, json_encode(O, State) | Acc]
	end,
    [$, | Acc1] = lists:foldl(F, "[", L),
    lists:reverse([$] | Acc1]).

json_encode_proplist([], _State) -> "{}";
json_encode_proplist(Props, State) ->
    F = fun ({K, V}, Acc) ->
		KS = case K of
		       K when is_atom(K) ->
			   json_encode_string_utf8(atom_to_list(K));
		       K when is_integer(K) ->
			   json_encode_string(integer_to_list(K), State);
		       K when is_list(K); is_binary(K) ->
			   json_encode_string(K, State)
		     end,
		VS = json_encode(V, State),
		[$,, VS, $:, KS | Acc]
	end,
    [$, | Acc1] = lists:foldl(F, "{", Props),
    lists:reverse([$} | Acc1]).

json_encode_string(A, _State) when is_atom(A) ->
    json_encode_string_unicode(from_utf8(atom_to_list(A)));
json_encode_string(B, _State) when is_binary(B) ->
    json_encode_string_unicode(from_utf8(B));
json_encode_string(S,
		   #encoder{input_encoding = utf8}) ->
    json_encode_string_utf8(S);
json_encode_string(S,
		   #encoder{input_encoding = unicode}) ->
    json_encode_string_unicode(S).

json_encode_string_utf8(S) ->
    [?Q | json_encode_string_utf8_1(S)].

json_encode_string_utf8_1([C | Cs])
    when C >= 0, C =< 127 ->
    NewC = case C of
	     $\\ -> "\\\\";
	     ?Q -> "\\\"";
	     _ when C >= $\s, C < 127 -> C;
	     $\t -> "\\t";
	     $\n -> "\\n";
	     $\r -> "\\r";
	     $\f -> "\\f";
	     $\b -> "\\b";
	     _ when C >= 0, C =< 127 -> unihex(C);
	     _ -> exit({json_encode, {bad_char, C}})
	   end,
    [NewC | json_encode_string_utf8_1(Cs)];
json_encode_string_utf8_1(All = [C | _])
    when C >= 128, C =< 1114111 ->
    json_encode_string_unicode(from_utf8(All));
json_encode_string_utf8_1([]) -> "\"".

json_encode_string_unicode(S) ->
    [?Q | json_encode_string_unicode_1(S)].

json_encode_string_unicode_1([C | Cs]) ->
    NewC = case C of
	     $\\ -> "\\\\";
	     ?Q -> "\\\"";
	     _ when C >= $\s, C < 127 -> C;
	     $\t -> "\\t";
	     $\n -> "\\n";
	     $\r -> "\\r";
	     $\f -> "\\f";
	     $\b -> "\\b";
	     _ when C >= 0, C =< 1114111 -> unihex(C);
	     _ -> exit({json_encode, {bad_char, C}})
	   end,
    [NewC | json_encode_string_unicode_1(Cs)];
json_encode_string_unicode_1([]) -> "\"".

dehex(C) when C >= $0, C =< $9 -> C - $0;
dehex(C) when C >= $a, C =< $f -> C - $a + 10;
dehex(C) when C >= $A, C =< $F -> C - $A + 10.

hexdigit(C) when C >= 0, C =< 9 -> C + $0;
hexdigit(C) when C =< 15 -> C + $a - 10.

unihex(C) when C < 65536 ->
    <<D3:4, D2:4, D1:4, D0:4>> = <<C:16>>,
    Digits = [hexdigit(D) || D <- [D3, D2, D1, D0]],
    [$\\, $u | Digits];
unihex(C) when C =< 1114111 ->
    N = C - 65536,
    S1 = 55296 bor (N bsr 10) band 1023,
    S2 = 56320 bor N band 1023,
    [unihex(S1), unihex(S2)].

json_decode(B, S) when is_binary(B) ->
    json_decode(binary_to_list(B), S);
json_decode(L, S) ->
    {Res, L1, S1} = decode1(L, S),
    {eof, [], _} = tokenize(L1, S1#decoder{state = trim}),
    Res.

decode1(L, S = #decoder{state = null}) ->
    case tokenize(L, S#decoder{state = any}) of
      {{const, C}, L1, S1} -> {C, L1, S1};
      {start_array, L1, S1} ->
	  decode_array(L1, S1#decoder{state = any}, []);
      {start_object, L1, S1} ->
	  decode_object(L1, S1#decoder{state = key}, [])
    end.

make_object(V, #decoder{object_hook = null}) -> V;
make_object(V, #decoder{object_hook = Hook}) -> Hook(V).

decode_object(L, S = #decoder{state = key}, Acc) ->
    case tokenize(L, S) of
      {end_object, Rest, S1} ->
	  V = make_object({struct, lists:reverse(Acc)}, S1),
	  {V, Rest, S1#decoder{state = null}};
      {{const, K}, Rest, S1} when is_list(K) ->
	  {colon, L2, S2} = tokenize(Rest, S1),
	  {V, L3, S3} = decode1(L2, S2#decoder{state = null}),
	  decode_object(L3, S3#decoder{state = comma},
			[{K, V} | Acc])
    end;
decode_object(L, S = #decoder{state = comma}, Acc) ->
    case tokenize(L, S) of
      {end_object, Rest, S1} ->
	  V = make_object({struct, lists:reverse(Acc)}, S1),
	  {V, Rest, S1#decoder{state = null}};
      {comma, Rest, S1} ->
	  decode_object(Rest, S1#decoder{state = key}, Acc)
    end.

decode_array(L, S = #decoder{state = any}, Acc) ->
    case tokenize(L, S) of
      {end_array, Rest, S1} ->
	  {{array, lists:reverse(Acc)}, Rest,
	   S1#decoder{state = null}};
      {start_array, Rest, S1} ->
	  {Array, Rest1, S2} = decode_array(Rest,
					    S1#decoder{state = any}, []),
	  decode_array(Rest1, S2#decoder{state = comma},
		       [Array | Acc]);
      {start_object, Rest, S1} ->
	  {Array, Rest1, S2} = decode_object(Rest,
					     S1#decoder{state = key}, []),
	  decode_array(Rest1, S2#decoder{state = comma},
		       [Array | Acc]);
      {{const, Const}, Rest, S1} ->
	  decode_array(Rest, S1#decoder{state = comma},
		       [Const | Acc])
    end;
decode_array(L, S = #decoder{state = comma}, Acc) ->
    case tokenize(L, S) of
      {end_array, Rest, S1} ->
	  {{array, lists:reverse(Acc)}, Rest,
	   S1#decoder{state = null}};
      {comma, Rest, S1} ->
	  decode_array(Rest, S1#decoder{state = any}, Acc)
    end.

tokenize_string(IoList = [C | _],
		S = #decoder{input_encoding = utf8}, Acc)
    when is_list(C); is_binary(C); C >= 127 ->
    List = from_utf8(iolist_to_binary(IoList)),
    tokenize_string(List,
		    S#decoder{input_encoding = unicode}, Acc);
tokenize_string("\"" ++ Rest, S, Acc) ->
    {lists:reverse(Acc), Rest, ?INC_COL(S)};
tokenize_string("\\\"" ++ Rest, S, Acc) ->
    tokenize_string(Rest, ?ADV_COL(S, 2), [$" | Acc]);
tokenize_string("\\\\" ++ Rest, S, Acc) ->
    tokenize_string(Rest, ?ADV_COL(S, 2), [$\\ | Acc]);
tokenize_string("\\/" ++ Rest, S, Acc) ->
    tokenize_string(Rest, ?ADV_COL(S, 2), [$/ | Acc]);
tokenize_string("\\b" ++ Rest, S, Acc) ->
    tokenize_string(Rest, ?ADV_COL(S, 2), [$\b | Acc]);
tokenize_string("\\f" ++ Rest, S, Acc) ->
    tokenize_string(Rest, ?ADV_COL(S, 2), [$\f | Acc]);
tokenize_string("\\n" ++ Rest, S, Acc) ->
    tokenize_string(Rest, ?ADV_COL(S, 2), [$\n | Acc]);
tokenize_string("\\r" ++ Rest, S, Acc) ->
    tokenize_string(Rest, ?ADV_COL(S, 2), [$\r | Acc]);
tokenize_string("\\t" ++ Rest, S, Acc) ->
    tokenize_string(Rest, ?ADV_COL(S, 2), [$\t | Acc]);
tokenize_string([$\\, $u, C3, C2, C1, C0 | Rest], S,
		Acc) ->
    % coalesce UTF-16 surrogate pair?
    C = dehex(C0) bor (dehex(C1) bsl 4) bor
	  (dehex(C2) bsl 8)
	  bor (dehex(C3) bsl 12),
    tokenize_string(Rest, ?ADV_COL(S, 6), [C | Acc]);
tokenize_string([C | Rest], S, Acc)
    when C >= $\s; C < 1114111 ->
    tokenize_string(Rest, ?ADV_COL(S, 1), [C | Acc]).

tokenize_number(IoList = [C | _], Mode,
		S = #decoder{input_encoding = utf8}, Acc)
    when is_list(C); is_binary(C); C >= 127 ->
    List = from_utf8(iolist_to_binary(IoList)),
    tokenize_number(List, Mode,
		    S#decoder{input_encoding = unicode}, Acc);
tokenize_number([$- | Rest], sign, S, []) ->
    tokenize_number(Rest, int, ?INC_COL(S), [$-]);
tokenize_number(Rest, sign, S, []) ->
    tokenize_number(Rest, int, S, []);
tokenize_number([$0 | Rest], int, S, Acc) ->
    tokenize_number(Rest, frac, ?INC_COL(S), [$0 | Acc]);
tokenize_number([C | Rest], int, S, Acc)
    when C >= $1, C =< $9 ->
    tokenize_number(Rest, int1, ?INC_COL(S), [C | Acc]);
tokenize_number([C | Rest], int1, S, Acc)
    when C >= $0, C =< $9 ->
    tokenize_number(Rest, int1, ?INC_COL(S), [C | Acc]);
tokenize_number(Rest, int1, S, Acc) ->
    tokenize_number(Rest, frac, S, Acc);
tokenize_number([$., C | Rest], frac, S, Acc)
    when C >= $0, C =< $9 ->
    tokenize_number(Rest, frac1, ?ADV_COL(S, 2),
		    [C, $. | Acc]);
tokenize_number([E | Rest], frac, S, Acc)
    when E == $e; E == $E ->
    tokenize_number(Rest, esign, ?INC_COL(S),
		    [$e, $0, $. | Acc]);
tokenize_number(Rest, frac, S, Acc) ->
    {{int, lists:reverse(Acc)}, Rest, S};
tokenize_number([C | Rest], frac1, S, Acc)
    when C >= $0, C =< $9 ->
    tokenize_number(Rest, frac1, ?INC_COL(S), [C | Acc]);
tokenize_number([E | Rest], frac1, S, Acc)
    when E == $e; E == $E ->
    tokenize_number(Rest, esign, ?INC_COL(S), [$e | Acc]);
tokenize_number(Rest, frac1, S, Acc) ->
    {{float, lists:reverse(Acc)}, Rest, S};
tokenize_number([C | Rest], esign, S, Acc)
    when C == $-; C == $+ ->
    tokenize_number(Rest, eint, ?INC_COL(S), [C | Acc]);
tokenize_number(Rest, esign, S, Acc) ->
    tokenize_number(Rest, eint, S, Acc);
tokenize_number([C | Rest], eint, S, Acc)
    when C >= $0, C =< $9 ->
    tokenize_number(Rest, eint1, ?INC_COL(S), [C | Acc]);
tokenize_number([C | Rest], eint1, S, Acc)
    when C >= $0, C =< $9 ->
    tokenize_number(Rest, eint1, ?INC_COL(S), [C | Acc]);
tokenize_number(Rest, eint1, S, Acc) ->
    {{float, lists:reverse(Acc)}, Rest, S}.

tokenize([], S = #decoder{state = trim}) ->
    {eof, [], S};
tokenize([L | Rest], S) when is_list(L) ->
    tokenize(L ++ Rest, S);
tokenize([B | Rest], S) when is_binary(B) ->
    tokenize(from_utf8(B) ++ Rest, S);
tokenize("\r\n" ++ Rest, S) ->
    tokenize(Rest, ?INC_LINE(S));
tokenize("\n" ++ Rest, S) ->
    tokenize(Rest, ?INC_LINE(S));
tokenize([C | Rest], S) when C == $\s; C == $\t ->
    tokenize(Rest, ?INC_COL(S));
tokenize("{" ++ Rest, S) ->
    {start_object, Rest, ?INC_COL(S)};
tokenize("}" ++ Rest, S) ->
    {end_object, Rest, ?INC_COL(S)};
tokenize("[" ++ Rest, S) ->
    {start_array, Rest, ?INC_COL(S)};
tokenize("]" ++ Rest, S) ->
    {end_array, Rest, ?INC_COL(S)};
tokenize("," ++ Rest, S) -> {comma, Rest, ?INC_COL(S)};
tokenize(":" ++ Rest, S) -> {colon, Rest, ?INC_COL(S)};
tokenize("null" ++ Rest, S) ->
    {{const, null}, Rest, ?ADV_COL(S, 4)};
tokenize("true" ++ Rest, S) ->
    {{const, true}, Rest, ?ADV_COL(S, 4)};
tokenize("false" ++ Rest, S) ->
    {{const, false}, Rest, ?ADV_COL(S, 5)};
tokenize("\"" ++ Rest, S) ->
    {String, Rest1, S1} = tokenize_string(Rest, ?INC_COL(S),
					  []),
    {{const, String}, Rest1, S1};
tokenize(L = [C | _], S)
    when C >= $0, C =< $9; C == $- ->
    case tokenize_number(L, sign, S, []) of
      {{int, Int}, Rest, S1} ->
	  {{const, list_to_integer(Int)}, Rest, S1};
      {{float, Float}, Rest, S1} ->
	  {{const, list_to_float(Float)}, Rest, S1}
    end.

%% testing constructs borrowed from the Yaws JSON implementation.

%% Create an object from a list of Key/Value pairs.

obj_new() -> {struct, []}.

is_obj({struct, Props}) ->
    F = fun ({K, _}) when is_list(K) -> true;
	    (_) -> false
	end,
    lists:all(F, Props).

obj_from_list(Props) ->
    Obj = {struct, Props},
    case is_obj(Obj) of
      true -> Obj;
      false -> exit(json_bad_object)
    end.

%% Test for equivalence of Erlang terms.
%% Due to arbitrary order of construction, equivalent objects might
%% compare unequal as erlang terms, so we need to carefully recurse
%% through aggregates (tuples and objects).

equiv({struct, Props1}, {struct, Props2}) ->
    equiv_object(Props1, Props2);
equiv({array, L1}, {array, L2}) -> equiv_list(L1, L2);
equiv(N1, N2) when is_number(N1), is_number(N2) ->
    N1 == N2;
equiv(S1, S2) when is_list(S1), is_list(S2) -> S1 == S2;
equiv(true, true) -> true;
equiv(false, false) -> true;
equiv(null, null) -> true.

%% Object representation and traversal order is unknown.
%% Use the sledgehammer and sort property lists.

equiv_object(Props1, Props2) ->
    L1 = lists:keysort(1, Props1),
    L2 = lists:keysort(1, Props2),
    Pairs = lists:zip(L1, L2),
    true = lists:all(fun ({{K1, V1}, {K2, V2}}) ->
			     equiv(K1, K2) and equiv(V1, V2)
		     end,
		     Pairs).

%% Recursively compare tuple elements for equivalence.

equiv_list([], []) -> true;
equiv_list([V1 | L1], [V2 | L2]) ->
    case equiv(V1, V2) of
      true -> equiv_list(L1, L2);
      false -> false
    end.

test_all() -> test_one(e2j_test_vec(utf8), 1).

test_one([], _N) ->
    %% io:format("~p tests passed~n", [N-1]),
    ok;
test_one([{E, J} | Rest], N) ->
    %% io:format("[~p] ~p ~p~n", [N, E, J]),
    true = equiv(E, decode(J)),
    true = equiv(E, decode(encode(E))),
    test_one(Rest, 1 + N).

e2j_test_vec(unicode) ->
    [{"foo" ++ [500] ++ "bar",
      [$", $f, $o, $o, 500, $b, $a, $r, $"]}];
e2j_test_vec(utf8) ->
    [{1, "1"},
     {3.14159999999999994813,
      "3.14160"}, % text representation may truncate, trail zeroes
     {-1, "-1"}, {-3.14159999999999994813, "-3.14160"},
     {1.2e+11, "1.20000e+11"}, {1.234e+10, "1.23400e+10"},
     {-1.234e-10, "-1.23400e-10"}, {1.0e+1, "1.0e+01"},
     {1.23456e+2, "1.23456E+2"}, {1.0e+1, "1e1"},
     {"foo", "\"foo\""},
     {"foo" ++ [5] ++ "bar", "\"foo\\u0005bar\""},
     {"", "\"\""}, {"\"", "\"\\\"\""},
     {"\n\n\n", "\"\\n\\n\\n\""}, {"\\", "\"\\\\\""},
     {"\" \b\f\r\n\t\"", "\"\\\" \\b\\f\\r\\n\\t\\\"\""},
     {obj_new(), "{}"},
     {obj_from_list([{"foo", "bar"}]), "{\"foo\":\"bar\"}"},
     {obj_from_list([{"foo", "bar"}, {"baz", 123}]),
      "{\"foo\":\"bar\",\"baz\":123}"},
     {{array, []}, "[]"}, {{array, [{array, []}]}, "[[]]"},
     {{array, [1, "foo"]}, "[1,\"foo\"]"},
     {obj_from_list([{"foo", {array, [123]}}]),
      "{\"foo\":[123]}"},
     {obj_from_list([{"foo",
		      obj_from_list([{"bar", true}])}]),
      "{\"foo\":{\"bar\":true}}"},
     {obj_from_list([{"foo", {array, []}},
		     {"bar", obj_from_list([{"baz", true}])},
		     {"alice", "bob"}]),
      "{\"foo\":[],\"bar\":{\"baz\":true},\"alice\":\""
      "bob\"}"},
     {{array,
       [-123, "foo", obj_from_list([{"bar", {array, []}}]),
	null]},
      "[-123,\"foo\",{\"bar\":[]},null]"}].

%% from xmerl_ucs
%% expand_utf8([Byte]) -> {[UnicodeChar],NumberOfBadBytes}
%%  Expand UTF8 byte sequences to ISO 10646/Unicode
%%  charactes. Any illegal bytes are removed and the number of
%%  bad bytes are returned.
%%
%%  Reference:
%%     RFC 3629: "UTF-8, a transformation format of ISO 10646".
from_utf8(Bin) when is_binary(Bin) ->
    from_utf8(binary_to_list(Bin));
from_utf8(List) ->
    case expand_utf8(List) of
      {Result, 0} -> Result;
      {_Res, _NumBadChar} ->
	  exit({ucs, {bad_utf8_character_code}})
    end.

expand_utf8(Str) -> expand_utf8_1(Str, [], 0).

expand_utf8_1([C | Cs], Acc, Bad) when C < 128 ->
    %% Plain Ascii character.
    expand_utf8_1(Cs, [C | Acc], Bad);
expand_utf8_1([C1, C2 | Cs], Acc, Bad)
    when C1 band 224 =:= 192, C2 band 192 =:= 128 ->
    case C1 band 31 bsl 6 bor C2 band 63 of
      C when 128 =< C -> expand_utf8_1(Cs, [C | Acc], Bad);
      _ ->
	  %% Bad range.
	  expand_utf8_1(Cs, Acc, Bad + 1)
    end;
expand_utf8_1([C1, C2, C3 | Cs], Acc, Bad)
    when C1 band 240 =:= 224, C2 band 192 =:= 128,
	 C3 band 192 =:= 128 ->
    case C1 band 15 bsl 6 bor C2 band 63 bsl 6 bor
	   C3 band 63
	of
      C when 2048 =< C -> expand_utf8_1(Cs, [C | Acc], Bad);
      _ ->
	  %% Bad range.
	  expand_utf8_1(Cs, Acc, Bad + 1)
    end;
expand_utf8_1([C1, C2, C3, C4 | Cs], Acc, Bad)
    when C1 band 248 =:= 240, C2 band 192 =:= 128,
	 C3 band 192 =:= 128, C4 band 192 =:= 128 ->
    case C1 band 15 bsl 6 bor C2 band 63 bsl 6 bor
	   C3 band 63
	   bsl 6
	   bor C4 band 63
	of
      C when 65536 =< C -> expand_utf8_1(Cs, [C | Acc], Bad);
      _ ->
	  %% Bad range.
	  expand_utf8_1(Cs, Acc, Bad + 1)
    end;
expand_utf8_1([_ | Cs], Acc, Bad) ->
    %% Ignore bad character.
    expand_utf8_1(Cs, Acc, Bad + 1);
expand_utf8_1([], Acc, Bad) ->
    {lists:reverse(Acc), Bad}.

%% from mochinum

%% @spec digits(number()) -> string()
%% @doc Returns a string that accurately represents the given integer or float
%% using a conservative amount of digits. Great for generating
%% human-readable output, or compact ASCII serializations for floats.
digits(N) when is_integer(N) -> integer_to_list(N);
digits(0.0) -> "0.0";
digits(Float) ->
    {Frac, Exp} = frexp(Float),
    Exp1 = Exp - 53,
    Frac1 = trunc(abs(Frac) * (1 bsl 53)),
    [Place | Digits] = digits1(Float, Exp1, Frac1),
    R = insert_decimal(Place, [$0 + D || D <- Digits]),
    case Float < 0 of
      true -> [$- | R];
      _ -> R
    end.

%% @spec frexp(F::float()) -> {Frac::float(), Exp::float()}
%% @doc Return the fractional and exponent part of an IEEE 754 double,
%% equivalent to the libc function of the same name.
%% F = Frac * pow(2, Exp).
frexp(F) -> frexp1(unpack(F)).

int_pow(X, N, R) when N < 2 -> R * X;
int_pow(X, N, R) ->
    int_pow(X * X, N bsr 1,
	    case N band 1 of
	      1 -> R * X;
	      0 -> R
	    end).

insert_decimal(0, S) -> "0." ++ S;
insert_decimal(Place, S) when Place > 0 ->
    L = length(S),
    case Place - L of
      0 -> S ++ ".0";
      N when N < 0 ->
	  {S0, S1} = lists:split(L + N, S), S0 ++ "." ++ S1;
      N when N < 6 ->
	  %% More places than digits
	  S ++ lists:duplicate(N, $0) ++ ".0";
      _ -> insert_decimal_exp(Place, S)
    end;
insert_decimal(Place, S) when Place > -6 ->
    "0." ++ lists:duplicate(abs(Place), $0) ++ S;
insert_decimal(Place, S) ->
    insert_decimal_exp(Place, S).

insert_decimal_exp(Place, S) ->
    [C | S0] = S,
    S1 = case S0 of
	   [] -> "0";
	   _ -> S0
	 end,
    Exp = case Place < 0 of
	    true -> "e-";
	    false -> "e+"
	  end,
    [C] ++
      "." ++ S1 ++ Exp ++ integer_to_list(abs(Place - 1)).

digits1(Float, Exp, Frac) ->
    Round = Frac band 1 =:= 0,
    case Exp >= 0 of
      true ->
	  BExp = 1 bsl Exp,
	  case Frac /= (?BIG_POW) of
	    true ->
		scale(Frac * BExp * 2, 2, BExp, BExp, Round, Round,
		      Float);
	    false ->
		scale(Frac * BExp * 4, 4, BExp * 2, BExp, Round, Round,
		      Float)
	  end;
      false ->
	  case Exp == (?MIN_EXP) orelse Frac /= (?BIG_POW) of
	    true ->
		scale(Frac * 2, 1 bsl (1 - Exp), 1, 1, Round, Round,
		      Float);
	    false ->
		scale(Frac * 4, 1 bsl (2 - Exp), 2, 1, Round, Round,
		      Float)
	  end
    end.

scale(R, S, MPlus, MMinus, LowOk, HighOk, Float) ->
    Est = int_ceil(math:log10(abs(Float)) - 1.0e-10),
    %% Note that the scheme implementation uses a 326 element look-up table
    %% for int_pow(10, N) where we do not.
    case Est >= 0 of
      true ->
	  fixup(R, S * int_pow(10, Est), MPlus, MMinus, Est,
		LowOk, HighOk);
      false ->
	  Scale = int_pow(10, -Est),
	  fixup(R * Scale, S, MPlus * Scale, MMinus * Scale, Est,
		LowOk, HighOk)
    end.

fixup(R, S, MPlus, MMinus, K, LowOk, HighOk) ->
    TooLow = case HighOk of
	       true -> R + MPlus >= S;
	       false -> R + MPlus > S
	     end,
    case TooLow of
      true ->
	  [K + 1 | generate(R, S, MPlus, MMinus, LowOk, HighOk)];
      false ->
	  [K | generate(R * 10, S, MPlus * 10, MMinus * 10, LowOk,
			HighOk)]
    end.

generate(R0, S, MPlus, MMinus, LowOk, HighOk) ->
    D = R0 div S,
    R = R0 rem S,
    TC1 = case LowOk of
	    true -> R =< MMinus;
	    false -> R < MMinus
	  end,
    TC2 = case HighOk of
	    true -> R + MPlus >= S;
	    false -> R + MPlus > S
	  end,
    case TC1 of
      false ->
	  case TC2 of
	    false ->
		[D | generate(R * 10, S, MPlus * 10, MMinus * 10, LowOk,
			      HighOk)];
	    true -> [D + 1]
	  end;
      true ->
	  case TC2 of
	    false -> [D];
	    true ->
		case R * 2 < S of
		  true -> [D];
		  false -> [D + 1]
		end
	  end
    end.

%% @spec int_pow(X::integer(), N::integer()) -> Y::integer()
%% @doc Moderately efficient way to exponentiate integers.
%% int_pow(10, 2) = 100.
int_pow(_X, 0) -> 1;
int_pow(X, N) when N > 0 -> int_pow(X, N, 1).

%% @spec int_ceil(F::float()) -> integer()
%% @doc Return the ceiling of F as an integer. The ceiling is defined as
%% F when F == trunc(F);
%% trunc(F) when F &lt; 0;
%% trunc(F) + 1 when F &gt; 0.
int_ceil(X) ->
    T = trunc(X),
    case X - T of
      Neg when Neg < 0 -> T;
      Pos when Pos > 0 -> T + 1;
      _ -> T
    end.

unpack(Float) ->
    <<Sign:1, Exp:11, Frac:52>> = <<Float:64/float>>,
    {Sign, Exp, Frac}.

frexp1({_Sign, 0, 0}) -> {0.0, 0};
frexp1({Sign, 0, Frac}) ->
    Exp = log2floor(Frac),
    <<Frac1:64/float>> = <<Sign:1, (?FLOAT_BIAS):11,
			   (Frac - 1):52>>,
    {Frac1, -(?FLOAT_BIAS) - 52 + Exp};
frexp1({Sign, Exp, Frac}) ->
    <<Frac1:64/float>> = <<Sign:1, (?FLOAT_BIAS):11,
			   Frac:52>>,
    {Frac1, Exp - (?FLOAT_BIAS)}.

log2floor(Int) -> log2floor(Int, 0).

log2floor(0, N) -> N;
log2floor(Int, N) -> log2floor(Int bsr 1, 1 + N).

%% Pretty printer
%% %% @spec (Raw, Kind, Args) -> {ok, FormattedJSON}
%% Raw = string()
%% Kind = simplejson
%% Args = [term()]
%% FormattedJSON = string()

%% timeout for external pretty printer
-define(PRETTY_PRINTER_TIMEOUT, 5000).

pretty_printer(Raw, simplejson, [Path]) ->
      Port = open_port({spawn, Path}, [stream, eof, stderr_to_stdout]),
      try
        erlang:port_command(Port, Raw ++ "\n"),
        collect_output(Port, [], ?PRETTY_PRINTER_TIMEOUT)
      after
          port_close(Port)
      end;

pretty_printer(_Raw, Kind, _Args) ->
  throw({pretty_printer_not_supported, Kind}).
%% Collect output coming from the port  
collect_output(Port, Acc, Timeout) ->
  receive
    {Port, {data, Data}} ->
      collect_output(Port,  Acc ++ Data, Timeout);        
    {Port, eof} ->
    	{ok, Acc}	
	after Timeout ->
      {error, port_timeout}
  end.  

pretty_print(Json) ->
    {ok, pretty_print(Json, 0)}.

pretty_print({struct, Struct}, Level) ->
    pretty_print({[], {struct, Struct}}, Level);

pretty_print({Key, {struct, Struct}}, Level) ->
    Tabs = string:copies("\t", Level),
    Header = io_lib:format(Tabs ++ 
              case Key of 
            			[] -> [];
               K ->
                   "\"" ++ K ++ "\": " 
              end
                   ++ "{~n", []),
    Body = lists:flatten(lists:map(fun(S) ->
                          ",\n" ++ pretty_print(S, Level + 1)
                  end, Struct)),
    Footer = "\n" ++ Tabs ++ "}",
				lists:flatten([Header, tl(tl(Body)), Footer]);

pretty_print({Key, {array, Array}}, Level) ->
    Tabs = string:copies("\t", Level),
    Header = io_lib:format(Tabs ++ 
              case Key of 
            			[] -> [];
               K ->
                   "\"" ++ K ++ "\": " 
              end
                   ++ "[~n", []),				
    Body = case Array of
		[] -> [];
		_ -> tl(tl(lists:flatten(lists:map(fun(S) ->
                          ",\n" ++ pretty_print(S, Level + 1) 
                  end, Array))))
	end,
    Footer = "\n" ++ Tabs ++ "]",
				lists:flatten([Header, Body, Footer]);

pretty_print({Key, Value}, Level) ->
    Tabs = string:copies("\t", Level),
    io_lib:format(Tabs ++ 
              case Key of 
            			[] -> [];
               K ->
                   "\"" ++ K ++ "\": " 
              end
                   ++ "~p", [Value]);

pretty_print(Value, Level) ->
    Tabs = string:copies("\t", Level),
    io_lib:format(Tabs ++ "~p", [Value]).

