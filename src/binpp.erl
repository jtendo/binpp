%%%
%%%           DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
%%%                   Version 2, December 2004
%%%
%%% Copyright (C) 2011 Adam Rutkowski <hq@mtod.org>
%%%
%%% Everyone is permitted to copy and distribute verbatim or modified
%%% copies of this license document, and changing it is allowed as long
%%% as the name is changed.
%%%
%%%            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
%%%   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
%%%

%% @doc Pretty printer for Erlang binaries
-module(binpp).
-author('Adam Rutkowski hq@mtod.org').

-export([pprint/1, pprint/2, pprint/3]).
-export([compare/2]).
-export([from_str/1]).
-export([convert/1, convert/2]).

-opaque opt() :: {return, iolist} | {return, binary} | {printer, function()}.
-opaque opts() :: list(opt()).

%% Printer constants
-define(SPACE,    $ ).
-define(SPECIAL,  $.).
-define(FILL,     $0).

%% Comparison glyphs
-define(MISSING,  "??").
-define(EQUAL,    "--").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   API                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Pretty print a binary to stdout using io:format/2
-spec pprint(binary() | bitstring()) -> ok.
pprint(Bin) ->
    pprint(Bin, []).

%% @doc Print a binary w/ custom function or return the prettified result.
-spec pprint(binary() | bitstring(), opts()) -> ok | any().
pprint(Bin, Opts) when is_list(Opts) ->
    {ok, Octets} = convert(Bin, hex),
    Buckets = buckets(16, Octets),
    apply_opts(lists:map(fun print_bucket/1, Buckets), Opts).

%% @doc Pretty print a slice of binary.
-spec pprint(binary() | bitstring(), {non_neg_integer(), non_neg_integer()},
             opts()) -> ok | any().
pprint(Bin, {Pos, Len}, Opts) when Len =< byte_size(Bin), (Pos+Len) =< byte_size(Bin) ->
    pprint(binary:part(Bin, Pos, Len), Opts);
pprint(Bin, {Pos, _}, Opts) ->
    pprint(binary:part(Bin, Pos, byte_size(Bin)-Pos), Opts).

%% @doc Pretty print byte-to-byte comparsion of two binaries to stdout.
-spec compare(binary() | bitstring(), binary() | bitstring()) -> ok.
compare(Bin1, Bin2) when is_binary(Bin1) orelse is_bitstring(Bin1),
                         is_binary(Bin2) orelse is_bitstring(Bin2) ->
    {ok, Octets1} = convert(Bin1, hex),
    {ok, Octets2} = convert(Bin2, hex),
    {ok, {D1, D2}} = diff(Octets1, Octets2),
    print_comparison(buckets(16, D1), buckets(16, D2)).

%% @doc Construct binary from hexstring.
%% Hexstring octets can be optionally separated with spaces.
-spec from_str(string()) -> binary().
from_str(Str) when is_list(Str) ->
    Bytes = case lists:member(?SPACE, Str) of
                true ->
                    string:tokens(Str, [?SPACE]);
                false when length(Str) rem 2 =:= 0 ->
                    buckets(2, Str)
            end,
    list_to_binary([list_to_integer(B, 16) || B <- Bytes]).

%% @doc Convert binary to hex string.
-spec convert(binary() | bitstring()) -> {ok, list()}.
convert(Bin) when is_binary(Bin) orelse is_bitstring(Bin) ->
    convert(Bin, hex).

%% @doc Convert binary to hex string or binary string.
-spec convert(binary() | bitstring(), hex | bin) -> {ok, list()}.
convert(Bin, hex) when is_binary(Bin) orelse is_bitstring(Bin) ->
    convert(Bin, [], fun byte_to_hexstr/1);
convert(Bin, bin) when is_binary(Bin) orelse is_bitstring(Bin) ->
    convert(Bin, [], fun byte_to_binstr/1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Internals                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec apply_opts(iolist(), opts()) -> ok | iolist() | binary().
apply_opts(IoList, []) ->
    io:format("~s~n", [IoList]);
apply_opts(IoList, [{return, iolist}]) ->
    IoList;
apply_opts(IoList, [{return, binary}]) ->
    iolist_to_binary(IoList);
apply_opts(IoList, [{printer, Fun}]) when is_function(Fun) ->
    Fun(IoList);
apply_opts(_, _) -> erlang:error(badarg).

-spec convert(binary() | bitstring(), list(), function()) -> {ok, string()}.
convert(<<>>, Acc, _) ->
    {ok, lists:reverse(Acc)};
convert(Bin, [], FormatFun) when is_bitstring(Bin), not is_binary(Bin) ->
    %% byte align bistring() to make a complementary binary()
    Align = (8 - (bit_size(Bin) rem 8)),
    error_logger:info_msg("Aligned bitstring with ~.10B bit(s).~n", [Align]),
    convert(<<Bin/bitstring, 0:Align>>, [], FormatFun);
convert(<<Bin:8/integer, Rest/binary>>, SoFar, FormatFun) ->
    convert(Rest, [FormatFun(Bin)|SoFar], FormatFun).

print_bucket(Bucket) ->
    OctetLine = string:join(Bucket, [?SPACE]),
    OctetRepr = lists:map(
            fun(B) ->
                case list_to_integer(B, 16) of
                    Code when Code >= ?SPACE -> Code;
                    _ -> ?SPECIAL
                end
            end,
            Bucket),
    io_lib:format("~s ~s~n", [string:left(OctetLine, 16*2 + 16, ?SPACE), OctetRepr]).

-spec print_comparison(list(), list()) -> ok.
print_comparison([], []) ->
    ok;
print_comparison([L|LRest], [R|RRest]) ->
    Zfill = fun(Line) -> string:left(Line, 16*2 + 16, ?SPACE) end,
    DiffL = Zfill(string:join(L, [?SPACE])),
    DiffR = Zfill(string:join(R, [?SPACE])),
    io:format("~s  ~s~n", [DiffL, DiffR]),
    print_comparison(LRest, RRest).

-spec diff(list(), list()) -> {ok, {list(), list()}}.
diff(L1, L2) when is_list(L1), is_list(L2) ->
    diff(L1, L2, [], []).

-spec diff(list(), list(), list(), list()) -> {ok, {list(), list()}}.
diff([], [], LD, RD) ->
    {ok, {lists:reverse(LD), lists:reverse(RD)}};
diff([], [H2|R2], LD, RD) ->
    diff([], R2, [?MISSING|LD], [H2|RD]);
diff([H1|R1], [], LD, RD) ->
    diff(R1, [], [H1|LD], [?MISSING|RD]);
diff([H1|R1], [H1|R2], LD, RD) -> %% H1 =:= H2
    diff(R1, R2, [?EQUAL|LD], [?EQUAL|RD]);
diff([H1|R1], [H2|R2], LD, RD) ->
     diff(R1, R2, [H1|LD], [H2|RD]).

-spec byte_to_hexstr(byte()) -> string().
byte_to_hexstr(B) when B >= 0, B =< 255 ->
    to_hexstr(B, 16, 2).

-spec byte_to_binstr(byte()) -> string().
byte_to_binstr(B) when B >= 0, B =< 255 ->
    to_hexstr(B, 2, 8).

-spec to_hexstr(byte(), non_neg_integer(), non_neg_integer()) -> string().
to_hexstr(B, Base, Len) ->
    string:right(integer_to_list(B, Base), Len, ?FILL).

%% @doc Divide list L into X lists of size N
%% courtesy of MononcQc
-spec buckets(non_neg_integer(), list()) -> list(list()).
buckets(N, L) ->
    buckets(1, N, length(L) div N, L, [[]]).
buckets(_, _, 0, [], [[]|Acc]) ->
    lists:reverse(Acc);
buckets(_, _, 0, Rest, [[]|Acc]) ->
    lists:reverse([Rest|Acc]);
buckets(N, N, M, [H|T], [A|Acc]) ->
    buckets(1, N, M-1, T, [[], lists:reverse([H|A]) | Acc]);
buckets(X, N, M, [H|T], [A|Acc]) ->
    buckets(X+1, N, M, T, [[H|A]|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                 Tests                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(MAX_BIN_SIZE, 2048).
-define(RUNS, 100).

pprint_crash_test_() ->
    F = fun pprint/1,
    _ = random:seed(erlang:now()),
    Tests = [ { crypto:rand_bytes(random:uniform(?MAX_BIN_SIZE)), ok } || _ <- lists:seq(1, ?RUNS) ],
    [ { <<"Random pprint">>, fun() -> ?assertEqual(R, F(I)) end }
             || { I, R } <- Tests ].

pprint_bitstring_crash_test_() ->
    F = fun pprint/1,
    _ = random:seed(erlang:now()),
    Tests = [ { << (crypto:rand_bytes(random:uniform(?MAX_BIN_SIZE)))/binary, 0:(random:uniform(7))>>, ok }
             || _ <- lists:seq(1, ?RUNS) ],
    [ { <<"Random pprint (bitstring)">>, fun() -> ?assertEqual(R, F(I)) end }
             || { I, R } <- Tests ].

compare_crash_test_() ->
    F = fun compare/2,
    _ = random:seed(erlang:now()),
    Rand = fun() -> crypto:rand_bytes(random:uniform(?MAX_BIN_SIZE)) end,
    Tests = [ { { Rand(), Rand() }, ok } || _ <- lists:seq(1, ?RUNS) ],
    [ { <<"Random compare">>, fun() -> ?assertEqual(R, F(I1, I2)) end }
             || { {I1, I2}, R } <- Tests ].

pprint_opts_test_() ->
    F = fun pprint/2,
    _ = random:seed(erlang:now()),
    CustomPrinter = fun(B) when is_list(B) -> works end,
    OptsMap = [
                %% Option                          %% Predicate
                { {return,  binary},               fun erlang:is_binary/1  },
                { {return,  iolist},               fun erlang:is_list/1    },
                { {printer, CustomPrinter},        fun(works) -> true; (_) -> false end  },
                { {invalid, option},               fun({'EXIT', {badarg, _}}) -> true; (O) -> O end }
            ],
    Range = length(OptsMap),
    Rand = fun() ->
        Input = crypto:rand_bytes(random:uniform(?MAX_BIN_SIZE)),
        {Opt, Predicate} = lists:nth(random:uniform(Range), OptsMap),
        {Input, Opt, Predicate}
    end,
    Tests = [ Rand() || _ <- lists:seq(1, ?RUNS) ],
    Title = fun(Opt) ->
            iolist_to_binary([ "Random pprint w/ opt: ", io_lib:format("~p", [Opt]) ]) end,
    [ { Title(Opt), fun() -> ?assertEqual(true, Pred( catch( F(I, [Opt]) ) )) end }
             || {I, Opt, Pred} <- Tests ].

pprint_slice_test_() ->
    F = fun pprint/3,
    _ = random:seed(erlang:now()),
    Rand = fun() ->
            Bytes = crypto:rand_bytes(random:uniform(?MAX_BIN_SIZE)),
            Pos = random:uniform(byte_size(Bytes)),
            Len = random:uniform(byte_size(Bytes)),
            {Bytes, Pos, Len}
    end,
    Tests = [ Rand() || _ <- lists:seq(1, ?RUNS) ],
    Title = fun(Size, Slice) ->
            iolist_to_binary(io_lib:format("Random pprint w/ slice: (~p) ~p", [Size, Slice]))
    end,
    [ { Title(byte_size(Bytes), {Pos, Len}), fun() -> ?assertEqual(ok, F(Bytes, {Pos, Len}, [])) end }
        || { Bytes, Pos, Len } <- Tests ].

from_str_test_() ->
    F = fun from_str/1,
    _ = random:seed(erlang:now()),
    Rand = fun() ->
            Bytes = crypto:rand_bytes(random:uniform(?MAX_BIN_SIZE)),
            {ok, Converted} = convert(Bytes),
            case random:uniform(2) of
                1 -> {lists:flatten(Converted), Bytes};
                2 -> {string:join(Converted, " "), Bytes}
            end
    end,
    Tests = [ Rand() || _ <- lists:seq(1, ?RUNS) ],
    [ { <<"Random from_str">>, fun() -> ?assertEqual(R, F(I)) end }
             || { I, R } <- Tests ].

buckets_test_() ->
    F = fun buckets/2,
    Tests = [
            { {1, [1,2,3,4]}, [ [1], [2], [3], [4] ] },
            { {2, [1,2,3,4]}, [ [1,2], [3,4] ] },
            { {3, [1,2,3,4]}, [ [1,2,3], [4] ] },
            { {4, [1,2,3,4]}, [ [1,2,3,4] ] },
            { {5, [1,2,3,4]}, [ [1,2,3,4] ] }
        ],
    [ { <<"Buckets">>, fun() -> ?assertEqual(R, F(I1, I2)) end }
             || { {I1, I2}, R } <- Tests ].

byte_to_binstr_test_() ->
    F = fun byte_to_binstr/1,
    Tests = [
            { 0,    "00000000" },
            { 1,    "00000001" },
            { 255,  "11111111" }
        ],
    [ { iolist_to_binary(["Convert ", integer_to_list(I)]),
       fun() -> ?assertEqual(R, F(I)) end }
             || { I, R } <- Tests ].

byte_to_hexstr_test_() ->
    F = fun byte_to_hexstr/1,
    Tests = [
            { 0,    "00" },
            { 1,    "01" },
            { 255,  "FF" }
        ],
    [ { iolist_to_binary(["Convert ", integer_to_list(I)]),
       fun() -> ?assertEqual(R, F(I)) end }
             || { I, R } <- Tests ].

diff_test_() ->
    F = fun diff/2,
    Tests = [
            { { [], [] },            { [], []} },
            { { [1, 2], [1, 1] },    { [?EQUAL, 2], [?EQUAL, 1]} },
            { { [1], [1, 1] },       { [?EQUAL, ?MISSING], [?EQUAL, 1]} },
            { { [1, 2, 3], [2, 1] }, { [1, 2, 3], [2, 1, ?MISSING]} },
            { { [], [2, 1] },        { [?MISSING, ?MISSING], [2, 1]} }
        ],
    [ { <<"Diff">>, fun() -> ?assertEqual({ok, R}, F(I1, I2)) end }
             || { {I1, I2}, R } <- Tests ].

print_bucket_test_() ->
    F = fun print_bucket/1,
    Tests = [
            { ["00", "FF"],
              ["00 FF                                           ", ?SPACE, [?SPECIAL, 255], "\n"] },

            { ["41" || _ <- lists:seq(1, 16) ],
              ["41 41 41 41 41 41 41 41 41 41 41 41 41 41 41 41 ", ?SPACE, [$A || _ <- lists:seq(1, 16)], "\n"] }
        ],
    [ { iolist_to_binary(["Print ", I]), fun() -> ?assertEqual(R, F(I)) end }
             || { I, R } <- Tests ].

convert_hex_test_() ->
    F = fun convert/1,
    Tests = [
            { <<1,2,3>>,  ["01", "02", "03"] },
            { <<256:32>>, ["00", "00", "01", "00"] },
            { <<"AAA">>,  ["41", "41", "41"] },
            { <<256:7>>,  ["00"] },
            { <<256:9>>,  ["80", "00"] }
        ],
    [ { <<"Convert">>, fun() -> ?assertEqual({ok, R}, F(I)) end } || { I, R } <- Tests ].

convert_bin_test_() ->
    F = fun convert/2,
    Tests = [
            { <<1,2,3>>,  ["00000001","00000010","00000011"] },
            { <<256:32>>, ["00000000","00000000","00000001", "00000000"] },
            { <<"AAA">>,  ["01000001","01000001","01000001"] },
            { <<256:7>>,  ["00000000"] },
            { <<256:9>>,  ["10000000","00000000"] }
        ],
    [ { <<"Convert">>, fun() -> ?assertEqual({ok, R}, F(I, bin)) end } || { I, R } <- Tests ].

-endif.
