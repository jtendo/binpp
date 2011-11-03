%           DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
%                   Version 2, December 2004
%
% Copyright (C) 2011 Adam Rutkowski <adam@mtod.org>
%
% Everyone is permitted to copy and distribute verbatim or modified
% copies of this license document, and changing it is allowed as long
% as the name is changed.
%
%            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
%   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
%
%  0. You just DO WHAT THE FUCK YOU WANT TO.

-module(binpp).
-author('Adam Rutkowski adam@mtod.org').
-export([pprint/1, pprint/3]).
-export([cmprint/2]).
-export([from_str/1, from_str/2]).
-export([format/1, format/2]).
-export([convert/1, convert/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   API                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec convert(binary(), atom()) -> {ok, list()}.

convert(Bin, hex) when is_binary(Bin) orelse is_bitstring(Bin) ->
    convert(Bin, [], fun hexstr/1);

convert(Bin, bin) when is_binary(Bin) orelse is_bitstring(Bin) ->
    convert(Bin, [], fun binstr/1).

-spec convert(binary()) -> {ok, list()}.

convert(Bin) when is_binary(Bin) ->
    convert(Bin, hex).

-spec format(binary(), atom()) -> ok.

format(Bin, Base) ->
    {ok, Octets} = convert(Bin, Base),
    io:format("~p~n", [string:join(Octets, " ")]),
    ok.

-spec format(binary()) -> ok.

format(Bin) ->
    format(Bin, hex),
    ok.

-spec pprint(binary()) -> ok.

pprint(Bin) ->
    {ok, Octets} = convert(Bin, hex),
    Buckets = buckets(16, Octets),
    lists:foreach(fun print_bucket/1, Buckets),
    ok.

pprint(Bin, Pos, Len) when Len =< size(Bin) ->
    pprint(binary:part(Bin, Pos, Len));

pprint(Bin, Pos, _) ->
    pprint(binary:part(Bin, Pos, size(Bin)-Pos)).

-spec cmprint(binary(), binary()) -> ok.

cmprint(Bin1, Bin2) when is_binary(Bin1) orelse is_bitstring(Bin1),
                         is_binary(Bin2) orelse is_bitstring(Bin2) ->
    {ok, Octets1} = convert(Bin1, hex),
    {ok, Octets2} = convert(Bin2, hex),
    {ok, {D1, D2}} = diff(Octets1, Octets2),
    print_comparsion(buckets(16, D1), buckets(16, D2)).

-spec from_str(string(), hex) -> binary().

from_str(Str, hex) when is_list(Str) ->
    Bytes = string:tokens(Str, " "),
    list_to_binary(lists:map(fun(Byte) ->
                list_to_integer(Byte, 16)
        end, Bytes)).

-spec from_str(string()) -> binary().

from_str(Str) when is_list(Str) ->
    from_str(Str, hex).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                 Core :)                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert(<<>>, Acc, _) ->
    {ok, lists:reverse(Acc)};

%% byte align bistring() to make a complementary binary()
convert(Bin, [], FormatFun) when is_bitstring(Bin), not is_binary(Bin) ->
    Align = (8 - (bit_size(Bin) rem 8)),
    io:format("Warning! Aligned bitstring with ~.10B bit(s).~n", [Align]),
    convert(<<Bin/binary, 0:Align>>, [], FormatFun);

convert(<<Bin:8/integer, Rest/binary>>, SoFar, FormatFun) ->
    convert(Rest, [FormatFun(Bin)|SoFar], FormatFun).

print_bucket(Bucket) ->
    OctetLine = string:join(Bucket, " "),
    OctetRepr = lists:map(
            fun(B) ->
                case list_to_integer(B, 16) of
                    Code when Code >= 32 -> Code;
                    _Else -> $.
                end
            end,
            Bucket),
    io:format("~s ~s~n", [string:left(OctetLine, 16*2 + 16, $ ), OctetRepr]).

print_comparsion([], []) ->
    ok;

print_comparsion([L|LRest], [R|RRest]) ->
    Zfill = fun(Line) -> string:left(Line, 16*2 + 16, $ ) end,
    DiffL = Zfill(string:join(L, " ")),
    DiffR = Zfill(string:join(R, " ")),
    io:format("~s  ~s~n", [DiffL, DiffR]),
    print_comparsion(LRest, RRest).

diff([], [], LD, RD) ->
    {ok, {lists:reverse(LD), lists:reverse(RD)}};

diff([], [H2|R2], LD, RD) ->
    diff([], R2, ["??"|LD], [H2|RD]);

diff([H1|R1], [], LD, RD) ->
    diff(R1, [], [H1|LD], ["??"|RD]);

diff([H1|R1], [H2|R2], LD, RD) when H1 =:= H2 ->
    diff(R1, R2, ["--"|LD], ["--"|RD]);

diff([H1|R1], [H2|R2], LD, RD) ->
     diff(R1, R2, [H1|LD], [H2|RD]).

diff(L1, L2) when is_list(L1), is_list(L2) ->
    diff(L1, L2, [], []).

hexstr(B) -> string:right(integer_to_list(B, 16), 2, $0).
binstr(B) -> string:right(integer_to_list(B, 2), 8, $0).

%% Divide list L into X lists of size N
%% courtesy of MononcQc
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

