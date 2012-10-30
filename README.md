binpp - Erlang Binary Pretty Printer
===================================

[![Build Status](https://secure.travis-ci.org/jtendo/binpp.png)](http://travis-ci.org/jtendo/binpp)

Example usage:

    1> Bin.
    <<12,242,207,49,82,69,45,130,212,69,80,88,8,81,23,36,86,7,
      68,19,133,97,51,216,56,145,88,8,81,...>>
    2> binpp:pprint(Bin).
    0C F2 CF 31 52 45 2D 82 D4 45 50 58 08 51 17 24  .òÏ1RE-ÔEPX.Q.$
    56 07 44 13 85 61 33 D8 38 91 58 08 51 17 24 56  V.D.a3Ø8X.Q.$V
    0A 14 20 4E 24 16 09 60 F4 0A 15 11 01 30 13 89  .. N$..`ô....0.
    05 81 0F 09 15 C5 61 33 D8 54 91 52 5D 81 17 24  ....Åa3ØTR].$
    11 14 60 23 D1 3D 55 80                          ..`#Ñ=U
    ok

Binpp will use io:format to output the formatted binary by default, however
there are options making pprint functions return formatted data instead
of performing direct IO write:

    1> Bin2 = <<"foo bar baz">>.
    <<"foo bar baz">>
    2> binpp:pprint(Bin2, [{return, iolist}]).
    [["66 6F 6F 20 62 61 72 20 62 61 7A                ",32,
      "foo bar baz","\n"]]
    3> binpp:pprint(Bin2, [{return, binary}]).
    <<"66 6F 6F 20 62 61 72 20 62 61 7A                 foo bar baz\n">>

You may use a custom printer function as well:

    4> binpp:pprint(Bin2, [{printer, fun(O) -> io:format("~s~n", [O]) end}]).
    66 6F 6F 20 62 61 72 20 62 61 7A                 foo bar baz

    ok

An additional API is provided for printing binary fragments:

    5> binpp:pprint(Bin2, {0, 3}, []).
    66 6F 6F                                         foo

Also, binary byte-to-byte comparsion:

    6> binpp:compare(<<1,2,1024:512,3,4>>, <<1,3,1024:512,5,6>>).
    -- 02 -- -- -- -- -- -- -- -- -- -- -- -- -- --   -- 03 -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- -- 03 04                                       -- -- 05 06
    ok

Plus a handy little helper:

    7> binpp:compare(<<1,2,255,3,1024:512>>, binpp:from_str("01 02 FF CC")).
    -- -- -- 03 00 00 00 00 00 00 00 00 00 00 00 00   -- -- -- CC ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ??
    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00   ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ??
    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00   ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ??
    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00   ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ??
    00 00 04 00                                       ?? ?? ?? ??
    ok


Edoc available at http://jtendo.github.com/binpp/doc/.

That's all folks!
