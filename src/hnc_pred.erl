%% Copyright (c) 2025, Maria Scott <maria-12648430@hnc-agency.org>
%% 
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(hnc_pred).

-moduledoc """
A collection of handy predicate tools.
""".

-export([id/0,
	 matches/1, not_matches/1,
	 eq/1, ne/1,
	 lt/1, lte/1,
	 gt/1, gte/1,
	 with/2, with/3,
	 oneof/1,
	 switch/1, switch/2,
	 all/1, not_all/1,
	 any/1, not_any/1,
	 complement/1]).

-type predicate() :: fun((term()) -> boolean()).

-export_type([predicate/0]).

-doc """
Returns a predicate function that returns its argument.
""".
-spec id() -> predicate().
id() ->
    fun(Y) -> Y end.

-doc """
Returns a predicate function that returns `true` if its argument
matches `X`, otherwise `false`.
""".
-spec matches(X :: term()) -> predicate().
matches(X) ->
    fun(Y) -> Y =:= X end.

-doc """
Returns a predicate function that returns `true` if its argument
does not match `X`, otherwise `false`.
""".
-spec not_matches(X :: term()) -> predicate().
not_matches(X) ->
    fun(Y) -> Y =/= X end.

-doc """
Returns a predicate function that returns `true` if its argument
compares equal to `X`, otherwise `false`.
""".
-spec eq(X :: term()) -> predicate().
eq(X) ->
    fun(Y) -> Y == X end.

-doc """
Returns a predicate function that returns `true` if its argument
does not compare equal to `X`, otherwise `false`.
""".
-spec ne(X :: term()) -> predicate().
ne(X) ->
    fun(Y) -> Y /= X end.

-doc """
Returns a predicate function that returns `true` if its argument
compares less than `X`, otherwise `false`.
""".
-spec lt(X :: term()) -> predicate().
lt(X) ->
    fun(Y) -> Y < X end.

-doc """
Returns a predicate function that returns `true` if its argument
compares less than or equal to `X`, otherwise `false`.
""".
-spec lte(X :: term()) -> predicate().
lte(X) ->
    fun(Y) -> Y =< X end.

-doc """
Returns a predicate function that returns `true` if its argument
compares greater than `X`, otherwise `false`.
""".
-spec gt(X :: term()) -> predicate().
gt(X) ->
    fun(Y) -> Y > X end.

-doc """
Returns a predicate function that returns `true` if its argument
compares greater than or equal to `X`, otherwise `false`.
""".
-spec gte(X) -> predicate()
      when X :: term().
gte(X) ->
    fun(Y) -> Y >= X end.

-doc #{equiv => with(F, Args, [])}.
-spec with(F :: fun((...) -> boolean()), Args :: [term()]) -> predicate().
with(F, Args) ->
    with(F, Args, []).

-doc """
Returns a predicate function based on the given function `F`, where the argument given
to it is used as an argument to `F` between the given `Args1` and `Args2`. The sum of the
number of elements in `Args1` and `Args2` must match the number of arguments `F` accepts
minus `1`.
""".
-spec with(F :: fun((...) -> boolean()), Args1 :: [term()], Args2 :: [term()]) -> predicate().
with(F, [], []) when is_function(F, 1) ->
    F;
with(F, [Arg], []) when is_function(F, 2) ->
    fun(Y) -> F(Arg, Y) end;
with(F, [], [Arg]) when is_function(F, 2) ->
    fun(Y) -> F(Y, Arg) end;
with(F, [Arg1], [Arg2]) when is_function(F, 3) ->
    fun(Y) -> F(Arg1, Y, Arg2) end;
with(F, [_|_] = Args1, []) when is_function(F, length(Args1) + 1) ->
    fun(Y) -> erlang:apply(F, Args1 ++ [Y]) end;
with(F, [], [_|_] = Args2) when is_function(F, 1 + length(Args2)) ->
    fun(Y) -> erlang:apply(F, [Y|Args2]) end;
with(F, [_|_] = Args1, [_ | _] = Args2) when is_function(F, length(Args1) + 1 + length(Args2)) ->
    fun(Y) -> erlang:apply(F, Args1 ++ [Y|Args2]) end.

-doc """
Returns a predicate function that returns `true` if the argument given to it is contained
in the list of `X`s, otherwise `false`.
""".
-spec oneof([X :: term()]) -> predicate().
oneof([]) ->
    fun(_) -> false end;
oneof([X]) ->
    fun(Y) -> Y =:= X end;
oneof([_|_] = Xs) ->
    switch(maps:from_keys(Xs, true), false).

-doc """
Returns a predicate function that returns the value `V` associated with the key `K` in the
map `M` given as argument to it. If the argument given to it is not contained in `M`, a
`badarg` error is raised.
""".
-spec switch(M :: #{K :: term() => V :: boolean()}) -> predicate().
switch(#{} = Cs) ->
    fun(Y) -> maps:get(Y, Cs) end.

-doc """
Returns a predicate function that returns the value `V` associated with the key `K` in the
map `M` given as argument to it. If the argument given to it is not contained in `M`,
`Default` is returned.
""".
-spec switch(M :: #{K :: term() => V :: boolean()}, Default :: boolean()) -> predicate().
switch(#{} = Cs, D) ->
    fun(Y) -> maps:get(Y, Cs, D) end.

-doc """
Returns a predicate function that returns `true` if all the predicate functions in `Fs`
return `true` when called with the argument given to it, otherwise `false`.
""".
-spec all(Fs :: [predicate()]) -> predicate().
all([]) ->
    fun(_) -> true end;
all([F]) when is_function(F, 1) ->
    F;
all([_, _|_] = Fs) ->
    _ = lists:all(with(fun erlang:is_function/2, [], [1]), Fs) orelse error(badarg, [Fs]),
    fun(Y) -> lists:all(fun(F) -> F(Y) end, Fs) end.

-doc """
Returns a predicate function that returns `true` if any of the predicate functions in `Fs`
returns `false` when called with the argument given to it, otherwise `false`.
""".
-spec not_all(Fs :: [predicate()]) -> predicate().
not_all(Fs) ->
    complement(all(Fs)).

-doc """
Returns a predicate function that returns `true` if any of the predicate functions in `Fs`
returns `true` when called with the argument given to it, otherwise `false`.
""".
-spec any(Fs :: [predicate()]) -> predicate().
any([]) ->
    fun(_) -> false end;
any([F]) when is_function(F, 1) ->
    F;
any([_, _|_] = Fs) ->
    _ = lists:all(with(fun erlang:is_function/2, [], [1]), Fs) orelse error(badarg, [Fs]),
    fun(Y) -> lists:any(fun(F) -> F(Y) end, Fs) end.

-doc """
Returns a predicate function that returns `true` if none of the predicate functions in `Fs`
returns `true`, otherwise `false`.
""".
-spec not_any(Fs :: [predicate()]) -> predicate().
not_any(Fs) ->
    complement(any(Fs)).

-doc """
Returns a predicate function that is the complement of `F`, that is it returns the negation
of the result of calling `F` with the argument given to it.
""".
-spec complement(F :: predicate()) -> predicate().
complement(F) when is_function(F, 1) ->
    fun(Y) -> not F(Y) end.

