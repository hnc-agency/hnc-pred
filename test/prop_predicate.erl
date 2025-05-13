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

-module(prop_predicate).

-include_lib("proper/include/proper.hrl").

prop_id() ->
    ?FORALL(
	B,
	boolean(),
	B =:= (hnc_pred:id())(B)
    ).

prop_complement() ->
    ?FORALL(
	{F, B},
	{function1(boolean()), boolean()},
	not F(B) =:= (hnc_pred:complement(F))(B)
    ).

prop_matches() ->
    ?FORALL(
	{T1, T2},
	oneof([{any(), any()},
	       ?LET(
		   T,
		   any(),
		   {T, T}
		)]),
		(T1 =:= T2) =:= (hnc_pred:matches(T1))(T2)
	andalso (T1 =/= T2) =:= (hnc_pred:not_matches(T1))(T2)
    ).

prop_compare() ->
    ?FORALL(
	{T1, T2},
	oneof([{any(), any()},
	       ?LET(
		   T,
		    any(),
		    {T, T}
		)]),
		(T1 == T2) =:= (hnc_pred:eq(T1))(T2)
	andalso (T1 /= T2) =:= (hnc_pred:ne(T1))(T2)
	andalso (T1 > T2) =:= (hnc_pred:lt(T1))(T2)
	andalso (T1 >= T2) =:= (hnc_pred:lte(T1))(T2)
	andalso (T1 < T2) =:= (hnc_pred:gt(T1))(T2)
	andalso (T1 =< T2) =:= (hnc_pred:gte(T1))(T2)
    ).

prop_with_2() ->
    ?FORALL(
	{F, Args, T},
	?LET(
	    N,
	    range(0, 19),
	    {function(N + 1, boolean()), vector(N, any()), any()}
	),
	apply(F, Args ++ [T]) =:= (hnc_pred:with(F, Args))(T)
    ).

prop_with_3() ->
    ?FORALL(
	{F, Args1, Args2, T},
	?LET(
	    N1,
	    range(0, 19),
	    ?LET(
		N2,
		range(0, 19 - N1),
		{function(N1 + N2 + 1, boolean()), vector(N1, any()), vector(N2, any()), any()}
	    )
	),
	apply(F, Args1 ++ [T] ++ Args2) =:= (hnc_pred:with(F, Args1, Args2))(T)
    ).

prop_oneof() ->
    ?FORALL(
 	{C, T},
	?LET(
	    {L, R, T},
	    {list(), list(), any()},
	    {oneof([L ++ R, L ++ [T|R]]), T}
	),
	lists:member(T, C) =:= (hnc_pred:oneof(C))(T)
    ).

prop_switch_2() ->
    ?FORALL(
	{S, T},
	?LET(
	    S,
	    ?SUCHTHAT(M, map(any(), boolean()), M =/= #{}),
 	    {S, oneof(maps:keys(S))}
	),
	maps:get(T, S) =:= (hnc_pred:switch(S))(T)
    ).

prop_switch_3() ->
    ?FORALL(
	{S, D, T},
	?LET(
	    {S, D},
	    {map(any(), boolean()), boolean()},
	    {S, D, oneof([any() | maps:keys(S)])}
	),
	case S of
	    #{T := V} -> V =:= (hnc_pred:switch(S, D))(T);
	    #{} -> D =:= (hnc_pred:switch(S, D))(T)
	end
    ).

prop_all_any() ->
    ?FORALL(
	{Fs, T},
	{list(function1(boolean())), any()},
	begin
		Results = lists:map(fun(F) -> F(T) end, Fs),
			(not lists:member(false, Results)) =:= (hnc_pred:all(Fs))(T)
		andalso lists:member(false, Results) =:= (hnc_pred:not_all(Fs))(T)
		andalso lists:member(true, Results) =:= (hnc_pred:any(Fs))(T)
		andalso (not lists:member(true, Results)) =:= (hnc_pred:not_any(Fs))(T)
	end
    ).
