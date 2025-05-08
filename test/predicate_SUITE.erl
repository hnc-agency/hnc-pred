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

-module(predicate_SUITE).

-export([all/0]).
-export([id/1,
	 matches/1, not_matches/1,
	 eq/1, ne/1,
	 lt/1, lte/1,
	 gt/1, gte/1,
	 oneof/1,
	 switch_1/1, switch_2/1,
	 with_2/1, with_3/1,
	 all_test/1, not_all/1,
	 any/1, not_any/1,
	 complement/1]).

all() ->
    [id,
     matches, not_matches,
     eq, ne,
     lt, lte,
     gt, gte,
     oneof,
     switch_1, switch_2,
     with_2, with_3,
     all_test, not_all,
     any, not_any,
     complement].

id(_) ->
    F = hnc_pred:id(),
    true = F(true),
    false = F(false).

matches(_) ->
    F = hnc_pred:matches(1),
    false = F(0),
    false = F(0.0),
    true = F(1),
    false = F (1.0),
    false = F(2),
    false = F(2.0).

not_matches(_) ->
    F = hnc_pred:not_matches(1),
    true = F(0),
    true = F(0.0),
    false = F(1),
    true = F(1.0),
    true = F(2),
    true = F(2.0).

eq(_) ->
    F = hnc_pred:eq(1),
    false = F(0),
    false = F(0.0),
    true = F(1),
    true = F(1.0),
    false = F(2),
    false = F(2.0).

ne(_) ->
    F = hnc_pred:ne(1),
    true = F(0),
    true = F(0.0),
    false = F(1),
    false = F(1.0),
    true = F(2),
    true = F(2.0).

lt(_) ->
    F = hnc_pred:lt(1),
    true = F(0),
    true = F(0.0),
    false = F(1),
    false = F(1.0),
    false = F(2),
    false = F(2.0).

lte(_) ->
    F = hnc_pred:lte(1),
    true = F(0),
    true = F(0.0),
    true = F(1),
    true = F(1.0),
    false = F(2),
    false = F(2.0).

gt(_) ->
    F = hnc_pred:gt(1),
    false = F(0),
    false = F(0.0),
    false = F(1),
    false = F(1.0),
    true = F(2),
    true = F(2.0).

gte(_) ->
    F = hnc_pred:gte(1),
    false = F(0),
    false = F(0.0),
    true = F(1),
    true = F(1.0),
    true = F(2),
    true = F(2.0).

oneof(_) ->
    F = hnc_pred:oneof([1, 2]),
    false = F(0),
    false = F(0.0),
    true = F(1),
    false = F(1.0),
    true = F(2),
    false = F(2.0),
    false = F(3),
    false = F(3.0).

switch_1(_) ->
    F = hnc_pred:switch(#{1 => true, 2 => false}),
    {'EXIT', {{badkey, 0}, _}} = catch F(0),
    {'EXIT', {{badkey, +0.0}, _}} = catch F(0.0),
    true = F(1),
    {'EXIT', {{badkey, 1.0}, _}} = catch F(1.0),
    false = F(2),
    {'EXIT', {{badkey, 2.0}, _}} = catch F(2.0),
    {'EXIT', {{badkey, 3}, _}} = catch F(3),
    {'EXIT', {{badkey, 3.0}, _}} = catch F(3.0),
    ok.

switch_2(_) ->
    F = hnc_pred:switch(#{1 => true, 2 => false}, false),
    false = F(0),
    false = F(0.0),
    true = F(1),
    false = F(1.0),
    false = F(2),
    false = F(2.0),
    false = F(3),
    false = F(3.0).

with_2(_) ->
    B = fun(A, B) -> A > B end,
    F = hnc_pred:with(B, [1]),
    true = F(0),
    true = F(0.0),
    false = F(1),
    false = F(1.0),
    false = F(2),
    false = F(2.0).

with_3(_) ->
    B = fun(A, B, C) -> A > B andalso B > C end,
    F = hnc_pred:with(B, [3], [1]),
    false = F(0),
    false = F(0.0),
    false = F(1),
    false = F(1.0),
    true = F(2),
    true = F(2.0),
    false = F(3),
    false = F(3.0).

all_test(_) ->
    F = hnc_pred:all([fun(X) -> 3 > X end, fun(X) -> X > 1 end]),
    false = F(0),
    false = F(0.0),
    false = F(1),
    false = F(1.0),
    true = F(2),
    true = F(2.0),
    false = F(3),
    false = F(3.0).

not_all(_) ->
    F = hnc_pred:not_all([fun(X) -> 3 > X end, fun(X) -> X > 1 end]),
    true = F(0),
    true = F(0.0),
    true = F(1),
    true = F(1.0),
    false = F(2),
    false = F(2.0),
    true = F(3),
    true = F(3.0).

any(_) ->
    F = hnc_pred:any([fun(X) -> X < 1 end, fun(X) -> 2 < X end]),
    true = F(0),
    true = F(0.0),
    false = F(1),
    false = F(1.0),
    false = F(2),
    false = F(2.0),
    true = F(3),
    true = F(3.0).

not_any(_) ->
    F = hnc_pred:not_any([fun(X) -> X < 1 end, fun(X) -> 2 < X end]),
    false = F(0),
    false = F(0.0),
    true = F(1),
    true = F(1.0),
    true = F(2),
    true = F(2.0),
    false = F(3),
    false = F(3.0).

complement(_) ->
    F = hnc_pred:complement(fun erlang:is_atom/1),
    false = F(x),
    true = F(1),
    true = F("abc").
