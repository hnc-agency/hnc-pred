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
-export([
	 matches/1,
	 not_matches/1,
	 eq/1,
	 ne/1,
	 lt/1,
	 lte/1,
	 gt/1,
	 gte/1
	]).

all() ->
    [
     matches,
     not_matches,
     eq,
     ne,
     lt,
     lte,
     gt,
     gte
    ].

do_cmp_check(F, E1, E2, E3, E4, E5, E6, E7, E8, E9) ->
    ok = do_cmp_check1(F, 0, E1, E2, E3),
    ok = do_cmp_check1(F, +0.0, E4, E5, E6),
    ok = do_cmp_check1(F, -0.0, E7, E8, E9),
    ok.

do_cmp_check1(F, A, E1, E2, E3) ->
    Fun = hnc_pred:F(A),
    E1 = Fun(0),
    E2 = Fun(+0.0),
    E3 = Fun(-0.0),
    ok.

matches(_) ->
    ok = do_cmp_check(matches, true, false, false,
		               false, true, false,
			       false, false, true),
    ok.

not_matches(_) ->
    ok = do_cmp_check(not_matches, false, true, true,
				   true, false, true,
				   true, true, false),
    ok.

eq(_) ->
    ok = do_cmp_check(eq, true, true, true,
			  true, true, true,
			  true, true, true),
    ok.

ne(_) ->
    ok = do_cmp_check(ne, false, false, false,
			  false, false, false,
			  false, false, false),
    ok.

lt(_) ->
    ok = do_cmp_check(lt, false, false, false,
			  false, false, false,
			  false, false, false),
    ok.

lte(_) ->
    ok = do_cmp_check(lte, true, true, true,
			   true, true, true,
			   true, true, true),
    ok.

gt(_) ->
    ok = do_cmp_check(gt, false, false, false,
			  false, false, false,
			  false, false, false),
    ok.

gte(_) ->
    ok = do_cmp_check(gte, true, true, true,
			   true, true, true,
			   true, true, true),
    ok.

