:- use_module('unit/moves/bishop.pl').
:- use_module('unit/moves/queen.pl').
:- use_module('unit/moves/king.pl').
:- use_module('unit/moves/pawn.pl').
:- use_module('unit/moves/knight.pl').
:- use_module('unit/moves/pin.pl').
:- use_module('unit/parser/san.pl').
:- use_module('unit/parser/pgn.pl').
:- use_module(library(plunit)).

run :- run_tests, halt.

:- initialization(run).
