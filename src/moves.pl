
% ---------------------------------------------------------------------------------------------------------
%                                                   Moves 
% ---------------------------------------------------------------------------------------------------------
% In this module, the possible move rules of every piece are defined. This is not with respect to a board,
%           where the set of possible moves is reduced if other pieces are in the way. 
% ---------------------------------------------------------------------------------------------------------



% -------------------------------------------------- Rook -------------------------------------------------

move_rule(rook, Fromco, ToCo) :- on_row(Co, Row).
move_rule(rook, Co, Col) :- column_through_co(C, Col).

% ------------------------------------------------- Bischop -----------------------------------------------

move_rule(bischop, Co, Diagonal) :- diagonal_through_co(Co, Diagonal).
move_rule(bischop, Co, AntiDiagonal) :- anti_diagonal_through_co(Co, AntiDiagonal).

% -------------------------------------------------  Queen  -----------------------------------------------

move_rule(queen, Co, Moves) :- move_rule(bischop, Co, Moves).
move_rule(queen, Co, Moves) :- move_rule(rook, Co, Moves).

% -------------------------------------------------  Knight  -----------------------------------------------

