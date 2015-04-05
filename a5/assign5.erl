%%%-------------------------------------------------------------------
%%% @author Dante
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2015 10:49 AM
%%%-------------------------------------------------------------------
-module(assign5).
-author("Dante").

-export([main/0]).
-define(NULLIFY, fun(_,_) -> nothing end).

-record(boardstate, {board, blackCaptures, whiteCaptures, turn, message}).


vectAdd(X, Y) -> lists:zipwith(fun(X,Y) -> X+Y end, X, Y).

show(black) -> "B";
show(white) -> "W";
show(king) -> "K";
show(queen) -> "Q";
show(rook) -> "R";
show(knight) ->"Kn";
show(bishop) -> "B";
show(pawn) -> "P";
show(empty) -> "   ";
show({knight, A}) -> show(A) ++ show(knight);
show({Pce, A}) -> show(A) ++ show(Pce) ++ " ";
show([H|T]) -> show(H) ++ show(T);
show([]) -> [].
negate(black) -> white;
negate(white) -> black.

dirTable("N" ) -> [0,1];
dirTable("S" ) -> [0,-1];
dirTable("E" ) -> [1,0];
dirTable("W" ) -> [-1,0];
dirTable("NE") -> [1,1];
dirTable("NW") -> [-1,1];
dirTable("SE") -> [1,-1];
dirTable("SW") -> [-1,-1];
dirTable(_) -> [0,0].


eooHelper(nothing, _,_) -> nothing;
eooHelper(empty, _, Crd) -> {occupy,Crd};
eooHelper({_,TCol},Col,Crd) ->
  if Col /= TCol -> {eat,Crd};
     Col == TCol -> nothing
  end.

eatOrOccupy(Board,Col,Crd) -> eooHelper(select(Board, Crd), Col,Crd).

takeIfOccupied(Board,Col,Crd) ->
  case eatOrOccupy(Board, Col, Crd) of
    {occupy, Crd} -> nothing;
    X -> X
  end.

getAnalyzer(empty, _) -> ?NULLIFY;
getAnalyzer(X,[D,$\s]) -> getAnalyzer(X, [D|" 8"]);
getAnalyzer(X,[D1,D2,$\s]) -> getAnalyzer(X, [D1,D2|" 8"]);
getAnalyzer(X,[D1,D2,D3,$\s]) -> getAnalyzer(X, [D1,D2,D3|" 8"]);
getAnalyzer(X,[D]) -> getAnalyzer(X, [D|" 8"]);
getAnalyzer(X,[D1,D2]) -> getAnalyzer(X, [D1,D2|" 8"]);
getAnalyzer({pawn, black}, [$S,$\s,D]) ->
  fun(Board,[X, Y]) ->
    Sel1 = select(Board, [X, Y-1]),
    Sel2 = select(Board, [X, Y-2]),
    if (D /= $1) and (Y == 7) and (Sel1 == empty)
        and (Sel2 == empty) -> {occupy, [X, Y-2]};
      (Sel1 ==empty) -> {occupy, [X,Y-1]};
      true -> nothing
    end
  end;
getAnalyzer({pawn, white}, [$N,$\s,D]) ->
  fun(Board,[X, Y]) ->
    Sel1 = select(Board, [X, Y+1]),
    Sel2 = select(Board, [X, Y+2]),
    if (D /= $1) and (Y == 2) and (Sel1 == empty)
      and (Sel2 == empty) -> {occupy, [X, Y+2]};
      (Sel1==empty) -> {occupy, [X,Y+1]};
      true -> nothing
    end
  end;
getAnalyzer({pawn,Col}, [A,B,$\s,_]) -> fun(Board, Crd) -> takeIfOccupied(Board, Col, vectAdd(Crd,dirTable([A,B]))) end;
getAnalyzer({pawn,_}, _) -> ?NULLIFY;
getAnalyzer({knight, C},"NNE") -> fun(Board, [X,Y]) -> eatOrOccupy(Board, C, [X+1,Y+2]) end;
getAnalyzer({knight, C},"NNW") -> fun(Board, [X,Y]) -> eatOrOccupy(Board, C, [X-1,Y+2]) end;
getAnalyzer({knight, C},"EEN") -> fun(Board, [X,Y]) -> eatOrOccupy(Board, C, [X+2,Y+1]) end;
getAnalyzer({knight, C},"EES") -> fun(Board, [X,Y]) -> eatOrOccupy(Board, C, [X+2,Y-1]) end;
getAnalyzer({knight, C},"SSE") -> fun(Board, [X,Y]) -> eatOrOccupy(Board, C, [X+1,Y-2]) end;
getAnalyzer({knight, C},"SSW") -> fun(Board, [X,Y]) -> eatOrOccupy(Board, C, [X-1,Y-2]) end;
getAnalyzer({knight, C},"WWN") -> fun(Board, [X,Y]) -> eatOrOccupy(Board, C, [X-2,Y+1]) end;
getAnalyzer({knight, C},"WWS") -> fun(Board, [X,Y]) -> eatOrOccupy(Board, C, [X-2,Y-1]) end;
getAnalyzer({knight,_}, _) -> ?NULLIFY;

getAnalyzer({knight,_}, _) -> ?NULLIFY;

getAnalyzer({rook,_}, [_,_,$\s,_]) -> ?NULLIFY;
getAnalyzer({bishop,_}, [_,$\s,_]) -> ?NULLIFY;
getAnalyzer({king,Col}, [A,B,$\s,_]) -> fun(Board, Crd) -> eatOrOccupy(Board, Col, vectAdd(Crd,dirTable([A,B]))) end;
getAnalyzer({king,Col}, [A,$\s,_]) -> fun(Board, Crd) -> eatOrOccupy(Board, Col, vectAdd(Crd,dirTable([A]))) end;
getAnalyzer({king,Col}, [A,$\s,_]) -> fun(Board, Crd) -> eatOrOccupy(Board, Col, vectAdd(Crd,dirTable([A]))) end;
getAnalyzer({_, Col}, [A,$\s,L]) ->
  D = dirTable([A]),
  if D == [0,0] -> ?NULLIFY;
    true -> analHelper(D, L - $0, 1, Col)
  end;
getAnalyzer({_, Col}, [A, B, $\s,L]) ->
  D = dirTable([A, B]),
  if D == [0,0] -> ?NULLIFY;
    true -> analHelper(D, L - $0, 1, Col)
  end;
getAnalyzer(A, B) -> ?NULLIFY.

analHelper([Dx,Dy], Limit, I,C)->fun(Board, [X,Y]) ->
  Coords = [X+I*Dx, Y+I*Dy],
  Sel = select(Board, Coords),
  if
    I>Limit -> nothing;
    Sel == nothing -> nothing;
    Sel /= empty -> eatOrOccupy(Board, C, Coords);
    true ->
      Next = (analHelper([Dx,Dy],Limit,I+1,C))(Board, [X,Y]),
      if Next == nothing -> eatOrOccupy(Board,C,Coords);
        true -> Next
      end
  end
end.

select(_, [X,Y]) when Y>8 ; Y=<0; X>8 ; X=<0 -> nothing;
select(Board, [X,Y]) -> lists:nth(X, lists:nth(Y,Board)).

divider() -> " +---+---+---+---+---+---+---+---+".
letters()  -> "   a   b   c   d   e   f   g   h".

showRow([H2|T2]) -> "|" ++ show(H2) ++ showRow(T2);
showRow([]) -> "|\n".

showHelper([H|T], I, Msg) ->
  A = showHelper(T, I+1, Msg),
  B =[I+$0] ++ showRow(H) ++ divider() ++ Msg(I),
  A ++ B;
showHelper([],_, _) -> [].



showBoard(#boardstate{board = Board, blackCaptures = B, whiteCaptures = W,turn = Turn, message = M}) ->
  Msg = fun(I) -> if
    I==8 -> "  White Captures: " ++ show(W) ++"\n";
    I==6 -> "  Black Captures: " ++ show(B) ++"\n";
    I==4 -> "  Current Turn: " ++ show(Turn) ++"\n";
    I==2 -> "  " ++ M ++"\n";
    true -> "\n"
    end
  end,
  divider() ++"\n"++showHelper(Board, 1, Msg) ++ letters().

applyMove(BS = #boardstate{board=Board, blackCaptures = B, whiteCaptures = W,turn = Turn}, MV) ->
  Prs = parse(Board, MV),
  {Pce, Coord, Anlz} = if Prs == nothing -> {empty, nil, nil};true -> Prs end,
  {_, C} = if Pce == empty -> {nil, nil};true -> Pce end,
  if
    Pce == empty -> #boardstate{board=Board, blackCaptures = B, whiteCaptures = W,turn = Turn, message = "Could not select Piece"};
    C/= Turn -> #boardstate{board=Board, blackCaptures = B, whiteCaptures = W,turn = Turn, message = "Not Your Turn!"};
    true ->
      Act = Anlz(Board, Coord),
      if
        Act == nothing -> #boardstate{board=Board, blackCaptures = B, whiteCaptures = W,turn = Turn, message = "Could not perform Move"};
        true -> updateState(BS, Act,Coord)
      end
  end.

updateState(#boardstate{board=Board, blackCaptures = B, whiteCaptures = W,turn = Turn},{Act,[X1,Y1]}, [X0,Y0]) ->
  Pce = select(Board, [X0,Y0]),
  Opc = select(Board, [X1,Y1]),
  UpdateBoard = fun UpdateBoard(HT1, X2, Y2) ->
    UpdateRow = fun UpdateRow(HT2, X3,Y3) ->
      case HT2 of
        []->[];
        [H2|T2] ->[if
                    (X1 == X3) and (Y1 == Y3) -> Pce;
                    (X0 == X3) and (Y0 == Y3) -> empty;
                    true -> H2
        end | UpdateRow(T2, X3+1, Y3)]
      end
    end,
    case HT1 of
      [] -> [];
      [H1|T1] -> [UpdateRow(H1, X2, Y2)|UpdateBoard(T1, X2, Y2+1)]
    end
  end,
  if
    (Act == eat) ->
      if (Turn == white) -> #boardstate{board=UpdateBoard(Board, 1,1), blackCaptures = B++[Pce], whiteCaptures = W,turn = black, message = show(Pce) ++ " ate " ++ show(Opc)};
         (Turn == black) -> #boardstate{board=UpdateBoard(Board, 1,1), blackCaptures = B, whiteCaptures = W ++ [Pce],turn = white, message = show(Pce) ++ " ate " ++ show(Opc)}
      end;
    (Act == occupy) -> #boardstate{board=UpdateBoard(Board, 1,1), blackCaptures = B, whiteCaptures = W, turn = negate(Turn), message= show(Pce) ++" was moved"}
  end.

parse(Board, [A,N,$\s|XS]) ->
  Coord = [(A - $a) + 1 , N - $0],
  [X,Y] = Coord,
  Pce = select(Board, Coord),
  if
    Pce == nothing -> nothing;
    (A<$a) or (A>$h) or (N<$1) or (N>$8) -> nothing;
    true -> {Pce, Coord, getAnalyzer(Pce, lists:droplast(XS))}
  end;
parse(_,_) -> nothing.

defaultBoard()->[
  [ {rook ,   white},
    {knight , white},
    {bishop , white},
    {queen ,  white},
    {king ,   white},
    {bishop , white},
    {knight , white},
    {rook ,   white}
],
lists:duplicate(8, {pawn ,white}),
lists:duplicate(8, empty),
lists:duplicate(8, empty),
lists:duplicate(8, empty),
lists:duplicate(8, empty),
lists:duplicate(8, {pawn ,black}),
[ {rook ,   black},
  {knight , black},
  {bishop , black},
  {queen ,  black},
  {king ,   black},
  {bishop , black},
  {knight , black},
  {rook ,   black}
]
].

main() ->
  BS = #boardstate{board=defaultBoard(), whiteCaptures = [],blackCaptures = [], turn = white,message = []},
  Main2 = fun Main2(BoardState) ->
    MV = io:get_line(showBoard(BoardState)++"\nEnter Move >"),
    NBS = applyMove(BoardState, MV),
    Main2(NBS)
  end(BS).