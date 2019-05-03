treechk(X,[],t([],X,[])).
treechk([X1,Y1,X2,Y2],t(L,[X1,Y11,X2,Y22],R),t(L,[X1,Yr,X2,Yr2],R)):- (Y1=Y22;Y2=Y11),!,Yr is min(Y1,Y11),Yr2 is max(Y2,Y22). %union
treechk(X,t(L,Root,R),t(L,Root,NewR)):- righter(X,Root),!,treechk(X,R,NewR).
treechk(X,t(L,Root,R),t(NewL,Root,R)):- not(cross(X,Root)),treechk(X,L,NewL).

righter([X1,_,_,_],[_,_,X2,_]):-X1>X2.

findsum([],Sres,Sres,LConerRes,LConerRes,RConerRes,RConerRes,_).
findsum([[Lx,Ly,Rx,Ry]|T],Scur,Sres,LConerCur,LConerRes,RConerCur,RConerRes,RectTree):-
 coner(Lx:Ly,LConerCur,=<,LConerCur2),
 coner(Rx:Ry,RConerCur,>=,RConerCur2),
 Scur2 is Scur+abs(Rx-Lx)*abs(Ry-Ly),
 treechk([Lx,Ly,Rx,Ry],RectTree,RectTree2),!,
 findsum(T,Scur2,Sres,LConerCur2,LConerRes,RConerCur2,RConerRes,RectTree2).

isRectangleCover(Rects):-
    [[Lx,Ly,Rx,Ry]|_]=Rects,
    findsum(Rects,0,S,Lx:Ly,LconerX:LconerY,Rx:Ry,RconerX:RconerY,[]),!,
    S=:= abs(RconerX-LconerX)*abs(RconerY-LconerY).

coner(X1:Y1,X2:Y2,Dir,X1:Y1):-apply(Dir,[X1,X2]),apply(Dir,[Y1,Y2]),!.
coner(_,XY,_,XY).

cross(X,X):-!.
cross(X,Y):-cross2(X,Y),!.
cross(X,Y):-cross2(Y,X).

cross2([X11,Y11,X12,Y12],[_,_,X22,Y22]):-X11<X22,X22=<X12, Y11<Y22,Y22=<Y12,!.  %right-top
cross2([X11,Y11,X12,Y12],[X21,_,_,Y22]):-X11=<X21,X21<X12, Y11<Y22,Y22=<Y12,!.  %left-top
cross2([X11,Y11,X12,Y12],[_,Y21,X22,_]):-X11<X22,X22=<X12, Y11=<Y21,Y21<Y12,!.  %right-bottom
cross2([X11,Y11,X12,Y12],[X21,Y21,_,_]):-X11=<X21,X21<X12, Y11=<Y21,Y21<Y12.    %left-bottom


%unit-tests framework
assert_are_equal(Goal, false):-get_time(St),not(Goal),!,get_time(Fin),Per is round(Fin-St),writeln(goal-false->ok:Per/sec).
assert_are_equal(Goal, true):- get_time(St),Goal,     !,get_time(Fin),Per is round(Fin-St),writeln(goal-true->ok:Per/sec).
assert_are_equal(Goal, Exp):-writeln(Goal->failed:expected-Exp).

:-assert_are_equal(isRectangleCover([[1,1,3,3],[3,1,4,2],[3,2,4,4],[1,3,2,4],[2,3,3,4]]),true).
:-assert_are_equal(isRectangleCover([[1,1,2,3],[1,3,2,4],[3,1,4,2],[3,2,4,4]]),false).
:-assert_are_equal(isRectangleCover([[1,1,3,3],[3,1,4,2],[1,3,2,4],[3,2,4,4]]),false).
:-assert_are_equal(isRectangleCover([[1,1,3,3],[3,1,4,2],[1,3,2,4],[2,2,4,4]]),false).
:-assert_are_equal(isRectangleCover([[0,0,4,1],[0,0,4,1]]),false).
:-assert_are_equal(isRectangleCover([[0,0,4,1],[7,0,8,2],[6,2,8,3],[5,1,6,3],[4,0,5,1],[6,0,7,2],[4,2,5,3],[2,1,4,3],[0,1,2,2],[0,2,2,3],[4,1,5,2],[5,0,6,1]]),true).
:-assert_are_equal(isRectangleCover([[0,0,4,1],[7,0,8,2],[5,1,6,3],[6,0,7,2],[4,0,5,1],[4,2,5,3],[2,1,4,3],[0,2,2,3],[0,1,2,2],[6,2,8,3],[5,0,6,1],[4,1,5,2]]),true).
:-assert_are_equal(isRectangleCover([[0,0,4,1]]),true).
:-assert_are_equal(isRectangleCover([[0,0,3,3],[1,1,2,2]]),false).
:-assert_are_equal(isRectangleCover([[1,1,2,2],[1,1,2,2],[2,1,3,2]]),false).

:-assert_are_equal(isRectangleCover([[0,0,1,1],[0,1,3,2],[1,0,2,2]]),false).

:-assert_are_equal(isRectangleCover([[0,0,1,1],[0,1,1,2],[0,2,1,3],[0,3,1,4]]),true).
:-assert_are_equal(isRectangleCover([[0,0,1,1],[1,0,2,1],[2,0,3,1],[3,0,4,1]]),true).
:-assert_are_equal(isRectangleCover([[0,0,2,2],[1,1,3,3],[2,0,3,1],[0,3,3,4]]),false).
:-assert_are_equal(isRectangleCover([[0,0,3,1],[0,1,2,3],[1,0,2,1],[2,2,3,3]]),false).

:-assert_are_equal(isRectangleCover([[1,1,3,3],[2,2,4,4],[4,1,5,4],[1,3,2,4]]),false).
:-assert_are_equal(isRectangleCover([[0,0,1,1],[0,0,2,1],[1,0,2,1],[0,2,2,3]]),false).
:-assert_are_equal(isRectangleCover([[0,0,2,1],[0,1,2,2],[0,2,1,3],[1,0,2,1]]),false).
:-assert_are_equal(isRectangleCover([[1,1,2,2],[0,1,1,2],[1,0,2,1],[0,2,3,3],[2,0,3,3]]),false).

:-assert_are_equal(isRectangleCover([[0,0,4,1],[7,0,8,2],[6,2,8,3],[5,1,6,3],[6,0,7,2],[4,2,5,3],[2,1,4,3],[0,1,2,2],[0,2,2,3],[4,1,5,2],[5,0,6,1]]),false).
:-assert_are_equal(isRectangleCover([[0,0,4,1],[7,0,8,2],[5,1,6,4],[6,0,7,2],[4,0,5,1],[4,2,5,3],[2,1,4,3],[0,2,2,3],[0,1,2,2],[6,2,8,3],[5,0,6,1],[4,1,5,2]]),false).
:-assert_are_equal(isRectangleCover([[0,0,4,1],[7,0,8,3],[5,1,6,3],[6,0,7,2],[4,0,5,1],[4,2,5,3],[2,1,4,3],[0,2,2,3],[0,1,2,2],[6,2,8,3],[5,0,6,1],[4,1,5,2]]),false).
:-assert_are_equal(isRectangleCover([[0,0,5,1],[7,0,8,2],[5,1,6,3],[6,0,7,2],[4,0,5,1],[4,2,5,3],[2,1,4,3],[0,2,2,3],[0,1,2,2],[6,2,8,3],[5,0,6,1],[4,1,5,2]]),false).
:-assert_are_equal(isRectangleCover([[0,0,1,1],[0,0,1,1],[0,2,1,3]]),false).
:-assert_are_equal(isRectangleCover([[0,0,3,3],[1,1,2,2],[1,1,2,2]]),false).
:-assert_are_equal(isRectangleCover([[1,1,4,4],[1,3,4,5],[1,6,4,7]]),false).
:-assert_are_equal(isRectangleCover([[0,0,3,1],[0,1,2,3],[2,0,3,1],[2,2,3,3]]),false).
:-assert_are_equal(isRectangleCover([[0,0,1,1],[0,0,1,1],[1,1,2,2],[1,1,2,2]]),false).
:-assert_are_equal(isRectangleCover([[1,1,2,2],[1,1,2,2],[1,1,2,2],[2,1,3,2],[2,2,3,3]]),false).
:-assert_are_equal(isRectangleCover([[1,1,2,2],[2,1,3,2],[2,1,3,2],[2,1,3,2],[3,1,4,2]]),false).
:-assert_are_equal(isRectangleCover([[0,1,2,3],[0,1,1,2],[2,2,3,3],[1,0,3,1],[2,0,3,1]]),false).
:-assert_are_equal(isRectangleCover([[0,0,1,1],[0,2,1,3],[1,1,2,2],[2,0,3,1],[2,2,3,3],[1,0,2,3],[0,1,3,2]]),false).
:-assert_are_equal(isRectangleCover([[0,0,1,1],[0,0,1,1],[0,0,1,1],[0,0,1,1],[0,0,1,1],[0,0,1,1],[0,0,1,1],[0,0,1,1],[2,2,3,3]]),false).
:-assert_are_equal(isRectangleCover([[0,0,1,1],[0,1,1,2],[0,2,1,3],[1,0,2,1],[1,0,2,1],[1,2,2,3],[2,0,3,1],[2,1,3,2],[2,2,3,3]]),false).
:-assert_are_equal(isRectangleCover([[0,0,4,1],[7,0,8,2],[5,1,6,3],[6,0,7,2],[2,1,4,3],[0,2,2,3],[0,1,2,2],[6,2,8,3],[5,0,6,1]]),false).
:-assert_are_equal(isRectangleCover([[0,0,4,1],[7,0,8,2],[5,1,6,3],[6,0,7,2],[4,0,5,1],[4,2,5,3],[2,1,4,3],[-1,2,2,3],[0,1,2,2],[6,2,8,3],[5,0,6,1],[4,1,5,2]]),false).
:-assert_are_equal(isRectangleCover([[0,0,1,1],[1,0,2,1],[1,0,3,1],[3,0,4,1]]),false).

:-assert_are_equal(isRectangleCover([[1,2,4,4],[1,0,4,1],[0,2,1,3],[0,1,3,2],[3,1,4,2],[0,3,1,4],[0,0,1,1]]),true).
:- see('41'),read(X),length(X,L),writeln(41:length=L), seen,assert_are_equal(isRectangleCover(X),true).
:- see('42'),read(X),length(X,L),writeln(42:length=L), seen,assert_are_equal(isRectangleCover(X),true).
:- see('43'),read(X),length(X,L),writeln(43:length=L), seen,assert_are_equal(isRectangleCover(X),true).
:- see('44'),read(X),length(X,L),writeln(44:length=L), seen,assert_are_equal(isRectangleCover(X),false).
:- see('45'),read(X),length(X,L),writeln(45:length=L), seen,assert_are_equal(isRectangleCover(X),true).

