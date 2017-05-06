# Battleship

The predicate battleship(L,W,H,TotalSub,TotalDes,TotalRows,TotalColumns) holds if L represents
a battleship grid with a width W and height H such that the count of submarines in L corresponds to
TotalSub and the count of destroyers is equal to TotalDes. The sum of each row and each column of the
grid is represented in TotalRows,TotalColumns correspondingly.

%for the hints: at(5,0,w). at(3,5,c). at(9,6,c).
?-battleship(L,10,10,2,1,[0,0,0,0,0,1,1,0,0,2],[0,0,0,1,0,0,1,1,0,1]).

L = [
w, w, w, w, w, w, w, w, w, w,
w, w, w, w, w, w, w, w, w, w,
w, w, w, w, w, w, w, w, w, w,
w, w, w, w, w, w, w, w, w, w,
w, w, w, w, w, w, w, w, w, w,
w, w, w, c, w, w, w, w, w, w,
w, w, w, w, w, w, w, w, w, c,
w, w, w, w, w, w, w, w, w, w,
w, w, w, w, w, w, w, w, w, w,
w, w, w, w, w, w, l, r, w, w] 
