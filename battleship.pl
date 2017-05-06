at(5,0,w). at(3,5,c). at(9,6,c).
%at(2,0,c). at(2,1,r).
%at(0,0,w).


/*
*The predicate holds if each r is preceded by l 
*/

%mycheck(L)
mycheck([]).
mycheck([l]).
mycheck([l,r|T]):-
    mycheck(T).

mycheck([w|T]):-
    mycheck(T).

mycheck([c|T]):-
    mycheck(T).

/*
The base case is a list of elements that there length is less than the width of the grid
*/
list_to_llists(L,W,[]):-
   length(L,Ll),
   Ll < W.
 /*
 The predicate calls a helper method that uses accumilator technique to generate each list of the list of lists.
 Then append each generated list to the list of lists.
 */
list_to_llists(L,W,R):-
    list_to_llists_helper(L,W,L2,R1),
    list_to_llists(L2,W,R2),
    append([R1],R2,R).

list_to_llists_helper(L,0,L,[]).
list_to_llists_helper([H|T],W,L2,[H|T2]):-
    W1 is W-1,
    list_to_llists_helper(T,W1,L2,T2).



% holds if X is the first element of L
%getZeroth(L,X)
getZeroth([H|_],H).

% holds if R is the tail element of L
%rest(L,R)
rest([],[]).
rest([_|T],T).

/*
calculates the number of elements between the two indices and calls a helper method
*/
%sublist(I1,I2,L,Sub)
sublist(I1,I2,L,Sub):-
    D is I2-I1+1, 
    sublist_helper(L,I1,D,Sub).
/*
sublist_helper uses the value of the first index as a counter and to jump to the element having the 
first index and then calls sublist_helper2
*/
sublist_helper(_,_,D,[]):-
    D < 0.
sublist_helper(L,0,D,Sub):-
    D > 0,sublist_helper2(L,D,Sub).
sublist_helper([_|T],N,D,Sub):- 
    D > 0,N > 0,N1 is N-1,sublist_helper(T,N1,D,Sub).

/*
sublist_helper2 takes as parameters:-
L: a list having the element of the first index in the beginning
D: the number of elements to be included in the sub list 
S: the returned sub list.
the predicate accumilates the elements in L to S untill D reaches 0 
*/
sublist_helper2([],_,[]).
sublist_helper2([_|_],0,[]).
sublist_helper2([H|T2],D,[H|T]):-
    D>0, D1 is D-1,sublist_helper2(T2,D1,T).
    
% put all the hints in a list H
%collect_hints
collect_hints(H):-
    bagof(at(A,B,C),at(A,B,C),H).


%produce random lists containg w, c, l, r 
random_assignment([]).
random_assignment([w|T]):-
    random_assignment(T).
random_assignment([c|T]):-
    random_assignment(T).
random_assignment([l|T]):-
    random_assignment(T).
random_assignment([r|T]):-
    random_assignment(T).

/*
calculates the length of the given list (Lr) and compare it to the length of the grid (H*W) flattened 
    if Lr is less than H*W then the given list is part of the grid if not then the given list is the complete grid
    if its the complete grid the given part is converted to 2-D list with each list representing a row using list_to_llists()
    then a check_rows_LL is called.
*/
%check_rows(L,W,H,Totals)
check_rows(L,W,H,Totals):-
    length(L,Lr),
    W * H =:= Lr,
    list_to_llists(L,W,R),
    check_rows_LL(R,Totals).
/*
if the given L is part of the grid the a check is performed using Modulus to check whether the there is a portion of a row represent
    in L then  check_row_remain() is called to check this portion if any.
    Then the complete rows are converted to 2-D list with each list representing a row using list_to_llists()
    then a check_rows_LL is called.
*/
check_rows(L,W,H,Totals):-
    length(L,Lr),
    W * H > Lr,
    Mod is mod(Lr,W),
    check_row_remain(L,W,Mod,Totals),
    list_to_llists(L,W,R),
    check_rows_LL(R,Totals).   


check_row_remain(_,_,0,_).
check_row_remain(L,W,Mod,Totals):-
    Mod > 0,
    length(L,Lr),
    Len is Lr // W,
    I1 is (Len * W), I2 is (Lr - 1),
    sublist( I1, I2 , L, Remain),
    sublist(Len,Len,Totals,Value),
    check_rows_helper(Remain,V),
    V =< Value.

check_rows_LL([],_).
check_rows_LL([H|T],[H2|T2]):-
    check_rows_helper(H,H2),
    check_rows_LL(T,T2).

check_rows_helper([],0).
check_rows_helper([w|T],C):-
    check_rows_helper(T,C).
check_rows_helper([X|T],C):- 
    ( X = r; X = l; X = c ),
    check_rows_helper(T,C1),
    C is C1 + 1.
    
    
/*
calculates the length of the given list (Lr) and compare it to the length of the grid (H*W) flattened 
    if Lr is less than H*W then the given list is part of the grid if not then the given list is the complete grid
    if its the complete grid the given part is converted to 2-D list with each list representing a row using list_to_llists()
    then a check_columns_helper is called.
*/
check_columns([],_,_,_).
check_columns(L,W,H,Totals):-
        length(L,Lr),
        Lr =:= W * H,
        list_to_llists(L,W,R),
        rows_to_coloumns(R,C),
        check_columns_helper(C,H,Totals).
/*
if the given L is part of the grid the a check is performed using Modulus to check whether the there is a portion of a row represent
    in L then  check_row_remain() is called to check this portion if any.
    Then the complete rows are converted to 2-D list with each list representing a row using list_to_llists()
    then a check_columns_helper is called.
*/
check_columns(L,W,H,Totals):-
        length(L,Lr),  Lr >= W, Lr < W * H,  Mod is mod(Lr,W),
        Mod = 0,
        list_to_llists(L,W,R),
        rows_to_coloumns(R,C),
        check_columns_helper(C,H,Totals).

check_columns(L,W,H,Totals):-
        length(L,Lr),  Lr >= W, Lr < W * H, Len is Lr // W, Mod is mod(Lr,W),
        Mod > 0,
        I1 is (Len * W), I2 is (Lr - 1),
        sublist( I1, I2 , L, Remain),
        list_to_llists(L,W,R),
        rows_to_coloumns(R,C),
        remain_elements(C,Remain,C2),
        check_columns_helper(C2,H,Totals).

check_columns(L,W,H,Totals):-
        length(L,Lr),
        Lr < W * H,
        check_columns_helper4(L,Totals).

check_columns_helper([],_,_).
check_columns_helper([H|T],Height,[H2|T2]):-
        length(H,Lr),
        Lr = Height,
        check_columns_helper2(H,H2) ,
        check_columns_helper(T,Height,T2).

check_columns_helper([H|T],Height,[H2|T2]):-
        length(H,Lr),
        Lr < Height,
        check_columns_helper3(H,H2) ,
        check_columns_helper(T,Height,T2).

check_columns_helper2([],0).
check_columns_helper2([w|T],C):-
    check_columns_helper2(T,C).

check_columns_helper2([X|T],C):- 
    ( X = r; X = l; X = c ),
    check_columns_helper2(T,C1),
    C is C1 + 1.

check_columns_helper3([],X):-  
X >= 0 .
check_columns_helper3([w|T],C):-
    check_columns_helper3(T,C).
check_columns_helper3([X|T],C):- 
    ( X = r; X = l; X = c ),
    C1 is C - 1,
    check_columns_helper3(T,C1).

check_columns_helper4([],_).
check_columns_helper4([w|T],[V|Tail]):-
        V >= 0,
        check_columns_helper4(T,Tail).
check_columns_helper4([X|T],[V|Tail]):-
        ( X = r; X = l; X = c ),
        V > 0,
        check_columns_helper4(T,Tail).


% produce a a list of lists with each list is a column from the list of lists representing rows
rows_to_coloumns([],[]).
rows_to_coloumns([[]|_],[]).
rows_to_coloumns(L,[C1|CTail]):-
        L \= [],
        construct_column(L,C1,Rest),
        rows_to_coloumns(Rest,CTail).

construct_column([],[],[]).
construct_column([[H|T]|Tail],[H|CTail],[T|Rest]):-
        construct_column(Tail,CTail,Rest).

remain_elements(L,[],L).
remain_elements([H|T],[H2|T2],[R|Tail]):-
        append(H,[H2],R),
        remain_elements(T,T2,Tail).



%check_submarines(L,W,H,TotalSub).
check_submarines(L,W,H,TotalSub):-
    length(L,Lr),
    W * H > Lr,
    check_submarines_helper(L,TotalS),
    TotalS =< TotalSub,
    TotalSub =< TotalS + (( W * H ) - Lr) .
       
check_submarines(L,W,H,TotalSub):-
    length(L,Lr),
    Lr =:= W * H ,
    check_submarines_helper(L,TotalSub).
        
%check_submarines_helper(H,TotalSub)
check_submarines_helper([],0).

check_submarines_helper([c|T], TotalSub):-
    check_submarines_helper(T, TotalSub2),
    TotalSub is TotalSub2 + 1.

check_submarines_helper([w|T],TotalSub):-
    check_submarines_helper(T,TotalSub).

check_submarines_helper([l|T],TotalSub):-
    check_submarines_helper(T,TotalSub).

check_submarines_helper([r|T],TotalSub):-
    check_submarines_helper(T,TotalSub).



%check_destroyer(L,W,H,TotalDestroyer)
check_destroyer([],_,_,_).
check_destroyer(L,W,H,TotalDestroyer):-
    length(L,Lr),
    W * H > Lr,
    list_to_llists(L,W,Ll),
    check_destroyer_LL(Ll,TotalD),
    Mod is mod(Lr,W),
    check_destroyer_remain(L,W,Mod,RemainD),
    TotalD + RemainD =< TotalDestroyer,
    Rest is (( W * H ) - Lr) // 2 ,
    TotalDestroyer =< TotalD + RemainD + Rest .

check_destroyer(L,W,H,TotalDestroyer):-
    length(L,Lr),
    W * H =:= Lr,
    list_to_llists(L,W,Ll),
    check_destroyer_LL(Ll,TotalDestroyer).

check_destroyer_remain(_,_,0,0).
      
check_destroyer_remain(L,W,Mod,RemainD):-
    Mod > 0,
    length(L,Lr),
    Len is Lr // W,
    I1 is (Len * W), I2 is (Lr - 1),
    sublist( I1, I2 , L, Remain),
    check_destroyer_helper(Remain,RemainD).


check_destroyer_LL([],0).
check_destroyer_LL([H|T],TotalDestroyer):-
    check_destroyer_helper(H,TotalDestroyer1),
    check_destroyer_LL(T,TotalDestroyer2),
    TotalDestroyer is TotalDestroyer1 + TotalDestroyer2.
    
        
%check_destroyer_helper(H,TotalDestroyer)
check_destroyer_helper([],0).
check_destroyer_helper([l],0).
check_destroyer_helper([l,r|T], TotalDestroyer):-
    check_destroyer_helper(T, TotalDestroyer2),
    TotalDestroyer is TotalDestroyer2 + 1.
check_destroyer_helper([w|T],TotalDestroyer):-
    check_destroyer_helper(T,TotalDestroyer).
check_destroyer_helper([c|T],TotalDestroyer):-
    check_destroyer_helper(T,TotalDestroyer).
check_destroyer_helper([l,Y|T],TotalDestroyer):-
    Y \=r,  
    check_destroyer_helper(T,TotalDestroyer).
check_destroyer_helper([r|T],TotalDestroyer):-  
    check_destroyer_helper(T,TotalDestroyer).

%ensure_hints(L,Hints,W,H)
ensure_hints(_,[],_,_).
ensure_hints(L,[Head|T],W,H):-
    ensure_hints_helper(L,Head,W),
    ensure_hints(L,T,W,H).

%ensure_hints_helper(L,Hint,W)
ensure_hints_helper(L,at(C,R,T),W):-
    I is (R * W) + C ,
    sublist(I,I,L,[T]).

%battleship(L,W,H,TotalSub,TotalDes,TotalRows,TotalColumns)
battleship(L,W,H,TotalSub,TotalDes,TotalRows,TotalColumns):-
    
    NumberOfCells is W * H , 
    length(L, NumberOfCells) ,
    collect_hints(Hints),
    ensure_hints(L,Hints,W,H),
    random_assignment(L),
    mycheck(L),
    check_submarines(L,W,H,TotalSub),
    check_rows(L,W,H,TotalRows),
    check_columns(L,W,H,TotalColumns),
    check_destroyer(L,W,H,TotalDes).   
    
   
