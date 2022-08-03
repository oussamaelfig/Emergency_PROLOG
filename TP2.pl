:- dynamic patient/3.
:- dynamic tempsConsultation/1.
:- use_module(library(lists)).

tempsConsultation( 15 ).



% Sorted est la liste des patient ordonnée
% T est la liste des patient splited 
% [[patient(204,4,2),patient(203,3,2),patient(202,2,2),patient(201,1,2)],[patient(304,8,3),patient(303,7,3),patient(302,6,3),patient(301,5,3)],[patient(404,12,4),patient(403,11,4),patient(402,10,4),patient(401,9,4)],[patient(504,16,5),patient(503,15,5),patient(502,14,5),patient(501,13,5)]]


resoudre( NomFichier ) :-    
    p_sort(Unsorted, Sorted),
    forall(member(patient(X, Y, Z), Sorted),
    format('~w ~w ~w~n', [X, Y, Z])),
    findall(I, (member(A, Sorted), arg(3, A, I)), Is),
    sort(Is, Js),
    findall(S, (member(J, Js), findall(P, (member(P, Sorted), arg(3, P, J)), S)), T),
    write(T).
    
    


    % consult( NomFichier ),
    % findall(patient(X,Y,Z),
    % patient(X,Y,Z), Ps),
    % sort(3, @=<, Ps, Result),
    % forall(order_by([asc(Z)],
    % patient(X,Y,Z)),
    % format("~w ~w ~w~n", [X,Y,Z])).
    

patient( 201, 1, 2 ).
patient( 202, 2, 2 ).
patient( 203, 3, 2 ).
patient( 204, 4, 2 ).
patient( 301, 5, 3 ).
patient( 302, 6, 3 ).
patient( 303, 7, 3 ).
patient( 304, 8, 3 ).
patient( 401, 9, 4 ).
patient( 402, 10, 4 ).
patient( 403, 11, 4 ).
patient( 404, 12, 4 ).
patient( 501, 13, 5 ).
patient( 502, 14, 5 ).
patient( 503, 15, 5 ).
patient( 504, 16, 5 ).


key_value(k(C,NegB), patient(A,B,C)) :-
    patient(A, B, C),
    NegB is -B.

p_sort(PairsUnsorted, List) :-
    bagof(K-P, key_value(K, P), PairsUnsorted),
    keysort(PairsUnsorted, PairsSorted),
    pairs_values(PairsSorted, List).

%to exectue : p_sort(Unsorted, Sorted), maplist(writeln, Sorted).




tempsDattente( 15 ):-
    patient(_,_,2).
    
tempsDattente( 30 ):-
    patient(_,_,3).
    
tempsDattente( 60 ):-
    patient(_,_,4).
    
tempsDattente( 120 ):-
    patient(_,_,5).
    

nbrPatientPrio([],[]).
nbrPatientPrio([patient(_,_,2,aTemps)|T], NbrATempsPrio):-
    nbrPatientPrio(T,N1), NbrATempsPrio is N1+1.
nbrPatientPrio([X|T],NbrATempsPrio):-
    X \=  patient(_,_,2,aTemps), nbrPatientPrio(T,NbrATempsPrio).


count([],X,0).
count([patient(_,_,X)|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([patient(_,_,X1)|T],X,Z):- X1\=X,count(T,X,Z).

countall(List,X,C) :-
    count(List,X,C).

%%%%%%%%%%%%%%%%%%%
%count_occurrences(List, Occ):-
%    findall([patient(_,_,X,aTemps),L], (bagof(true,member(patient(_,_,X,aTemps),List),Xs), length(Xs,L)), Occ).
%%%%%%%%%%%%%%%%%%%%
%count_occurrences([patient(201, 4, 2, aTemps), patient(202, 3, 2, retard), patient(203, 2, 3, retard), patient(204, 1, 3, retard), patient(204 , 1, 4, aTemps),patient(203, 2, 4, retard), patient(204, 1, 5, aTemps),patient(204, 1, 5, aTemps)],Occ).
%Occ = [[patient(_, _, 2, aTemps), 1], [patient(_, _, 4, aTemps), 1], [patient(_, _, 5, aTemps), 2]].

%?- maplist(div, Occ, Occ_all, Result).


count_occurrences(ListP,CountInTime) :-
    map_list_to_pairs(arg(3),ListP,Keyed),
    group_pairs_by_key(Keyed,Grouped),
    findall([patient(_,_,K,aTemps),N],
            (   member(K-L,Grouped),
                aggregate_all(count,member(patient(_,_,K,aTemps),L),N)
            ),CountInTime).


count_occurrences_all(ListP,Counted) :-
    map_list_to_pairs(arg(3),ListP,Keyed),
    group_pairs_by_key(Keyed,Grouped),
    findall([patient(_,_,K,_),N],
            (   member(K-L,Grouped),
                length(L,N)
            ),Counted).

div([_,X], [_,Y], Z) :- Z is X/Y.


avg( List, Avg ):-
    sumlist( List, Sum ),
    length( List, Length),
    (  Length > 0
    -> Avg is Sum / Length
    ;  Avg is 0
    ).
    
% count_occurrences_all(List, Occ):-
%     findall([patient(_,_,X),L], (bagof(true,member(patient(_,_,X),List),Xs), length(Xs,L)), Occ).


Occ = [
    [patient(_, _, 2, inTime), 1],
    [patient(_, _, 3, inTime), 0],
    [patient(_, _, 4, inTime), 1],
    [patient(_, _, 5, inTime), 2]
 ].
 
 Occ_all = [
    [patient(_, _, 2, _), 2],
    [patient(_, _, 3, _), 2],
    [patient(_, _, 4, _), 2],
    [patient(_, _, 5, _), 2]
 ].

%div([_,X], [_,Y], Z) :- Z is X/2.
