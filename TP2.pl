:- dynamic patient/3.
:- dynamic tempsConsultation/1.

tempsConsultation( 15 ).




resoudre( NomFichier ) :-    
    p_sort(Unsorted, Sorted),
    forall(member(patient(X, Y, Z), Sorted),
    format('~w ~w ~w~n', [X, Y, Z])).
    


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

priorite2(15).
priorite3(30).
priorite4(60).
priorite5(120).

