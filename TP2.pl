:- dynamic patient/3.
:- dynamic tempsConsultation/1.

resoudre( NomFichier ) :-
	consult( NomFichier ),
    % votre code commence ici.

tempsConsultation( 15 ).

patient( 201, 15, 2 ).
patient( 301, 15, 3 ).
patient( 401, 30, 4 ).
patient( 501, 75, 5 ).

key_value(k(C,NegB), patient(A,B,C)) :-
    patient(A, B, C),
    NegB is -B.

p_sort(PairsUnsorted, List) :-
    bagof(K-P, key_value(K, P), PairsUnsorted),
    keysort(PairsUnsorted, PairsSorted),
    pairs_values(PairsSorted, List).

%to exectue : p_sort(Unsorted, Sorted), maplist(writeln, Sorted).