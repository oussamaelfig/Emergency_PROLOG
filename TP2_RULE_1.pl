:- dynamic patient/3.
:- dynamic tempsConsultation/1.
:- use_module(library(lists)).


tempsConsultation( 15 ).




resoudre( NomFichier ) :-    
    p_sort(Unsorted, Sorted),
    forall(member(patient(X, Y, Z), Sorted),
    format('~w ~w ~w~n', [X, Y, Z])),
    affFractileMoyenne(Sorted, ListeFractile, MoyenneArit),
    writeln('-----------'),
    maplist(writeln, ListeFractile),
    writeln(MoyenneArit).
    



%[patient(201, 4, 2 ), patient(202, 3, 2 ), patient(203, 2, 3 ), patient(204, 1, 3 ), patient(204 , 1, 4 ),patient(203, 2, 4 ), patient(204, 1, 5 ),patient(204, 1, 5 )]

affFractileMoyenne(SortedList, ListeFractile, MoyenneArit):-
    tempsConsultation(X),
    listeATempsEtRetards(X, SortedList, ListeEtat, 0),
    count_occurrences(ListeEtat, NbPatientAtempsParPrio),
    count_occurrences_all(ListeEtat, NbPatientParPrio),
    maplist(fractile, NbPatientAtempsParPrio, NbPatientParPrio, ListeFractile),
    moyenne(ListeFractile, MoyenneArit).
    


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

% priorite2(15).
% priorite3(30).
% priorite4(60).
% priorite5(120).

/** https://stackoverflow.com/questions/4380624/how-compute-index-of-element-in-a-list*/
trouveIndex(Elem, [Elem|_], 0).
trouveIndex(Elem, [_|Reste], Index) :-
    trouveIndex(Elem, Reste, ResteIndex), Index is ResteIndex + 1.

trouveMaxAttente(patient(_, _, 2), 15).
trouveMaxAttente(patient(_, _, 3), 30).
trouveMaxAttente(patient(_, _, 4), 60).
trouveMaxAttente(patient(_, _, 5), 120).

patientPassATemps(patient(A, B, C, aTemps), Index, TempsConsultation) :-  
    trouveMaxAttente(patient(A, B, C), Max),
    Attente is TempsConsultation * Index + B,
    Attente =< Max.
patientPassATemps(patient(A, B, C, retard), Index, TempsConsultation) :- 
    trouveMaxAttente(patient(A, B, C), Max),
    Attente is TempsConsultation * Index + B,
    Attente > Max.

/**listeATempsEtRetards(TmpsCons, [patient(A, B, C)], [patient(A, B, C, Pass)]) :-
    patientPassATemps(patient(A, B, C, Pass), trouveIndex(patient(A, B, C), [patient(A, B, C)], Index), TmpsCons).

listeATempsEtRetards(TmpsCons, [Debut|Suite], [DebutP|SuiteP]) :-
    listeATempsEtRetards(TmpsCons, [Debut], [DebutP]),
    listeATempsEtRetards(TmpsCons, Suite, SuiteP).*/

traductionPatient(patient(A, B, C), patient(A, B, C, Pass), Index, TCons) :-
    patientPassATemps(patient(A, B, C, Pass), Index, TCons).

listeATempsEtRetards(_, [], [], _).
listeATempsEtRetards(TCons, [Debut|Suite], [DebutP|SuiteP], Index) :-
    traductionPatient(Debut, DebutP, Index, TCons),
    IndexSuite is Index + 1,
    listeATempsEtRetards(TCons, Suite, SuiteP, IndexSuite).
/** listeATempsEtRetards(15, Liste, ListeTrans, 0).
 Index doit toujours Ãªtre 0 dans le query */


%uneListePatient([patient(201, 4, 2), patient(202, 3, 2), patient(203, 2, 2), patient(204, 1, 2)]).
/** [patient(201, 4, 2, aTemps), patient(202, 4, 2, retard), ...]*/
% tempsConsultation( 15 ).


/**nbrPatientPrio(LaListe, NbrATempsPrio, NbrPatPrio).*/

%tu lui donne la liste des patients avec leur état à temps ou en retards puis il retourne combien de patient à temps par priorité
count_occurrences(ListP,CountInTime) :-
    map_list_to_pairs(arg(3),ListP,Keyed),
    group_pairs_by_key(Keyed,Grouped),
    findall([patient(_,_,K,aTemps),N],
            (   member(K-L,Grouped),
                aggregate_all(count,member(patient(_,_,K,aTemps),L),N)
            ),CountInTime).


%t u lui passe la liste des patients pusi il te retourne le nombre de patient par priorité
count_occurrences_all(ListP,Counted) :-
    map_list_to_pairs(arg(3),ListP,Keyed),
    group_pairs_by_key(Keyed,Grouped),
    findall([patient(_,_,K,_),N],
            (   member(K-L,Grouped),
                length(L,N)
            ),Counted).


% calcul les fractiles des deux listes (liste des patient à temps avec la liste de nbr de patient par prio)
fractile([_,X], [_,Y], Z) :- Z is X/Y.


%calcul la moyenne arithmetique
moyenne( List, Avg ):-
    sumlist( List, Sum ),
    length( List, Length),
    (  Length > 0
    -> Avg is Sum / Length
    ;  Avg is 0
    ).





/** ---------------------------------------- 2eme regle ---------------------------------------------------- */

calculScore(patient(A, B, C, Score), TCons) :-
    trouveMaxAttente(patient(A, B, C), Max),
    TimeLeft is Max - B,
    Diff is mod(TimeLeft, TCons),
    Score is (TimeLeft - Diff) / TCons.