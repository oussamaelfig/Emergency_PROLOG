:- dynamic patient/3.
:- dynamic tempsConsultation/1.
:- use_module(library(lists)).

%Temps de consultation
tempsConsultation( 15 ).


% Liste des patients
patient( 201, 1, 2 ).
patient( 401, 1, 4 ).
patient( 501, 1, 5 ).


%Prédicat général
resoudre( NomFichier ) :-    
    patient_ordon(NonOrdon, Ordonne),
    forall(member(patient(X, Y, Z), Ordonne),format('~w ~w~n', [X, Z])),
    affFractileMoyenne(Ordonne, ListeFractile, MoyenneArit),
    writeln('--------'),
    forall(member(fract(A, B), ListeFractile),format('~w ~w~n', [A, B])),
    maplist(writeln, Answer),
    writeln(MoyenneArit).
    

% test(List,Indices,Pairs):-
%     length(List, LLen),
%     End is LLen + 1,
%     numlist(2, End, Indices),
%     pairs_keys_values(Pairs, Indices, List),
%     maplist(writeln, Indices List),


%[patient(201, 4, 2 ), patient(202, 3, 2 ), patient(203, 2, 3 ), patient(204, 1, 3 ), patient(204 , 1, 4 ),patient(203, 2, 4 ), patient(204, 1, 5 ),patient(204, 1, 5 )]

% Le prédicat suivant prend une liste de patient ordonné selon la première régle, et retourne la liste des fractile ainsi que la moyenne arithmétique
% le prédicat transforme la liste en une liste decrivant l'état de chque patient est ce qu'il est à temps ou en retards,
% On compte le nombre de patient arrivé à temps par priorité
% On compte le nombre de patient par priorité
% On calcul les fractiles en divisant les patient à temps par prio sur le nombre de patient par prio
% On calcul la moyenne géométrique à partir de la liste des fractiles
affFractileMoyenne(SortedList, ListeFractile, MoyenneArit):-
    tempsConsultation(X),
    listeATempsEtRetards(X, SortedList, ListeEtat, 0),
    count_occurrences(ListeEtat, NbPatientAtempsParPrio),
    count_occurrences_all(ListeEtat, NbPatientParPrio),
    maplist(fractile, NbPatientAtempsParPrio, NbPatientParPrio, ListeFractile),
    rawFractileListe(ListeFractile,RawListeFractile),
    moyenne(RawListeFractile, MoyenneArit).
    


    % consult( NomFichier ),
    % findall(patient(X,Y,Z),
    % patient(X,Y,Z), Ps),
    % sort(3, @=<, Ps, Result),
    % forall(order_by([asc(Z)],
    % patient(X,Y,Z)),
    % format("~w ~w ~w~n", [X,Y,Z])).

% Première régle :
% ordonner les prédicats patients selon la première régles et le retourner sous forme de liste
key_value(k(C,NegB), patient(A,B,C)) :-
    patient(A, B, C),
    NegB is -B.

patient_ordon(PairesNonOrdon, List) :-
    bagof(K-P, key_value(K, P), PairesNonOrdon),
    keysort(PairesNonOrdon, PairesOrdon),
    pairs_values(PairesOrdon, List).

%to exectue : patient_ordon(Unsorted, Sorted), maplist(writeln, Sorted).

% priorite2(15).
% priorite3(30).
% priorite4(60).
% priorite5(120).

/** https://stackoverflow.com/questions/4380624/how-compute-index-of-element-in-a-list*/
trouveIndex(Elem, [Elem|_], 0).
trouveIndex(Elem, [_|Reste], Index) :-
    trouveIndex(Elem, Reste, ResteIndex), Index is ResteIndex + 1.

% pour chaque patient à priorité lui associer son temps d'attente maximum
trouveMaxAttente(patient(_, _, 2), 15).
trouveMaxAttente(patient(_, _, 3), 30).
trouveMaxAttente(patient(_, _, 4), 60).
trouveMaxAttente(patient(_, _, 5), 120).

% Trouver les patients à temps en se basant sur le temps d'attente mnaximum et le temps attendu
patientPassATemps(patient(A, B, C, aTemps), Index, TempsConsultation) :-  
    trouveMaxAttente(patient(A, B, C), Max),
    Attente is TempsConsultation * Index + B,
    Attente =< Max.
% Trouver les patients en retards en se basant sur le temps d'attente mnaximum et le temps attendu
patientPassATemps(patient(A, B, C, retard), Index, TempsConsultation) :- 
    trouveMaxAttente(patient(A, B, C), Max),
    Attente is TempsConsultation * Index + B,
    Attente > Max.

/**listeATempsEtRetards(TmpsCons, [patient(A, B, C)], [patient(A, B, C, Pass)]) :-
    patientPassATemps(patient(A, B, C, Pass), trouveIndex(patient(A, B, C), [patient(A, B, C)], Index), TmpsCons).

listeATempsEtRetards(TmpsCons, [Debut|Suite], [DebutP|SuiteP]) :-
    listeATempsEtRetards(TmpsCons, [Debut], [DebutP]),
    listeATempsEtRetards(TmpsCons, Suite, SuiteP).*/

% traduire le prédicat patient de 3 arguments à 4 argument en ajoutant l'etat du patient
traductionPatient(patient(A, B, C), patient(A, B, C, Pass), Index, TCons) :-
    patientPassATemps(patient(A, B, C, Pass), Index, TCons).

% prédicat qui prends la liste des patients et retourne une liste des patient avec leur état (à temps ou en retards)
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

% prédicat qui prends la liste des patients avec leur état à temps ou en retards puis il retourne combien de patient à temps par priorité
count_occurrences(ListP,CountInTime) :-
    map_list_to_pairs(arg(3),ListP,Keyed),
    group_pairs_by_key(Keyed,Grouped),
    findall([patient(_,_,K,aTemps),N],
            (   member(K-L,Grouped),
                aggregate_all(count,member(patient(_,_,K,aTemps),L),N)
            ),CountInTime).


% prédicat qui prends la liste des patients puis il retourne le nombre de patient par priorité
count_occurrences_all(ListP,Counted) :-
    map_list_to_pairs(arg(3),ListP,Keyed),
    group_pairs_by_key(Keyed,Grouped),
    findall([patient(_,_,K,_),N],
            (   member(K-L,Grouped),
                length(L,N)
            ),Counted).

% calcul les fractiles à partir des deux listes (liste des patient à temps avec la liste de nbr de patient par prio)
fractile([patient(_,_,Prio,_),X], [patient(_,_,Prio,_),Y], fract(Prio,Z)) :- Z is X/Y.

rawFractile(fract(_,Z),Z).
rawFractileListe(ListeFractile,ListeRawFractile):- maplist(rawFractile,ListeFractile,ListeRawFractile).

% Pérdicat qui calcul la moyenne arithmétique d'une liste
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
