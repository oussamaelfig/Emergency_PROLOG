:- dynamic patient/3.
:- dynamic tempsConsultation/1.
:- use_module(library(lists)).

%Temps de consultation
tempsConsultation( 6 ).

%[patient( 204, 4, 2 ),patient( 203, 3, 2 ),patient( 202, 2, 2 ),patient( 304, 8, 3 ),patient( 404, 12, 4 ),patient( 403, 11, 4 ),patient( 401, 9, 4 ),patient( 402, 10, 4 ),patient( 401, 9, 4 ),patient( 504, 16, 5 ),patient( 503, 15, 5 ),patient( 502, 14, 5 ),patient( 501, 13, 5 ),patient( 201, 1, 2 ),patient( 303, 7, 3 ),patient( 302, 6, 3 ),patient( 301, 5, 3 )]

% Liste des patients
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

% Prédicat général prends le nom de fichier et ordonne les prédicats patient selon la deuxieme régle et affiche les éléments dans la sortie standard
resoudre( NomFichier ) :-    
    patient_ordon(NonOrdon, OrdonneRegle1,Indexes),
    ordonnancerRegle2(OrdonneRegle1, Ordonne),
    maplist( patientIndexe, Ordonne,Indexes, OrdonneI),
    forall(member(patient(I, X, Y, Z), OrdonneI),format('~w ~w ~w~n', [I, X, Z])),
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


% SortedList : Liste des patients ordonnés
% ListeFractile : Liste des fractiles
% MoyenneArit : La moyenne arithmetique
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
% Prend un patient et lui associe une clée qui est le troisieme argument (priorité)
key_value(k(C,NegB), patient(A,B,C)) :-
    patient(A, B, C),
    NegB is -B.

% prend les prédicats patients et les ordonnes selon la première régles
% PairesNonOrdon : patients non ordonnés
% List : Liste des patients ordonnée
% ListIndex : Liste des index pour chaque patient 
patient_ordon(PairesNonOrdon, List, ListIndex) :-
    bagof(K-P, key_value(K, P), PairesNonOrdon),
    keysort(PairesNonOrdon, PairesOrdon),
    pairs_values(PairesOrdon, List),
    length(List,Taille),
    numlist(1, Taille, ListIndex).


% patientIndexe prend un patient puis ajoute un indexe pour afficher le patient avec son index
% patient(A,B,C) : un patient avec 3 args
% Index : l'indice du patient
% patient(Index,A,B,C) : patient avec son indice 
patientIndexe(patient(A,B,C), Index, patient(Index,A,B,C)).


%%%%%%%%%%%%%
listIndex1(0,[],_).
listIndex1(Taille, [DebutL|TailL], Position):-
    DebutL is Position+1,
    TailleTrans is Taille -1,
    listIndex1(TailleTrans, TailL, DebutL).



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


% traduire le prédicat patient de 3 arguments à 4 argument en ajoutant l'etat du patient
traductionPatient(patient(A, B, C), patient(A, B, C, Pass), Index, TCons) :-
    patientPassATemps(patient(A, B, C, Pass), Index, TCons).

% prédicat qui prends la liste des patients et retourne une liste des patient avec leur état (à temps ou en retards)
listeATempsEtRetards(_, [], [], _).
listeATempsEtRetards(TCons, [Debut|Suite], [DebutP|SuiteP], Index) :-
    traductionPatient(Debut, DebutP, Index, TCons),
    IndexSuite is Index + 1,
    listeATempsEtRetards(TCons, Suite, SuiteP, IndexSuite).

% prédicat qui prends la liste des patients avec leur état à temps ou en retards puis il retourne combien de patient à temps par priorité
% map_list_to_pairs pour avoir une liste avec les prédicats et leurs clees qui sont le 3eme argument
% sort : pour ordonner les listes des cles
% group_pairs_by_key grouper les pairs avec leur cle priorité
% ListP : La liste des patients avec leurs état (aTemps ou en retard)
% CountInTime : Le nombre de patient avec l'Etat a temps par priorité
count_occurrences(ListP,CountInTime) :-
    map_list_to_pairs(arg(3),ListP,Keyed),
    sort(1, @=<, Keyed, Ps),
    group_pairs_by_key(Ps,Grouped),
    findall([patient(_,_,K,aTemps),N],
            (   member(K-L,Grouped),
                aggregate_all(count,member(patient(_,_,K,aTemps),L),N)
            ),CountInTime).


% prédicat qui prends la liste des patients puis il retourne le nombre de patient par priorité
% map_list_to_pairs pour avoir une liste avec les prédicats et leurs clees qui sont le 3eme argument
% sort : pour ordonner les listes des cles
% group_pairs_by_key grouper les pairs avec leur cle priorité
% ListP : La liste des patients avec leurs état (aTemps ou en retard)
% Counted : Le nombre de patient par priorité
count_occurrences_all(ListP,Counted) :-
    map_list_to_pairs(arg(3),ListP,Keyed),
    sort(1, @=<, Keyed, Ps),
    group_pairs_by_key(Ps,Grouped),
    findall([patient(_,_,K,_),N],
            (   member(K-L,Grouped),
                length(L,N)
            ),Counted).


%[patient(201, 4, 2, aTemps), patient(202, 4, 3, retard),patient(201, 4, 3, aTemps), patient(202, 4, 2, retard)]

% count_occurrences(ListP, [Count2, Count3, Count4, Count5]) :-
%     count_OccurPrioX(ListP, patient(_, _, 2, aTemps), Count2),
%     count_OccurPrioX(ListP, patient(_, _, 3, aTemps), Count3),
%     count_OccurPrioX(ListP, patient(_, _, 4, aTemps), Count4),
%     count_OccurPrioX(ListP, patient(_, _, 5, aTemps), Count5).

% count_occurrences_all(ListP, [Count2, Count3, Count4, Count5]) :-
%     count_OccurPrioX(ListP, patient(_, _, 2, _), Count2),
%     count_OccurPrioX(ListP, patient(_, _, 3, _), Count3),
%     count_OccurPrioX(ListP, patient(_, _, 4, _), Count4),
%     count_OccurPrioX(ListP, patient(_, _, 5, _), Count5).

% count_OccurPrioX([], _,[_,0]).
% count_OccurPrioX([patient(_,_,X,Etat)|Tail], X, Etat, [patient(_,_,X,Etat), NbrOcc]) :-
%     count_OccurPrioX(Tail, X, Etat, [patient(_,_,X,Etat), NbrOccVieux]),
%     NbrOcc is NbrOccVieux + 1.    
% count_OccurPrioX([patient(_,_,X,Etat)|Tail],Y, EtatY, [patient(_,_,Y,Etat1), NbrOcc]) :-
%     X \= Y;
%     Etat \= Etat1,
%     count_OccurPrioX(Tail, Y, EtatY, [patient(_,_,Y,Etat1), NbrOcc]).

% calcul les fractiles à partir des deux listes (liste des patient à temps avec la liste de nbr de patient par prio)
% [patient(_,_,Prio,_),X] : représente les patients avec leur etat à temps
% [patient(_,_,Prio,_),Y] : représente le nombre de patient par peiorite
fractile([patient(_,_,Prio,_),X], [patient(_,_,Prio,_),Y], fract(Prio,Z)) :- Z is X/Y.

rawFractile(fract(_,Z),Z).
rawFractileListe(ListeFractile,ListeRawFractile):- maplist(rawFractile,ListeFractile,ListeRawFractile).

% Pérdicat qui calcul la moyenne arithmétique d'une liste
% List des fractiles 
% Avg  la moyenne calcule 
moyenne( List, Avg ):-
    sumlist( List, Sum ),
    length( List, Length),
    (  Length > 0
    -> Avg is Sum / Length
    ;  Avg is 0
    ).





/** ---------------------------------------- 2eme regle ---------------------------------------------------- */

calculScore(patient(A, B, C),patient(A, B, C, Score)) :-
    tempsConsultation(TCons),
    trouveMaxAttente(patient(A, B, C), Max),
    TimeLeft is Max - B,
    Diff is mod(TimeLeft, TCons),
    Score is (TimeLeft - Diff) / TCons.


calculScoreList(List, ListRet):-
    maplist(calculScore,List,ListRet).



/** indique quel patient a prioritÃ© selon le score, la premiÃ¨re regle devrait avoir Ã©tÃ© dÃ©jÃ  appliquÃ©e */
compareScore(patient(A, B, C, ScoreA), patient(_, _, _, ScoreB), patient(A, B, C, ScoreA)) :-
    ScoreA >= 0,
    ScoreB >= 0,
    ScoreA < ScoreB.
compareScore(patient(_, _, _, ScoreA), patient(ID, Wait, Prio, ScoreB), patient(ID, Wait, Prio, ScoreB)) :-
    ScoreA >= 0,
    ScoreB >= 0,
    ScoreA > ScoreB.
compareScore(patient(A, B, C, ScoreA), patient(_, _, _, ScoreB), patient(A, B, C, ScoreA)) :-
    ScoreA < 0,
    ScoreB < 0,
    ScoreA < ScoreB.
compareScore(patient(_, _, _, ScoreA), patient(ID, Wait, Prio, ScoreB), patient(ID, Wait, Prio, ScoreB)) :-
    ScoreA < 0,
    ScoreB < 0,
    ScoreA > ScoreB.

compareScore(patient(A, B, C, ScoreA), patient(_, _, _, ScoreB), patient(A, B, C, ScoreA)) :-
    ScoreA >= 0,
    ScoreB < 0.
compareScore(patient(_, _, _, ScoreA), patient(ID, Wait, Prio, ScoreB), patient(ID, Wait, Prio, ScoreB)) :-
    ScoreA < 0,
    ScoreB >= 0.

compareScore(patient(A, B, C, ScoreA), patient(_, _, Prio, ScoreB), patient(A, B, C, ScoreA)) :-
    =(ScoreA, ScoreB),
    C =< Prio.
compareScore(patient(_, _, C, ScoreA), patient(ID, Wait, Prio, ScoreB), patient(ID, Wait, Prio, ScoreB)) :-
    =(ScoreA, ScoreB),
    C > Prio.

/** permet de passer de patient/4 Ã  patient/3 */
transformPatientBack(patient(A, B, C, _), patient(A, B, C)).

/** pour enlever les listes vides, d'une liste de listes, et ainsi Ã©viter les erreurs*/
enleverListesVide([], []).
enleverListesVide([Debut|Suite], [Debut|SuiteClean]) :-
    Debut \= [],
    enleverListesVide(Suite, SuiteClean).
enleverListesVide([Debut|Suite], SuiteClean) :-
    =(Debut, []),
    enleverListesVide(Suite, SuiteClean).

/** Pour facilement obtenir le premier Ã©lÃ©ment d'une liste*/
takeFirstElement([Debut|_], Debut).

/**prend une liste: ex.: [[patient(201, 4, 2, 0), patient(202, 3, 2, 0)], [patient(301, 3, 3, 1)], [patient(401, 15, 4, 3)]]
    et prend le premier Ã©lÃ©ment de chaque pour faire une liste de patients, les listes utilisÃ© devraient avoir Ã©tÃ© ordonnÃ©e
    selon la rÃ¨gle 1*/
listePlusImportChaquePrio([], []).
listePlusImportChaquePrio([Prio1|AutrePrio], [PatientPrio1|PatientsPrios]) :-
    takeFirstElement(Prio1, PatientPrio1), 
    listePlusImportChaquePrio(AutrePrio, PatientsPrios).

/** devrait utilisÃ©e une liste obtenue avec listePlusImportChaquePrio, et donne le patient le plus important
    selon la rÃ¨gle 2*/
determinePatientPlusImport([ProchainPatient], ProchainPatient).
determinePatientPlusImport([Pat1|Pats], ProchainPatient) :-
    determinePatientPlusImport(Pats, AutrePat),
    compareScore(Pat1, AutrePat, ProchainPatient).

/** enlever patient d'une (simple) liste de patients */
enleverPat(_, [], []).
enleverPat(LePatient, [LePatient|Pats], Pats).
enleverPat(LePatient, [Pat1|Pats], [Pat1|ListeResultat]) :-
    LePatient \= Pat1,
    enleverPat(LePatient, Pats, ListeResultat).


enleverPatGlob(_, [], []).
enleverPatGlob(LePatient, [Debut|Tail], [DebutT|TailT]):-
    enleverPat(LePatient,Debut,DebutT),
    enleverPatGlob(LePatient,Tail,TailT).

/** -1 au score */
enlever1Score(patient(A, B, C, Score), patient(A, B, C, ScoreSous)) :- ScoreSous is Score - 1.

enlever1ScoreList([] | []).
enlever1ScoreList(ListeB, ListeRES) :-
    maplist(enlever1Score, ListeB, ListeRES).

ordreParPrio(OrdonRegel1, OrdonneRegroupe):-
    findall(I, (member(A, OrdonRegel1), arg(3, A, I)), Is), sort(Is, Js),
    findall(S, (member(J, Js), findall(P, (member(P, OrdonRegel1), arg(3, P, J)), S)), OrdonneRegroupe).


regle2([],[]).
regle2(ListL, [Head|Tail]):-
    listePlusImportChaquePrio(ListL, ListElems),
    determinePatientPlusImport(ListElems,ProchainPatient),
    enleverPatGlob(ProchainPatient,ListL,ListL2),
    Head = ProchainPatient,
    enleverListesVide(ListL2, ListL3),
    maplist(enlever1ScoreList,ListL3,ListL4),
    regle2(ListL4, Tail).


ordonnancerRegle2(OrdonRegle1, Ordon):-
    ordreParPrio(OrdonRegle1,ListL),
    maplist(calculScoreList,ListL, ListLS),
    regle2(ListLS,ListOrd),
    maplist(transformPatientBack, ListOrd, Ordon).
