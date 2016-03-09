/*---------------------------------------------------------------------------------------
*CIS 365
*Dr. Jonathan Leidig
*Project 2
*
*Project Description - Convert the set of familial characters in Norse or Greek* mythology to Prolog facts. Then create rules and goals as needed to answer the following questions:
*
*1. List all of Thor’s descendants.
*2. Who is/are Loki’s parents?
*3. Who is Vidar’s father?
*4. Who are Forseti’s aunts and uncles?
*5. List all of Freya’s ancestors.
*6. Is Fenrir a descendant of Bor?
*7. List all pairs of second-cousins.
*8. List all first-cousins once removed. 
*
*@author Laura Young and Joel Truman
*@version Winter 2016
---------------------------------------------------------------------------------------*/

/* -------- Facts ----------- */

male(odin).
/* parent facts are encoded as parent(parent, child) */
parent(ymir, giants).
parent(giants, bestia).
parent(giants, loki).
parent(audhumia, burl).
parent(burl, bor).
parent(loki, narvi).
parent(loki, vali).
parent(loki, hel).
parent(loki, fenris).
parent(loki, midgard_serpent).
parent(sigyn, narvi).
parent(sigyn, vali).
parent(angrboda, hel).
parent(angrboda, fenris).
parent(angrboda, midgard_serpent).
parent(bor, vili).
parent(bor, ve).
parent(bor, odin).
parent(bestia, vili).
parent(bestia, ve).
parent(bestia, odin).
parent(odin, vidor).
parent(odin, thor).
parent(odin, balder).
parent(odin, hoder).
parent(odin, njord).
parent(odin, hermod).
parent(odin, bragi).
parent(odin, tyr).
parent(odin, heimdall).
parent(grid, vidor).
parent(earth, thor).
parent(thor, magni).
parent(thor, modi).
parent(sif, magni).
parent(sif, modi).
parent(frigg, balder).
parent(frigg, hoder).
parent(frigg, njord).
parent(frigg, hermod).
parent(frigg, bragi).
parent(frigg, tyr).
parent(frigg, heimdall).
parent(balder, forseti).
parent(nanna, forseti).
parent(njord, freya).
parent(njord, freyr).
parent(skadi, freya).
parent(skadi, freyr).
/* NOTE: Charles does not exist. We made him up so question 7 would have answers */
parent(fenris, charles).


/* -----------------------------Rules------------------- --------- */

/* ----- Rules to find individuals ----- */
/* X is a sibling of Y if they share a parent */
siblings(X, Y) :- parent(A, X), parent(A, Y), \+(X = Y).
/* X descends from Y if Y is X's parent */
descend(X, Y) :- parent(Y, X).
/* A descends from C if A descends from someone descended from C */
descend(A, C) :-  parent(B, A), descend(B, C).
/* X is the aunt or uncle of Y. Though the rule is named "aunt," it actually finds both */
aunt(X, Y) :- parent(A, Y), siblings(A, X).  
/* X is a cousin of Y if X has an aunt or uncle who is Y's parent */
cousins(X, Y) :- aunt(A, X), parent(A, Y).
/* X is the father of Y if X is Y's male parent */
father(X, Y) :- parent(X,Y), male(X).
/* X and Y are second-cousins if they share a great grandparent, but are not siblings or first cousins */
second_cousins(X, Y) :- parent(A, X), parent(B, A), parent(C, B), parent(D, Y),
	parent(E, D), parent(C, E), \+(X = Y), \+(siblings(X,Y)), \+(cousins(X,Y)).
/* Y is the first cousin once removed of X if Y is the child X's cousin */
fc_once_removed(X, Y) :- cousins(X, A), parent(A, Y).

/* ----- Rules to find sets ----- */
/* All aunts or uncles of Y are Z */
all_aunts(Y, Z) :- setof(X, aunt(X, Y), Z).
/* All descendants of Y are Z */
descendants(Y, Z) :- findall(X, descend(X, Y), Z).
/* All ancestors of Y are Z */
ancestors(Y, Z) :- findall(X, descend(Y, X), Z).
/* All parents of Y are Z */
parents(Y, Z) :- findall(X, parent(X, Y), Z).
/* All second cousins */
all_second_cousins(Z) :- findall((X, Y), second_cousins(X, Y), Z).
/* All first cousins once removed */
all_fcorm(Z) :- setof((X, Y), fc_once_removed(X, Y), Z).

/* ------- Goals --------- */
print_solution :-
	write("Norse mythology family tree in Prolog"), nl,
	write("---------------------------------------------"), nl,
	descendants(thor, Thors_descendants),
	write("1. Thor's descendants are "), write(Thors_descendants), nl,
	parents(loki, Lokis_parents),
	write("2. Loki's parents are "), write(Lokis_parents), nl,
	father(Vidors_father, vidor),
	write("3. Vidor's father is "), write(Vidors_father), nl,
	all_aunts(forseti, Forsetis_uncles),
	write("4. Forseti's aunts and uncles are "), write(Forsetis_uncles), nl,
	ancestors(freya, Freyas_ancestors),
	write("5. Freya's ancestors are "), write(Freyas_ancestors), nl,
	findall(Question6, descend(fenris, bor), Question6),
	write("6. Does Fenris descend from Bor? "), write(Question6), nl,
	all_second_cousins(Question7),
	write("7. All pairs of second cousins are "), write(Question7), nl,
	all_fcorm(Question8),
	write("8. All pairs of first cousins once removed are "), write(Question8).


?- print_solution.