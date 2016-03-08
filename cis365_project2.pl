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

/* ------- Rules --------- */
descend(X, Y) :- parent(Y, X).
descend(A, C) :-  parent(B, A), descend(B, C).

/* X is the aunt or uncle of Y */
aunt_or_uncle(X, Y) :- parent(A, Y), parent(B, A), parent(B, X).  

all_uncles(Y, Z) :- setof(X, aunt_or_uncle(X, Y), Z).
descendants(Y, Z) :- findall(X, descend(X, Y), Z).
parents(Y, Z) :- findall(X, parent(X, Y), Z).

/* ------- Goals --------- */
print_solution :-
	descendants(thor, Thors_descendants),
	write("1. Thor's descendants are "), write(Thors_descendants), nl,
	parents(loki, Lokis_parents),
	write("2. Loki's parents are "), write(Lokis_parents), nl,
	all_uncles(forseti, Forsetis_uncles),
	write("4. Forseti's aunts and uncles are "), write(Forsetis_uncles).


?- print_solution.