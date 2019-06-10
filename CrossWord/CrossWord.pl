word([z,o,m,b,i,f,i,e,s]).
word([a,k,e,c,a,b,e,l,e]).
word([b,r,i,c,k,w,o,r,k]).
word([b,a,c,k,c,h,e,c,k]).
word([a,c,m,r,r,e,m,a,d]).
word([n,h,g,w,p,f,a,b,z]).
word([j,e,l,l,y,b,e,a,n]).
word([e,a,r,r,e,o,d,e,d]).
	
getall:-	
word([A1,A2,A3,A4,A5,A6,A7,A8,A9]),
atomic_list_concat([A1,A2,A3,A4,A5,A6,A7,A8,A9],R),
assertz(word2(R,A1,A2,A3,A4,A5,A6,A7,A8,A9)),false.

agPuzzle(V1,V2,V3,V4,H1,H2,H3,H4):-
(getall ; true) -> (agPuzzle2(V1,V2,V3,V4,H1,H2,H3,H4);
retractall(word2(R,A1,A2,A3,A4,A5,A6,A7,A8,A9))).

agPuzzle2(V1,V2,V3,V4,H1,H2,H3,H4):-
 word2(V1, _, E1, _, E2, _, E3, _, E4, _),
 word2(V2, _, E5, _, E6, _, E7, _, E8, _),
 word2(V3, _, E9, _, E10, _, E11, _, E12, _),
 word2(V4, _, E13, _, E14, _, E15, _, E16, _),
 word2(H1, _, E1, _, E5, _, E9, _,E13, _),
 word2(H2, _, E2, _, E6, _, E10, _,E14, _),
 word2(H3, _, E3, _, E7, _, E11, _,E15, _),
 word2(H4, _, E4, _, E8, _, E12, _,E16, _),
\+ (H1=V1).

