fun insertAll(E,L) = if L=[] then [] else
 (E::hd(L))::insertAll(E,tl(L));

fun member(X,L) = if L=[] then false  else
 if hd(L)=X then true else member(X,tl(L));

fun union(L1,L2) =  if(L1=[]) then L2 else
  if(L2=[]) then L1 else 
  if member(hd(L1),L2) then union(tl(L1),L2)
  else hd(L1)::union(tl(L1),L2);

fun insertAll(E,L) = if L=[ ] then [ ] else
 (E::hd(L))::insertAll(E,tl(L));

fun powers(L) = if L=[] then [[]] else
 powerset(tl(L)) @ insertAll(hd(L), powerset(tl(L)));
 powerset([1,2,3,4,5]);

fun add(L) = if L=[] then 0 else hd(L)+add(tl(L));
fun add([]) = 0 | add(h::t) = h+add(t);

fun filter(f,L) = if L=[] then []  else
 if f(hd(L)) then hd(L)::filter(f,tl(L)) else
 filter(f,tl(L));

fun filteredPowerset(L) = filter( (fn S=>add(S)<10), powerset(L));
  fun prevLast(L) = if L=[] orelse tl(L)=[] then 0 else
  if tl(tl(L))=[] then hd(L) else
  prevLast(tl(L)); 
 (4,(3,(1,[],[]),(2,[],[])),(5,[],[]));

fun kth(N,L) = if L=[] then 0 else
 if N=1 then hd(L) else
 kth(N-1,tl(L));
 kth(3,[6,5,4,3,2]);

fun length([]) = 0 | length(h::t) = 1+length(t);

fun reverse([]) = []
 | reverse(h::t) = reverse(t) @ [h];

fun palindrome(L) = L=reverse(L);

fun flatten([]) =[] | flatten(h::t) = h@flatten(t);

fun eliminateConsequtive([]) = [] 
  | eliminateConsequtive(h::t) = if t=[] then h else 
    if hd(t)=h then eliminateConsequtive(t) else
     h::eliminateConsequtive(t);

fun eliminateDuplicates(L) = if L=[] then [] else
 if member(hd(L),tl(L)) then eliminateDuplicates(tl(L)) else
 hd(L)::eliminateDuplicates(tl(L));

fun removeAll(E,L) =  if L=[] then [] else 
  if hd(L)=E then removeAll(E,tl(L)) else 
  hd(L)::removeAll(E,tl(L));


fun prefix(E,L) = if L=[] then [] else
  if hd(L)=E then E::prefix(E,tl(L)) else [];

fun deleteprefix(E,L) = if L=[] then [] else
 if hd(L)=E then deleteprefix(E,tl(L)) else L;

fun pack(L) =  if L=[] then [] else
 prefix(hd(L),L):: pack(deleteprefix(hd(L), tl(L)));

pack[1,1,1,2,2,3,1,1,2,3]; [[1,1,1],[2,2],[3],[1,1],[2],[3]];

fun encode(L) = map( (fn S=>(length(S),hd(S))), pack(L));
encode([1,1,1,2,2,3,1,1,2,3]);
[(3,1),(2,2),(1,3),(2,1),(1,2),(1,3)];

fun map(f,L) = if L=[] then [] else f(hd(L))::map(f,tl(L));
fun map(f,[]) = [] | map(f,h::t) = f(h)::map(f,t); 

fun insertAll(E,L) = if L=[] then [[E]] else
 (E::L)::map((fn P=>hd(L)::P),  insertAll(E,tl(L)));

fun permutation(L) =  if L=[] then [[]] else 
  flatten( map((fn S=>insertAll(hd(L),S)), permutation(tl(L)))  );
