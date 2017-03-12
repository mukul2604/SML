(* SML code to find strategic companies 
  On SML prompt: use "/path/to/strategic_companies.sml";
  val input = <give input>;
  strategic_companies(input);
*)
fun member(E,L) =  if L=[] then false else
  if hd(L)=E then true else member(E,tl(L));

fun notmember(E,L) =  if L=[] then true else
  if hd(L)=E then false else notmember(E,tl(L));

fun map(f,L)=if L=[] then [] else
  f(hd(L))::map(f,tl(L));

fun filter(f,L) = if L=[] then [] else
  if f(hd(L)) then filter(f,tl(L)) else
  hd(L)::filter(f,tl(L));

fun subseq(L1,L2) = if L1 = L2 then false else 
  if L1 = []  then true else if L2 = []  then false
  else if member(hd(L1),L2) then subseq(tl(L1),L2) else false;

fun isnotuniqueset(E, L) =  if E = [] then true else if L = []  then false
  else if subseq(E,hd(L)) then true 
  else isnotuniqueset(E,tl(L));

fun insertAll(E,[]) = []
  | insertAll(E,h::t) = (E::h)::insertAll(E, t);

fun powerset(L) = if L =[] then [[]]
  else powerset(tl(L))@insertAll(hd(L), powerset(tl(L)));

fun removedups(L) = if L=[] then []
  else if member(hd(L),tl(L)) then removedups(tl(L))
  else hd(L)::removedups(tl(L));
(*
Test input
val input =
  [("traderJoes",["pasta","meat"],["aldi"]),("aldi",[],[]),
   ("costco",["water","tires","icecream"],[]),("wholeFoods",["icecream"],[])];
*
val input =
    [("1",["pasta","meat"],[]), ("2",["water","tires","icecream"],[]), ("3",["water","tires"],["1","2"]), ("4",["icecream"],["3"])];
val uniquesets = filter(
                          fn S => isnotuniqueset(#2(S),
                          map(fn S=> #2(S), input))
                          , input
                       );
*
val companies = map(fn S=>(#1(S),#3(S)),
                         filter(
                                  fn S => isnotuniqueset(#2(S),
                                  map(fn S=> #2(S), input))
                                  , input
                               )
                   );
*)

(* min set which contains all products *)
fun get_minset(input:(string * string list * string list) list) = if  input = []  then []  else
   map(fn S=>(#1(S)),
          filter(
                   fn S => isnotuniqueset(#2(S),
                   map(fn S=> #2(S), input))
                   , input
                )
      );

fun get_all_tuples(input:(string * string list * string list) list) =  if input = []  then []  else
  (#1(hd(input)), #3(hd(input))) :: get_all_tuples(tl(input));

(* get those tuples who has no owner *)
fun get_valid_tuples(input:(string * string list * string list) list) = if input = [] then  [] else
  filter(fn S=> #2(S) = [] , get_all_tuples(input));


fun possible_owner_set(input:(string * string list * string list) list) =  if input = [] then []
  else tl(powerset(get_minset(input)));

fun get_ownerset(input) =
  filter(fn S=> notmember(S,map(fn S=> #2(S), get_valid_tuples(input))), possible_owner_set(input));

fun get_company(E, L: (string * string list) list) =  if L = [] then nil else
 if #2(hd(L)) = E then [#1(hd(L))] else get_company(E,tl(L));

fun get_final_with_dups(L1, L2, VT) = if L2 =[] then L1 else
  L1 @ get_company(hd(L2), VT) @ get_final_with_dups(L1, tl(L2), VT);

(*
get_final_with_dups(get_minset(input), get_ownerset(input));
*)

fun strategic_companies(input:(string * string list * string list) list) =  if input = []  then []  else
  removedups(get_final_with_dups(get_minset(input), get_ownerset(input), get_valid_tuples(input)));
