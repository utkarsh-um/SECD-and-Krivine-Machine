exception BADSTACK;;
exception VARNOTFOUND;;
type exp = Num of int|Var of string|Funabs of (string)*(exp)|Funapp of exp*(exp)|Plus of (exp)*(exp);;
type answer = Num of int|Cl of table*(exp)
and table = (string*(answer)) list;;

let rec lookup g x = match g with
| [] -> raise VARNOTFOUND
| (y,c)::gs -> if(String.equal x y) then c else (lookup gs x);;


let add c1 c2 = match (c1,c2) with
| (Cl(_,Num(n1)), Cl(_,Num(n2)) ) -> (Cl([],Num(n1+n2))) 
| _ -> raise BADSTACK;;
let rec stk (c) (s) = match (c,s) with
| (Cl(g,Var(x)), _) -> stk ((lookup g x)) s
| (Cl(g,Funabs(a,e)), d::ms) -> stk (Cl( ((a,d)::g) , e)) ms
| (Cl(g,Funapp(e1,e2)), _) -> stk (Cl(g,e1)) (Cl(g,e2)::s)
| (Cl(g,Num(n)) , _) -> Cl(g,Num(n))
| (Cl(g,Plus(e1,e2)),_) -> stk (add (stk (Cl(g,e1)) []) (stk (Cl(g,e2)) [])) s
| _ -> raise BADSTACK;;

let h = Cl([],Funapp(Funabs("x",Plus(Var("x"),Num(1))), Num(2)));;
let h1 = Cl([],Funapp(Funabs("x",Num(1)),Plus(Num(1), Num(2))));; (* output shows that in the env Plus(Num(1),Num(2)) is not evaluated yet*)