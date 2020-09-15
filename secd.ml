open Printf;;
exception BADSTACK;;
exception VARNOTFOUND;;
type exp = Var of string|Funabs of (string)*(exp)|Funapp of exp*(exp)|Num of int|T|F|Plus of exp*(exp)|ITE of (exp)*(exp)*(exp)|Equal of (exp)*(exp) ;;
type opcode = LOOKUP of string|LDC of int|ABS of string*(opcode list)|LDCB of exp|COND of (opcode list)*(opcode list)|APPLY|PLUS|EQUAL;;
type answer = Num of int|T|F|VCL of (table*string*(opcode list))
and table = (string*(answer)) list;;


let rec compile e = match e with
| Var(x) -> [LOOKUP(x)]
| Funabs(s,e1) -> [ABS(s,compile(e1))]
| Funapp(e1,e2) -> compile(e1)@compile(e2)@[APPLY]
| Num(n) -> [LDC(n)]
| T -> [LDCB(T)]
| F -> [LDCB(F)]
| Plus(e1,e2) -> compile(e1)@compile(e2)@[PLUS]
| Equal(e1,e2) -> compile(e1)@compile(e2)@[EQUAL]
| ITE(e,e1,e2) -> compile(e)@[COND(compile(e1),compile(e2))];;



let rec lookup g x = match  g with
| [] -> raise VARNOTFOUND
| (y,a)::gs -> if(String.equal x y) then a else lookup gs x;;


let rec stkmc (s:answer list) (c:opcode list) (g:table) (d: ((opcode list)*(answer list)*(table)) list) = match (c,s,d) with
| ([],[],[]) -> raise BADSTACK
| ([],m::ms,[]) -> m
| ([],m::ms,(cp,sp,gp)::ds) -> stkmc (m::sp) cp gp ds
| (LOOKUP(x)::cs,s,d) -> stkmc ((lookup g x)::s) cs g d
| (ABS(vr,ex)::cs,s,d) -> stkmc (VCL(g,vr,ex)::s) cs g d
| (APPLY::cs, a::VCL(g1,vr,ex)::ms, d) -> stkmc [] ex ((vr,a)::g1) ((cs,ms,g)::d)
| (LDC(n)::cs,s,d) -> stkmc (Num(n)::s) cs g d
| (PLUS::cs, Num(n2)::Num(n1)::ms, d) ->  (* let k = printf "evaluated" in *) stkmc (Num(n1+n2)::ms) cs g d
| (EQUAL::cs, Num(n2)::Num(n1)::ms, d) -> if(n1 = n2) then stkmc (T::ms) cs g d else stkmc (F::s) cs g d
| (LDCB(T)::cs,s,d) -> stkmc (T::s) cs g d
| (LDCB(F)::cs,s,d) -> stkmc (F::s) cs g d
| (COND(c1,c2)::cs, T::ms, d) -> stkmc ms (c1@cs) g d
| (COND(c1,c2)::cs, F::ms, d) -> stkmc ms (c2@cs) g d
| _ -> raise BADSTACK;;


let h = Funabs("x",Plus(Var("x"),Num(1)));;
let ABS(l,t)::ks = compile h;;
let g2 = [("y", VCL([],"x",t))];;
let e1 = Funapp(Var("y"),Num(2));;
let k = stkmc [] (compile e1) g2 [];;

let e2 = Funapp(Funabs("x",Num(8)),Plus(Num(1),Num(2)));;
let k1 = stkmc [] (compile e2) [] [];;

(*to show that Plus(Num(1),Num(2)) uncomment the print statment in Plus of stkmc*)