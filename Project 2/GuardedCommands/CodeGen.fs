namespace GuardedCommands.Backend
// Michael R. Hansen 05-01-2016
// This file is obtained by an adaption of the file MicroC/Comp.fs by Peter Sestoft
open System
open Machine

open GuardedCommands.Frontend.AST
module CodeGeneration =


(* A global variable has an absolute address, a local one has an offset: *)
   type Var = 
     | GloVar of int                   (* absolute address in stack           *)
     | LocVar of int                   (* address relative to bottom of frame *)

(* The variable environment keeps track of global and local variables, and 
   keeps track of next available offset for local variables *)

   type varEnv = Map<string, Var*Typ> * int

(* The function environment maps function name to label and parameter decs *)

   type ParamDecs = (Typ * string) list
   type funEnv = Map<string, label * Typ option * ParamDecs>

   let getOffset (map, p) = p


   (* Bind declared variable in env and generate code to allocate it: *)   
   let allocate (kind : int -> Var) (typ, x) (vEnv : varEnv)  =
    let (env, fdepth) = vEnv 
    match typ with
    | ATyp (ATyp _, _) -> 
      raise (Failure "allocate: array of arrays not permitted")
    | ATyp (t, Some i) -> 
      let newEnv = (Map.add x (kind (fdepth+i), typ) env, fdepth+i+1)
      let code = [INCSP i; GETSP; CSTI (i-1); SUB]
      (newEnv, code)
    | _ -> 
      let newEnv = (Map.add x (kind fdepth, typ) env, fdepth+1)
      let code = [INCSP 1]
      (newEnv, code)

   let unpack = function
                |VarDec(a,b) -> (a,b) 
                |_ -> failwith "Please do not use function declarations as parameters"

   //l = (Typ * string) list
   let rec bindLocal (l:Dec list) ven code = match l with
                                             | [] -> (ven, code)
                                             | h::t -> let (env, code2) = allocate LocVar (unpack h)  ven
                                                       bindLocal t env (code @ code2)
            
/// CE vEnv fEnv e gives the code for an expression e on the basis of a variable and a function environment
   let rec CE vEnv fEnv = 
       function
       | N n          -> [CSTI n]
       | B b          -> [CSTI (if b then 1 else 0)]
       | Access acc   -> CA vEnv fEnv acc @ [LDI] 
       | Addr acc     -> CA vEnv fEnv acc

       | Apply("-", [e]) -> CE vEnv fEnv e @  [CSTI 0; SWAP; SUB]
       
       | Apply("!", [e]) -> CE vEnv fEnv e @  [NOT]

       | Apply("||",[b1;b2]) -> let labend  = newLabel()
                                let labtrue = newLabel()

                                CE vEnv fEnv b1 @ [IFNZRO labtrue] @ CE vEnv fEnv b2
                                @ [GOTO labend; Label labtrue; CSTI 1; Label labend]

       | Apply("&&",[b1;b2]) -> let labend   = newLabel()
                                let labfalse = newLabel()
                                CE vEnv fEnv b1 @ [IFZERO labfalse] @ CE vEnv fEnv b2
                                @ [GOTO labend; Label labfalse; CSTI 0; Label labend]

       | Apply(o,[e1;e2]) when List.exists (fun x -> o=x) ["+"; "*"; "=";"-";"<";"<=";">";">=";"<>"]
                             -> let ins = match o with
                                          | "+"  -> [ADD]
                                          | "-"  -> [SUB]
                                          | "*"  -> [MUL]
                                          | "="  -> [EQ] 
                                          | "<>" -> [EQ;NOT]
                                          | "<"  -> [LT]
                                          | ">=" -> [LT; NOT]
                                          | ">"  -> [SWAP; LT]
                                          | "<=" -> [SWAP; LT; NOT]
                                          | _    -> failwith "CE: this case is not possible"
                                CE vEnv fEnv e1 @ CE vEnv fEnv e2 @ ins 

       | Apply(f,l) when Map.containsKey f fEnv->  
                                        let (startlabel, tyOpt, xs) = Map.find f fEnv
                                        let m = List.length l //len(xs) = len(l)
                                        List.collect (fun x -> CE vEnv fEnv x) l @ [CALL (m ,startlabel)]
                         
       | _            -> failwith "Undefined behaviour"
       

/// CA vEnv fEnv acc gives the code for an access acc on the basis of a variable and a function environment
   and CA vEnv fEnv = function | AVar x         -> match Map.find x (fst vEnv) with
                                                   | (GloVar addr,_) -> [CSTI addr]
                                                   | (LocVar addr,_) -> [GETBP; CSTI addr; ADD]
                               | AIndex(acc, e) -> CA vEnv fEnv acc @ [LDI] @ CE vEnv fEnv e @ [ADD] 
                               | ADeref e -> CE vEnv fEnv e

   and CGCA vEnv fEnv = function 
                        | GC [] -> [STOP]
                        | GC a -> a |> List.collect (fun (ex, stms) -> 
                                    let labelend = newLabel()
                                    CE vEnv fEnv ex @ [IFZERO labelend] @ List.collect (CS vEnv fEnv) stms @ [Label labelend]
                                    ) //We chose to not include the STOP behaviour (when all expressions evaluate to false)
   and CGCD vEnv fEnv = function 
                        | GC [] -> []
                        | GC a ->  
                                let labelstart = newLabel()
                                [Label labelstart] @
                                List.collect (fun (ex, stms) -> 
                                let labelend = newLabel()
                                CE vEnv fEnv ex @ [IFZERO labelend] @
                                List.collect (CS vEnv fEnv) stms @ [GOTO labelstart; Label labelend]
                                    ) a

   and CR vEnv fEnv = function 
                      | Some(r) -> CE vEnv fEnv r @ [RET (snd vEnv)]
                      | None -> [RET (snd vEnv)]

/// CS vEnv fEnv s gives the code for a statement s on the basis of a variable and a function environment                          
   and CS vEnv fEnv = function
       | PrintLn e        -> CE vEnv fEnv e @ [PRINTI; INCSP -1] 

       | Ass(acc,e)       -> CA vEnv fEnv acc @ CE vEnv fEnv e @ [STI; INCSP -1]

       | Block([],stms) ->   CSs vEnv fEnv stms

       | Block(decs, stms) ->let (vEnv2, code) = bindLocal decs vEnv [] 
                             code @ CSs vEnv2 fEnv stms @ [INCSP (snd vEnv  - snd vEnv2)]

       | Alt a -> CGCA vEnv fEnv a

       | Do d -> CGCD vEnv fEnv d

       | Call (name, exps) when Map.containsKey name fEnv ->  
                                        let (startlabel, tyOpt, xs) = Map.find name fEnv
                                        let m = List.length exps //len(xs) = len(l)
                                        List.collect (fun x -> CE vEnv fEnv x) exps @ [CALL (m ,startlabel); INCSP -1]

       | Return v ->  CR vEnv fEnv v

       | _                ->  failwith "Invalid Statement"

   and CSs vEnv fEnv stms = List.collect (CS vEnv fEnv) stms 




(* ------------------------------------------------------------------- *)

(* Build environments for global variables and functions *)


   let makeGlobalEnvs decs = 
       let rec addv decs vEnv fEnv = 
           match decs with 
           | []         -> (vEnv, fEnv, [],[])
           | dec::decr  -> 
             match dec with
             | VarDec (typ, var) -> let (vEnv1, code1) = allocate GloVar (typ, var) vEnv
                                    let (vEnv2, fEnv2, code2, functions) = addv decr vEnv1 fEnv
                                    (vEnv2, fEnv2, code1 @ code2, functions)
             | FunDec (tyOpt, f, xs, body) -> let funlabel = newLabel()
                                              let fEnv1 =  Map.add f (funlabel, tyOpt, xs) fEnv
                                              let (vEnv, fEnv2, code2, functions) = addv decr vEnv fEnv1
                                              let (localEnv, code) = bindLocal xs ( fst vEnv,0) [] 
                                              let m = match tyOpt with
                                                      | Some(f) ->  List.length xs 
                                                      | None ->  (List.length xs - 1)
                                              (vEnv, fEnv2,code2,functions @ [Label funlabel] @ CS localEnv fEnv2 body @ [RET m])

       addv decs (Map.empty, 0) Map.empty
    
   
   //Small optimization, but it helps reduce INCSP instructions monumentally
   let rec aggINCSP (l: instr list) = match l with
                                        | h1::h2::t -> match h1,h2 with
                                                       | (INCSP i, INCSP j) -> aggINCSP ([INCSP (i+j)] @ t)
                                                       | _ -> [h1;h2] @ aggINCSP t
                                        | _ -> l

/// CP prog gives the code for a program prog
   let CP (P(decs,stms)) = 
       let _ = resetLabels ()
       let ((gvM,_) as gvEnv, fEnv, initCode, funcs) = makeGlobalEnvs decs
       initCode @ CSs gvEnv fEnv stms @ [STOP] @ funcs |> aggINCSP  
