namespace GuardedCommands.Frontend
// Michael R. Hansen 06-01-2016

open System
open Machine
open GuardedCommands.Frontend.AST

module TypeCheck = 

/// tcE gtenv ltenv e gives the type for expression e on the basis of type environments gtenv and ltenv
/// for global and local variables 
   let rec tcE gtenv ltenv = function                            
         | N _              -> ITyp   
         | B _              -> BTyp
         | Access acc       -> tcA gtenv ltenv acc
         | Addr acc         -> PTyp(tcA gtenv ltenv acc)
         | Apply(f,[e]) when List.exists (fun x ->  x=f) ["-";"!"]  
                            -> tcMonadic gtenv ltenv f e        

         | Apply(f,[e1;e2]) when List.exists (fun x ->  x=f) ["+";"*"; "-";"="; "&&"; "<>"; "||"; "<"; ">"; "<="; ">="]        
                            -> tcDyadic gtenv ltenv f e1 e2
         | Apply(f,e)       -> tcNaryFunction gtenv ltenv f e
         | _                -> failwith "tcE: not supported yet"

/// tcA gtenv ltenv e gives the type for access acc on the basis of type environments gtenv and ltenv
/// for global and local variables 
   and tcA gtenv ltenv = 
         function 
         | AVar x         -> match Map.tryFind x ltenv with
                             | None   -> match Map.tryFind x gtenv with
                                         | None   -> failwith ("no declaration for : " + x)
                                         | Some t -> t
                             | Some t -> t           
         | AIndex(acc, e) -> match tcE gtenv ltenv e with
                             | ITyp -> match tcA gtenv ltenv acc with 
                                       | ATyp(t,_) -> t
                                       | _ -> failwith "Not an array type"
                             | _ -> failwith("Index has to be a arithmetical expression")
         | ADeref e       -> match e with
                             | Access acc -> match tcDeref (acc, (tcE gtenv ltenv e)) with
                                             | PTyp(t) -> t
                                             | t -> t
                             | _ -> failwith "Invalid pointer dereference"

 /// tcS gtenv ltenv retOpt s checks the well-typeness of a statement s on the basis of type environments gtenv and ltenv
/// for global and local variables and the possible type of return expressions 
   and tcS gtenv ltenv fTyp = function                           
                         | PrintLn e        -> tcE gtenv ltenv e |> ignore
                         | Ass(acc,e)       -> match (acc, tcA gtenv ltenv acc, tcE gtenv ltenv e)  with
                                                | (ADeref(_), PTyp(t1), t2) when t1 = t2 -> ()
                                                | (_, t1, t2) when t1 = t2 -> ()
                                                | _ -> failwith ("illtyped assignment.")                              

                         | Block([],stms)   -> List.iter (tcS gtenv ltenv fTyp) stms
                         | Block(exps,stms)  -> let (env2,_) = tcLDecs ltenv [] exps
                                                List.iter (tcS gtenv env2 fTyp) stms
                         | Alt a            -> tcGuardedCommand gtenv ltenv fTyp a
                         | Do d             -> tcGuardedCommand gtenv ltenv fTyp d
                         | Return e         -> match (fTyp, e) with
                                                | (Some t, Some e) when t = tcE gtenv ltenv e -> ()
                                                | (None _, None _) -> ()
                                                | _ -> failwith "Return statement doesn't match function return value"
                         | Call(p, es)      -> tcNaryProcedure gtenv ltenv p es

 //=====================
 //TCE helper functions
 //=====================
   and tcMonadic gtenv ltenv f e =     
        match (f, tcE gtenv ltenv e) with
                                   | ("-", ITyp) -> ITyp
                                   | ("!", BTyp) -> BTyp
                                   | _           -> failwith "illegal/illtyped monadic expression" 
    
   and tcDyadic gtenv ltenv f e1 e2 = 
        match (f, tcE gtenv ltenv e1, tcE gtenv ltenv e2) with
                                      | (o, ITyp, ITyp) when List.exists (fun x ->  x=o) ["+";"*";"-"]  -> ITyp
                                      | (o, ITyp, ITyp) when List.exists (fun x ->  x=o) ["="; "<>";">"; "<"; "<="; ">="] -> BTyp
                                      | (o, BTyp, BTyp) when List.exists (fun x ->  x=o) ["&&";"=";"<>";"||"]     -> BTyp 
                                      | _                      -> failwith("illegal/illtyped dyadic expression: " + f)

   //Unwraps the function and calls the recursive function for input typecheck
   and tcNaryFunction gtenv ltenv f es =      
         match Map.tryFind f gtenv with
            | Some (FTyp(decs, Some(ftyp))) -> tcProcFunParams gtenv ltenv f es decs
                                               ftyp
            | None   -> failwith ("No defined function for : " + f)
            | _ -> failwith("No defined function for : " + f)
    
     //Unwraps the procedure and calls the recursive function for input validation for procedures
   and tcNaryProcedure gtenv ltenv p es = 
        match Map.tryFind p gtenv with
        | Some(FTyp(decs, None)) -> tcProcFunParams gtenv ltenv p es decs
        | None -> failwith ("No defined procedure for " + p)
        | _ ->  failwith ("No defined procedure for " + p)

            
   and tcProcFunParams gtenv ltenv f esL decL = 
        match esL, decL with
        | e :: esL, dec :: decL ->    let typ = tcE gtenv ltenv e
                                      match (typ,dec) with
                                      | (ATyp(t1,_),ATyp(t2,_)) when t1 = t2->  tcProcFunParams gtenv ltenv f esL decL
                                      | (t1,t2)  when t1 = t2 -> tcProcFunParams gtenv ltenv f esL decL
                                      |_ -> failwith("Non-matching parameter for function/procedure " + f)
        | [],[] -> ()  
        | _ -> failwith ("Incorrect number of arguments for function/procedure : " + f)

//=====================
// TCS helper functions
//=====================
   and tcGuardedCommand gtenv ltenv fTyp gc = 
            match gc with
                | GC a -> a |> List.iter (fun (exp, stms) -> 
                                    match tcE gtenv ltenv exp with
                                    | BTyp -> 
                                        stms |> List.iter (fun stm -> tcS gtenv ltenv fTyp stm)
                                    | _ -> failwith "illegal/illtyped guarded command expression"
                              )
//=====================
// TCA helper functions
//=====================
            
   and tcDeref (ac, typ) = 
        match (ac, typ) with
           | (ADeref(exp), PTyp(t)) -> match exp with
                                         | Access acc -> tcDeref (acc, t) 
                                         | _ -> failwith "ADeref unpacked to unvalid value"
           | (ADeref(exp), t)       -> PTyp(t)
           | (AVar _, t) -> t
           | _ -> failwith "Invalid pointer reference"
                                    

//===========================
//Global variable declaration
//===========================
   and tcGDecs gtenv = function
                       | dec::decs -> tcGDecs (tcGDec gtenv dec) decs
                       | _         -> gtenv

   and tcGDec gtenv = function  
                      | VarDec(t,s)               -> Map.add s t gtenv
                      | FunDec(topt,f, decs, stm) ->
                            //Typecheck of function parameters and add function to global env
                            let (ltenv, ls) = tcLDecs Map.empty [] decs
                            let tl = ls |> List.map(fun (x,y) -> x)
                            let fTyp = FTyp(tl, topt)
                            let genv = Map.add f fTyp gtenv

                            //Typecheck of function block
                            tcS genv ltenv topt stm
                            genv

   //Typecheck local variables - ensure no duplicates
   and tcLDecs ltenv (ls: (Typ * string) list) dl = match dl with
                                                    | dec::decs -> let (ltenv1, ls1) = tcLDec ltenv ls dec
                                                                   tcLDecs ltenv1 ls1 decs
                                                    | []         -> (ltenv, ls)
    //Validate function parameters
    and tcLDec ltenv ls = function
                        | VarDec(t,s)  -> match (t,s) with
                                            | (t,s) when Map.exists(fun k v -> k = s ) ltenv -> failwith("Formal parameter already exists for function.")  
                                            | (FTyp(ts, r), s) -> failwith "Functions cannot be a function/procedure argument"
                                            | (t,s) ->  (Map.add s t ltenv, ls @ [(t,s)])
                        | _ -> failwith("Illegal argument: Functions/procedures does not accept function as input.")

/// tcP prog checks the well-typeness of a program prog
   and tcP(P(decs, stms)) = let gtenv = tcGDecs Map.empty decs
                            List.iter (tcS gtenv Map.empty None) stms

