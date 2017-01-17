

type Tree<'a> =  Node of 'a * (Tree<'a> list)

type Extent = (float*float) list

let movetree (Node((label, x), subtrees), y) = Node((label, x+y), subtrees)

let moveextents (l:Extent,x) = List.map (fun (p,q) -> (p+x,q+x)) l

let rec merge = function
            | ([], qs)  -> qs
            | (ps, []) -> ps
            | ((p,_)::ps, (_,q)::qs) -> (p,q) :: merge (ps, qs)

let mergelist es = List.fold (fun a b -> merge (a,b) ) [] es 

let rec fit = function
            |((_,p)::ps),((q,_)::qs) -> max (fit (ps,qs)) (p - q + 1.0)
            | (_,_) -> 0.0

let rec fitlistl1 acc = function
            | [] -> []
            | (e::es) -> let x = (fit (acc,e))
                         x :: fitlistl1 (merge (acc, moveextents (e,x) )) es

let rec fitlistl acc = fitlistl1 [] acc 

//The short variation for fitlistr
let flipextent l = l |> List.map (fun (p,q) -> (-q,-p)) 

let fitlistr acc = List.rev acc |> List.map flipextent |> fitlistl  |> List.map (fun x -> -x) |> List.rev

let mean (x,y) = (x+y)/2.0

let fitlist es = List.map mean (List.zip (fitlistl es) (fitlistr es))


let rec design (Node(label, subtrees)) =    let (trees, extents) = List.unzip (List.map design subtrees)
                                            let positions = fitlist extents
                                            let ptrees = List.map movetree (List.zip trees positions)
                                            let pextents = List.map moveextents (List.zip extents positions)
                                            let resultextent = (0.0,0.0) :: (mergelist pextents)
                                            let resulttree = Node((label, 0.0), ptrees)
                                            let x = (resulttree, resultextent)
                                            (resulttree,resultextent)
and designAux t = fst (design t)

//-------------------------------------------------------------------------------------GENERATE POSTSCRIPT FROM Tree<string*float>--------------------------------------------------------------
let fac = 30.0
let hfac = 50.0
let rec traverse = function
                     | (Node((label,o),[]), (plc,vert)) -> generateLeaf (label,((plc+o)*hfac,vert - fac))
                     | (Node((label,o),sub), (plc,vert) ) ->  
                            generateLeaf (label, ((plc+o)*hfac, vert-fac)) @
                            traverseChildren (plc + o, vert - fac, sub)

and traverseChildren  = function 
| (px, py, Node((label,o),sub)::xs) -> drawLine (px,py,px+o,py-fac) @ traverse (Node((label,o),sub), (px, py)) @ traverseChildren(px, py, xs)
| (px, py, []) -> []

and drawLine (px, py, cx, cy) =
    [sprintf "%d %d moveto\n%d %d lineto\n%d %d lineto\n%d %d lineto\nstroke\n" (int(px*hfac)) (int(py-5.0)) (int(px*hfac)) (int(py-0.5 * fac)) (int(cx*hfac)) (int(py - 0.5 * fac)) (int(cx*hfac)) (int(cy+10.0))]               

and generateLabelPS lbl = 
        let temp = match lbl with
                   | s when String.length s > 17 -> s.Substring(0, 15) + ".."
                   | s -> s
        
        [sprintf "(%s) dup stringwidth pop 2 div neg 0 rmoveto show\n" temp]

and generateLeaf (label, (x, y)) = 
    string (int x)::" "::string (int y):: " moveto \n "::(generateLabelPS label)


let generatePSStart l = let mxx = fst (List.maxBy (fun (x,y) -> abs (x - y)) l)
                        let mxy = float (List.length l)
                        "%!" :: [(sprintf "\n<</PageSize[%d %d]/ImagingBBox null>> setpagedevice\n1 1 scale \n%d %d translate\nnewpath\n/Times-Roman findfont 7 scalefont setfont\n" (int(2.*hfac+abs((mxx * hfac)*2.))) (int(2.*fac+abs((mxy * fac)*2.))) (int(hfac+abs(mxx * hfac))) (int(fac+abs(mxy * fac))))]

let generatePSEnd  = ["showpage\n"]                
let makePS (t,e) = (generatePSStart e) @ traverse (t,(0.0,0.0)) @ generatePSEnd           


//-------------------------------------------------------------------LOAD AST into Tree<string * float>-------------------------------------------------------------
#load "Input.fsx"
open ast.input
tree1;;

let rec makeTree tree = match tree with                             
                                  | (a,b) -> Node("P", makeTreeFromDecs a @ makeTreeFromStms b)
                                 
and makeTreeFromDecs (exps:ast.Dec list) = match exps with
                                            | ast.FunDec(t,n,decs,stm)::xs -> [Node(n+"()", makeTreeFromDecs decs @ makeTreeFromStms [stm])] @ makeTreeFromDecs xs
                                            | ast.VarDec(t,n)::xs ->  Node(n+":"+makeTreeFromType t, []) :: makeTreeFromDecs xs
                                            | [] -> []

and makeTreeFromStms = function  
                        | x::xs -> (makeTreeFromStm x) :: makeTreeFromStms xs
                        | [] -> []

and makeTreeFromStm (stms:ast.Stm) = match stms with
                                                | ast.Block(a,b) -> Node("Block", makeTreeFromDecs a @ makeTreeFromStms b)
                                                | ast.Do(a) ->makeTreeFromGC "Do" a
                                                | ast.Alt(a) ->makeTreeFromGC "Alt" a
                                                | ast.Ass(a,b) ->  Node("Ass", makeTreeFromAccess a :: [makeTreeFromExp b]) 
                                                | ast.PrintLn(e) ->  Node("print ", [makeTreeFromExp e]) 
                                                | ast.Call(a,b) ->  Node("Call "+a+"()",  makeTreeFromExps b )
                                                | x ->  Node(x.GetType().Name, [])
and makeTreeFromGC t gc = match gc with
                          | ast.GC(a) ->  Node(t, a |> List.collect (fun (a,b) -> makeTreeFromExp a :: makeTreeFromStms b))

and makeTreeFromAccess exp = match exp with
                                | ast.AVar(x) -> Node(x, [])
                                | ast.AIndex(a,e) ->Node("AIndex", makeTreeFromAccess a :: [makeTreeFromExp e])
                                | ast.ADeref(e) ->  Node("ADeref", [makeTreeFromExp e])

and makeTreeFromType = function
                        | ast.ITyp -> "ITyp"
                        | ast.BTyp -> "BTyp"
                        | ast.ATyp (t, len) -> let l = match len with
                                                       | Some(n) -> "[" + string n + "]"
                                                       | None -> ""
                                               "ATyp:" + (makeTreeFromType t) + l
                        | ast.PTyp(t) -> "PTyp : " + makeTreeFromType t + ""
                        | _ -> failwith "Function declarations already handled in FunDec"

and makeTreeFromExps = function 
                        | x::xs -> (makeTreeFromExp x) :: makeTreeFromExps xs
                        | [] -> []
                             

and makeTreeFromExp exp = match exp with
                                     |ast.N n -> Node(string n, [])
                                     |ast.B b -> Node(string b, [])
                                     |ast.Access a -> Node("Access", [makeTreeFromAccess a])
                                     |ast.Apply (a,e) -> Node(a, makeTreeFromExps e)
                                     |ast.Addr (a) -> Node("Addr", [makeTreeFromAccess a])
                                    
let makeTreeFromP p = match p with
                          | ast.P(a,b) -> (a,b)


//-------------------------------------------------------------------------RUN TESTS AND PERFORMANCE GAUGES--------------------------------------------------------
#time "on"
fsi.ShowDeclarationValues <- false //Prevent var output

let newTree = makeTree (makeTreeFromP tree2)
//let newnewTree = Node("TOPLEVEL", List.map (fun x -> newTree) [1..100])

let tree2d = design newTree
let out =  (makePS (fst tree2d,  snd tree2d))
//----------------------- String concat (Fast)
let cOut = String.concat "" out 
//----------------------- Naive string addition (Slow)
//let sOut = List.fold(fun x y -> x+y) "" out
//----------------------- StringBuilder (Fast)
(*
open System.Text
let sb = new StringBuilder()
List.iter(fun (x:string) -> sb.Append x |> ignore ) out
let sbOut = sb.ToString()
*)
//-----------------------

System.IO.Directory.SetCurrentDirectory @"C:\users\stefan\git\02257-anvendt-funktionsprogrammering\Project 3"

System.IO.File.WriteAllText(@"output.ps", cOut)
