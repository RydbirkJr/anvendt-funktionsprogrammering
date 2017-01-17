namespace ast
#load "AST.fs"
module input =
    let tree1 =   P ([VarDec (ITyp,"res");
      FunDec
        (None,"p",[VarDec (ITyp,"x")],Block ([],[PrintLn (Access (AVar "x"))]))],
         [Ass (AVar "res",N 4); Call ("p",[Access (AVar "res")]);
          PrintLn (Access (AVar "res"))])

    let tree3 =  P ([VarDec (ITyp,"c"); VarDec (PTyp ITyp,"x"); VarDec (PTyp (PTyp ITyp),"y");
          VarDec (PTyp (PTyp (PTyp ITyp)),"z")],
         [Ass (AVar "c",N 24); Ass (AVar "x",Addr (AVar "c"));
          Ass (AVar "y",Addr (AVar "x"));
          PrintLn (Access (ADeref (Access (ADeref (Access (AVar "y"))))));
          Ass (ADeref (Access (ADeref (Access (ADeref (Access (AVar "z")))))),N 0);
          PrintLn
            (Access
               (ADeref (Access (ADeref (Access (ADeref (Access (AVar "z"))))))))])

    let tree2 = P ([FunDec
            (None,"swap",
             [VarDec (ATyp (ITyp,None),"a"); VarDec (ITyp,"i"); VarDec (ITyp,"j")],
             Block
               ([VarDec (ITyp,"tmp")],
                [Ass (AVar "tmp",Access (AIndex (AVar "a",Access (AVar "i"))));
                 Ass
                   (AIndex (AVar "a",Access (AVar "i")),
                    Access (AIndex (AVar "a",Access (AVar "j"))));
                 Ass (AIndex (AVar "a",Access (AVar "j")),Access (AVar "tmp"))]));
          FunDec
            (None,"printA",[VarDec (ATyp (ITyp,None),"a"); VarDec (ITyp,"len")],
             Block
               ([VarDec (ITyp,"i")],
                [Ass (AVar "i",N 0);
                 Do
                   (GC
                      [(Apply
                          ("!",
                           [Apply ("=",[Access (AVar "i"); Access (AVar "len")])]),
                        [PrintLn (Access (AIndex (AVar "a",Access (AVar "i"))));
                         Ass (AVar "i",Apply ("+",[Access (AVar "i"); N 1]))])])]));
          FunDec
            (Some ITyp,"partition",
             [VarDec (ATyp (ITyp,None),"a"); VarDec (ITyp,"x0"); VarDec (ITyp,"i");
              VarDec (ITyp,"j")],
             Block
               ([],
                [Do
                   (GC
                      [(Apply
                          ("&&",
                           [Apply ("<=",[Access (AVar "i"); Access (AVar "j")]);
                            Apply
                              ("<=",
                               [Access (AIndex (AVar "a",Access (AVar "i")));
                                Access (AVar "x0")])]),
                        [Ass (AVar "i",Apply ("+",[Access (AVar "i"); N 1]))]);
                       (Apply ("<=",[Access (AVar "i"); Access (AVar "j")]),
                        [Call
                           ("swap",
                            [Access (AVar "a"); Access (AVar "i");
                             Access (AVar "j")]);
                         Ass (AVar "j",Apply ("-",[Access (AVar "j"); N 1]))])]);
                 Return (Some (Access (AVar "i")))]));
          FunDec
            (None,"qs",
             [VarDec (ATyp (ITyp,None),"a"); VarDec (ITyp,"i"); VarDec (ITyp,"j")],
             Block
               ([VarDec (ITyp,"p")],
                [Alt
                   (GC
                      [(Apply ("<",[Access (AVar "i"); Access (AVar "j")]),
                        [Ass
                           (AVar "p",
                            Apply
                              ("partition",
                               [Access (AVar "a");
                                Access (AIndex (AVar "a",Access (AVar "i")));
                                Apply ("+",[Access (AVar "i"); N 1]);
                                Access (AVar "j")]));
                         Call
                           ("swap",
                            [Access (AVar "a"); Access (AVar "i");
                             Apply ("-",[Access (AVar "p"); N 1])]);
                         Call
                           ("qs",
                            [Access (AVar "a"); Access (AVar "i");
                             Apply ("-",[Access (AVar "p"); N 2])]);
                         Call
                           ("qs",
                            [Access (AVar "a"); Access (AVar "p");
                             Access (AVar "j")])]); (B true, [Do (GC [])])])]));
          FunDec
            (None,"quicksort",[VarDec (ATyp (ITyp,None),"a"); VarDec (ITyp,"len")],
             Call
               ("qs",
                [Access (AVar "a"); N 0; Apply ("-",[Access (AVar "len"); N 1])]));
          VarDec (ATyp (ITyp,Some 1500),"a"); VarDec (ITyp,"len");
          VarDec (ITyp,"i")],
         [Ass (AVar "i",N 0); Ass (AVar "len",N 1500);
          Do
            (GC
               [(Apply ("<",[Access (AVar "i"); Access (AVar "len")]),
                 [Ass
                    (AIndex (AVar "a",Access (AVar "i")),
                     Apply ("-",[Access (AVar "len"); Access (AVar "i")]));
                  Ass (AVar "i",Apply ("+",[Access (AVar "i"); N 1]))])]);
          Ass (AIndex (AVar "a",N 1),N 3);
          Call ("quicksort",[Access (AVar "a"); Access (AVar "len")])])

     