
{-# LANGUAGE UnicodeSyntax #-}

module MT where 

    ------------------- Tarea 3 ------------------------
    -- Nombre: Francisco Topolansky
    -- Número: 288469
    ----------------------------------------------------
    
    import Prelude hiding (iterate, reverse)
    
    type Σ = String
    
    blanck :: Σ 
    blanck = "#"

    type Q = String                 

    i :: Q                  
    i = "i"

    h :: Q 
    h = "h"

    type Tape = ([Σ],Σ,[Σ])    

    data Action = L | R | W Σ  
 
    type M = [(Q, [Branch])]           

    type Branch = (Σ, (Action, Q))

    exec :: Tape -> M -> Tape       
    exec t m = case (iterate t m i) of {
                (t', h) -> t'
            }

    type Config = (Tape, Q)     
        
    iterate :: Tape -> M -> Q -> Config
    iterate t m "h" = (t, h)                                        
    iterate t m q   = case (step t (buscoEstado q m)) of {          
                        (t'', q'') -> iterate (t'') m q''
                    };

    buscoEstado :: Q -> M -> [Branch]
    buscoEstado q m = case (lookup q m) of {
                        Just branches -> branches;
                        Nothing -> error "No se definio el simbolo"
                    } 


    step :: Tape -> [Branch] -> Config    
    step (izq, v, der) bs = case (lookup' v bs) of {
                    (L, q') -> ((init izq, last izq, v:der), q');
                    (R, q') -> ((izq ++ [v], head der, tail der), q');
                    (W o, q') -> ((izq, o, der), q')
                }
                
    lookup' :: Σ -> [Branch] -> (Action, Q)
    lookup' o bs = case (lookup o bs) of {
                        Just algo -> algo; 
                        Nothing -> case (lookup "_" bs) of {
                            Just alg -> alg;
                            Nothing -> error "No se definieron acciones para este simbolo"
                        }                         
                    }


    left_sigma :: Σ -> M            
    left_sigma o = [(i,     [("_", (L, "q0"))]),         
                    ("q0",  [(o,      (W o, h )),
                             ("_",    (L, "q0"))
                            ])
                   ]  

     
    t1 = (["#", "o", "g", "g", "g"],"#",["#", "#", "#"])
    t2 = (["#", "o", "o", "o", "$"],"#",["#", "#", "#"])
    t3 = (["#", "o", "f", "f", "f", "f"],"#",["#", "#", "#"])

    pr11 = exec t1 (left_sigma "o") == (["#"],"o",["g","g","g","#","#","#","#"])
    pr12 = exec t2 (left_sigma "o") == (["#", "o", "o"],"o",["$","#","#","#","#"])
    pr13 = exec t3 (left_sigma "o") == (["#"],"o",["f","f","f","f","#", "#", "#", "#"])
    all1 = and [pr11, pr12, pr13]

    par :: M 
    par = [(i,      [("_", (L, "q0"))]),
           ("q0",   [("I",  (L, "q4")),
                     ("#",  (R, "q1"))
                    ]),
           ("q1",   [("#",  (R, "q2")),
                     ("I",  (R, "q1"))
                    ]),
           ("q2",   [("#",  (W "True", "q3"))]),
           ("q3",   [("True", (R, h))]),
           ("q4",   [("I",  (L, "q0")),
                     ("#",  (R, "q5"))
                    ]),
           ("q5",   [("I",  (R, "q6"))]),
           ("q6",   [("I",  (R, "q6")),
                     ("#",  (R, "q7"))
                    ]),
           ("q7",   [("#",  (W "False", "q8"))]),
           ("q8",   [("False", (R, h))])
          ] 

    t5 = (["#", "#", "I", "I", "I", "I"],"#",["#", "#"])
    t6 = (["#", "I"],"#",["#", "#"])
    t7 = (["#", "#"],"#",["#", "#"])

    pr21 = exec t5 par == (["#","#","I","I","I","I","#","True"],"#",[])
    pr22 = exec t6 par == (["#","I", "#","False"],"#",[])
    pr23 = exec t7 par == (["#","#","#","True"],"#",[])
    all2 = and [pr21, pr22, pr23]

    elem_sigma :: Σ -> M
    elem_sigma o = [(i,     [("_", (L, "q0"))]),
                    ("q0",  [(o,   (R, "q1")),
                             ("#", (R, "q3")),
                             ("_", (L, "q0"))
                            ]),
                    ("q1",  [("#", (R, "q2")),
                             ("_", (R, "q1"))
                            ]),
                    ("q2",  [("_", (W "True", "q5"))]),
                    ("q3",  [("#", (R, "q4")),
                             ("_", (R, "q3"))
                            ]),
                    ("q4",  [("_", (W "False", "q6"))]),
                    ("q5",  [("True", (R, h))]),
                    ("q6",  [("False", (R, h))])
                   ] 
    
    t4 = (["#", "o", "%", "&", "f", "f"],"#",["#", "#", "#"])

    pr31 = exec t1 (elem_sigma "o") == (["#", "o", "g", "g", "g","#", "True"], "#", ["#"])
    pr32 = exec t2 (elem_sigma "o") == (["#", "o", "o", "o", "$", "#", "True"], "#", ["#"])
    pr33 = exec t3 (elem_sigma "o") == (["#", "o", "f", "f", "f", "f", "#", "True"], "#", ["#"])
    pr34 = exec t3 (elem_sigma "$") == (["#", "o", "f", "f", "f", "f", "#", "False"], "#", ["#"])
    pr35 = exec t3 (elem_sigma "*") == (["#", "o", "f", "f", "f", "f", "#", "False"], "#", ["#"])
    pr36 = exec t4 (elem_sigma "o") == (["#", "o", "%", "&", "f", "f", "#", "True"], "#", ["#"])
    pr37 = exec t4 (elem_sigma "%") == (["#", "o", "%", "&", "f", "f", "#", "True"], "#", ["#"])
    pr38 = exec t4 (elem_sigma "&") == (["#", "o", "%", "&", "f", "f", "#", "True"], "#", ["#"])
    pr39 = exec t4 (elem_sigma "f") == (["#", "o", "%", "&", "f", "f", "#", "True"], "#", ["#"])
    pr310 = exec t4 (elem_sigma ")") == (["#", "o", "%", "&", "f", "f", "#", "False"], "#", ["#"])
    pr311 = exec t4 (elem_sigma "@") == (["#", "o", "%", "&", "f", "f", "#", "False"], "#", ["#"])
    all3 = and [pr31, pr32, pr33, pr34, pr35, pr36, pr37, pr38, pr39, pr310, pr311]

    reverse :: M 
    reverse = [(i,      [("#", (L,     "q0"))]),
               ("q0",   [("a", (W "X", "q3")),
                         ("b", (W "Y", "q9")),
                         ("#", (R,     "q1"))
                        ]),
               ("q3",   [("X", (R,     "q4"))]),  
               ("q4",   [("#", (R,     "q5")),
                         ("_", (R,     "q4"))
                        ]),
               ("q5",   [("#", (W "a", "q6")),
                         ("_", (R,     "q5"))
                        ]),
               ("q6",   [("a", (L,     "q7"))]),
               ("q7",   [("X", (W "a", "q8")),
                         ("_", (L,     "q7"))
                        ]),
               ("q8",   [("a", (L,     "q0"))]),
               ("q9",   [("Y", (R,     "q10"))]),
               ("q10",  [("#", (R,     "q11")),
                         ("_", (R,     "q10"))
                        ]),
               ("q11",  [("#", (W "b", "q12")),
                         ("_", (R,     "q11"))
                        ]),
               ("q12",  [("b", (L,     "q13"))]),
               ("q13",  [("Y", (W "b", "q14")),
                         ("_", (L,     "q13"))
                        ]),
               ("q14",  [("b", (L,     "q0"))]),
               ("q1",   [("#", (R,     "q2")),
                         ("a", (R,     "q1")),
                         ("b", (R,     "q1"))
                        ]),
               ("q2",   [("#", (W "#",  h)),
                         ("a", (R,     "q2")),
                         ("b", (R,     "q2"))
                        ])                          
              ]

    t8 = (["#", "#", "b", "b", "b", "a", "a", "a"], "#", ["#", "#", "#", "#", "#", "#", "#", "#"])
    t9 = (["#", "b", "a", "b", "a", "b", "a"], "#", ["#", "#", "#", "#", "#", "#", "#", "#"])
    t10 = (["#", "b", "a"], "#", ["#", "#", "#", "#", "#"])

    pr41 = exec t8 reverse  == (["#", "#", "b", "b", "b", "a", "a", "a", "#", "a", "a", "a", "b", "b", "b"], "#" ,["#"])
    pr42 = exec t9 reverse  == (["#", "b", "a", "b", "a", "b", "a", "#", "a", "b", "a", "b", "a", "b"], "#" ,["#"])
    pr43 = exec t10 reverse == (["#", "b", "a", "#", "a", "b"], "#" ,["#", "#"])
    all4 = and [pr41, pr42, pr43]

    allTests = and [all1, all2, all3, all4]

