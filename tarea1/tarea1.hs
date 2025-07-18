

module Tarea1 where

------------------- Tarea 1 ------------------------
-- Nombre: Francisco Topolansky
-- Número: 288469
----------------------------------------------------

data E = Var X  
        | Cons K [E]               
        | Lamb X E                 
        | Apl E E                  
        | Case E [B]               
        | Rec X E                  
        deriving (Show)            

type X = String                    
type K = String 

type B = (K, [X], E)                 

data V = ConsV K [V]               
       | LambV X E                 
       deriving (Show)

data W = ConsW K [E]               
       | LambW X E                 
       deriving (Show)

type S = [(X,E)]

efecto :: E -> S -> E
efecto (Var x) s            = busqueda x s

       where busqueda :: X -> S -> E             
             busqueda x [] = Var x
             busqueda x ((xi,e):s) | x == xi = e
                                   | otherwise = busqueda x s

efecto (Cons k exps) s      = Cons k ((map (\e-> efecto e s) exps))         
efecto (Lamb x e) s         = Lamb x (efecto e (bajas s [x]))
efecto (Apl e e') s         = Apl (efecto e s) (efecto e' s)
efecto (Case e branches) s  = Case (efecto e s) (sustRamas s branches)
       
       where sustRamas :: S -> [B] -> [B]                      
             sustRamas s []              = []
             sustRamas s ((k,xs,e):bs)   = (k, xs, efecto e (bajas s xs)) : sustRamas s bs 

efecto (Rec x e) s          = Rec x (efecto e (bajas s [x]))


bajas :: S -> [X] -> S
bajas s [] = s
bajas s (x:xs) = bajas (baja s x) xs
       where baja :: S -> X -> S
             baja [] v = []
             baja ((xi,e):s) v | xi == v = baja s v
                               | otherwise = (xi,e): baja s v

weak :: E -> W
weak (Cons k es)     = ConsW k es
weak (Lamb x e)      = LambW x e
weak (Apl e e')      = case (weak e) of {
                            ConsW k es -> ConsW k (es ++ [e']);                   
                            LambW x e'' -> weak (efecto e'' [(x,e')])             
                     }
weak (Case e bs)     = case (weak e) of {
                            ConsW k es -> case (buscoRama bs k) of {                       
                                   (xs, e') -> case (length xs == length es) of {          
                                          True -> weak (efecto e' (sustSimul xs es))
                                   }
                            }
                     }
weak (Rec x e)       = weak (efecto e [(x, Rec x e)])

buscoRama :: [B] -> K -> ([X],E)
buscoRama ((ki,xs,e):bs) k | k == ki = (xs,e)
                           | otherwise = buscoRama bs k


sustSimul :: [X] -> [E] -> [(X,E)]
sustSimul [] [] = []
sustSimul (x:xs) (e:es) = (x,e) : sustSimul xs es

eval :: E -> V                                             
eval e = case weak e of {
              ConsW k es -> ConsV k (map eval es);         
              LambW x exp  -> LambV x exp
       }
    
or :: E
or = Lamb "b1" (Lamb "b2" (Case (Var "b1") [
                     ("True", [], Cons "True" []),
                     ("False", [], Case (Var "b2") [
                            ("True", [], Cons "True" []),
                            ("False", [], Cons "False" [])
                     ])
              ]))


triple :: E
triple = Rec "triple" (Lamb "n" (Case (Var "n") [
                            ("0", [], Cons "0" []),
                            ("S", ["x"], Apl (Apl (Var "suma") (Cons "S" [Cons "S" [Cons "S" [Cons "0" []]]]))
                                         (Apl (Var "triple") (Var "x"))
                            )     
                     ]))

suma :: E
suma = Rec "suma" (Lamb "m" (Lamb "n" (Case (Var "m") [
                     ("0", [], Var "n"),                                                      
                     ("S", ["x"], Cons "S" [Apl (Apl (Var "suma") (Var "n")) (Var "x")])      
              ]
       )))


duplicar :: E 
duplicar = Rec "duplicar" (Lamb "l" (Case (Var "l") [
                     ("[]", [], Cons "[]" []),
                     (":", ["x", "xs"], Cons ":" [Var "x", Cons ":" [Var "x", Apl (Var "duplicar") (Var "xs")]]) 
              ]
       ))


ramaC :: E
ramaC = Rec "ramaC" (Lamb "t" (Case (Var "t") [
                     ("Hoja", ["x"], Cons ":" [Var "x", Cons "[]" []]),
                     ("Nodo", ["i", "c", "d", "x"], Cons ":" [Var "x", Apl (Var "ramaC") (Var "c")])
              ]
       ))




