

module Tarea1 where

------------------- Tarea 1 ------------------------
-- Nombre: Francisco Topolansky
-- Número: 288469
----------------------------------------------------

-- Ejercicio 1

data E = Var X  
        | Cons K [E]               -- Nombre del constructor y lista de expresiones. Constructor aplicado a sus argumentos
        | Lamb X E                 -- x es una variable ligada
        | Apl E E                  -- Aplicación
        | Case E [B]               -- Case, expresion y lista de ramas
        | Rec X E                  -- Recursión "nombre" expresion
        deriving (Show)            

type X = String                    
type K = String 

type B = (K, [X], E)               --  b ::= k → [x] e    Branch es como un "data" que almacena esas tres cosas  



-- Ejercicio 2
                                   -- Valores :  Expresiones que no pueden o no necesitan ser reducidas.
data V = ConsV K [V]               -- Nombre del constructor y lista de expresiones } NO SE REDUCEN
       | LambV X E                 -- Expresion lambda                              } NO SE REDUCEN
       deriving (Show)

-- En evaluacion fuerte, e ↓ v            Evalúa muchas veces débil hasta que llega a un valor.

                                   -- Formas Canonicas Debiles : Los utilizaremos para representar aquellas expresiones que no necesitan ser reducidas para seguir evaluando una expresión.
data W = ConsW K [E]               -- Nombre del constructor aplicacio a expresiones               
       | LambW X E                 -- Expresión lambda
       deriving (Show)

-- En evaluación débil, e ↓ w             Llego a una forma canónica débil aplicando reglas de deducción.



-- Ejercicio 3

type S = [(X,E)]                    -- (ESTRUCTURA) Tabla que asocia identificadores con expresiones [x1, x2, . . . , xn := e1, e2, . . . , en].

efecto :: E -> S -> E
efecto (Var x) s            = busqueda x s

       where busqueda :: X -> S -> E             -- [x,y,x := \x.x, 3, True] Lista con sustituciones.
             busqueda x [] = Var x
             busqueda x ((xi,e):s) | x == xi = e
                                   | otherwise = busqueda x s

efecto (Cons k exps) s      = Cons k ((map (\e-> efecto e s) exps))          -- (map (`efecto` s) exps) 
efecto (Lamb x e) s         = Lamb x (efecto e (bajas s [x]))
efecto (Apl e e') s         = Apl (efecto e s) (efecto e' s)
efecto (Case e branches) s  = Case (efecto e s) (sustRamas s branches)
       
       where sustRamas :: S -> [B] -> [B]                      --devuelve la lista de ramas con sus sustituciones
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


-- Ejercicio 4 

-- e ⇓ w    con e una expresión cerrada y w una forma canónica débil, se lee “e evalúa o computa debilmente a w”
--          osea, la evaluación de e da como resultado w.

-- Partimos de la base que no hay reglas de evaluación débil para variables. No podemos hacer (x + 3) o (+3) x porque x
-- es libre y no se computa un valor, no debería pasar.

weak :: E -> W
weak (Cons k es)     = ConsW k es
weak (Lamb x e)      = LambW x e
weak (Apl e e')      = case (weak e) of {
                            ConsW k es -> ConsW k (es ++ [e']);                   --ap-α
                            LambW x e'' -> weak (efecto e'' [(x,e')])             --ap-β sustituyo en e'' las libres por e'
                     }
weak (Case e bs)     = case (weak e) of {
                            ConsW k es -> case (buscoRama bs k) of {                       -- [b] -(k)→ ([x],e′)
                                   (xs, e') -> case (length xs == length es) of {          -- #[x] = #[e]
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

--Consultar si es correcto armar una tabla una a una.

-- Ejercicio 5

eval :: E -> V                                             
eval e = case weak e of {
              ConsW k es -> ConsV k (map eval es);         -- Evaluo todas las expresiones obteniendo W hasta llegar a V en cada una
              LambW x exp  -> LambV x exp
       }
    
-- Ejercicio 6
 
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
                            )     --Sumo 3, S(S(S(0))), con el llamado recursivo con triple de x
                     ]))

suma :: E
suma = Rec "suma" (Lamb "m" (Lamb "n" (Case (Var "m") [
                     ("0", [], Var "n"),                                                      -- 0 + n = n
                     ("S", ["x"], Cons "S" [Apl (Apl (Var "suma") (Var "n")) (Var "x")])      -- S x + n = S (suma x n)
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




