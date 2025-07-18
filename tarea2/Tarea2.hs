
module Tarea2 where

------------------- Tarea 2 ------------------------
-- Nombre: Francisco Topolansky
-- Número: 288469
----------------------------------------------------

-- Ejercicio 1. Definir tipos apropiados para representar los programas, expresiones y valores de Imp.

data P = (:=) [X] [E]           -- Asignacion multiple
        | Local [X] P           -- Declaración de variables locales
        | (:.) P P              -- Secuencia P1 ; P2
        | Case X [B]            -- Selección
        | While X [B]           -- Iteración

data E = Ce C [E] | Var X       -- Constructor o variable

type X = String 
type B = (C, ([X], P))          -- c [x] -> p         Con esto puedo usar lookup de Haskell que ya utiliza Maybe
type C = String

data V = Cv C [V]               -- Constructor 
        | Null
        deriving (Show)

-- Ejercicio 2. Definir el tipo de la Memoria y las funciones para operar sobre ella (búsqueda, actualización, alta y bajas).

type M = [(X, V)]


upd :: [(X,V)] -> M -> M        -- Actualizacion multiple de variables
upd [] m = m
upd l [] = l   
upd ((x,v):xvs) m = upd xvs (updAux (x,v) m)
        where 
            updAux :: (X,V) -> M -> M
            updAux (x,v) [] = [(x,v)]
            updAux (x,v) ((a,b):mem) | x == a = (x,v) : mem 
                                     | otherwise = (a,b) : updAux (x,v) mem

lkup :: X -> M -> V
lkup x [] = error ("No variable '" ++ x ++ "' in memory")
lkup x ((x',v):mem) | x == x' = v 
                    | otherwise = lkup x mem


alta :: [X] -> M -> M           -- [x] ++ M
alta xs m = listNull xs ++ m 
        where 
            listNull [] = [] 
            listNull [x] = [(x, Null)]
            listNull (x:xs) = (x, Null) : listNull xs   

bajas :: M -> [X] -> M          -- M - [x]
bajas m [] = m
bajas [] xs = []
bajas m (x:xs) = bajas (baja x m) xs   
        where 
            baja :: X -> M -> M
            baja x [] = []
            baja x ((x',v):ms) | x == x' = ms 
                               | otherwise = (x',v) : baja x ms   


-- Ejercicio 3. Definir la funcion de evaluacion de expresiones de Imp.

eval :: E -> M -> V                             -- e -(M)-> V   v es el valor resultante de evaluar e bajo M
eval (Ce c es) m = Cv c (map (`eval` m) es)     
eval (Var x) m = lkup x m

-- Ejercicio 4. Definir la función (parcial) de ejecución de un programa de Imp

-- La función exec tiene el simbolo |> en representacion del triangulo de ejecución

(|>) :: M -> P -> M                        
(|>) m (xs := es)   = case (length xs == length es) of {
                        True -> upd (zip xs (map (`eval` m) es)) m
                    }
(|>) m (Local xs p) = ((alta xs m) |> p) `bajas` xs     
(|>) m (p1 :. p2)   = (m |> p1) |> p2 
(|>) m (Case x bs)  = case (eval (Var x) m) of {
                        Cv c vs -> case (lkupBranch c bs) of {                          -- c -(bs)-> ([x], p)
                            Just (xs, p) -> case (length xs == length vs) of {
                                True -> m |> (Local xs ((xs := (v2e vs)) :. p));          -- |xs| = |vs|
                            };
                            Nothing -> error "Non exhaustive patterns in function"
                        }
                    }
(|>) m (While x bs) = case (eval (Var x) m) of {
                        Cv c vs -> case (lkupBranch c bs) of {
                            Nothing -> m;
                            Just (xs, p) -> case (length xs == length vs) of {          -- |xs| = |vs|
                                True -> m |> (Local xs ((xs := (v2e vs)) :. p)) |> (While x bs)
                            }
                        };
                        Null -> error x
                    }

-- Búsqueda de constructores segun un c dado

lkupBranch :: C -> [B] -> Maybe ([X], P) 
lkupBranch c bs = lookup c bs

-- Pasaje de variables a expresiones

v2e :: [V] -> [E]
v2e [] = []
v2e ((Cv c vs'):vs) = Ce c (v2e vs') : v2e vs

-- Ejercicio 5. Codificar en Imp embebido en Haskell los programas:

par :: X -> P
par (n) = Local ["n'"] (
                (["result", "n'"] := [Ce "True" [], (Var n)]) :.
                (While "n'" [
                    ("S", (["x"], (Case "result" [
                                    ("True",  ([], ["result"] := [Ce "False" []])),
                                    ("False", ([], ["result"] := [Ce "True" []]))
                                  ] 
                                  :.          --Punto y coma
                                  (["n'"] := [Var "x"]))    
                        )
                    )
                ])                         
        )


m :: M 
m = [("n", Cv "S" [Cv "0" []]), ("result", Cv "True" [])]

mem = [("m", Cv "S" [Cv "0" []]), ("n", Cv "S" [Cv "0" []])]

suma :: (X, X) -> P
suma (m, n) = Local ["n'"] (
                (["n'", "result"] := [(Var n) , (Var m)]) :.
                (While "n'" [
                    ("S", (["x"], (["n'", "result"] := [Var "x", Ce "S" [Var "result"]])))     
                ])    
            )

largo :: X -> P 
largo (l)   = Local ["l'"] (
                (["l'", "result"] := [(Var l), (Ce "0" [])]) :. 
                (While "l'" [
                    (":", (["x", "xs"], (["result", "l'"] := [Ce "S" [Var "result"], Var "xs"])))
                ])        
            )

mm = [("l", Cv ":" [Cv "1" [], Cv ":" [Cv "2" [], Cv "3" []]])]       -- 1 : (2 : 3)

igualdadN :: (X, X) -> P
igualdadN (m, n) = Local ["m'", "n'"] (
                    (["m'", "n'", "result"] := [Var m, Var n, Ce "True" []]) :.
                    (While "m'" [
                        ("S", (["x"], (Case "n'" [
                            ("S", (["y"], ["m'", "n'"] := [Var "x", Var "y"])),
                            ("0", ([], ["result"] := [Ce "False" []]))
                        ])))
                    ]) :.  
                    (Case "n'" [
                        ("S", (["x"], ["result"] := [Ce "False" []])),   --Cuando salgo del while es porque m' llegó a cero. Si n' todavia no es cero, son distintos
                        ("0", ([], ["result"] := [Ce "True" []]))
                    ])
                )

mIgualdadN = [("m", Cv "S" [Cv "0" []]), ("n", Cv "S" [Cv "0" []])]

concatP :: (X, X) -> P
concatP (l1, l2) = Local ["l1'", "rev"] (
                        (["l1'", "rev"] := [Var l1, Ce "[]" []] :.
                        While "l1'" [
                            (":", (["x", "xs"], ["l1'", "rev"] := [Var "xs", Ce ":" [Var "x", Var "rev"]]))
                        ]) :.   -- Invierto l1' y luego itero sobre la revertida agregando el primer element delante del result que tiene el valor de l2
                        (["result"] := [Var l2] :.
                        While "rev" [
                            (":", (["x", "xs"], ["rev", "result"] := [Var "xs", Ce ":" [Var "x", Var "result"]]))
                        ])          
                )
    
mConcat = [("l1", Cv ":" [Cv "6" [], Cv "2" []]), ("l2", Cv ":" [Cv "4" [], Cv "1" []])]

--------------------------------
-- Pruebas de otras funciones --
--------------------------------

andP :: (X,X) -> P 
andP (p,q) = (Case p [
                ("False", ([], (["result"] := [Ce "False" []]))),
                ("True", ([], (["result"] := [Var q])))
            ])

mAnd = [("p", Cv "False" []), ("q", Cv "True" [])]

