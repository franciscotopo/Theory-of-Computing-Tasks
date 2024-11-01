
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
                        True -> upd m (zip xs (map (`eval` m) es))
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
                        }
                    }
                 
-- Búsqueda de constructores segun un c dado

lkupBranch :: C -> [B] -> Maybe ([X], P) 
lkupBranch c [] = Nothing
lkupBranch c bs = lookup c bs

-- Pasaje de variables a expresiones

v2e :: [V] -> [E]
v2e [] = []
v2e ((Cv c vs'):vs) = Ce c (v2e vs') : v2e vs

 


-- Ejercicio 5. Codificar en Imp embebido en Haskell los programas:


par :: X -> P
par (n) = Local ["n'"] (
                ["result", "n'"] := [Ce "True" [], Var n] :.
                While "n'" [
                    ("S", (["x"], (Case "result" [
                                    ("True",  ([], ["result"] := [Ce "False" []])),
                                    ("False", ([], ["result"] := [Ce "True" []]))
                                  ] 
                                  :.          --Punto y coma
                                  (["n'"] := [Var "x"]))    
                        )
                    )
                ]                         
        )

m :: M 
m = [("n", Cv "S" [Cv "0" []])]


{--
PAR (n) = {
            Local n' {
                 result , n' := True [], n
                 ;
                 while n' is [
                        S [x] -> {
                               Case result of [
                                    True [] -> {result := False []} ,
                                    False [] -> {result := True []}
                                ];
                                n' := x
                               }                            
                            ]                       
                    }           
            }

--}












{--
HASKELL
reverse :: X -> P 
reverse (l) = Local ["l'"] (
                ["l'", "result"] := [Var l, Const "[]" []]

                :.
                
                While "l'" [
                    (":",(["x","xs"], ["l'", "result"] := [Var "xs", Const ":" [Var "x", Var "result"]]))
                ]
                )

IMP
REVERSE(l) = {
            local l' {
                l', result := [][];
                while l' is [
                    : [x,xs] -> l', result := xs, :[x,result] 
                ]
                    }   --cuando sale, es que la lista esta vacia y ya revirtio la lista
            }
--}



suma :: (X, X) -> P
suma (m, n) = undefined

largo :: X -> P 
largo (l) = undefined

igualdadN :: (X, X) -> P
igualdadN (m, n) = undefined

concatP :: (X, X) -> P
concatP (l1, l2) = undefined



{--

andP (p, q) = {
                case p of [
                    True [] -> { result := q; := }
                    False [] -> { result := False []}        
                ]
            }

andP :: (X,X) -> P 
andP (p,q) = (Case p [
                ("True", ([], (["result"] := [Var q] :> := ))),
                ("False", ([], (["result"] := [C "False" []])))
            ] ) :> 
            := 

exec [("m", Cv "True" []), ("r", Cv "6" [])] ((andP ("m", "r"))

= [("m", Cv "True" []), ("r", Cv "6" []), ("result", Cv "6" [])]

--}










-- Buscar una rama y no encontrarla esta bien EN EL WHILE. No es un error en el WHILE, pero si en el case 