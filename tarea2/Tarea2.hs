
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
eval (Ce c es) m = Cv c (valores es m)          -- Chequear largos de [E] y [V]
        where 
            valores [] m = []
            valores (e:es) m = eval e m : valores es m
eval (Var x) m = lkup x m

-- Ejercicio 4. Definir la función (parcial) de ejecución de un programa de Imp

(|>) :: M -> P -> M         -- exec   En el exec del case si no encuentra rama devuelve error non exhaustive!!
(|>) m p = undefined        


-----------------
-- No es parte de la semantica este.
lkupBranch :: C -> [B] -> Maybe ([X], P)        -- Esto es que capaz que encuentra un [x], P. En vez de definirla asi puedo usar la de haskell que ya devuelve un Nothing 
lkupBranch c bs = lookup c bs
-----------------









par :: X -> P
par (n) = undefined

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