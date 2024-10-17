
module Tarea2 where

------------------- Tarea 2 ------------------------
-- Nombre: Francisco Topolansky
-- Número: 288469
----------------------------------------------------

-- 1. Definir tipos apropiados para representar los programas, expresiones y valores de Imp.


data P = (:=) [X] [E]       -- Assign
        | Local [X] P 
        | (:>) P P          -- P1 ; P2
        | Case X [B]
        | While X [B]

data E = ConsExp C [E] | Var X

type X = String 
type B = (C, ([X], P))        -- c [x] -> p         Con esto puedo usar lookup de Haskell que ya utiliza Maybe
type C = String


-- 2. Definir el tipo de la Memoria y las funciones para operar sobre ella (búsqueda, actualización, alta y bajas).


data V = ConsV C [V]          -- Constructor 
        | Null

type M = [(X, V)]


upd :: M -> [(X,V)] -> M 
upd m xvs = undefined


lkup :: X -> M -> M
lkup x m = undefined

alta :: [X] -> M -> M 
alta xs m = undefined 

bajas :: M -> [X] -> M
bajas xs m = undefined      -- Usar filter o usar una auxiliar que haga una baja bajas = .. baja where baja ...

lkupBranch :: C -> [B] -> Maybe ([X], P)        -- Esto es que capaz que encuentra un [x], P. En vez de definirla asi puedo usar la de haskell que ya devuelve un Nothing 
lkupBranch c bs = lookup c bs

-- Definir la evaluacion de expresiones en imp

eval :: M -> E -> V
eval m eval = undefined

exec :: M -> P -> M         -- (triangulito |>)
exec m p = undefined        --En el exec del case si no encuentra rama devuelve error non exhaustive!!

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