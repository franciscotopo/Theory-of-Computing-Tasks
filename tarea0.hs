
module Tarea0 where

import Prelude
import Data.List (nub, sort)
 
------------------- Tarea 0 ------------------------
-- Nombre: Francisco Topolansky
-- Número: 288469
----------------------------------------------------

-- 1. Expresiones:
--    Declarar un tipo inductivo (data) apropiado para representar las 
--    expresiones de conjuntos finitos de enteros (sintaxis abstracta).

data E = Var X
       | Empty
       | Unit Z 
       | Z `Pert` E
       | E `Union` E
       | E `Inter` E
       | E `Difer` E
       | E `Inclu` E
       | X `Assign` E       deriving (Show)

type X = String
type Z = Int

-- 2. Valores:
--    Declarar un tipo inductivo (data) apropiado para representar los
--    valores de esas expresiones.

data V = B Bool
       | C Conj  deriving (Show, Eq)           --conjuntos finitos de enteros

type Conj = [Int]

-- 3.  Memoria:

-- 3.1 Definir un tipo (type) apropiado para representar a la memoria.

type M = [(X,V)]

-- 3.2 Definir la busqueda de una variable en la memoria (lkup: x -M-> v)

lkup :: X -> M -> V
lkup x [] = error ("Variable " ++ show x ++ " no inicializada")
lkup x ((n,v):ts) | x == n = v
                  | otherwise = lkup x ts

-- 3.3 Definir la actualización de la memoria (upd: M ≺+ (x, v)).

upd :: M -> (X,V) -> M
upd [] p = [p]
upd ((n,c):ts) p | fst p == n = ((n, snd p):ts)
                 | otherwise = (n,c) : upd ts p 

-- 4. Reglas de evaluación

-- 4.1 Programar las funciones auxiliares definidas en la especificación del lenguaje:

belongs :: Z -> Conj -> Bool
belongs z [] = False
belongs z (i:is) | z == i = True 
                 | otherwise = belongs z is

union :: Conj -> Conj -> Conj 
union c1 c2 = nub (c1 ++ c2)

intersection :: Conj -> Conj -> Conj 
intersection [] c2 = []
intersection c1 [] = []
intersection (i:is) c2 | elem i c2 = nub $ i : intersection is c2
                       | otherwise = nub $ intersection is c2
              
difference :: Conj -> Conj -> Conj
difference [] c2 = []
difference c1 [] = c1
difference (a:as) c2 | not $ elem a c2 = nub $ a : difference as c2
                     | otherwise = difference as c2

included :: Conj -> Conj -> Bool
included [] c2 = True
included (i:is) c2 | elem i c2 = included is c2
                   | otherwise = False


-- 4.2

eval :: (M, E) -> (M, V)
eval (m, Var x)       = (m, lkup x m)
eval (m, Empty)       = (m, C [])
eval (m, Unit z)      = (m, C [z])
eval (m, Pert z e)    = case eval (m, e) of {
                             (m', B bool) -> error "No es posible evaluar la pertenencia de un entero en un Booleano";
                             (m', C c) -> (m', B (belongs z c))
                      }
eval (m, Union e1 e2) = case eval (m, e1) of {
                             (m', B bool) -> error "No es posible evaluar la union de un Booleano con otro valor";
                             (m', C c1) -> case eval (m', e2) of {
                                    (m'', C c2) -> (m'', C (union c1 c2));
                                    (m'', B bool) -> error "No es posible evaluar la union de un Conjunto con un Booleano"
                             } 
                      }
eval (m, Inter e1 e2) = case eval (m, e1) of {
                             (m', B bool) -> error "No es posible evaluar la interseccion de un Booleano con otro valor";
                             (m', C c1) -> case eval (m', e2) of {
                                    (m'', C c2) -> (m'', C (intersection c1 c2));
                                    (m'', B bool) -> error "No es posible evaluar la interseccion de un Conjunto con un Booleano"
                             }                                   
                      }
eval (m, Difer e1 e2) = case eval (m, e1) of {
                            (m', B bool) -> error "No es posible obtener la diferencia de un Booleano con otro valor";
                            (m', C c1) -> case eval (m', e2) of {
                                   (m'', C c2) -> (m'', C (difference c1 c2));
                                   (m'', B bool) -> error "No es posible obtener la diferencia de un Conjunto y un Booleano"
                            }
                      }
eval (m, Inclu e1 e2) = case eval (m, e1) of {
                            (m', B bool) -> error "No es posible evaluar la inclusion de un Booleano en otro valor";
                            (m', C c1) -> case eval (m', e2) of {
                                   (m'', C c2) -> (m'', B (included c1 c2));
                                   (m'', B bool) -> error "No es posible evaluar la inclusion de un Conjunto y un Booleano"
                            }
                      }
eval (m, Assign x e) = case eval (m, e) of {
                            (m', B bool) -> (upd m' (x, B bool), B bool);
                            (m', C conj) -> (upd m' (x, C conj), C conj);
                     }


-- 5  Codificar en el lenguaje de las expresiones de conjuntos embebido en Haskell los siguientes conjutnos:

conj1 :: E           -- {1,2,3}
conj1 = (Unit 1) `Union`((Unit 2) `Union` (Unit 3))

conj2 :: E           -- {2,3,4}
conj2 = (Unit 2) `Union`((Unit 3) `Union` (Unit 4))

conj3 :: E           -- {1,2,3} U {2,3,4} = {1,2,3,4}
conj3 = Union conj1 conj2

conj4 :: E           -- {1,2,3} ∩ {2,3,4} = {2,3}
conj4 = Inter conj1 conj2

pert1 :: E           -- 2 ∈ {1,2,3} = True
pert1 = Pert 2 conj1

pert2 :: E           -- 3 ∈ ({1,2,3} ∩ {2,3,4}) = True
pert2 = Pert 3 conj4

incl1 :: E           -- {1,2,3} ⊆ {2,3,4} = False
incl1 = Inclu conj1 conj2

incl2 :: E           -- {1,2,3} ∩ {2,3,4}) ⊆ {2,3,4} = True
incl2 = Inclu conj4 conj2

incl3 :: E           -- {1,2,3} ⊆ ({1,2,3} ∪ {2,3,4}) = True
incl3 = Inclu conj1 conj3

ass1 :: E            -- x := {1,2,3}
ass1 = Assign "w" conj1

ass2 :: E            -- x := {2,3}
ass2 = Assign "x" conj4

ass3 :: E            -- y := True
ass3 = Assign "y" pert2

ass4 :: E            -- z := True
ass4 = Assign "z" incl2


--------------------------------------------------------
------------------ Ejemplos para probar ----------------
--------------------------------------------------------

m1 :: M
m1 = []

m2 :: M 
m2 = [("a", B True), ("b", B False), ("c", C [1,2,3]), ("d", C [])]

runTests = all (==True) [belTest, uniTest, intTest, difTest, incTest, luTest, updTest]

lu1 = lkup "x" [("x", B True), ("y", C [1,2,3])] == B True
lu2 = lkup "x" [("x", B False), ("y", C [1,2,3])] == B False
lu3 = lkup "y" [("x", B True), ("y", C [1,2,3])] == C [1,2,3]
lu4 = lkup "a" [("x", B False), ("y", C [4,5]), ("a", B True)] == B True 
lu5 = lkup "x" [("x", C [1,2]), ("x", B False)] == C [1,2]
lu6 = lkup "x" [("a", B True), ("b", C [1]), ("x", C [2])] == C [2]
lu7 = lkup "x" [("x", B False)] == B False       
luTest = all (==True) [lu1, lu2, lu3, lu4, lu5, lu6, lu7]

upd1 = upd [("x", B True), ("y", C [1,2,3])] ("x", C [4,5]) == [("x", C [4,5]), ("y", C [1,2,3])]
upd2 = upd [("x", B True), ("y", C [1,2,3])] ("y", B False) == [("x", B True), ("y", B False)]
upd3 = upd [("x", B True), ("y", C [1,2,3])] ("z", C [6]) == [("x", B True), ("y", C [1,2,3]), ("z", C [6])]
upd4 = upd [] ("x", B True) == [("x", B True)]
upd5 = upd [("a", B False), ("b", C [1,2])] ("a", C [3,4]) == [("a", C [3,4]), ("b", C [1,2])]
upd6 = upd [("a", B False)] ("b", C [3]) == [("a", B False), ("b", C [3])]
upd7 = upd [("a", B False)] ("c", B True) == [("a", B False), ("c", B True)]
upd8 = upd [("x", B False), ("x", C [2,3])] ("x", C [4,5]) == [("x", C [4,5]), ("x", C [2,3])]
updTest = all (==True) [upd1, upd2, upd3, upd4, upd5, upd6, upd7, upd8]


bel1 = belongs 4 [4] == True
bel2 = belongs 0 [] == False
bel3 = belongs 3 [4,4,4,3] == True
bel4 = belongs 5 [7,6,5] == True
bel5 = belongs 4 (intersection [1,2,3,4] [5,4,3]) == True
bel6 = belongs 2 (intersection [3,4] [2,4]) == False
bel7 = belongs 1 (union [2,3,4] [1,2]) == True
bel8 = belongs 8 (union [1,2,3] [6,7]) == False
belTest = all (==True) [bel1, bel2, bel3, bel4, bel5, bel6, bel7, bel8]

uni1 = union [1,2,3] [1,2,3] == [1,2,3]
uni2 = union [] [1,2,3] == [1,2,3]
uni3 = union [1,2,3] [] == [1,2,3]
uni4 = union [1,2,3] [3,2,1] == [1,2,3]
uni5 = union [4,5,6] [3,2,1] == [4,5,6,3,2,1]
uni6 = union [6] [6] == [6]
uni7 = union [6] (union [7] [9]) == [6,7,9]
uni8 = union [6] (intersection [7,9,0] [9,5,4]) == [6,9]
uni9 = union [6,7] (difference [7,9,0] [9,5,4,9]) == [6,7,0]
uniTest = all (==True) [uni1, uni2, uni3, uni4, uni5, uni6, uni7, uni8, uni9]

int1 = intersection [] [] == []
int2 = intersection [] [1,2,3] == []
int3 = intersection [7,6,5,4] [] == []
int4 = intersection [3,2,1] [1,2,3] == [3,2,1]
int5 = intersection [3,4,5,6,4,3] [3,3] == [3]
int6 = intersection [7,6,7] [7,8,7,6,6,6] == [7,6]
intTest = all (==True) [int1, int2, int3, int4, int5, int6]

dif1 = difference [1,2,3,4] [1] == [2,3,4]
dif2 = difference [1,2,3,4] [4,3,2,1] == []
dif3 = difference [] [1,2,3] == []
dif4 = difference [1,2,3] [] == [1,2,3]
dif5 = difference [5,6,7] [5,6,7,8] == []
dif6 = difference [4,4,4] [5,5,5] == [4]
dif7 = difference [4,4,4] [5,4,5] == []
dif8 = difference [7,7,6] [5,4,5] == [7,6]
difTest = all (==True) [dif1, dif2, dif3, dif4, dif5, dif6, dif7, dif8]

inc1 = included [5,6] [1,6,4,5,7] == True
inc2 = included [5,6] [1,6,4,7] == False
inc3 = included [] [1,6,4,5,7] == True
inc4 = included [1,2,3] [] == False
inc5 = included [1,2,3] [1,3,2] == True
inc6 = included [1,2,3] [1,2] == False
inc7 = included [4,5,6] [7,6,5,4] == True
inc8 = included [] [] == True                    
inc9 = included [1,1,1] [1,2,3] == True         
inc10 = included [1,2,3] [1,3,2,1] == True
inc11 = included [1,2,3,4] [1,2,3] == False
inc12 = included [1,2,3] [1,2,3] == True
inc13 = included [1,2,3,4] [2,3] == False
inc14 = included [2] [1,2,3] == True             
inc15 = included [4] [1,2,3] == False            
incTest = all (==True) [inc1, inc2, inc3, inc4, inc5, inc6, inc7, inc8, inc9, inc10, inc11, inc12, inc13, inc14, inc15]
