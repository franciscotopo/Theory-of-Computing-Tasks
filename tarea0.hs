
module Lab0 where

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
       | C Conj  deriving (Show)           --conjuntos finitos de enteros

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
union c1 c2 = sort $ nub (c1 ++ c2)

intersection :: Conj -> Conj -> Conj 
intersection [] c2 = []
intersection c1 [] = []
intersection (a:as) c2 | elem a c2 = a : intersection as c2
                       | otherwise = intersection as c2

difference :: Conj -> Conj -> Conj
difference [] c2 = []
difference c1 [] = c1
difference (a:as) c2 | not $ elem a c2 = sort $ a : difference as c2
                     | otherwise = sort $ difference as c2

included :: Conj -> Conj -> Bool
included c1 c2 | intersection c1 c2 == c1 = True
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
                             (m', B bool) -> error "No es posible evaluar la union de un Booleano";
                             (m', C c1) -> case eval (m', e2) of {
                                    (m'', C c2) -> (m'', C (union c1 c2));
                                    (m'', B bool) -> error "No es posible evaluar la union de un Conjunto con un Booleano"
                             } 
                      }
eval (m, Inter e1 e2) = case eval (m, e1) of {
                             (m', B bool) -> error "No es posible evaluar la interseccion de un Booleano";
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

------------------ Ejemplos para probar ----------------

evalua (m,e) = snd $ eval (m,e)

true = B True
false = B False

a = "a"
b = "b"
h = "h"

m0 :: M
m0 = [(a,true),(b,true),(h,false)]

m1 :: M
m1 = []