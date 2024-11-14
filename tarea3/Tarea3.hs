
{-# LANGUAGE UnicodeSyntax #-}

module MT where 

    import Prelude hiding (iterate)
    

    -- 1. Definir tipos apropiados para representar los simbolos Σ, estados , cintas, acciones y el código.

    type Σ = String
    
    blanck :: Σ 
    blanck = "#"

    type Q = String                 --State o estados

    i :: Q                  
    i = "i"

    h :: Q 
    h = "h"

    type Tape = ([Σ],Σ,[Σ])          -- El sigma del medio es el cabezal        

    data Action = L | R | W Σ  
 
    type M = [(Q, [Branch])]           -- Dado un estado me duevuelve su branch. Es CODE o MAQUINA DE TURING

    type Branch = (Σ, (Action, Q))

    -- 2. Definir la función (parcial) de ejecución de un código sobre una cinta dada

    exec :: Tape -> M -> Tape       -- ver la clase pasada lo que hicimos con imp (esta funcion usa una auxiliar iteracion que da un paso )
    exec t m = case (iterate t m i) of {
                (t', h) -> t'
            }

    type Config = (Tape, Q)     
        
    iterate :: Tape -> M -> Q -> Config
    iterate t m "h" = (t, h)                                        -- Caso itero desde halt
    iterate t m q   = case (step t (buscoEstado q m)) of {          -- Caso cinta con estado no halt
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
                            Nothing -> error "No se definieron acciones para esa accion"
                        }                         
                    }


    -- Funciones

    left_sigma :: Σ -> M            -- este es el codigo. Luego hago exec con este codigo, que busca el sigma dado 
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

    -- Elijo tres chirimbolos que quiera y mi maquina funciona para esos tres simbolos. cuando llamo la funcino le paso uno de esos tres

    par :: M 
    par = undefined 

    elem_sigma :: Σ -> M
    elem_sigma o = [(i,     [("_", (L, "q0"))]),
                    ("q0",  [(o,   (R, "q1")),
                             ("#", (R, "q3")),
                             ("_", (L, "q0"))
                            ]),
                    ("q1",  [("#", (R, "q2")),
                             ("_", (R, "q1"))
                            ]),
                    ("q2",  [("_", (W "True", h))]),
                    ("q3",  [("#", (R, "q4")),
                             ("_", (R, "q3"))
                            ]),
                    ("q4",  [("_", (W "False", h))])
                   ] 
    
    t4 = (["#", "#", "#", "#"], "#" ,["#", "#"])

    pr31 = exec t1 (elem_sigma "o") == (["#", "o", "g", "g", "g","#"], "True",["#", "#"])
    pr32 = exec t2 (elem_sigma "o") == (["#", "o", "o", "o", "$", "#"], "True" ,["#", "#"])
    pr33 = exec t3 (elem_sigma "o") == (["#", "o", "f", "f", "f", "f", "#"], "True",["#", "#"])
    pr34 = exec t4 (elem_sigma "o") == (["#", "#", "#", "#", "#"], "False" ,["#"])
    

    reverse :: M 
    reverse = undefined


















-- -- Simbolos: Uno de los simbolos es # denominado blanco.
-- Sigma alfabeto = Tiene al menos dos simbolos. Blank # y comodin _
-- -- Nuestro grafo es una LA. 




-- --La ejecucion o control (PROGRAMA EN MT) 

-- La clave es un estado inicial.

-- la clave es i y tiene otra tabla.
-- Desde i puedo ir a varios lugares

-- estado inicial | simbolo || a   |    nuevo estado
-- i                   #       r           q0
-- i                   a       l           q1 
-- i                   b       #           i 

-- Para no tener que escribir varias veces i, tengo otra tabla que me representa los estados de i 

-- Ahora tengo esto. Una tabla con estados iniciales que van a otra tabla de ramas con las branches del estado

-- (estado inicial ) | simbolo || a   |    nuevo estado
-- i  ------------->   #       r           q0
--                     a       l           q1 
--                     b       #           i 

-- Tengo una entrada para el estado i y me determina una tabla. Y dependiendo el simbolo, me determina un aaccion y un nuevo estado. 

-- La tabla de ramas tiene como clave el simbolo del alfabeto o un comodin _, y el dato asociado a la clave es una pareja accion estado 

-- -- Acciones

-- l, r, sigma o, donde o es cualquier simbolo con los soguientes significados
-- left mueve a la izquierda
-- right a la derecha
-- sigma sobreescribe el simbolo corriente por sigma y deja el cabezal sobre el simbolo escrito

-- -- TABLA

-- PARA CADA ESTADO TENGO UNA TABLITA CON SUS BRANCHES = (simbolo leido, (accion y estado))
-- Lkista de parejas i - tabla. Y la tabla es una lista de parejas simbolo - pareja (a, q')

-- [(i, [(o, [(a, q')])])] 

-- Hay dos elementos distinguidos. i de init y h de halt. Luego hay finitos otros elementos.
-- El estado terminal no aparece como clave en ninguna entrada d ela tabla