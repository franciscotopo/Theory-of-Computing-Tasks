
{-# LANGUAGE UnicodeSyntax #-}

module MT where 
    
    type Σ = String
    
    blanck :: Σ 
    blanck = "#"

    -- Representacion del cabezal  
    
    type Tape = ([Σ],Σ,[Σ]) --terna con lista: izquierda - simbolo que estoy leyendo - lista derecha 

    data Action = Left Tape | Right Tape | Write Tape Σ

    type Q = String

    i :: Q              --State o estados
    i = "i"

    h :: Q 
    h = "h"  

    type Branch = [(Σ, [(Action, Q)])]

    type M = [(Q, Branch)]           -- Dado un estado me duevuelve su branch ES CODE o MAQUINA DE TURING


    -- Exec hace todas las transiciones

    exec :: Tape -> M -> Tape       -- Llama a iteracion
    exec t c = undefined            -- ver la clase pasada lo que hicimos con imp (esta funcion usa una auxiliar que da un paso )

    -- Funcion step que hace una transicion (un paso)

    type Config = (Tape, Q)     --Dice para cierta cinta, en que estado estoy. 
    -- 
    
    -- Una config es una cinta en cierto estado. Una pareja que me dice como se encuentra mi cinta en un cierto estado
    -- Es el dibujo de la cinta en ciertoe estado.

    iterate :: Tape -> M -> Q -> Config
    iterate t m q = undefined

    step :: [Branch] -> Tape -> Config    -- es succ    Cuando entro a step entro con la cinta y con las branches de haber buscado en el codigo
    step bs t = 


    -- Funciones

    left_sigma :: Σ -> M            -- este es el codigo. Luego hago exec con este codigo, que busca el sigma dado 
    left_sigma o = undefined 

    -- Elijo tres chirimbolos que quiera y mi maquina funciona para esos tres simbolos. cuando llamo la funcino le paso uno de esos tres


    {--
    q   |   [branch]

    i       _ -> l, q0
    qo      o -> 0, h
            _ -> l, q0
    
    --}

    par :: M 
    par = undefined 

    elem_sigma :: Σ -> M            -- Recibe un simbolo del alfabeto y me hace una maquina que bsuca ese simbolo
    elem_sigma o = undefined 

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