
-- Simbolos: Uno de los simbolos es # denominado blanco.
Sigma alfabeto = 
-- Nuestro grafo es una LA. 




--La ejecucion o control (PROGRAMA EN MT) 

La clave es un estado inicial.

la clave es i y tiene otra tabla.
Desde i puedo ir a varios lugares

estado inicial | simbolo || a   |    nuevo estado
i                   #       r           q0
i                   a       l           q1 
i                   b       #           i 

Para no tener que escribir varias veces i, tengo otra tabla que me representa los estados de i 

Ahora tengo esto

(estado inicial ) | simbolo || a   |    nuevo estado
i  ------------->   #       r           q0
                    a       l           q1 
                    b       #           i 

Tengo una entrada para el estado i y me determina una tabla. Y dependiendo el simbolo, me determina un aaccion y un nuevo estado. 