#lang racket

;Define las funciones publicas que se expondran a la interfaz
(provide calcular_bombas
         crear_tablero_completo
         crear_tablero_juego
         expandir_desde
         crear_matriz_bool
         imprimir_tablero
         obtener_en_tablero
         revelar_bombas
         es_victoria)

;Crea una matriz de 0s
;Entradas: dos enteros para el numero de filas y columnas
;Salida: una matriz de 0s de filasxcolumnas
(define (crear_tablero_vacio filas columnas)
  (cond
    [(zero? filas) '()]
    [else (cons (crear_fila columnas)
                (crear_tablero_vacio (- filas 1) columnas))]))

;Función auxiliar de crear_tablero_vacio
;Entrada: numero columnas de la fila
;Salida: una lista de tamaño n con 0s
(define (crear_fila n)
  (cond
    [(zero? n) '()]
    [else (cons 0 (crear_fila (- n 1)))]))

;Imprime matriz en consola
;Entrada: matriz
;Salida: Imprime matriz en consola
(define (imprimir_tablero matriz)
  (cond
    [(null? matriz) '()]
    [else (begin (imprimir_fila (car matriz))
                 (imprimir_tablero (cdr matriz)))]))

;Función auxliar para imprimir matriz
;Entrada: Fila de matriz
;Salida: Imprime la fila
(define (imprimir_fila fila)
  (cond
    [(null? fila) (displayln "")]
    [else (begin (display (car fila)) (display " ")
                 (imprimir_fila (cdr fila)))]))

;Define el numerador del porcentaje por nivel
;Entrada: Entero que representa el nivel
;Salida: 1 si nivel es 1, 3 si el nivel es 2 y 1 si el nivel es 3
(define (num_por_nivel nivel)
  (cond [(= nivel 1) 1] [(= nivel 2) 3] [(= nivel 3) 1] [else 1]))

;Define el denominador del porcentaje por nivel
;Entrada: Entero que representa el nivel
;Salida: 10 si nivel es 1, 20 si nivel es 2 y 5 si nivel es 3
(define (den_por_nivel nivel)
  (cond [(= nivel 1) 10] [(= nivel 2) 20] [(= nivel 3) 5] [else 10]))

(define (ceil_div a b) (quotient (+ a (- b 1)) b))

;Calcula en numero de bombas para un tablero a cierto nivel
;Entrada: 3 enteros que representan filas, columnas y nivel
;Salida: Número de bombas para un tablero
(define (calcular_bombas filas columnas nivel)
  (cond
    [(or (<= filas 0) (<= columnas 0)) 0] ;caso base
    [else
     (cond
       [(= (* filas columnas) 1) 1] ;caso especial
       [else
        (define k0
          (ceil_div (* (* filas columnas) (num_por_nivel nivel))
                    (den_por_nivel nivel)))
        (cond
          [(<= k0 0) 1]
          [(>= k0 (- (* filas columnas) 1)) (- (* filas columnas) 1)]
          [else k0])])]))

;Asigna un valor a una posicion especifica de la matriz
;Entradas: Matriz y 3 enteros para representar fila, columna y valor
;Salida: Matriz con valor agregado en pos filaxcolumna
(define (reemplazar_en_tablero tablero fila col valor)
  (cond
    [(null? tablero) '()] ;caso base
    [(= fila 0) (cons (reemplazar_en_fila (car tablero) col valor) (cdr tablero))] ;caso base si la fila donde se quiere cambiar es la primera
    [else (cons (car tablero) (reemplazar_en_tablero (cdr tablero) (- fila 1) col valor))]))

;Función auxliaar de reemplazar_en_tablero
;Entrada: una lista con los elementos de una fila, el numero de columna y el valor 
;Salida: Fila con valor agregado
(define (reemplazar_en_fila fila col valor)
  (cond
    [(null? fila) '()] ;caso base
    [(= col 0) (cons valor (cdr fila))] ;caso base, pos es primer elemento de fila
    [else (cons (car fila) (reemplazar_en_fila (cdr fila) (- col 1) valor))]))

;Retorna el valor en la pos filaxcolumna de la matriz
;Entrada: matriz, entero para fila y columna
(define (obtener_en_tablero tablero fila col)
  (cond
    [(null? tablero) 0] ;caso base
    [(= fila 0) (obtener_en_fila (car tablero) col)] ;caso base se quiere elemento de primer fila
    [else (obtener_en_tablero (cdr tablero) (- fila 1) col)]))

;Función auxiliar obtener_en_tablero
;Entradas: lista que representa fila y entero que representa pos en fila
;Salida: Valor en filaxcolumna
(define (obtener_en_fila fila col)
  (cond
    [(null? fila) 0] ;caso base
    [(= col 0) (car fila)] ;caso base en caso de querer valor de la fila en primer columna
    [else (obtener_en_fila (cdr fila) (- col 1))]))

;Coloca las bombas (9) en la matriz
;Entradas: matriz y lista de posiciones donde coloca las bombas
;Salida: Matriz con bombas
(define (colocar_bombas tablero posiciones)
  (cond
    [(null? posiciones) tablero] ;caso base
    [else
     (colocar_bombas
      (colocar_bomba tablero
                     (car (car posiciones))
                     (car (cdr (car posiciones))))
      (cdr posiciones))]))

;Función auxiliar de colocar_bombas
;Entradas: matriz, entero para fila y columa
;Salida: matriz con bombas
(define (colocar_bomba tablero fila col)
  (reemplazar_en_tablero tablero fila col 9))

;Una dos listas
;Entradas: dos lista
;Salida: Las listas concatenadas
(define (concatenar lista1 lista2)
  (cond
    [(null? lista1) lista2]
    [else (cons (car lista1) (concatenar (cdr lista1) lista2))]))


;Determina si una posición es parte de la matriz, se usa para validar posiciones de vecinos
;Entradas: número de filas y columnas, posición (f, c) que se quiere verificar
;Salida: #t si la pos (f, c) es valida para el tamaño de la matriz, #f si no
(define (en_rango filas columnas f c)
  (cond
    [(< f 0) #f] [(>= f filas) #f] 
    [(< c 0) #f] [(>= c columnas) #f]
    [else #t]))

;Inverso función en rango
(define (fuera_de_rango filas columnas f c)
  (cond
    [(< f 0) #t] [(>= f filas) #t]
    [(< c 0) #t] [(>= c columnas) #t]
    [else #f]))

;Retorna una lista de posiciones de los vecinos validos
;Entradas: enteros para filas, columnas, pos fxc, lista de vecinos
;Salida: Lista de posiciones de vecinos validos
(define (vecinos_en_rango filas columnas f c vecinos)
  (cond
    [(null? vecinos) '()] ;caso base
    [else
     (cond
       [(en_rango filas columnas (+ f (car (car vecinos))) (+ c (car (cdr (car vecinos))))) ;valida si vecinos son validos
        (cons (cons (+ f (car (car vecinos)))
                    (cons (+ c (car (cdr (car vecinos)))) '()))
              (vecinos_en_rango filas columnas f c (cdr vecinos)))]
       [else
        (vecinos_en_rango filas columnas f c (cdr vecinos))])])) ;excluye vecinos no validos

;Llama a vecinos_en_rango pasando la lista de posiciones de los vecinos
;Entrada: enteros para filas, columnas, pos fxc
;Salida: llamada a vecinos_en_rango con lista de vecinos
(define (vecinos filas columnas f c)
  (vecinos_en_rango filas columnas f c '((-1 -1) (-1 0) (-1 1) ( 0 -1) ( 0 1) ( 1 -1) ( 1 0)  ( 1 1))))

;Valida si dos posiciones fxc son iguales
;Entradas: posiciones fxc
;Salida; #t si posiciones son iguales, #f si no
(define (posicion_igual p q)
  (cond
    [(= (car p) (car q)) (cond [(= (car (cdr p)) (car (cdr q))) #t] [else #f])]
    [else #f]))

;Valida si una posicion fxc esta en lista de posiciones
;Entrada: lista de posiciones y una pos fxc
;Salida: #true si es miembro, #f si no
(define (miembro_pos posiciones pos)
  (cond
    [(null? posiciones) #f] ;caso base, lista posiciones vacia
    [(posicion_igual (car posiciones) pos) #t] ;caso base
    [else (miembro_pos (cdr posiciones) pos)]))

;Determina las posiciones de los vecinos de las bombas
;Entradas: filas, columnas y lista con pos de bombas
;Salida: Lista de los vecinos de todas las bombas
(define (vecinos_de_bombas filas columnas pos_bombas)
  (cond
    [(null? pos_bombas) '()] ;caso base
    [else
     (concatenar
      (vecinos filas columnas (car (car pos_bombas)) (car (cdr (car pos_bombas))))
      (vecinos_de_bombas filas columnas (cdr pos_bombas)))]))


;Excluye las bombas de la lista de vecinos_de_bombas
;Entradas: lista de posciones y lista de posiciones de bombas
;Salida: Lista de pociciones omitiendo bombas
(define (filtrar_no_bombas posiciones pos_bombas)
  (cond
    [(null? posiciones) '()] ;caso base
    [(miembro_pos pos_bombas (car posiciones)) (filtrar_no_bombas (cdr posiciones) pos_bombas)]
    [else
     (cons (car posiciones) (filtrar_no_bombas (cdr posiciones) pos_bombas))]))

;Genera una lista con posicion y conteno de bombas alrededor
;Entrada: lista de posiciones
;Salida: lista de conteo de bombas alrededor de las posiciones
(define (contar_frecuencias posiciones)
  (cond
    [(null? posiciones) '()] ;caso base
    [else
     (incrementar_conteo (contar_frecuencias (cdr posiciones)) (car posiciones))]))

;Función auxiliar de contar_frecuencias
;Entradas: lista de conteos y una posicion
;Salida: lista de conteos actualizada la la posición de entrada
(define (incrementar_conteo conteos pos)
  (cond
    [(null? conteos) (cons (cons pos (cons 1 '())) '())] ;caso base, se agregar primer elemento a lista conteos
    [(posicion_igual (car (car conteos)) pos) (cons (cons (car (car conteos)) (cons (+ (car (cdr (car conteos))) 1) '())) (cdr conteos))] ;suma conteo
    [else (cons (car conteos) (incrementar_conteo (cdr conteos) pos))]))

;Función que actualiza el tablero con los valores de la lista de conteo
;Entrada: Matriz de 0 y 8, lista de conteo de bombas alrededor de una posición
;Salida: Nueva matriz con valores alrededor de bombas
(define (crear_tablero_completo filas columnas pos_bombas)
  (aplicar_conteos
    (colocar_bombas (crear_tablero_vacio filas columnas) pos_bombas)
    (contar_frecuencias (filtrar_no_bombas (vecinos_de_bombas filas columnas pos_bombas) pos_bombas))))


;Función auxiliar de crear_tablero_completo
;Entradas; matriz, lista de conteos
;Salida: Matriz actualizada
(define (aplicar_conteos tablero conteos)
  (cond
    [(null? conteos) tablero] ;caso base
    [else
     (aplicar_conteos
      (reemplazar_en_tablero
        tablero
        (car (car (car conteos))) ;fila
        (car (cdr (car (car conteos)))) ;columna
        (car (cdr (car conteos)))) ;
      (cdr conteos))]))

;Función que crea matriz booleana para controlar desbloqueo de celdas
;Entradas: número de filas y columnas, valor booleano por asignar
;Salida: Matriz booleana
(define (crear_matriz_bool filas columnas v)
  (cond
    [(zero? filas) '()] ;caso base 
    [else (cons (crear_fila_bool columnas v) (crear_matriz_bool (- filas 1) columnas v))]))

;Función auxiliar de crear_matriz_bool
;Entradas: número de columnas, valor booleano por asignar
;Salida: Lista o fila booleana para la matriz
(define (crear_fila_bool n v)
  (cond
    [(zero? n) '()]
    [else (cons v (crear_fila_bool (- n 1) v))]))

;Función que retorna un elemento en la pos filaxcol de la matriz booleana
;Entradas: matriz, fila y columna
;Salida: valor booleano de esa posición
(define (obtener_en_matriz_bool m fila col)
  (cond
    [(null? m) #f] ;caso base lista vacia
    [(= fila 0) (obtener_en_fila_bool (car m) col)] ;primer elemento
    [else (obtener_en_matriz_bool (cdr m) (- fila 1) col)]))

;Función auxiliar de obtener_en_matriz_bool
;Entradas: lista boolena y posición o columna
;Salida: valor boolenao de esa fila
(define (obtener_en_fila_bool fila col)
  (cond
    [(null? fila) #f] ;caso base lista vacia
    [(= col 0) (car fila)] ;primer elemento
    [else (obtener_en_fila_bool (cdr fila) (- col 1))]))

;Función para actualizar matriz booleana cuando se revela una casilla
;Entradas: matriz, fila, columna y valor booleano nuevo
;Salida: Matriz actualizada
(define (reemplazar_en_matriz_bool m fila col v)
  (cond
    [(null? m) '()]
    [(= fila 0) (cons (reemplazar_en_fila_bool (car m) col v) (cdr m))]
    [else (cons (car m) (reemplazar_en_matriz_bool (cdr m) (- fila 1) col v))]))

;Función auxiliar de reemplazar_en_matriz_bool
;Entradas: lista que representa fila, posicion o columna y valor booleano por agregar
;Salida: Fila actualizada
(define (reemplazar_en_fila_bool fila col v)
  (cond
    [(null? fila) '()] ;caso base lista vacia
    [(= col 0) (cons v (cdr fila))] ;primer elemento
    [else (cons (car fila) (reemplazar_en_fila_bool (cdr fila) (- col 1) v))]))


;Función para contar elementos de lista
(define (contar_lista lista)
  (cond
    [(null? lista) 0] ;caso base
    [else (+ 1 (contar_lista (cdr lista)))]))

(define (filas_de tablero) (contar_lista tablero)) ;obtiene filas tablero

;obtiene columnas tablero
(define (columnas_de tablero)
  (cond [(null? tablero) 0]
    [else (contar_lista (car tablero))]))

;Función que revela una posicion en el tablero
;Entradas: matriz de tablero y boolenada, fila y columna donde se dio click
;Salida: Matriz booleana actualizada, puede ser solo el click o varias si era vacio
(define (expandir_desde tablero revelado f c)
  (cond
    [(fuera_de_rango (filas_de tablero) (columnas_de tablero) f c) revelado] ;valores fuera de rango devuelve la misma matriz
    [(obtener_en_matriz_bool revelado f c) revelado] ;si ya esta revelado devuelve la misma
    [(= (obtener_en_tablero tablero f c) 9) revelado] ;si es bomba, caso especial
    [(= (obtener_en_tablero tablero f c) 0) ;si es 0 revela recursivamente
     (expandir_vecinos tablero (reemplazar_en_matriz_bool revelado f c #t) f c '((-1 -1) (-1 0) (-1 1) ( 0 -1) ( 0 1) ( 1 -1) ( 1 0) ( 1 1)))]
    [else
     (reemplazar_en_matriz_bool revelado f c #t)]))

;Función auxiliar de expandir_desde
;Entradas: matriz de tablero y booleana, fila, columna, lista de vecinos
;Salida: Matriz booleana actualizada segun expansión
(define (expandir_vecinos tablero revelado f c vecinos)
  (cond
    [(null? vecinos) revelado] ;caso base
    [else
     (expandir_vecinos tablero (expandir_desde tablero revelado
                                               (+ f (car (car vecinos)))
                                               (+ c (car (cdr (car vecinos))))) f c
      (cdr vecinos))]))

;Genera posición (f, c) dentro matriz
;Entradas: cantidad de filas y columnas
;Salida: posicion (f, c) aleatoria
(define (nueva_pos_aleatoria filas columnas)
  (cons (random filas) (cons (random columnas) '())))

;Función que genera k posiciones unicas dentro de una matriz de filasxcolumnas
;Entradas: cantidad de filas y columnas y numero k de posiciones
;Salida: lista de k posiciones 
(define (generar_posiciones filas columnas k)
  (generar_posiciones_aux filas columnas k '()))

;Función auxiliar de generar_pocisiones
;Entradas: numero de filas, columnas, posiciones, y lista de posiciones
;Salida: Lista de k posiciones
(define (generar_posiciones_aux filas columnas k posiciones)
  (cond
    [(zero? k) posiciones] ;caso base 0 posiciones
    [else
     (generar_posiciones_unicas_aux2 filas columnas k posiciones (nueva_pos_aleatoria filas columnas))]))

;Funcion auxiliar de generar_posiciones_aux para evitar duplicador
;Entradas:  numero de filas, columnas, posiciones, lista de posiciones y nueva posicion por validar que no sea repetida
;Salida: Lista de posiciones sin duplicados
(define (generar_posiciones_unicas_aux2 filas columnas k posiciones candidato)
  (cond
    [(miembro_pos posiciones candidato) (generar_posiciones_unicas_aux2 filas columnas k posiciones (nueva_pos_aleatoria filas columnas))] ;valida que no sea duplicado
    [else
     (generar_posiciones_aux filas columnas (- k 1) (cons candidato posiciones))])) ;sigue el proceso de aux tras verificar no duplicado

;Función que construye matriz para interfaz
;Entradas: numero de filas, columnas, y nivel
;Salida: matriz con bombas y numeros
(define (crear_tablero_juego filas columnas nivel)
  (crear_tablero_completo filas columnas
    (generar_posiciones filas columnas (calcular_bombas filas columnas nivel)))) ;genera bombas y numeros

;Función que revela todas las bombas al perder
;Entradas: matriz del tablero y booleana
;Salida: Matriz booleana actualizada
(define (revelar_bombas tablero revelado)
  (cond
    [(null? tablero) '()] ;caso base
    [else
     (cons (revelar_bombas_fila (car tablero) (car revelado)) (revelar_bombas (cdr tablero) (cdr revelado)))]))

;Función auxiliar de revelar_bombas
;Entradas: Lista que representa fila de matriz tablero y booleana
;Salida: Fila booleana actualizada
(define (revelar_bombas_fila filaT filaR)
  (cond
    [(null? filaT) '()] ;caso base
    [else
     (cons (cond
             [(= (car filaT) 9) #t]
             [else (car filaR)])
           (revelar_bombas_fila (cdr filaT) (cdr filaR)))]))

;Función que verifica si todas las celdas no bomba estan reveladas
;Entradas: Matriz de tablero y booleana
;Salida: #t si todas las celdas no bomba estan reveladas, #f en caso contrario
(define (es_victoria tablero revelado)
  (cond
    [(null? tablero) #t] ;caso base
    [else
     (and (es_victoria_fila (car tablero) (car revelado))
          (es_victoria (cdr tablero) (cdr revelado)))]))

;Función auxiliar de es_victoria
;Entradas: lista de fila matriz tablero y booleana
;Salida: #t si todos los elementos de la fila estan revelados, excluye bombas
(define (es_victoria_fila filaT filaR)
  (cond
    [(null? filaT) #t]
    [else
     (and (or (= (car filaT) 9) (car filaR))
          (es_victoria_fila (cdr filaT) (cdr filaR)))]))
