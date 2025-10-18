.MODEL SMALL ;Define el modelo de memoria que usara el programa, small es segmento de codigo de 64kb + segmento de datos de 64kb
.STACK 100    ;reserva 256 bytes para la pila , para guardar direcciones de retorno(como al usar ROC), variables temporales, 
.DATA   ;marca el inicio del segmento de datos
; ==========================================================================
; DEFINICION DE MENSAJES
; ==========================================================================
MI1 DB 13,10,"Bienvenidos a RegistroCE$"  ;MI1 es la etiqueta, DB es define Byte
MI2 DB 13,10,"1. Ingresar calificaciones$"    ;13,10 secuencia CRLF carriage return+line feed
MI3 DB 13,10,"2. Mostrar estadisticas$"       ;13 = retorno de carro(mover cursor al inicio de linea), 10:nueva linea (mover cursos a siguiente linea)
MI4 DB 13,10,"3. Buscar estudiante$"          ;$ = Marca el fin del string para la función 09h de INT 21h
MI5 DB 13,10,"4. Ordenar calificaciones$"
MI6 DB 13,10,"5. Salir$"
MSJ DB 13,10,"Digite opcion: $"
M11 DB 13,10,"Por favor ingrese su estudiante o digite 9 para salir al menu principal",13,10,"$"
M12 DB 13,10,"Estudiante agregado exitosamente!",13,10,"$"
M13 DB 13,10,"Error: Formato incorrecto. Use: Nombre Apellido1 Apellido2 Nota",13,10,"$"
M14 DB 13,10,"Estudiante agregado exitosamente!",13,10,"$"
M15 DB 13,10,"Error: Lista llena (max 15 estudiantes)",13,10,'$'
M21 DB 13,10,"===ESTADISTICAS===", 13,10,"$"
M22 DB 13,10,"Promedio general: $"
M23 DB 13,10,"Nota maxima: $"
M24 DB 13,10,"Nota minima: $"
M25 DB 13,10,"Aprobados (>=70): $"
M26 DB 13,10,"Reprobados (<70): $"
M27 DB " estudiantes ($"
M28 DB "%)",13,10,"$"
M29 DB 13,10,"No hay estudiantes registrados.$"
M30 DB 13,10,"===========================",13,10,"$"
M31 DB 13,10,"Ingrese el numero de estudiante (1-15): $"
M32 DB 13,10,"Estudiante encontrado: $"
M33 DB 13,10,"Estudiante no encontrado o numero invalido$"
M34 DB 13,10,"Nota: $"
M35 DB 13,10,"Numero de estudiante: $"
M41 DB 13,13,"Como desea ordenar las calificaciones$"
M42 DB 13,10,"1. Asc$"
M43 DB 13,10,"2. Des",13,10,"$"
M44 DB 13,10,"Calificaciones ascendentes$"
M45 DB 13,10,"Calificaciones descendentes$"             
MSJ_SIN_ESTUDIANTES DB 13,10,"No hay estudiantes para ordenar.$"

; ==========================================================================
; DEFINICION DE OFFSETS PARA ESTRUCTURA DE ESTUDIANTE ;no ocupan memoria,   el ensamblador las reemplaza donde aparezcan, le da a un valor un nombre para evitar numeros magicos
; ==========================================================================
NOMBRE_OFFSET = 0          ; OFFSET PARA NOMBRE (20 BYTES)                 ;cada estudiante es un espacio, un conjunto de bytes, segun que       -estructura en memoria de cada estudiante: ;    [0-19]: Nombre (20 bytes)
                                                                                                                              ;                                                                  [20-39]: Apellido1 (20 bytes)  
APELLIDO1_OFFSET = 20      ; OFFSET PARA PRIMER APELLIDO (20 BYTES)        ;tanto me desplace puedo obtener un valor u otro.                                                                     [40-59]: Apellido2 (20 bytes)
APELLIDO2_OFFSET = 40      ; OFFSET PARA SEGUNDO APELLIDO (20 BYTES)                                                                                                                             [60-61]: Nota entera (2 bytes)
NOTA_ENTERA_OFFSET = 60    ; OFFSET PARA PARTE ENTERA (2 BYTES)                                                                                                                                  [62-63]: Nota decimal (2 bytes)
NOTA_DECIMAL_OFFSET = 62   ; OFFSET PARA PARTE DECIMAL (2 BYTES) - FORMATO x10000
ESTUDIANTE_SIZE = 64       ; TAMANO TOTAL POR ESTUDIANTE (20+20+20+2+2)

; ==========================================================================
; VARIABLES
; ==========================================================================
estudiantes db 15 * ESTUDIANTE_SIZE dup(0)  ; ARRAY PARA 15 ESTUDIANTES  ;reserva los 64bytes de cada estudiante, como son 15 es un total de 960bytes, con dup(0) todos los bytes se inicializan en cero.
contador db 0              ; CONTADOR DE ESTUDIANTES INGRESADOS         ;1 byte que cuenta cuantos estudiantes hay. 
indices DB 15 DUP(?)       ; ARRAY DE INDICES PARA ORDENAMIENTO    ;dup(?) reserva el espacio pero no lo inicializa, es para valores basura, se usa para ordenar sin modificar el array original de estudiantes.
hubo_swap db 0      ; bandera por pasada: 0 = sin swaps, 1 = hubo al menos un swap       ;esta bandera es para optimizar el bubble sort, el 0 es que no hubo intercambios.
tmp_idx db 0         ;almacenamiento temporal para indices.

; ==========================================================================
; VARIABLES PARA ESTADISTICAS
; ==========================================================================
promedio_entera dw 0              ;define word reserva 2 bytes, las notas se divine en parte entera como 85
promedio_decimal dw 0             ;y su parte decimal, que seria .8501 x 10000 lo que seria un valor de 8501
nota_maxima_entera dw 0
nota_maxima_decimal dw 0
nota_minima_entera dw 0 
nota_minima_decimal dw 0
aprobados_count dw 0
reprobados_count dw 0
total_estudiantes dw 0

; ==========================================================================
; BUFFERS                                                                           ;un buffer es donde se almacenan datos temporales antes de procesarlos
; ==========================================================================        ;algo asi como una mesa de trabajo
buffer db 100 dup('$')     ; BUFFER PARA ENTRADA DE TECLADO  ;etiqueta buffer, reserva un espacio de 100 bytes, el dup($) duplicate, rellena los 100 espacios con el signo $
temp_campo db 20 dup('$')  ; BUFFER TEMPORAL PARA NOMBRE/APELLIDOS;se valida como texto           ;este es el caracter de terminador de strings en DOS, llena los espacios con ese signo.
temp_nota db 20 dup('$')   ; BUFFER TEMPORAL PARA NOTAS;se valida como numero
                                                                                                ;la Funcion 09h de INT 21h imprime strings hasta encontrar un $
.CODE    ;inicio del segmento del codigo. instrucciones que debe ejecutar el cpu
; ==========================================================================
; PROCEDIMIENTO PRINCIPAL
; ==========================================================================
MAIN PROC    ;es el punto de entrada del programa, se llama aqui primero al iniciar.
    ; INICIALIZAR SEGMENTO DE DATOS
    MOV AX,@DATA  ;@DATA constante que representa la direccion del segmento de datos
    MOV DS,AX     ;se guarda en el segmento de datos, esto porque por alguna razon no se puede hacer MOV DS,@DATA directamente.

; ==========================================================================
; INICIO: MUESTRA EL MENU PRINCIPAL
; ==========================================================================
INICIO:                                   ;etiqueta, es el punto de referencia para saltos.
    ; IMPRIME MENU COMPLETO
    MOV DX, OFFSET MI1                    ;obtiene la direccion de memoria del primer mensaje "Bienvenidos a RegistroCE" y la guarda en DX
    MOV AH,09h                            ;AH registro que especifica que funcion de int 21h se quiere, 09h es la funcion de imprimir strings
    INT 21h                               ;interrupcion 21h llamada al sistema de DOS, el sistema ve AH=09h y sabe que debe imprimir el string en DX.
    
    MOV DX, OFFSET MI2                    ;lo mismo para los demas casos.
    MOV AH,09h
    INT 21h
    
    MOV DX, OFFSET MI3
    MOV AH,09h
    INT 21h
    
    MOV DX, OFFSET MI4
    MOV AH,09h
    INT 21h
    
    MOV DX, OFFSET MI5
    MOV AH,09h
    INT 21h
    
    MOV DX, OFFSET MI6
    MOV AH,09h
    INT 21h
    
    MOV DX, OFFSET MSJ
    MOV AH,09h
    INT 21h

    ; LEE OPCION DEL USUARIO
    MOV AH,01h                   ;esta funcion 01h lee un solo caracter del usuario
    INT 21h                      ;espera que el usuario presione la tecla y el caracter leido se guarda en AL.

    ; COMPARA OPCION Y SALTA A RUTINA CORRESPONDIENTE
    CMP AL,'1'            ;segun el valor que guardo la funcion AH = 01h se hace un "Jump if equal"
    JE OPCION1
    CMP AL,'2'            ;si no es igual salta al siguiente valor y asi.
    JE OPCION2
    CMP AL,'3'            ;CMP es COMPARE
    JE OPCION3
    CMP AL,'4'
    JE OPCION4
    CMP AL,'5'
    JE SALIR
    JMP INICIO            ;si el caracter en AL no coincide con ningun valor, vuelve al inicio (menu)

; ==========================================================================
; OPCION1: INGRESAR CALIFICACIONES
; ==========================================================================
OPCION1:                   ;lo mismo etiquetas! utiles para hacer saltos xd en general se le da un nombre a una direccion en memoria para que sea mas facil moverse.
    CMP contador, 15       ; Verifica si hay espacio disponible
    JB OPCION1_AGREGAR     ; Si hay espacio, salta a agregar      jump if below, si contador es menor de 15 significa que aun hay campo
    
    ; LISTA LLENA - MUESTRA MENSAJE DE ERROR
    MOV DX, OFFSET M15                     ;si no hizo el salto es porque esta llena, imprime el mensaje.
    MOV AH,09h
    INT 21h
    JMP INICIO            ; y vuelve al meno de inicio

OPCION1_AGREGAR:
    ;SOLICITA DATOS DEL ESTUDIANTE
    MOV DX, OFFSET M11         ;imprime "por favor ingrese su estudiante" 
    MOV AH,09h
    INT 21h

    CALL LEER_ENTRADA_COMPLETA  ; Lee entrada del usuario      ;CALL guarda la direccion de retorno (para volver despues)
    JC OPCION1_SALIR            ; Si hay error, se sale         ;salta a leer entrada completa, ejecuta esa subrutina
                                                                  ;al encontrar RET, de return,vuelve a la direccion de retorno
    CALL PARSE_ENTRADA          ; Parsea la entrada                       ;como que va hace lo que tiene que hacer y vuelve a este punto.
    JC OPCION1_ERROR            ; Si hay error, muestra mensaje           ;JC es jump if carry,salta si hay acarreo o error, va a la etiqueta de error si pasa algo.
                                                                                 ;un CF = 0 es que no hay error, CF= 1 error. Bandera Carry = Indicador universal de error
    INC contador                ; Incrementa contador de estudiantes

    ;MUESTRA MENSAJE DE EXITO
    MOV DX, OFFSET M14
    MOV AH,09h
    INT 21h
    JMP INICIO       ;salto si o si a inicio. JMP = Salto incondicional.

OPCION1_ERROR:
    ;MUESTRA MENSAJE DE ERROR
    MOV DX, OFFSET M13
    MOV AH,09h
    INT 21h
    JMP OPCION1

OPCION1_SALIR:
    JMP INICIO

; ==========================================================================
; OPCION2: MOSTRAR ESTADISTICAS
; ==========================================================================
OPCION2:
    MOV DX, OFFSET M21
    MOV AH,09h
    INT 21h
    
    CALL CALCULAR_ESTADISTICAS
    JMP INICIO

; ==========================================================================
; OPCION3: BUSCAR ESTUDIANTE
; ==========================================================================
OPCION3:
    CALL BUSCAR_ESTUDIANTE
    JMP INICIO

; ==========================================================================
; OPCION4: ORDENAR CALIFICACIONES
; ==========================================================================
OPCION4:
    ; MUESTRA SUBMENU DE ORDENAMIENTO
    MOV DX, OFFSET M41
    MOV AH,09h
    INT 21h
    
    MOV DX, OFFSET M42
    MOV AH,09h
    INT 21h
    
    MOV DX, OFFSET M43
    MOV AH,09h
    INT 21h

    MOV DX, OFFSET MSJ
    MOV AH,09h
    INT 21h

ORDEN_MOSTRAR:
    ; LEE OPCION DE ORDENAMIENTO
    MOV AH,01h                       ;lee un solo caracter con eco, que es que se ve el caracter en pantalla al escribirlo, lo guarda en AL.
    INT 21h

    CMP AL,'1'                       
    JE OPCION4_ASC         ; Si es '1', orden ascendente
    
    CMP AL,'2'
    JE OPCION4_DES          ; Si es '2', orden descendente
    
    JMP ORDEN_MOSTRAR        ; Si no es 1 ni 2, vuelve a preguntar

OPCION4_ASC: ;ordenamiento ascendente
    CMP contador, 0        ; Verifica si hay estudiantes       verifica que haya datos que ordenar.
    JE SIN_ESTUDIANTES     ; Si no hay, muestra mensaje
    CALL INICIALIZAR_INDICES ; Inicializa array de Indices
    MOV AL, 0              ; Modo ascendente                 ;AL se vuelve un parametro que le dice a bubblesort AL=0=Ascendente o AL=1=Descendente.
    CALL BUBBLESORT_NOTAS  ; Ordenar notas      ;llama la subrutina bubblesort.
    MOV DX, OFFSET M44
    MOV AH,09h
    INT 21h
    CALL MOSTRAR_NOTAS_ORDENADAS ; Muestra la lista ordenada.
    JMP INICIO                    ;vuelve al menu principal

OPCION4_DES:   ;ordenamiento descendente.
    CMP contador, 0        ; Verifica si hay estudiantes             ;hace lo mismo que el anterior solo que le pasa al bubblesort un AL = 1.
    JE SIN_ESTUDIANTES     ; Si no hay, muestra mensaje
    CALL INICIALIZAR_INDICES ; Inicializa array de indices
    MOV AL, 1              ; Modo descendente
    CALL BUBBLESORT_NOTAS  ; Ordena notas
    MOV DX, OFFSET M45
    MOV AH,09h
    INT 21h
    CALL MOSTRAR_NOTAS_ORDENADAS ; Muestra resultados ordenados
    JMP INICIO

SIN_ESTUDIANTES:
    MOV DX, OFFSET MSJ_SIN_ESTUDIANTES    ;"No hay estudiantes para ordenar"
    MOV AH, 09h
    INT 21h
    JMP INICIO             ;vuelve al menu.

; ==========================================================================
; SALIR: TERMINAR PROGRAMA
; ==========================================================================
SALIR:
    MOV AH,4Ch      ;4Ch es la funcion "terminar programa" libera la memoria usada por el programa y devuelve el control al sistema operativo
    INT 21h
MAIN ENDP           ;marca el final del procedimiento main

; ==========================================================================
; RUTINA PARA BUSCAR ESTUDIANTE POR NUMERO
; ==========================================================================
BUSCAR_ESTUDIANTE PROC
    PUSH AX        ;estos push son para guardar los registros que estaban antes de esta rutina en el stack.
    PUSH BX        ;pues la rutina va a modificarlos.
    PUSH CX        ;al terminar se les hace pop y se recupera los valores originales antes de la rutina
    PUSH DX
    PUSH SI
    
    ; PIDE NUMERO DE ESTUDIANTE
    MOV DX, OFFSET M31
    MOV AH,09h
    INT 21h
    
    ; LEE PRIMER DIGITO
    MOV AH, 01h
    INT 21h
    
    ; VALIDA QUE SEA DIGITO (1-9)
    CMP AL, '1'
    JB numero_invalido  ;jump if below, si es menor a 1 es invalido.
    CMP AL, '9'
    JA verificar_10_15  ;jump if above, si es mayor a 1 es invalido
    
    ; NUMERO 1-9 - CONVIERTE A BINARIO
    SUB AL, '0'                         ;CONVERSION MAGICA, como los valores estan en ASCII
    JMP verificar_existe                ;con restar el codigo de 0 al valor que tenemos obtenemos su valor numerico
                                        ; AL = 57 - 48 = 9   donde 48 es 0 y 57 es 9.
verificar_10_15:
    CMP AL, '1'             ;Primer digito debe ser '1'
    JNE numero_invalido      ;Si no, invalido    Jump if Not Equal

leer_segundo_digito:
    ; LEE SEGUNDO DIGITO
    MOV AH, 01h
    INT 21h
    
    ; VALIDA SEGUNDO DIGITO (0-5)
    CMP AL, '0'            ;las mismas comparaciones de validar que 1-9
    JB numero_invalido      ; Si < '0', invalido
    CMP AL, '5'
    JA numero_invalido     ; Si > '5', invalido (max 15)
    
    ; CONVIERTE A NUMERO (10 + SEGUNDO_DIGITO) ;al valor que ya tenemos en AL le resta 48 con eso tenemos el 2do digito
    SUB AL, '0'       ; Convierte segundo digito
    ADD AL, 10        ; 10 + segundo_digito       ;a este le suma y 10 y ya tendriamos un valor de 10 a 15.
    JMP verificar_existe

numero_invalido:
    MOV DX, OFFSET M33       ; "Estudiante no encontrado o numero invalido"
    MOV AH, 09h
    INT 21h
    JMP fin_buscar           ; Salta al final sin mostrar estudiante

verificar_existe:
    MOV CL, AL              ; Guarda numero en CL (1-15)
    DEC CL                ; Convierte a indice base 0 (0-14)
    CMP CL, contador      ; Verifica si existe , indice < contador?
    JAE numero_invalido   ; Si no existe, muestra error    Jump if Above or Equal
    
    ; CALCULA POSICION DEL ESTUDIANTE
    MOV AL, CL   ;ejemplo estudiante de  indice 9, AL = 8 (indice base 0).
    MOV BL, ESTUDIANTE_SIZE ;BL = 64
    MUL BL                ; AX = indice * tamano_estudiante  ;AX = 8 x 64 = 512 , MUL multiplica AL x BL y el resultado lo guarda en AX.
    MOV SI, OFFSET estudiantes                            ;SI es la direccion base del array
    ADD SI, AX            ; SI = direccion del estudiante    SI = base + 512 bytes,a la direccion base le sumamos el AX anterior
                                                                                 ;con eso ubicamos la dir de memoria de ese estudiante
    ; MUESTRA MENSAJE DE ENCONTRADO
    MOV DX, OFFSET M32
    MOV AH, 09h
    INT 21h
    
    ; MUESTRA NUMERO DE ESTUDIANTE
    MOV DX, OFFSET M35
    MOV AH, 09h
    INT 21h
    
    MOV AL, CL         ;toma el valor que tenemos en base 0, en nuestro ejemplo del 9 es AL = 8 (indice base 0)
    INC AL                ; Convierte a numero base 1, AL = 9
    CALL MOSTRAR_NUMERO_1DIGITO  ; Muestra "9"
    
     ; SALTO DE LINEA
    MOV DL, 13      ; 13 = Carriage Return (CR) en ASCII - mueve cursor al inicio de la linea actual
    MOV AH, 02h     ; Funcion 02h: Imprimir un solo caracter (el que esta en DL)
    INT 21h         ; DOS interpreta el codigo 13 como "retorno de carro"
    MOV DL, 10      ; 10 = Line Feed (LF) en ASCII - mueve cursor a la siguiente linea  
    MOV AH, 02h
    INT 21h         ; DOS interpreta el codigo 10 como "avance de linea"
    
    ; MUESTRA DATOS DEL ESTUDIANTE
    CALL MOSTRAR_ESTUDIANTE_COMPLETO
    
fin_buscar:   ;todos los registros recuperan su valor original antes del call
    POP SI     ;se hace en orden inverso al PUSH.
    POP DX
    POP CX
    POP BX
    POP AX         
    RET          ;Saca la direccion de retorno de la pila y salta a esa direccion, osea vuelve al punto donde se llamo esta rutina.
BUSCAR_ESTUDIANTE ENDP ;End Procedure = Marca el final de este procedimiento, 

; ==========================================================================
; RUTINA PARA MOSTRAR NUMERO DE 1 DIGITO
; ==========================================================================
MOSTRAR_NUMERO_1DIGITO PROC
    PUSH AX   ;como la rutina solo modifica AX y DX solo guarda esos en el stack
    PUSH DX
    
    ADD AL, '0'           ;AL tiene un numero de 0 a 9, lo Convierte a caracter ASCII sumandole 48
    MOV DL, AL            ;lo guarda en DL
    MOV AH, 02h           ;funcion imprimir caracter y como lo que esta en DL es un caracter numerico lo imprime tal cual
    INT 21h               ;
    
    POP DX            ;recuperamos los valores originales de los registros
    POP AX
    RET               ;volvemos al call
MOSTRAR_NUMERO_1DIGITO ENDP

; ==========================================================================
; RUTINA PARA MOSTRAR ESTUDIANTE COMPLETO
; ==========================================================================
MOSTRAR_ESTUDIANTE_COMPLETO PROC
    PUSH AX
    PUSH BX         ;el push es como hacer una copia de segura, se guarda el valor en el stack  
    PUSH DX
    PUSH SI         ;pero los registros conservan el valor que tenian antes del push.
    
    ; MUESTRA NOMBRE
    MOV BX, SI       ;en BUSCAR_ESTUDIANTE(linea 339) se le asigno al SI la direccion del estudiante a mostrar
    ADD BX, NOMBRE_OFFSET  ;en BX esta la direccion del estudiante, al sumarle el offset del nombre obtenemos la ubicacion, aunque esto es trolleada pues offset del nombre es 0.
    MOV DX, BX  ;mueve a DX la direccion del nombre
    MOV AH,09h   ;funcion imprime nombre hasta encontrar $
    INT 21h
    
    MOV DL, ' '           ; Espacio entre campos
    MOV AH, 02h           ;imprime ese espacio vacio
    INT 21h
    
    ; MUESTRA APELLIDO1
    MOV BX, SI                   ; BX = direccion base del estudiante  
    ADD BX, APELLIDO1_OFFSET     ; BX = direccion del apellido1 (SI + 20)
    MOV DX, BX                   ; DX = direccion del string
    MOV AH, 09h                  ; Funcion: imprimir string
    INT 21h                      ; Muestra el apellido1
    
    MOV DL, ' '                  ; Caracter espacio
    MOV AH, 02h                  ; Funcion: imprimir caracter
    INT 21h                      ; Muestra espacio entre campos
    
    ; MUESTRA APELLIDO2
    MOV BX, SI
    ADD BX, APELLIDO2_OFFSET
    MOV DX, BX
    MOV AH,09h
    INT 21h
    
    ; MUESTRA NOTA
    MOV DX, OFFSET M34     ;"Nota: $"
    MOV AH,09h           
    INT 21h
    
    CALL MOSTRAR_NUMERO_CON_DECIMALES ; muestra nota con decimales
    
    POP SI
    POP DX
    POP BX
    POP AX
    RET
MOSTRAR_ESTUDIANTE_COMPLETO ENDP

; ==========================================================================
; RUTINAS DE ENTRADA Y PARSING
; ==========================================================================

; RUTINA PARA LEER ENTRADA COMPLETA DESDE TECLADO
LEER_ENTRADA_COMPLETA PROC
    MOV DI, OFFSET buffer  ;DI es Destination Index que señala donde guardar en el buffer, el buffer son 100 bytes inicializados como $
    MOV CX, 99             ; Maximo 99 caracteres a leer, se deja 1 byte para el $ al final.
                           ;actualmente DI apunta al inicio del buffer.
LEER_ENTRADA:
    MOV AH, 01h            ;Funcion: leer caracter con eco ,El caracter se muestra en pantalla ("con eco")
    INT 21h                ;El programa se detiene y espera que el usuario presione una tecla, resultado lo guarda en AL  como ASCII
    CMP AL, 13             ;Analiza si el caracter leido es enter 
    JE FIN_LECTURA         ; Si es enter termina lectura
    CMP AL, '9'            ; Verifica si es '9'
    JE VERIFICAR_SOLO_9    ;verifica si es solo 9 , caso especial
    MOV [DI], AL           ;Si no es enter ni 9, Guarda caracter en buffer[DI]        Ejemplo:   1. 'H' ? AL=72 ? guarda en buffer[0] ? DI=1, CX=98
    INC DI                 ;Mueve el "dedo" al siguiente byte
    LOOP LEER_ENTRADA      ; CX = CX - 1, si CX > 0 repite                            ;          2. 'o' ? AL=111 ? guarda en buffer[1] ? DI=2, CX=97  
    JMP FIN_LECTURA        ; Si CX = 0, termina por limite
                                                                                      ;          3. 'l' ? AL=108 ? guarda en buffer[2] ? DI=3, CX=96
VERIFICAR_SOLO_9:
    CMP DI, OFFSET buffer  ;Verifica si es el primer caracter, DI apunta al inicio del buffer?   4. 'a' ? AL=97 ? guarda en buffer[3] ? DI=4, CX=95
    JNE CONTINUAR_LECTURA  ; si no, es un 9 normal en medio del texto
    MOV BYTE PTR [DI], '$' ; Termina string, BYTE PTR MUEVE 1 BYTE, "En la direccion de memoria apuntada por DI, guarda el valor 36 (codigo ASCII de '$'), pero solo 1 byte"
    STC                    ; Set carry flag (indica salida) 1 (ERROR/SPECIAL)
    RET
                                                                               ;[DI] es lo que hay dentro de esa direccion de memoria
CONTINUAR_LECTURA:                                                             ;se podria decir que DI es el puntero, y [DI] a lo que apunta.
    MOV [DI], AL           ; Almacena '9' en buffer
    INC DI
    LOOP LEER_ENTRADA
    
FIN_LECTURA:
    MOV BYTE PTR [DI], '$' ; Termina string
    CLC                    ; Clear carry flag (éxito)
    RET
LEER_ENTRADA_COMPLETA ENDP

; RUTINA PRINCIPAL DE PARSING
PARSE_ENTRADA PROC
    PUSH SI
    PUSH DI
    PUSH BX
    
    ; CALCULA POSICION PARA NUEVO ESTUDIANTE
    MOV SI, OFFSET estudiantes
    MOV AL, contador
    MOV BL, ESTUDIANTE_SIZE
    MUL BL                 ; AX = contador * tamano_estudiante
    ADD SI, AX             ; SI = direccion del nuevo estudiante

    MOV DI, OFFSET buffer  ; DI = direccion del buffer
    
    ; LEE Y PROCESA NOMBRE
    CALL leer_token
    JC PARSE_ERROR
    MOV BX, NOMBRE_OFFSET
    CALL copiar_campo
    
    ; LEE Y PROCESA APELLIDO1
    CALL leer_token
    JC PARSE_ERROR
    MOV BX, APELLIDO1_OFFSET
    CALL copiar_campo
    
    ; LEE Y PROCESA APELLIDO2
    CALL leer_token
    JC PARSE_ERROR
    MOV BX, APELLIDO2_OFFSET
    CALL copiar_campo
    
    ; LEE Y PROCESA NOTA
    CALL leer_token_nota
    JC PARSE_ERROR
    
    
    CALL CONVERTIR_NOTA_CON_DECIMALES
    JC PARSE_ERROR
    
    CLC                    ; Clear carry flag (Exito)
    JMP PARSE_EXIT
     
PARSE_ERROR:
    STC                    ; Set carry flag (error)
    
PARSE_EXIT:
    POP BX
    POP DI
    POP SI
    RET
PARSE_ENTRADA ENDP

; RUTINA PARA LEER TOKEN
leer_token PROC
    PUSH BX
    PUSH CX
    
saltar_espacios:
    MOV AL, [DI]           ; Lee caracter actual
    CMP AL, ' '            ; Verifica si es espacio
    JNE iniciar_token
    INC DI                 ; Salta espacio
    JMP saltar_espacios
    
iniciar_token:
    MOV BX, OFFSET temp_campo ; BX = buffer temporal
    MOV CX, 0              ; Contador de caracteres
    MOV BYTE PTR [BX], '$' ; Inicializa buffer
    
leer_token_loop:
    MOV AL, [DI]           ; Lee caracter
    CMP AL, ' '            ; Verifica si es espacio
    JE token_completo
    CMP AL, 13             ; Verifica si es Enter
    JE token_completo
    CMP AL, '$'            ; Verifica si es fin de string
    JE token_completo
    MOV [BX], AL           ; Almacena caracter en buffer temporal
    INC BX
    INC DI
    INC CX                 ; Incrementa contador
    CMP CX, 19             ; Verificar limite maximo
    JB leer_token_loop
    
token_completo:
    MOV BYTE PTR [BX], '$' ; Termina string
    CMP CX, 0              ; Verifica si hay caracteres
    JNE token_valido
    STC                    ; Set carry flag (error)
    JMP fin_leer_token
    
token_valido:
    CLC                    ; Clear carry flag (éxito)
    INC DI                 ; Avanza al siguiente caracter
    
fin_leer_token:
    POP CX
    POP BX
    RET
leer_token ENDP

; RUTINA PARA LEER TOKEN DE NOTA
leer_token_nota PROC
    PUSH BX
    Push CX
    
saltar_espacios_nota:
    MOV AL, [DI]           ; Lee caracter actual
    CMP AL, ' '            ; Verifica si es espacio
    JNE iniciar_token_nota
    INC DI                 ; Saltar espacio
    JMP saltar_espacios_nota
    
iniciar_token_nota:
    MOV BX, OFFSET temp_nota ; BX = buffer temporal para nota
    MOV CX, 0              ; Contador de caracteres
    MOV BYTE PTR [BX], '$' ; Inicializar buffer
    
leer_token_nota_loop:
    MOV AL, [DI]           ; Lee caracter
    CMP AL, ' '            ; Verifica si es espacio
    JE token_completo_nota
    CMP AL, 13             ; Verifica si es Enter
    JE token_completo_nota
    CMP AL, '$'            ; Verifica si es fin de string
    JE token_completo_nota
    MOV [BX], AL           ; Almacena caracter en buffer temporal
    INC BX
    INC DI
    INC CX                 ; Incrementa contador
    CMP CX, 19             ; Verifica limite maximo
    JB leer_token_nota_loop
    
token_completo_nota:
    MOV BYTE PTR [BX], '$' ; Termina string
    CMP CX, 0              ; Verifica si hay caracteres
    JNE token_valido_nota
    STC                    ; Set carry flag (error)
    JMP fin_leer_token_nota
    
token_valido_nota:
    CLC                    ; Clear carry flag (éxito)
    INC DI                 ; Avanza al siguiente caracter
    
fin_leer_token_nota:
    POP CX
    POP BX
    RET
leer_token_nota ENDP

; RUTINA PARA COPIAR CUALQUIER CAMPO
copiar_campo PROC
    PUSH DI
    PUSH SI
    PUSH CX
    PUSH AX
    
    MOV DI, OFFSET temp_campo ; DI = buffer temporal
    MOV CX, 0              ; Contador de caracteres
    
copiar_campo_loop:
    MOV AL, [DI]           ; Lee caracte del buffer temporal
    CMP AL, '$'            ; Verifica si es fin de string
    JE fin_copiar_campo
    
    PUSH DI
    MOV DI, SI             ; DI = direccion del estudiante
    ADD DI, BX             ; + offset del campo
    ADD DI, CX             ; + posicion actual
    MOV [DI], AL           ; Almacena caracter en estructura
    POP DI
    
    INC DI                 ; Siguiente caracter en buffer temporal
    INC CX                 ; Incrementa contador
    CMP CX, 19             ; Verificar limite maximo
    JB copiar_campo_loop
    
fin_copiar_campo:
    PUSH DI
    MOV DI, SI             ; DI = direccion del estudiante
    ADD DI, BX             ; + offset del campo
    ADD DI, CX             ; + posicion actual
    MOV BYTE PTR [DI], '$' ; Terminar string
    POP DI
    
    POP AX
    POP CX
    POP SI
    POP DI
    RET
copiar_campo ENDP

; ==========================================================================
; RUTINA PARA CONVERTIR NOTA CON DECIMALES
; ==========================================================================
CONVERTIR_NOTA_CON_DECIMALES PROC
    PUSH BX
    PUSH CX
    PUSH DX
    PUSH DI
    PUSH SI

    MOV DI, OFFSET temp_nota ; DI = buffer temporal de nota
    XOR BX, BX        ; BX = parte entera (inicializar a 0)
    XOR DX, DX        ; DX = parte decimal temporal (inicializar a 0)
    MOV CX, 0         ; CX = contador de digitos decimales
    MOV AH, 0         ; AH = 0 (entero), 1 (decimal)

PROCESAR_CARACTER:
    MOV AL, [DI]      ; Lee caracter
    CMP AL, '$'       ; Verifica si es fin de string
    JE FIN_PROCESAMIENTO

    CMP AL, '.'       ; Verifica si es punto decimal
    JE ES_PUNTO

    CMP AL, '0'       ; Verifica si es digito
    JB ERROR_NOTA
    CMP AL, '9'
    JA ERROR_NOTA

    SUB AL, '0'       ; Convierte a valor numerico

    CMP AH, 0         ; Verifica si estamos procesando entero o decimal
    JE PROCESAR_ENTERO

    ; Procesa parte decimal
    INC CX            ; Incrementa contador de decimales
    CMP CX, 4         ; Maximo 4 decimales
    JA ERROR_NOTA

    ; Multiplica decimal actual por 10 y sumar nuevo digito
    PUSH AX
    MOV AX, DX        ; AX = valor decimal acumulado
    MOV DX, 10        ; Multiplica por 10
    MUL DX
    MOV DX, AX        ; Devuelve resultado a DX
    POP AX
    ADD DL, AL        ; Suma nuevo digito
    ADC DH, 0         ; Acarreo si hay overflow
    JMP SIGUIENTE_CARACTER

PROCESAR_ENTERO:
    PUSH AX
    MOV AX, BX        ; AX = valor entero acumulado
    MOV BX, 10        ; Multiplica por 10
    MUL BX
    MOV BX, AX        ; Devuelve resultado a BX
    POP AX
    ADD BL, AL        ; Suma nuevo digito
    ADC BH, 0         ; Acarreo si hay overflow
    JMP SIGUIENTE_CARACTER

ES_PUNTO:
    CMP AH, 0         ; Verifica que no haya ya un punto
    JNE ERROR_NOTA
    MOV AH, 1         ; Cambia a modo decimal
    JMP SIGUIENTE_CARACTER

SIGUIENTE_CARACTER:
    INC DI            ; Siguiente caracter
    JMP PROCESAR_CARACTER

FIN_PROCESAMIENTO:
    CMP BX, 0         ; Verificar que haya parte entera
    JE ERROR_NOTA 
    
    CMP BX, 100
    JBE CONTINUAR_GUARDAR
    JMP ERROR_NOTA

CONTINUAR_GUARDAR:
    ; GUARDAR el contador de digitos decimales leidos
    PUSH CX           ; Guarda CX (digitos leidos)

    ; Ajusta decimales a 4 digitos (multiplica por 10^(4-CX))
    MOV AX, DX        ; AX = valor decimal temporal
    POP CX            ; CX = digitos leidos
    MOV DX, 4
    SUB DX, CX        ; DX = digitos faltantes (4 - digitos_leidos)

    ; Si no hay digitos decimales, salta ajuste
    CMP CX, 0
    JE GUARDAR_NOTA

AJUSTAR_DECIMALES:
    CMP DX, 0
    JLE GUARDAR_NOTA
    MOV CX, 10    
    PUSH DX
    MUL CX           ; AX = AX * 10 
    POP DX
    DEC DX
    JMP AJUSTAR_DECIMALES

GUARDAR_NOTA: 
    ; Guarda parte entera
    MOV [SI + NOTA_ENTERA_OFFSET], BX
    
    ; Guarda parte decimal (x10000)
    MOV [SI + NOTA_DECIMAL_OFFSET], AX
    
    CLC              ; Clear carry flag (éxito)
    JMP FIN_CONVERTIR_NOTA

ERROR_NOTA:
    STC              ; Set carry flag (error)

FIN_CONVERTIR_NOTA:
    POP SI
    POP DI
    POP DX
    POP CX
    POP BX
    RET
CONVERTIR_NOTA_CON_DECIMALES ENDP

; ==========================================================================
; RUTINA PARA MOSTRAR NÚMERO CON DECIMALES
; ==========================================================================
MOSTRAR_NUMERO_CON_DECIMALES PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    PUSH SI
    
    ; Muestra parte entera
    MOV AX, [SI + NOTA_ENTERA_OFFSET]
    CALL MOSTRAR_NUMERO_ENTERO
    
    ; Muestra punto decimal
    MOV DL, '.'
    MOV AH, 02h
    INT 21h
    
    ; Muestra parte decimal (4 digitos siempre)
    MOV AX, [SI + NOTA_DECIMAL_OFFSET]
    MOV CX, 4          ; Mostrar 4 dígitos 
    MOV BX, 10
    
    ; Convierte a dagitos individuales
CONVERTIR_DECIMALES:
    XOR DX, DX
    DIV BX             ; AX = cociente, DX = residuo
    PUSH DX            ; Guardar digito
    LOOP CONVERTIR_DECIMALES
    
    MOV CX, 4          ; Recuperar 4 digitos
MOSTRAR_DIGITOS_DECIMAL:
    POP DX
    ADD DL, '0'        ; Convertir a ASCII
    MOV AH, 02h
    INT 21h
    LOOP MOSTRAR_DIGITOS_DECIMAL
    
FIN_MOSTRAR:
    POP SI
    POP DX
    POP CX
    POP BX
    POP AX
    RET
MOSTRAR_NUMERO_CON_DECIMALES ENDP

; ==========================================================================
; RUTINA PARA MOSTRAR NÚMERO ENTERO
; ==========================================================================
MOSTRAR_NUMERO_ENTERO PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    
    MOV CX, 0          ; Contador de digitos
    MOV BX, 10         ; Base 10
    
    CMP AX, 0          ; Verifica si es cero
    JNE DIV_LOOP_ENTERO
    MOV DL, '0'        ; Muestra '0'
    MOV AH, 02h
    INT 21h
    JMP FIN_MOSTRAR_ENTERO
    
DIV_LOOP_ENTERO:
    XOR DX, DX
    DIV BX             ; AX = cociente, DX = residuo
    PUSH DX            ; Guardar digito
    INC CX             ; Incrementa contador
    CMP AX, 0          ; Verifica si hay mas digitos
    JNE DIV_LOOP_ENTERO
    
MOSTRAR_ENTERO:
    POP DX
    ADD DL, '0'        ; Convierte a ASCII
    MOV AH, 02h
    INT 21h
    LOOP MOSTRAR_ENTERO
    
FIN_MOSTRAR_ENTERO:
    POP DX
    POP CX
    POP BX
    POP AX
    RET
MOSTRAR_NUMERO_ENTERO ENDP

; ==========================================================================
; RUTINAS DE ORDENAMIENTO
; ==========================================================================

; INICIALIZA ARRAY DE INDICES
INICIALIZAR_INDICES PROC
    PUSH CX
    PUSH SI
    MOV CL, 0           ; Contador
    MOV SI, 0           ; indice en array
INICIALIZAR_LOOP:
    CMP CL, contador    ; Verifica si llegamos al final
    JAE FIN_INICIALIZAR
    MOV indices[SI], CL ; Almacena indice
    INC SI              ; Siguiente posicion
    INC CL              ; Siguiente valor
    JMP INICIALIZAR_LOOP
FIN_INICIALIZAR:
    POP SI
    POP CX
    RET
INICIALIZAR_INDICES ENDP

; COMPARA NOTAS DE DOS ESTUDIANTES
COMPARAR_NOTAS PROC
    PUSH BX
    PUSH CX
    PUSH DX
    PUSH SI
    PUSH DI

    ; Guarda los indices originales
    MOV DH, AL  ; Guardar indice1 en DH
    MOV DL, BL  ; Guardar indice2 en DL

    ; Calcula direccion del estudiante1 (indice en DH)
    MOV AL, DH
    MOV AH, 0
    MOV CL, ESTUDIANTE_SIZE
    MUL CL                 ; AX = indice * tamano_estudiante
    MOV SI, OFFSET estudiantes
    ADD SI, AX             ; SI = direccion estudiante1

    ; Calcula direccion del estudiante2 (indice en DL)
    MOV AL, DL
    MOV AH, 0
    MUL CL                 ; AX = indice * tamano_estudiante
    MOV DI, OFFSET estudiantes
    ADD DI, AX             ; DI = direccion estudiante2

    ; Compara parte entera
    MOV AX, [SI + NOTA_ENTERA_OFFSET]
    MOV BX, [DI + NOTA_ENTERA_OFFSET]
    CMP AX, BX
    JNE FIN_COMPARAR       ; Si diferentes, flags ya configurados

    ; Si partes enteras iguales, compara decimales
    MOV AX, [SI + NOTA_DECIMAL_OFFSET] 
    MOV BX, [DI + NOTA_DECIMAL_OFFSET]
    CMP AX, BX

FIN_COMPARAR:
    POP DI
    POP SI
    POP DX
    POP CX
    POP BX
    RET
COMPARAR_NOTAS ENDP

BUBBLESORT_NOTAS PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    PUSH SI
    PUSH DI

    ; AL = 0 asc / 1 desc (viene del llamador)
    MOV DH, AL                ; direccion (0=asc, 1=des)

    ; Si hay 0 o 1 elemento, no hay nada que ordenar
    CMP contador, 1
    JBE FIN_BUBBLESORT

    MOV CH, 0                 ; i = 0 (bucle externo)

BUCLE_EXTERNO:
    ; DL = contador - i - 1  (limite superior de j + 1)
    MOV AL, contador
    SUB AL, CH
    DEC AL
    MOV DL, AL
    ; Si DL == 0 ya no hay pares por comparar
    CMP DL, 0
    JBE FIN_BUBBLESORT

    ; bandera de intercambio = 0 al inicio de cada pasada
    MOV BYTE PTR hubo_swap, 0

    MOV CL, 0                 ; j = 0 (bucle interno)

BUCLE_INTERNO:
    ; while (j < contador - i - 1)
    CMP CL, DL
    JAE FIN_BUCLE_INTERNO

    ; base del arreglo de indices
    MOV BX, OFFSET indices

    ; ---- cargar indices[j] en AL ----
    XOR AX, AX                ; AX = 0
    MOV AL, CL                ; AL = j (8-bit)
    MOV SI, AX                ; SI = j (zero-extended)
    MOV AL, [BX+SI]           ; AL = indices[j]

    ; ---- cargar indices[j+1] en AH ----
    MOV DI, SI                ; DI = j
    INC DI                    ; DI = j+1
    MOV AH, [BX+DI]           ; AH = indices[j+1]

    ; preparar argumentos para COMPARAR_NOTAS: AL = idx(j), BL = idx(j+1)
    MOV BL, AH                ; ojo: BL cambia BX, pero ya no lo usamos hasta recargar

    CALL COMPARAR_NOTAS       ; deja flags segun comparacion (entero y luego decimal)

    ; guardar flags y decidir segun direccion
    PUSHF
    CMP DH, 0
    JE  ASCENDENTE
    POPF
    JB  INTERCAMBIA           ; desc: if nota[j] < nota[j+1] -> swap
    JMP NO_INTERCAMBIA

ASCENDENTE:
    POPF
    JA  INTERCAMBIA           ; asc: if nota[j] > nota[j+1] -> swap
    JMP NO_INTERCAMBIA

INTERCAMBIA:
    ; Re-cargar base y posiciones, y hacer el swap seguro
    MOV BX, OFFSET indices

    ; SI = j
    XOR AX, AX
    MOV AL, CL
    MOV SI, AX

    ; DI = j+1
    MOV DI, SI
    INC DI

    ; lee de nuevo (COMPARAR_NOTAS pudo pisar AX)
    MOV AL, [BX+SI]           ; AL = indices[j]
    MOV AH, [BX+DI]           ; AH = indices[j+1]

    ; swap
    MOV [BX+SI], AH           ; indices[j]   = antiguo indices[j+1]
    MOV [BX+DI], AL           ; indices[j+1] = antiguo indices[j]

    MOV BYTE PTR hubo_swap, 1

NO_INTERCAMBIA:
    INC CL                    ; j++
    JMP BUCLE_INTERNO

FIN_BUCLE_INTERNO:
    ; si no hubo intercambios, ya esta ordenado
    CMP BYTE PTR hubo_swap, 0
    JE  FIN_BUBBLESORT

    INC CH                    ; i++
    JMP BUCLE_EXTERNO

FIN_BUBBLESORT:
    POP DI
    POP SI
    POP DX
    POP CX
    POP BX
    POP AX
    RET
BUBBLESORT_NOTAS ENDP


MOSTRAR_NOTAS_ORDENADAS PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    PUSH SI

    MOV CL, 0                       ; j = 0
MOSTRAR_LOOP:
    CMP CL, contador
    JAE FIN_MOSTRAR_ORDENADAS

    ; ===== 1) idx = indices[j] -> tmp_idx =====
    MOV BX, OFFSET indices          ; base de indices
    XOR AX, AX
    MOV AL, CL                      ; AL = j
    MOV SI, AX                      ; SI = j (zero-extended)
    MOV AL, [BX+SI]                 ; AL = indices[j] (0..14)
    MOV tmp_idx, AL                 ; guardar idx

    ; ===== 2) Imprimir ID humano = tmp_idx + 1 =====
    MOV DX, OFFSET M35              ; "Numero de estudiante: "
    MOV AH, 09h
    INT 21h

    XOR AX, AX
    MOV AL, tmp_idx
    INC AX                          ; AX = idx + 1
    CALL MOSTRAR_NUMERO_ENTERO

    ; salto de linea tras el ID
    MOV DL, 13
    MOV AH, 02h
    INT 21h
    MOV DL, 10
    MOV AH, 02h
    INT 21h

    ; ===== 3) SI = &estudiantes[tmp_idx] =====
    XOR AX, AX
    MOV AL, tmp_idx                 ; AX = idx
    MOV BL, ESTUDIANTE_SIZE         ; BL = 64
    MUL BL                          ; AX = idx * 64  (AL*BL -> AX)
    MOV SI, OFFSET estudiantes
    ADD SI, AX                      ; SI apunta al registro del estudiante

    ; ===== 4) Mostrar datos completos =====
    CALL MOSTRAR_ESTUDIANTE_COMPLETO

    ; salto de linea entre estudiantes
    MOV DL, 13
    MOV AH, 02h
    INT 21h
    MOV DL, 10
    MOV AH, 02h
    INT 21h

    INC CL
    JMP MOSTRAR_LOOP

FIN_MOSTRAR_ORDENADAS:
    POP SI
    POP DX
    POP CX
    POP BX
    POP AX
    RET
MOSTRAR_NOTAS_ORDENADAS ENDP 

; ==========================================================================
; RUTINA PRINCIPAL DE ESTADISTICAS
; ==========================================================================
CALCULAR_ESTADISTICAS PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    PUSH SI
    PUSH DI
    
    ;Verifica si hay estudiantes
    CMP contador, 0
    JNE HAY_ESTUDIANTES
    
    ;No hay estudiantes
    MOV DX, OFFSET M29
    MOV AH, 09h
    INT 21h
    JMP FIN_ESTADISTICAS
    
HAY_ESTUDIANTES:
    ;Inicializa variables
    CALL INICIALIZAR_ESTADISTICAS
    
    ;Calcula todas las estadisticas
    CALL CALCULAR_TODAS_ESTADISTICAS
    
    ;Muestra resultados
    CALL MOSTRAR_ESTADISTICAS
    
FIN_ESTADISTICAS:
    POP DI
    POP SI
    POP DX
    POP CX
    POP BX
    POP AX
    RET
CALCULAR_ESTADISTICAS ENDP

; ==========================================================================
; INICIALIZAR VARIABLES DE ESTADISTICAS
; ==========================================================================
INICIALIZAR_ESTADISTICAS PROC
    MOV promedio_entera, 0
    MOV promedio_decimal, 0
    MOV nota_maxima_entera, 0    
    MOV nota_maxima_decimal, 0
    MOV nota_minima_entera, 100
    MOV nota_minima_decimal, 0
    MOV aprobados_count, 0
    MOV reprobados_count, 0
    
    ;Convertir contador a word
    MOV AL, contador
    MOV AH, 0
    MOV total_estudiantes, AX
    
    RET
INICIALIZAR_ESTADISTICAS ENDP

; ==========================================================================
; CALCULA TODAS LAS ESTADISTICAS
; ==========================================================================
CALCULAR_TODAS_ESTADISTICAS PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    PUSH SI
    
    MOV CX, 0       ;Contador de estudiantes
    
CALCULAR_LOOP:
    CMP CL, contador
    JAE FIN_CALCULOS
    
    ;Obtiene direccion del estudiante
    MOV AL, CL
    MOV AH, 0
    MOV BL, ESTUDIANTE_SIZE
    MUL BL
    MOV SI, OFFSET estudiantes
    ADD SI, AX
    
    ;Obtiene nota del estudiante
    MOV AX, [SI + NOTA_ENTERA_OFFSET]
    MOV BX, [SI + NOTA_DECIMAL_OFFSET]
    
    ;Acumula para promedio
    CALL ACUMULAR_PROMEDIO
    
    ;Verifica nota maxima
    CALL ACTUALIZAR_NOTA_MAXIMA
    
    ;Verifica nota minima
    CALL ACTUALIZAR_NOTA_MINIMA
    
    ;Cuenta aprobados/reprobados
    CALL CONTAR_APROBADOS_REPROBADOS
    
    INC CX
    JMP CALCULAR_LOOP
    
FIN_CALCULOS:
    ;Calcular promedio final
    CALL CALCULAR_PROMEDIO_FINAL
    
    POP SI
    POP DX
    POP CX
    POP BX
    POP AX
    RET
CALCULAR_TODAS_ESTADISTICAS ENDP

; ==========================================================================
; ACUMULA NOTA PARA PROMEDIO
; ==========================================================================
ACUMULAR_PROMEDIO PROC
    PUSH CX
    PUSH DX
    
    ;Suma parte entera
    ADD promedio_entera, AX
    
    ;Sumarparte decimal
    MOV DX, promedio_decimal
    ADD DX, BX
    MOV promedio_decimal, DX
    
    ;Maneja acarreo si la parte decimal supera 99999 (x10000)
    CMP DX, 10000
    JB NO_ACARREO_DECIMAL
    
    ;Ajusta acarreo
    SUB DX, 10000
    MOV promedio_decimal, DX
    INC promedio_entera
    
NO_ACARREO_DECIMAL:
    POP DX
    POP CX
    RET
ACUMULAR_PROMEDIO ENDP

; ==========================================================================
; ACTUALIZA NOTA MAXIMA
; ==========================================================================
ACTUALIZAR_NOTA_MAXIMA PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    
    ;Compara parte entera primero
    MOV CX, nota_maxima_entera
    CMP AX, CX
    JA NUEVA_MAXIMA
    JB NO_ES_MAXIMA
    
    ;Si partes enteras iguales, comparar decimales
    MOV DX, nota_maxima_decimal
    CMP BX, DX
    JBE NO_ES_MAXIMA
    
NUEVA_MAXIMA:
    MOV nota_maxima_entera, AX
    MOV nota_maxima_decimal, BX
    
NO_ES_MAXIMA:
    POP DX
    POP CX
    POP BX
    POP AX
    RET
ACTUALIZAR_NOTA_MAXIMA ENDP

; ==========================================================================
; ACTUALIZA NOTA MINIMA
; ==========================================================================
ACTUALIZAR_NOTA_MINIMA PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    
    ;Compara parte entera primero
    MOV CX, nota_minima_entera
    CMP AX, CX
    JB NUEVA_MINIMA
    JA NO_ES_MINIMA
    
    ;Si partes enteras iguales, comparar decimales
    MOV DX, nota_minima_decimal
    CMP BX, DX
    JAE NO_ES_MINIMA
    
NUEVA_MINIMA:
    MOV nota_minima_entera, AX
    MOV nota_minima_decimal, BX
    
NO_ES_MINIMA:
    POP DX
    POP CX
    POP BX
    POP AX
    RET
ACTUALIZAR_NOTA_MINIMA ENDP

; ==========================================================================
; CONTAR APROBADOS Y REPROBADOS
; ==========================================================================
CONTAR_APROBADOS_REPROBADOS PROC
    PUSH AX
    PUSH BX
    
    ;Verifica si es aprobado (>=70)
    CMP AX, 70
    JA ES_APROBADO
    JB ES_REPROBADO
    
    ;Si es exactamente 70, verifica decimales
    CMP BX, 0
    JA ES_APROBADO
    JE ES_APROBADO ;70.00000 es aprobado
    
ES_APROBADO:
    INC aprobados_count
    JMP FIN_CONTEO
    
ES_REPROBADO:
    INC reprobados_count
    
FIN_CONTEO:
    POP BX
    POP AX
    RET
CONTAR_APROBADOS_REPROBADOS ENDP
    
; ==========================================================================
; CALCULA PROMEDIO FINAL
; ==========================================================================
CALCULAR_PROMEDIO_FINAL PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    
    ;Verifica division por cero
    CMP total_estudiantes, 0
    JE FIN_PROMEDIO
    
    ;Divide suma de partes enteras
    MOV AX, promedio_entera
    MOV DX, 0
    DIV total_estudiantes
    MOV promedio_entera, AX     ;Cociente= parte entera del promedio
    MOV CX, DX                  ;Residuo= parte para decimales
    
    ;Convierte residuo a decimal (x10000)
    MOV AX, CX
    MOV BX, 10000
    MUL BX                  ;DX:AX= residuo*10000
    DIV total_estudiantes   ;AX = (residuo*10000)/n
    MOV BX, AX              ;Guardar parte decimal
    
    ;Suma parte decimal acumulada
    MOV AX, promedio_decimal
    ADD AX, BX
    MOV promedio_decimal, AX
    
    ;Ajusta acarreos
    CMP AX, 10000
    JB FIN_PROMEDIO
    
    ;Si decimal >=10000, ajustar
    SUB AX, 10000
    MOV promedio_decimal, AX
    INC promedio_entera
    
FIN_PROMEDIO:
    POP DX
    POP CX
    POP BX
    POP AX
    RET        
CALCULAR_PROMEDIO_FINAL ENDP

; ==========================================================================
; MOSTRAR ESTADISTICAS
; ==========================================================================
MOSTRAR_ESTADISTICAS PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    
    ;1. Promedio general
    MOV DX, OFFSET M22
    MOV AH, 09h
    INT 21h
    
    MOV AX, promedio_entera
    MOV BX, promedio_decimal
    CALL MOSTRAR_NOTA_CON_DECIMALES
    
    ;2. Nota maxima
    MOV DX, OFFSET M23
    MOV AH, 09h
    INT 21h
    
    MOV AX, nota_maxima_entera
    MOV BX, nota_maxima_decimal
    CALL MOSTRAR_NOTA_CON_DECIMALES
    
    ;3. Nota minima
    MOV DX, OFFSET M24
    MOV AH, 09h
    INT 21h
    
    MOV AX, nota_minima_entera
    MOV BX, nota_minima_decimal
    CALL MOSTRAR_NOTA_CON_DECIMALES
    
    ;4. Aprobados
    MOV DX, OFFSET M25
    MOV AH, 09h
    INT 21h
    
    MOV AX, aprobados_count
    CALL MOSTRAR_NUMERO_ENTERO
    
    MOV DX, OFFSET M27
    MOV AH, 09h
    INT 21h
    
    CALL CALCULAR_PORCENTAJE_APROBADOS
    CALL MOSTRAR_PORCENTAJE
    
    ;5. Reprobados
    MOV DX, OFFSET M26
    MOV AH, 09h
    INT 21h
    
    MOV AX, reprobados_count
    CALL MOSTRAR_NUMERO_ENTERO
    
    MOV DX, OFFSET M27
    MOV AH, 09h
    INT 21h
    
    CALL CALCULAR_PORCENTAJE_REPROBADOS
    CALL MOSTRAR_PORCENTAJE
    
    ;Pie
    MOV DX, OFFSET M30
    MOV AH, 09h
    INT 21h
    
    POP DX
    POP CX
    POP BX
    POP AX
    RET
MOSTRAR_ESTADISTICAS ENDP

; ==========================================================================
; CALCULAR PORCENTAJE DE APROBADOS
; ==========================================================================
CALCULAR_PORCENTAJE_APROBADOS PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    
    MOV AX, aprobados_count
    MOV BX, 100
    MUL BX                   ;AX = aprobados*100
    DIV total_estudiantes    ;AX = (aprobados*100)/total
    
    MOV porcentaje_temp, AX
    
    POP DX
    POP CX
    POP BX
    POP AX
    RET
CALCULAR_PORCENTAJE_APROBADOS ENDP

; ==========================================================================
; CALCULAR PORCENTAJE DE REPROBADOS
; ==========================================================================
CALCULAR_PORCENTAJE_REPROBADOS PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    
    MOV AX, reprobados_count
    MOV BX, 100
    MUL BX                   ;AX = reprobados*100
    DIV total_estudiantes    ;AX = (reprobados*100)/total
    
    MOV porcentaje_temp, AX
    
    POP DX
    POP CX
    POP BX
    POP AX
    RET
CALCULAR_PORCENTAJE_REPROBADOS ENDP

; ==========================================================================
; MOSTRAR PORCENTAJE
; ==========================================================================
MOSTRAR_PORCENTAJE PROC
    PUSH AX
    PUSH DX
    
    MOV AX, porcentaje_temp
    CALL MOSTRAR_NUMERO_ENTERO
    
    MOV DX, OFFSET M28
    MOV AH, 09h
    INT 21h
    
    POP DX
    POP AX
    RET
MOSTRAR_PORCENTAJE ENDP

; ==========================================================================
; MOSTRAR NOTA CON DECIMALES (AX=entera, BX=decimal)
; ==========================================================================
MOSTRAR_NOTA_CON_DECIMALES PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    
    ;Mostar parte entera
    CALL MOSTRAR_NUMERO_ENTERO
    
    ;Mostar punto decimal
    MOV DL, '.'
    MOV AH, 02h
    INT 21h
    
    ;Mostrar 4 digitos decimales
    MOV AX, BX                  ;AX= parte decimal
    MOV CX, 4                   ;Mostrar 4 digitos
    MOV BX, 10                  ;Base 10
    
    ;Convertir a digitos individuales
CONVERTIR_DECIMALES_STATS:
    XOR DX, DX
    DIV BX                      ;AX= cociente, DX=residuo
    PUSH DX                     ;Guardar digito
    LOOP CONVERTIR_DECIMALES_STATS
    
    MOV CX, 4                   ;Recuperar 4 digitos
    
MOSTRAR_DIGITOS_STATS:
    POP DX
    ADD DL, '0'                 ;Convertir a ASCII
    MOV AH, 02h
    INT 21h
    LOOP MOSTRAR_DIGITOS_STATS
    
    ;Salto de linea
    MOV DL, 13
    MOV AH, 02h
    INT 21h
    MOV DL, 10
    INT 21h
    
    POP DX
    POP CX
    POP BX
    POP AX
    RET    
MOSTRAR_NOTA_CON_DECIMALES ENDP

; ==========================================================================
; VARIABLE TEMPORAL PARA PORCENTAJES
; ==========================================================================
porcentaje_temp dw 0


END MAIN