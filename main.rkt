#lang racket
(require "TDAParadigmadocs.rkt")
(require "TDAUser.rkt")
(require "TDADate.rkt")
(require "TDADocumento.rkt")
(require "TDAAcceso.rkt")


;-REGISTER-
;descripción: Función que registra a un usuario verificando que este sea único
;dom: Paradigmadocs X date X string X string
;rec: Paradigmadocs
;recursión: Natural
(define (register paradigmadocs date username password)
  (define (registrado? listaUsuarios usuario_1)
    (if (empty? listaUsuarios)
        #f
        (if (eq? (getNombre (car listaUsuarios)) (getNombre usuario_1))
            #t
            (registrado? (cdr listaUsuarios) usuario_1)))
    )
  (if (registrado? (getLista1 paradigmadocs) (usuario username password date))
      paradigmadocs
      (setLista1 paradigmadocs (cons (usuario username password date)(getLista1 paradigmadocs))))
  )


;-LOGIN-
;descripción: Función que autentica a un usuario registrado y le permite ejecutar un comando específico
;dom: paradigmadocs X string X string X function 
;rec: Paradigmadocs
;recursión: de cola
(define (login paradigmadocs username password operation)
  (define (verificado? listaUsuarios username_1 password_1)
    (if (empty? listaUsuarios)
        #f
        (if (and (eq? (getNombre (car listaUsuarios)) username_1)
                 (eq? (getContrasenna (car listaUsuarios)) password_1))
            #t
            (verificado? (cdr listaUsuarios) username_1 password_1)))
    )
  (if (verificado? (getLista1 paradigmadocs) username password)
      (cond
        [(eq? operation create)(operation (setLista2 paradigmadocs (list username)))]
        [(eq? operation share)(operation (setLista2 paradigmadocs (list username)))]
        [(eq? operation revokeAllAccesses)(operation (setLista2 paradigmadocs (list username)))]
        [else paradigmadocs]
        )
      (cond
        [(eq? operation create)(operation paradigmadocs)]
        [(eq? operation share)(operation paradigmadocs)]
        [(eq? operation revokeAllAccesses)(operation paradigmadocs)]
        [else paradigmadocs]
        )
      )
  )


;-CREATE-
;descripción: Función que crea un documento con los datos entregados. También se encarga de encriptar el contenido de este
;dom: paradigmadocs X date X String (titulo documento) X String  (contenido) 
;rec: Paradigmadocs
(define (create paradigmadocs)
  (lambda(date nombre contenido)
    (if (null? (getLista2 paradigmadocs))
        paradigmadocs
        (setLista3 (setLista2 paradigmadocs null)
                   (documento nombre date ((getEncrypt paradigmadocs) contenido) (car(getLista2 paradigmadocs)) (length (getLista3 paradigmadocs)) null null))
        )
    )
  )


;-SHARE-
;descripción: Función que le da distintos tipos de accesos a usuarios que previamente se hayan registrado. En caso de que el usuario sea el propietario del documento, este no puede darse permisos.
;dom: Paradigmadocs X Entero X Lista de accesos
;rec: Paradigmadocs
;recursión: de cola (buscarDoc, seleccionarDoc, buscarUserDoc)
(define (share paradigmadocs)
  (lambda(idDoc access . accesses)
    (if (null? (getLista2 paradigmadocs))
        paradigmadocs
        (if (buscarDoc (getLista3 paradigmadocs) idDoc)
            (setLista3_implante (setLista2 paradigmadocs null)
                                (reverse (list-set (reverse (getLista3 paradigmadocs)) idDoc (limpiarListAccesos (setAccesos (seleccionarDoc (getLista3 paradigmadocs) idDoc)
                                                                                                         (filtrarAccesos paradigmadocs (unirAccess access accesses) (buscarUserDoc (getLista3 paradigmadocs) idDoc)))))))
            paradigmadocs)
        )
    )
  )


;-ADD-
;descripción: Función que elimina todos accesos de todos los documentos
;dom: Paradigmadocs X Entero X Fecha X String
;rec: Paradigmadocs
;(define (add paradigmadocs)
;  (lambda(idDoc date contenidoTexto)
;    (if (or (esPropietario? (getLista3 paradigmadocs) (car (getLista2 paradigmadocs)) idDoc)
;            ())



;-REVOKEALLACCESSES-
;descripción: Función que elimina todos accesos de todos los documentos
;dom: Paradigmadocs
;rec: Paradigmadocs
(define (revokeAllAccesses paradigmadocs)
  (if (null? (getLista2 paradigmadocs))
      paradigmadocs
      (setLista3_implante (setLista2 paradigmadocs null) (map setAccesos_implante (getLista3 paradigmadocs))))
  )

           
;---EJEMPLOS DE CADA FUNCIÓN---
; GENERANDO PARADIGMADOCS
(define Gdocs000 (paradigmadocs "Gdocs" (fecha 25 10 2021) encryptFn encryptFn))

; 1) REGISTER 
(define Gdocs011 (register (register (register Gdocs000 (fecha 25 10 2021) "user1" "pass1") (fecha 25 10 2021) "user2" "pass2") (fecha 25 10 2021) "user3" "pass3"))
(define Gdocs012 (register Gdocs000 (fecha 25 3 2020) "user" "pass"))
(define Gdocs013 (register (register Gdocs000 (fecha 25 3 2020) "user" "pass") (fecha 25 3 2020) "user" "pass2"))

; 2) LOGIN
(define Gdocs021 (login Gdocs011 "user3" "pass3" share))
(define Gdocs022 ((login Gdocs011 "user1" "pass1" create) (fecha 12 12 1212) "doc1" "contenido1"))
(define Gdocs023 ((login Gdocs022 "user2" "pass2" create)(fecha 12 12 1212) "doc2" "contenido2"))

; 3) CREATE
(define Gdocs031 ((login Gdocs011 "user3" "pass3" create) (fecha 12 12 1220) "primerTitulo" "contenido"))
(define Gdocs032 ((login Gdocs012 "user" "pass" create) (fecha 12 12 1212) "doc1" "contenido1"))
(define Gdocs033 ((login Gdocs032 "user" "pass" create) (fecha 12 12 1212) "doc2" "contenido2"))

; 3) SHARE
(define Gdocs041 ((login Gdocs023 "user1" "pass1" share) 0 (access "user3" #\r) (access "user2" #\c)))
(define Gdocs042 ((login Gdocs023 "user1" "pass1" share) 0 (access "user1" #\r) (access "user2" #\c)))
(define Gdocs043 ((login Gdocs023 "user1" "pass1" share) 1 (access "user3" #\r) (access "user3" #\w) (access "user2" #\c)))

; 4) 
(define lol1 (sacarUsuariosAccesos (list (access "user2" #\r) (access "user1" #\r))))
(define lol2 (sacarUsuariosRegistrados (getLista1 Gdocs011)))
(define lol3 (filtrarAccesos Gdocs011 (list (access "user2" #\r) (access "user1" #\r)) "user5"))

; 5) REVOKEALLACCESSES
(define Gdocs051 (login Gdocs041 "user1" "pass1" revokeAllAccesses))
(define Gdocs052 (login Gdocs042 "user1" "pass1" revokeAllAccesses))
(define Gdocs053 (login Gdocs043 "user0" "pass1" revokeAllAccesses))