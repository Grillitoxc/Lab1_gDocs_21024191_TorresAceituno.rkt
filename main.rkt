#lang racket
(require "TDAParadigmadocs.rkt")
(require "TDAUser.rkt")
(require "TDADate.rkt")
(require "TDADocumento.rkt")

;descripción: Función que registra a un usuario verificando que este sea único.
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


;descripción: Función que autentica a un usuario registrado y le permite ejecutar un comando específico.
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
        [else paradigmadocs]
        )
      (cond
        [(eq? operation create)(operation paradigmadocs)]
        [else paradigmadocs]
        )
      )
  )


;descripción: Función que crea un documento con los datos entregados.Tabién e encarga de encriptar el contenido de este.
;dom: paradigmadocs X date X String (titulo documento) X String  (contenido) 
;rec: Paradigmadocs
(define (create paradigmadocs)
  (lambda(date nombre contenido)
    (if (null? (getLista2 paradigmadocs))
        paradigmadocs
        (setLista3 (setLista2 paradigmadocs null) (documento nombre date (encryptFn contenido) (car(getLista2 paradigmadocs))))))
  )
           

;---EJEMPLOS DE CADA FUNCIÓN---
; GENERANDO PARADIGMADOCS
(define Gdocs000 (paradigmadocs "Gdocs" (fecha 25 10 2021) encryptFn encryptFn))

; 1) REGISTER 
(define Gdocs011 (register (register (register Gdocs000 (fecha 25 10 2021) "user1" "pass1") (fecha 25 10 2021) "user2" "pass2") (fecha 25 10 2021) "user3" "pass3"))
(define Gdocs012 (register Gdocs000 (fecha 25 3 2020) "user" "pass"))
(define Gdocs013 (register (register Gdocs000 (fecha 25 3 2020) "user" "pass") (fecha 25 3 2020) "user" "pass2"))

; 2) LOGIN
(define Gdocs021 (login Gdocs011 "user3" "pass3" create))
(define Gdocs022 ((login Gdocs012 "user" "pass" create) (fecha 12 12 1212) "doc1" "contenido1"))
(define Gdocs023 ((login Gdocs012 "user1" "pass1" create)(fecha 12 12 1212) "doc1" "contenido1"))

; 3) CREATE
(define Gdocs031 ((login Gdocs011 "user3" "pass3" create) (fecha 12 12 1220) "primerTitulo" "contenido"))
(define Gdocs032 ((login Gdocs012 "user" "pass" create) (fecha 12 12 1212) "doc1" "contenido1"))
(define Gdocs033 ((login Gdocs012 "user1" "pass1" create)(fecha 12 12 1212) "doc1" "contenido1"))
