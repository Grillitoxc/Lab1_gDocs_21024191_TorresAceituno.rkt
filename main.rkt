#lang racket
(require "TDAParadigmadocs.rkt")
(require "TDAUser.rkt")
(require "TDADate.rkt")

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


;descripción: Función que autentica a un usuario registrado y le permite ejecutar un comando específico.
;dom: paradigmadocs X string X string X function 
;rec: Paradigmadocs
;recursión: 
;(define (login paradigmadocs username pasword operation)
  






















;---EJEMPLOS DE CADA FUNCIÓN---
; GENERANDO PARADIGMADOCS
(define Gdocs000 (paradigmadocs "Gdocs" (fecha 25 10 2021) encryptFn encryptFn))

; 1) REGISTER 
(define Gdocs011 (register (register (register Gdocs000 (fecha 25 10 2021) "user2" "pass1") (fecha 25 10 2021) "user2" "pass2") (fecha 25 10 2021) "user3" "pass3"))
(define Gdocs012 (register Gdocs000 (fecha 25 3 2020) "user" "pass"))
(define Gdocs013 (register (register Gdocs000 (fecha 25 3 2020) "user" "pass") (fecha 25 3 2020) "user" "pass2"))