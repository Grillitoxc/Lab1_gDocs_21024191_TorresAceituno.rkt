#lang racket
(require "TDAParadigmaDocs.rkt")
(require "TDAUser.rkt")
(require "TDADate.rkt")

;descripción: Función que registra a un usuario verificando que este sea único
;dom: Paradigmadocs X date X string X string
;rec: ParadigmaDocs
;recursión: Natural
(define (register paradigmaDocs date username password)
  (define (registrado? listaUsuarios usuario_1)
    (if (empty? listaUsuarios)
        #f
        (if (eq? (getNombre (car listaUsuarios)) (getNombre usuario_1))
            #t
            (registrado? (cdr listaUsuarios) usuario_1)))
    )
  (if (registrado? (getLista1 paradigmaDocs) (usuario username password date))
      paradigmaDocs
      (setLista1 paradigmaDocs (cons (usuario username password date)(getLista1 paradigmaDocs))))
  )

(define Gdocs (paradigmaDocs "gDocs" (fecha 25 10 2021) encryptFn encryptFn))
(define emptyGDocs (paradigmaDocs "gDocs" (fecha 25 10 2021) encryptFn encryptFn))
(define gDocs1 (register (register (register emptyGDocs (fecha 25 10 2021) "user2" "pass1") (fecha 25 10 2021) "user2" "pass2") (fecha 25 10 2021) "user3" "pass3"))
