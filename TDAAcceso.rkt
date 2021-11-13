#lang racket
(require "TDAParadigmadocs.rkt")
(require "TDAUser.rkt")
(require "TDADocumento.rkt")

;TDA Acceso

;Representación: (string X caracter)
;(usuario x tipo de acceso)

;---CONSTRUCTORES---
;descripción: Función que crea una lista con el usuario y su acceso correspondiente 
;dom: String X Caracter
;rec: Lista de acceso
(define (access user modo)
  (list user modo)
  )


;---PERTINENCIA---
;descripción: Función que verifica el formato del acceso
;dom: Lista
;rec: Booleano
(define (access? access)
  (list? access)
  )


;---SELECTORES---
;descripción: Función que selecciona el usuario 
;dom: Lista Acceso
;rec: String
(define (getUser access)
  (first access)
  )

;descripción: Función que selecciona el tipo de acceso del usuario
;dom: Lista Acceso
;rec: Caracter
(define (getModo access)
  (second access)
  )


;---MODIFICADORES---
;descripción: Función que cambia el usuario del acceso
;dom: Lista Acceso X string
;rec: Lista Acceso
(define (setUser access newUserA)
  (if (access? access)
      (list newUserA (getModo access))
      access)
  )

;descripción: Función que cambia el usuario del acceso
;dom: Lista Acceso X string
;rec: Lista Acceso
(define (setModo access newModoA)
  (if (access? access)
      (list (getUser access) newModoA)
      access)
  )


;---OTRAS FUNCIONES---
;descripción: Función une los accesos en una sola lista
;dom: Lista Acceso X Lista Accesos
;rec: Lista Acceso
(define (unirAccess access1 access2)
  (cons access1 access2)
  )

;descripción: Función que retorna el usuario de un acceso
;dom: Lista Acceso
;rec: String
(define (sacarUsuariosAccesos listaAccesos)
  (map getUser listaAccesos)
  )

;descripción: Función que saca el nombre de los usuarios registrados
;dom: Lista registrados
;rec: Lista nombres
(define (sacarUsuariosRegistrados listaRegistrados)
  (map getNombre listaRegistrados)
  )

;descripción: Función que se encarga de verificar si un usuario se repite
;dom: Lista usuarios X String
;rec: Boleano
(define (repiteUsuario lista elemento)
  (if (empty? lista)
      #f
      (if (eq? (car lista) elemento)
          #t
          (repiteUsuario (cdr lista) elemento)
          )
      )
  )

;descripción: Función que filtra usuarios de acuerdo a dos condiciones
;             1) Que el autor no se de permisos a si mismo
;             2) Que el usuario al que se le está dando acceso esté dentro de la lista de registrados
;dom: Lista Accesos X String
;rec: Lista Accesos
(define (filtrarAccesos paradigmadocs listaAccesos autor)
  (filter (lambda (usuarioAcceso)
            (and (not (eq? (getUser usuarioAcceso) autor))
                 (repiteUsuario (sacarUsuariosRegistrados (getLista1 paradigmadocs))(getUser usuarioAcceso))))listaAccesos)
  )



(define (userEnAcceso? user listaAccesos)
  (if (empty? listaAccesos)
      #f
      (if (eq? user (car listaAccesos))
          #t
          (userEnAcceso? user (cdr listaAccesos))
          )
      )
  )

(define (quitarComentarios listaAccesos)
  (filter (lambda (acceso)
            (not (eq? (getModo acceso) #\c))) listaAccesos)
  )

(define (buscarDocsAccesos listaDocs user)
  (filter (lambda (doc)
            (userEnAcceso? user (map getUser (quitarComentarios (getAccesos doc))))) listaDocs)
  )

;To import
(provide (all-defined-out))
