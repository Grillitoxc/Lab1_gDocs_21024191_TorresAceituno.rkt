#lang racket
;TDA Usuario
(require "TDADate.rkt")

;Representación: (string X entero X lista)
;(list nombreUsuario contrasenna fecha)

;---CONSTRUCTORES---
;descripción: Función que crea una lista con información del usuario
;dom: String X string X fecha
;rec: Lista
(define (usuario nombreUsuario contrasenna fecha)
  (if (and (string? nombreUsuario)
           (string? contrasenna)
           (fecha? fecha))
      (list nombreUsuario contrasenna fecha)
      null)
  )


;---PERTINENCIA---
;descripción: Función que rectifica que la lista de información sea correcta
;dom: Lista
;rec: Booleano
(define (usuario? usuario)
  (and (list? usuario)
       (= (length usuario) 3))
  )


;---SELECTORES---
;descripción: Función que selecciona el nombre de un usuario
;dom: Lista
;rec: String
(define (getNombre usuario)
  (if (usuario? usuario)
      (car usuario)
      null)
  )

;descripción: Función que selecciona la contraseña de un usuario
;dom: Lista
;rec: String
(define (getContrasenna usuario)
  (if (usuario? usuario)
      (cadr usuario)
      null)
  )

;descripción: Función que selecciona la contraseña de un usuario
;dom: Lista
;rec: Lista
(define (getFecha usuario)
  (if (usuario? usuario)
      (caddr usuario)
      null)
  )


;---MODIFICADORES---
;descripción: Función que modifica el nombre de un usuario ya hecho
;dom: Lista X string
;rec: Lista
(define (setNewNombre infoUsuario newNombre)
  (if (and (usuario? infoUsuario)
           (string? newNombre))
      (usuario newNombre (getContrasenna infoUsuario) (getFecha infoUsuario))
      infoUsuario)
  )

;descripción: Función que modifica la contrasenna de un usuario ya hecho
;dom: Lista X string
;rec: Lista
(define (setNewContrasenna infoUsuario newContrasenna)
  (if (and (usuario? infoUsuario)
           (string? newContrasenna))
      (usuario (getNombre infoUsuario) newContrasenna (getFecha infoUsuario))
      infoUsuario)
  )


;To import
(provide (all-defined-out))
;---EJEMPLOS DE CADA FUNCIÓN---
;(usuario "Grillitoxc" "123" (fecha 12 12 2021))