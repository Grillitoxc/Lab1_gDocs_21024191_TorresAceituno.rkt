#lang racket
;TDA Documento
(require "TDADate.rkt")
(require "TDAParadigmadocs.rkt")

;---CONSTRUCTORES---
;descripción: Función que crea un documento con sus parámetros específicos
;dom: String X fecha X String X String X Entero X Lista
;rec: Lista Documento
(define (documento titulo fecha contenido autor idDoc accesos historial)
  (if (and (string? titulo)
           (fecha? fecha)
           (string? autor)
           (string? contenido)
           )
      (list titulo fecha contenido autor idDoc accesos historial)
      null)
  )


;---PERTINENCIA---
;descripción: Función que verifica el formato de documento
;dom: Lista
;rec: Booleano
(define (documento? documento)
  (list? documento)
  )


;---SELECTORES---
;descripción: Función para seleccionar el título de un documento
;dom: Lista Documento
;rec: String
(define (getTitulo documento)
  (first documento)
  )

;descripción: Función que selecciona la fecha de creación del documento
;dom: Lista Documento
;rec: Lista fecha
(define (getFechaD documento)
  (second documento)
  )

;descripción: Función que selecciona el contenido del documento
;dom: Lista Documento
;rec: Contenido del documento
(define (getContenido documento)
  (third documento)
  )

;descripción: Función que selecciona el autor de un documento
;dom: Lista Documento
;rec: String
(define (getAutor documento)
  (fourth documento)
  )

;descripción: Función que selecciona el id del documento
;dom: Lista Documento
;rec: Entero
(define (getidDoc documento)
  (fifth documento)
  )

;descripción: Función que selecciona la lista de accesos del documento
;dom: Lista Documento
;rec: Lista de accesos
(define (getAccesos documento)
  (sixth documento)
  )

;descripción: Función que selecciona el historial de versiones del documento
;dom: Lista Documento
;rec: Lista de historial
(define (getHistorial documento)
  (seventh documento)
  )


;---MODIFICADORES---
;descripción: Función que cambia el titulo del documento
;dom: Lista documento X string
;rec: Lista documento
(define (setTitulo documento newTitulo)
  (if (string? newTitulo)
      (list newTitulo (getFechaD documento) (getContenido documento) (getAutor documento) (getidDoc documento) (getAccesos documento) (getHistorial documento))
      documento)
  )

;descripción: Función que cambia la fecha ingresada
;dom: Lista documento X fecha
;rec: Lista documento
(define (setFechaD documento newFecha)
  (if (fecha? newFecha)
      (list (getTitulo documento) fecha (getContenido documento) (getAutor documento) (getidDoc documento) (getAccesos documento) (getHistorial documento))
      documento)
  )

;descripción: Función que cambia el contenido de un documento
;dom: Lista documento X string
;rec: Lista documento
(define (setContenido documento newContenido)
  (list (getTitulo documento) (getFechaD documento) newContenido (getAutor documento) (getidDoc documento) (getAccesos documento) (getHistorial documento))
  )

;descripción: Función que modifica el autor del documento
;dom: Lista documento X string
;rec: Lista documento
(define (setAutor documento newAutor)
  (if (string? newAutor)
      (list (getTitulo documento) (getFechaD documento) (getContenido documento) newAutor (getidDoc documento) (getAccesos documento) (getHistorial documento))
      documento)
  )

;descripción: Función que modifica el número de id del documento
;dom: Lista documento X Entero
;rec: Lista documento
(define (setidDoc documento idDoc)
  (if (number? idDoc)
      (list (getTitulo documento) (getFechaD documento) (getContenido documento) (getAutor documento) idDoc (getAccesos documento) (getHistorial documento))
      documento)
  )
  
;descripción: Función que agrega una lista de accesos
;dom: Lista documento X Entero
;rec: Lista documento
(define (setAccesos documento accesos)
  (if (list? accesos)
      (list (getTitulo documento) (getFechaD documento) (getContenido documento) (getAutor documento) (getidDoc documento) (cons accesos (getAccesos documento)) (getHistorial documento))
      documento)
  )

;descripción: Función que implanta una lista de accesos vacía
;dom: Lista documento X Entero
;rec: Lista documento
(define (setAccesos_implante documento)
  (list-set documento 5 null)
  )

;descripción: Función que agrega una lista de acceso
;dom: Lista documento X Lista historial X String X Fecha X Entero X paradigmadocs
;rec: Lista documento
(define (setHistorial documento historial contenidoTexto date idDoc paradigmadocs)
  (if (list? historial)
      (list (getTitulo documento)
            date
            ((getEncrypt paradigmadocs) (string-append ((getDecrypt paradigmadocs) (getContenido (seleccionarDoc (getLista3 paradigmadocs) idDoc))) contenidoTexto))
            (getAutor documento)
            (getidDoc documento)
            (getAccesos documento)
            (cons historial (getHistorial documento)))
      documento)
  )

;descripción: Función que restaura un documento y el actual pasa a ser parte del historial
;dom: Lista documento X Lista Historial X Lista Documento X Entero
;rec: Lista documento
(define (setHistorial_restauracion documento historial documentoRestaurado idVersion)
  (list (getTitulo documentoRestaurado)
        (getFechaD documentoRestaurado)
        (getContenido documentoRestaurado)
        (getAutor documentoRestaurado)
        (getidDoc documentoRestaurado)
        (getAccesos documento)
        (cons historial (reverse (quitarElementoLista (reverse (getHistorial documento)) idVersion))))
  )


;---OTRAS FUNCIONES---
;descripción: Función que verifica si un documento está en la lsita de documentos mediante un id
;dom: Lista documentos X Entero
;rec: Booleano
;recursión: de cola
(define (buscarDoc listaDocs idDoc)
  (if (empty? listaDocs)
      #f
      (if (eq? (getidDoc (car listaDocs)) idDoc)
          #t
          (buscarDoc (cdr listaDocs) idDoc)
          )
      )
  )

;descripción: Función que busca el autor del documento por su id
;dom: Lista documentos X Entero
;rec: String
;recursión: de cola
(define (buscarUserDoc listaDocs idDoc)
  (if (empty? listaDocs)
      #f
      (if (eq? (getidDoc (car listaDocs)) idDoc)
          (getAutor (car listaDocs))
          (buscarUserDoc (cdr listaDocs) idDoc)
          )
      )
  )

;descripción: Función que retorna un documento específico por su id
;dom: Lista documentos X Entero
;rec: Lsita documento
;recursión: de cola
(define (seleccionarDoc listaDocs idDoc)
  (if (empty? listaDocs)
      #f
      (if (eq? (getidDoc (car listaDocs)) idDoc)
          (car listaDocs)
          (seleccionarDoc (cdr listaDocs) idDoc)
          )
      )
  )

;descripción: Función que limpia la lista de accesos de un documento de paradigmadocs (formato)
;dom: Lista documento
;rec: Lista documento
(define (limpiarListAccesos documento)
  (list (getTitulo documento) (getFechaD documento) (getContenido documento) (getAutor documento) (getidDoc documento) (car (getAccesos documento)) (getHistorial documento))
  )

;descripción: Función que verifica que el usuario que intenta hacer una acción, sea el propietario de un documento seleccionado
;dom: Lista documentos X String X Entero
;rec: Booleano
;recursión: de cola
(define (esPropietario? listaDocs autor idDoc)
  (if (empty? listaDocs)
      #f
      (if (buscarDoc listaDocs idDoc)
          (eq? (getAutor (seleccionarDoc listaDocs idDoc)) autor)
          #f
          )
      )
  )

;descripción: Función que verifica que un usuario exista
;dom: Lista Accesos x String
;rec: Boleano
;recursión: de cola
(define (estaElUser? listaAccesos usuario)
  (if (empty? listaAccesos)
      #f
      (if (eq? (car (car listaAccesos)) usuario)
          #t
          (estaElUser? (cdr listaAccesos) usuario)
          )
      )
  )   

;descripción: Función que verifica que el usuario ingresado tenga el permiso correspondiente a escritura
;dom: Lista Accesos x String
;rec: Booleano
;recursión: de cola
(define (filtrarUsuariosEscritura listaAccesos usuario)
  (if (empty? documento)
      #f
      (if (estaElUser? listaAccesos usuario)
          (if (eq? (first (car listaAccesos)) usuario)
              (if (eq? (second (car listaAccesos)) #\w)
                  #t
                  (filtrarUsuariosEscritura (cdr listaAccesos) usuario))
              (filtrarUsuariosEscritura (cdr listaAccesos) usuario)
              )
          #f
          )
      )
  )

;descripción: Función que elimina el índice de una lista
;dom: Lista documentos X Entero
;rec: Lista
(define (quitarElementoLista lista indice)
  (for/list ([i (length lista)]
             [elem lista]
             #:when (not (= i indice)))
    elem))

;descripción: 
;dom: 
;rec: 
(define (buscarDocsPropietario listaDocs user)
  (filter (lambda (doc)
            (eq? (getAutor doc) user)) listaDocs)
  )


;To import
(provide (all-defined-out))
