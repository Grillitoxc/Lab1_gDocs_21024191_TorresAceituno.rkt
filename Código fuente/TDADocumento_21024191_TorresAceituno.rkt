#lang racket
;TDA Documento
(require "TDADate_21024191_TorresAceituno.rkt")
(require "TDAParadigmaDocs_21024191_TorresAceituno.rkt")

;---CONSTRUCTORES---
;descripción: Función que crea un documento con sus parámetros específicos
;dom: String X Fecha X String X String X Entero X Lista X Fecha
;rec: Lista Documento
(define (documento titulo fecha contenido autor idDoc accesos historial fechaCreacion)
  (if (and (string? titulo)
           (fecha? fecha)
           (string? autor)
           (string? contenido)
           )
      (list titulo fecha contenido autor idDoc accesos historial fechaCreacion)
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

;descripción: Función que selecciona el historial de versiones del documento
;dom: Lista Documento
;rec: Lista de historial
(define (getFechaCreacion documento)
  (eighth documento)
  )


;---MODIFICADORES---
;descripción: Función que cambia el titulo del documento
;dom: Lista documento X string
;rec: Lista documento
(define (setTitulo documento newTitulo)
  (if (string? newTitulo)
      (list newTitulo (getFechaD documento) (getContenido documento) (getAutor documento) (getidDoc documento) (getAccesos documento) (getHistorial documento) (getFechaCreacion documento))
      documento)
  )

;descripción: Función que cambia la fecha ingresada
;dom: Lista documento X fecha
;rec: Lista documento
(define (setFechaD documento newFecha)
  (if (fecha? newFecha)
      (list (getTitulo documento) fecha (getContenido documento) (getAutor documento) (getidDoc documento) (getAccesos documento) (getHistorial documento) (getFechaCreacion documento))
      documento)
  )

;descripción: Función que cambia el contenido de un documento
;dom: Lista documento X string
;rec: Lista documento
(define (setContenido documento newContenido)
  (list (getTitulo documento) (getFechaD documento) newContenido (getAutor documento) (getidDoc documento) (getAccesos documento) (getHistorial documento) (getFechaCreacion documento))
  )

;descripción: Función que modifica el autor del documento
;dom: Lista documento X string
;rec: Lista documento
(define (setAutor documento newAutor)
  (if (string? newAutor)
      (list (getTitulo documento) (getFechaD documento) (getContenido documento) newAutor (getidDoc documento) (getAccesos documento) (getHistorial documento) (getFechaCreacion documento))
      documento)
  )

;descripción: Función que modifica el número de id del documento
;dom: Lista documento X Entero
;rec: Lista documento
(define (setidDoc documento idDoc)
  (if (number? idDoc)
      (list (getTitulo documento) (getFechaD documento) (getContenido documento) (getAutor documento) idDoc (getAccesos documento) (getHistorial documento) (getFechaCreacion documento))
      documento)
  )
  
;descripción: Función que agrega una lista de accesos
;dom: Lista documento X Entero
;rec: Lista documento
(define (setAccesos documento accesos)
  (if (list? accesos)
      (list (getTitulo documento) (getFechaD documento) (getContenido documento) (getAutor documento) (getidDoc documento) (cons accesos (getAccesos documento)) (getHistorial documento) (getFechaCreacion documento))
      documento)
  )

;descripción: Función que implanta una lista de accesos vacía
;dom: Lista documento X Entero
;rec: Lista documento
(define (setAccesos_implante documento)
  (list-set documento 5 null)
  )

;descripción: Función que actualiza el historial de una documento (Agregando strings al final de su contenido)
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
            (cons historial (getHistorial documento))
            (getFechaCreacion documento))
      documento)
  )

;descripción: Función que actualiza el historial de una documento (cambiando el contenido del documento actual)
;dom: Lista documento X Lista historial X String X Fecha X Entero X paradigmadocs
;rec: Lista documento
(define (setHistorial_delete documento contenido historial date paradigmadocs)
  (if (list? historial)
      (list (getTitulo documento)
            date
            contenido
            (getAutor documento)
            (getidDoc documento)
            (getAccesos documento)
            (cons historial (getHistorial documento))
            (getFechaCreacion documento))
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
        (cons historial (reverse (quitarElementoLista (reverse (getHistorial documento)) idVersion)))
        (getFechaCreacion documento))
  )


;---OTRAS FUNCIONES---
;descripción: Función que verifica si un documento está en la lsita de documentos mediante un id
;dom: Lista documentos X Entero
;rec: Booleano
;recursión: de cola (elegida por su fácil implementación y no dejar estados pendientes)
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
;recursión: de cola (elegida por su fácil implementación y no dejar estados pendientes)
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
;recursión: de cola (elegida por su fácil implementación y no dejar estados pendientes)
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
  (list (getTitulo documento) (getFechaD documento) (getContenido documento) (getAutor documento) (getidDoc documento) (car (getAccesos documento)) (getHistorial documento) (getFechaCreacion documento))
  )

;descripción: Función que verifica que el usuario que intenta hacer una acción, sea el propietario de un documento seleccionado
;dom: Lista documentos X String X Entero
;rec: Booleano
;recursión: de cola (elegida por su fácil implementación y no dejar estados pendientes)
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
;recursión: de cola (elegida por su fácil implementación y no dejar estados pendientes)
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
;recursión: de cola (elegida por su fácil implementación y no dejar estados pendientes)
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

;descripción: Función que revisa que el usuario ingresado sea el propietario en una lista de documentos
;dom: Lista Documentos X String
;rec: Lista Documentos
(define (buscarDocsPropietario listaDocs user)
  (filter (lambda (doc)
            (eq? (getAutor doc) user)) listaDocs)
  )

;descripción: Función que fuerza a entregar una lista como una string independiente del contenido
;dom: Lista
;rec: String
(define (list->string_force lista)
  (string-join (map ~a lista) " ")
  )

;descripción: Función que transforma a string la lsita de accesos de un documento
;dom: Lista Usuario
;rec: String
(define (modoDeAcceso user)
  (cond
    [(eq? (second user) #\r) (string-append "\n   ● El usuario: " (car user) " posee acceso de lectura")]
    [(eq? (second user) #\w) (string-append "\n   ● El usuario: " (car user) " posee acceso de escritura")]
    [(eq? (second user) #\c) (string-append "\n   ● El usuario: " (car user) " posee acceso de comentario")]
    [else user]
    )
  )

;descripción: Función que transfome toda una lista de accesos de un documento a una string
;dom: Lista Accesos
;rec: String
(define (actualizarModoDeAcceso listaUsers)
  (if (empty? listaUsers)
      "No hay accesos concedidos aún"
      (list->string_force (map modoDeAcceso listaUsers))
      )
  )

;descripción: Función que agrupa el formato del historial de un documento en una string con la fecha de modificación y su contenido
;dom: Lista Documento X Paradigmadocs
;rec: String
(define (formatoDoc_historial doc paradigmadocs)
  (string-append "\n   Fecha de modificación: " (fecha->string (getFechaD doc))
                 "\n     Contenido: " ((getDecrypt paradigmadocs) (getContenido doc))                                  
                 )
  )

;descripción: Función que da el formato necesario a todos los documentos de la plataforma
;dom: Lista Documentos X Paradigmadocs 
;rec: String
(define (darFormatoDocs_historial listaDocs paradigmadocs)
  (if (empty? listaDocs)
      "No se ha hecho ninguna modificación aún"
      (list->string_force (map (curryr formatoDoc_historial paradigmadocs) (reverse listaDocs)))
      )
  )

;descripción: Función que le da el formato completo a un documento 
;dom: Lista Documento X Paradigmadocs
;rec: String
(define (formatoDoc doc paradigmadocs)
  (string-append "\n  ✦ Número documento: " (number->string (getidDoc doc))
                 "\n  Título: " (getTitulo doc)
                 "\n  Fecha de creación: " (fecha->string (getFechaCreacion doc))
                 "\n  Autor: " (getAutor doc)
                 "\n  Última actualización de contenido: " (fecha->string (getFechaD doc))
                 "\n  Contenido actual: " ((getDecrypt paradigmadocs) (getContenido doc))
                 "\n  Accesos: " (actualizarModoDeAcceso (getAccesos doc))
                 "\n  Versiones anteriores (ennumeradas desde 0 a N): " (darFormatoDocs_historial (getHistorial doc) paradigmadocs)
                 "\n")
  )

;descripción: Función que da el formato a todos los documentos de una plataforma
;dom: Lista Documentos x Paradigmadocs
;rec: String
(define (darFormatoDocs listaDocs paradigmadocs)
  (list->string_force (map (curryr formatoDoc paradigmadocs) (reverse listaDocs)))
  )

;descripción: Función que da el formato a la lista de accesos de los documentos
;dom: Lista Documentos X Paradigmadocs
;rec: String
(define (darFormatoDocs_login_access listaDocs paradigmadocs)
  (if (empty? listaDocs)
      "No hay documentos los cuales este usuario tenga accesos"
      (list->string_force (map (curryr formatoDoc paradigmadocs) (reverse listaDocs)))
      )
  )

;descripción: Función que da el formato a los documentos donde el usuario el propietario
;dom: Lista Documentos X Paradigmadocs
;rec: String
(define (darFormatoDocs_login_propiedad listaDocs paradigmadocs)
  (if (empty? listaDocs)
      "No hay documentos en los que este usuario sea propietario"
      (list->string_force (map (curryr formatoDoc paradigmadocs) (reverse listaDocs)))
      )
  )


;To import
(provide (all-defined-out))
