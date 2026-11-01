# Tarjetas compactas con evento desacoplado ----
.tarjeta_dependency <- function() {
  htmltools::htmlDependency("racafe-modulos-ui", "0.1.0", src = c(file = system.file("ui", package = "racafeModulos")),
                            stylesheet = "tarjeta.css")
}
.tarjeta_val <- function(value) if (shiny::is.reactive(value)) value() else value
.tarjeta_text <- function(value, name, empty = FALSE) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || (!empty && !nzchar(value))) {
    stop(name, " debe ser texto escalar.", call. = FALSE)
  }
  as.vector(value, mode = "character")
}
.tarjeta_value <- function(value, formato) {
  if (length(value) != 1L || !is.atomic(value)) stop("valor debe ser escalar.", call. = FALSE)
  if (is.na(value)) return("—")
  if (is.numeric(value)) {
    if (is.null(formato)) {
      return(format(value, digits = 15, big.mark = ",", decimal.mark = ".", scientific = FALSE, trim = TRUE))
    }
    fn <- if (is.function(formato)) formato else racafeShiny::ObtenerFormato(formato)
    return(.tarjeta_text(as.character(fn(value)), "formato", empty = TRUE))
  }
  .tarjeta_text(as.character(value), "valor", empty = TRUE)
}

#' UI de tarjeta corporativa compacta
#' @param id ID del módulo Tarjeta; dentro de un padre use ns(id).
#' @return Contenedor con contenido reactivo y botón independiente, de altura por contenido.
#' @details El botón se renderiza independientemente del valor para conservar su binding
#'   y contador al actualizar los indicadores. El CSS pertenece al paquete.
#' @examples
#' TarjetaUI("pedidos")
#' @export
TarjetaUI <- function(id) {
  id <- .tarjeta_text(id, "id")
  ns <- shiny::NS(id)
  htmltools::attachDependencies(shiny::tags$section(id = ns("tarjeta"), class = "rcm-tarjeta",
    `aria-labelledby` = ns("titulo"),
    shiny::uiOutput(ns("contenido"), class = "rcm-tarjeta-content"),
    shiny::uiOutput(ns("boton"), class = "rcm-tarjeta-actions")), .tarjeta_dependency())
}

#' Módulo de indicador compacto con evento de acción
#' @param id ID usado por TarjetaUI, local al módulo padre.
#' @param titulo,valor,descripcion Escalares o expresiones reactive. descripcion NULL omite el texto secundario.
#' @param boton Incluir el botón de detalle; por defecto FALSE. No hace clic en toda la tarjeta.
#' @param icono_boton Nombre Font Awesome del botón.
#' @param formato NULL conserva precisión con separadores corporativos; nombre de un
#'   formato racafeShiny o función de formato, aplicado solo a valores numéricos.
#' @param icono Nombre Font Awesome opcional junto al título.
#' @param etiqueta_boton Texto accesible del botón; visible al enfocarlo como tooltip nativo.
#' @return Lista con accion (reactivo NULL hasta el primer clic, luego contador) y valor
#'   (reactivo con el dato original, sin convertirlo a texto).
#' @details Sigue la arquitectura UI/moduleServer de CajaModal, sin abrir un modal
#'   automáticamente ni depender de CRM. La aplicación decide qué hacer al observar accion().
#'   Los observers se registran una sola vez; actualizar titulo/valor/descripcion no recrea el botón.
#'   CajaModal y sus APIs anteriores no se modifican.
#' @examples
#' \dontrun{
#' tarjeta <- Tarjeta("pedidos", "Pedidos", shiny::reactive(2), "Creación: 20/10/2026", boton = TRUE)
#' shiny::observeEvent(tarjeta$accion(), { shiny::showModal(shiny::modalDialog("Detalle")) })
#' }
#' @export
Tarjeta <- function(id, titulo, valor, descripcion = NULL, boton = FALSE, icono_boton = "external-link-alt",
                    formato = NULL, icono = NULL, etiqueta_boton = "Ver detalle") {
  if (!is.logical(boton) || length(boton) != 1L || is.na(boton)) stop("boton debe ser lógico.", call. = FALSE)
  icono_boton <- .tarjeta_text(icono_boton, "icono_boton")
  etiqueta_boton <- .tarjeta_text(etiqueta_boton, "etiqueta_boton")
  if (!is.null(icono)) icono <- .tarjeta_text(icono, "icono")
  if (!is.null(formato) && !is.function(formato)) formato <- .tarjeta_text(formato, "formato")
  shiny::moduleServer(id, function(input, output, session) {
    accion <- shiny::reactiveVal(NULL)
    original <- shiny::reactive(.tarjeta_val(valor))
    output$contenido <- shiny::renderUI({
      title <- .tarjeta_text(.tarjeta_val(titulo), "titulo")
      description <- .tarjeta_val(descripcion)
      shiny::tagList(
        shiny::tags$div(id = session$ns("titulo"), class = "rcm-tarjeta-title",
        if (!is.null(icono)) shiny::icon(icono), title),
        shiny::tags$strong(class = "rcm-tarjeta-value", .tarjeta_value(original(), formato)),
        if (!is.null(description)) shiny::tags$div(class = "rcm-tarjeta-description",
          .tarjeta_text(description, "descripcion", empty = TRUE)))
    })
    output$boton <- shiny::renderUI({
      if (!boton) return(NULL)
      shiny::tagAppendAttributes(shiny::actionButton(session$ns("accion"), label = NULL,
        icon = shiny::icon(icono_boton), class = "rcm-tarjeta-action", title = etiqueta_boton),
        `aria-label` = etiqueta_boton, `aria-describedby` = session$ns("titulo"))
    })
    shiny::observeEvent(input$accion, {
      shiny::req(boton, is.numeric(input$accion), input$accion > 0)
      previous <- accion()
      accion(if (is.null(previous)) 1L else previous + 1L)
    }, ignoreInit = TRUE)
    list(accion = shiny::reactive(accion()), valor = original)
  })
}
