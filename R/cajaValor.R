# MODULO CAJA VALOR ----

#' UI del módulo CajaValor
#' @param id ID del módulo Shiny.
#' @param icono Nombre del icono FontAwesome.
#' @param texto Etiqueta descriptiva.
#' @param mostrar_boton Mostrar el botón de detalle.
#' @return Contenedor con outputs y botón namespaced.
#' @param color_borde Color CSS opcional para dibujar borde; NULL conserva la caja anterior.
#' @param color_icono Color CSS opcional del icono; NULL conserva el verde anterior.
#' @param detalle Texto o contenido Shiny opcional debajo del valor.
#' @details Implementación trasladada desde racafeShiny sin cambiar su HTML ni sus argumentos.
#' @examples
#' cajaValor_ui(shiny::NS("resumen")("ventas"), texto = "Ventas")
#' @export
cajaValor_ui <- function(id, icono = "chart-line", texto = "",
                         mostrar_boton = TRUE, color_borde = NULL, color_icono = NULL, detalle = NULL) {
  for (color in list(color_borde, color_icono)) {
    if (!is.null(color) && (!is.character(color) || length(color) != 1L || is.na(color) ||
        !nzchar(color) || grepl("[;{}<>]", color))) stop("Color CSS inv\u00e1lido.", call. = FALSE)
  }
  ns <- shiny::NS(id)

  shiny::div(
    class = "racafe-caja-valor",
    style = paste(
      "background:white;border-radius:8px;padding:16px;",
      "box-shadow:0 1px 4px rgba(0,0,0,0.08);",
      "display:flex;flex-direction:column;gap:8px;",
      if (!is.null(color_borde)) paste0("border:1px solid ", color_borde, ";")
    ),
    shiny::div(
      style = "display:flex;align-items:center;justify-content:space-between;",
      shiny::div(
        style = "display:flex;align-items:center;gap:10px;",
        shiny::div(
          style = paste0("color:", if (is.null(color_icono)) "#28B78D" else color_icono, ";font-size:1.6em;"),
          shiny::icon(icono)
        ),
        shiny::div(
          style = "font-size:0.85em;color:#666;font-weight:500;",
          texto
        )
      ),
      if (mostrar_boton) {
        shiny::actionButton(
          inputId = ns("detalle"),
          label   = NULL,
          icon    = shiny::icon("magnifying-glass"),
          class   = "btn btn-xs btn-outline-secondary",
          style   = "padding:2px 6px;border-radius:4px;"
        )
      }
    ),
    shiny::div(
      style = "font-size:1.5em;font-weight:700;",
      shiny::uiOutput(ns("valor_display"))
    ),
    if (!is.null(detalle)) shiny::tags$div(style = "font-size:11px;color:#667085;", detalle)
  )
}

#' Servidor del módulo CajaValor
#' @param id ID local del módulo.
#' @param valor_r Reactivo que devuelve el valor numérico.
#' @param formato Formato registrado en racafeShiny::DefinirFormato.
#' @param meta Meta numérica opcional; NA desactiva el color condicional.
#' @param empty Texto opcional para valor NULL/NA; NULL conserva la suspensión del output anterior.
#' @return Lista con click_detalle, reactivo del botón de detalle.
#' @details Conserva valores, formatos y eventos del módulo anterior de racafeShiny.
#' @examples
#' \dontrun{
#' ui <- shiny::fluidPage(cajaValor_ui("ventas", texto = "Ventas"))
#' server <- function(input, output, session) {
#'   ventas <- cajaValor_server("ventas", shiny::reactive(1234.56), "numero")
#'   shiny::observeEvent(ventas$click_detalle(), shiny::showNotification("Detalle"))
#' }
#' shiny::shinyApp(ui, server)
#' }
#' @export
cajaValor_server <- function(id, valor_r, formato, meta = NA, empty = NULL) {
  if (!is.null(empty) && (!is.character(empty) || length(empty) != 1L || is.na(empty))) {
    stop("empty debe ser un texto escalar.", call. = FALSE)
  }
  shiny::moduleServer(id, function(input, output, session) {

    output$valor_display <- shiny::renderUI({
      value <- valor_r()
      if (!is.null(empty) && (!length(value) || (length(value) == 1L && is.na(value)))) {
        return(shiny::tags$span(as.vector(empty, mode = "character")))
      }
      shiny::req(value)
      racafeShiny::FormatearNumero(value, formato, meta = meta)
    })

    list(
      click_detalle = shiny::reactive(input$detalle)
    )
  })
}
