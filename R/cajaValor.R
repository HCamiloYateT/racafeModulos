# MODULO CAJA VALOR ----

#' UI del módulo CajaValor
#' @param id ID del módulo Shiny.
#' @param icono Nombre del icono FontAwesome.
#' @param texto Etiqueta descriptiva.
#' @param mostrar_boton Mostrar el botón de detalle.
#' @return Contenedor con outputs y botón namespaced.
#' @param color_borde Color CSS opcional del borde lateral; NULL utiliza color.
#' @param color_icono Color CSS opcional del icono; NULL utiliza color.
#' @param detalle Texto o contenido Shiny opcional debajo del valor.
#' @param compact Reducir padding, separación vertical e icono; FALSE utiliza la densidad normal.
#' @param color Color principal para borde lateral, título, icono, KPI y footer.
#' @param color_kpi,color_footer Colores opcionales por zona; NULL utiliza color.
#' @param footer Texto o contenido Shiny opcional del pie.
#' @param label_boton,icono_boton Etiqueta e icono del botón situado abajo a la derecha.
#' @param mostrar_kpi Mostrar el valor reactivo; FALSE permite tarjetas informativas sin servidor.
#' @details Conserva los argumentos posicionales e IDs del módulo trasladado desde racafeShiny.
#'   Desde 0.5.0 usa borde lateral y botón inferior opcional, manteniendo IDs y click_detalle.
#'   Los colores específicos prevalecen sobre color. La aplicación decide qué hacer con el evento.
#' @examples
#' cajaValor_ui(shiny::NS("resumen")("ventas"), texto = "Ventas")
#' @export
cajaValor_ui <- function(id, icono = "chart-line", texto = "",
                         mostrar_boton = TRUE, color_borde = NULL, color_icono = NULL,
                         detalle = NULL, compact = FALSE, color = "#DA291C", color_kpi = NULL,
                         color_footer = NULL, footer = NULL, label_boton = "Ver detalle",
                         icono_boton = "arrow-right", mostrar_kpi = TRUE) {
  for (flag in list(compact, mostrar_boton, mostrar_kpi)) {
    if (!is.logical(flag) || length(flag) != 1L || is.na(flag)) stop("Estado l\u00f3gico inv\u00e1lido.", call. = FALSE)
  }
  color <- .cv_color(color)
  color_borde <- if (is.null(color_borde)) color else .cv_color(color_borde)
  color_icono <- if (is.null(color_icono)) color else .cv_color(color_icono)
  color_kpi <- if (is.null(color_kpi)) color else .cv_color(color_kpi)
  color_footer <- if (is.null(color_footer)) color else .cv_color(color_footer)
  ns <- shiny::NS(id)
  card <- shiny::tags$div(class = paste("racafe-caja-valor rmcv-card", if (compact) "rmcv-card--compact"),
    style = paste0("--rmcv-color:", color, ";--rmcv-border:", color_borde, ";--rmcv-icon:", color_icono,
      ";--rmcv-kpi:", color_kpi, ";--rmcv-footer:", color_footer, ";"),
    shiny::tags$div(class = "rmcv-heading",
      shiny::tags$div(class = "rmcv-heading-copy",
        shiny::tags$div(class = "rmcv-icon", shiny::icon(icono)),
        shiny::tags$div(class = "rmcv-title", texto))),
    if (mostrar_kpi) shiny::tags$div(class = "rmcv-value", shiny::uiOutput(ns("valor_display"))),
    if (!is.null(detalle)) shiny::tags$div(class = "rmcv-detail", detalle),
    if (!is.null(footer) || mostrar_boton) shiny::tags$div(class = "rmcv-footer",
      if (!is.null(footer)) shiny::tags$div(class = "rmcv-footer-content", footer),
      if (mostrar_boton) shiny::actionButton(ns("detalle"), label = label_boton, icon = shiny::icon(icono_boton),
        class = "rmcv-button",
        title = if (is.null(label_boton) || !nzchar(label_boton)) "Ver detalle" else label_boton)))
  htmltools::attachDependencies(card, htmltools::htmlDependency("racafe-caja-valor", "0.5.0",
    src = c(file = system.file("caja-valor", package = "racafeModulos")), stylesheet = "caja-valor.css"))
}

# COLOR CORPORATIVO DE TARJETAS ----
.cv_color <- function(color) {
  if (!is.character(color) || length(color) != 1L || is.na(color) || !nzchar(color) || grepl("[;{}<>]", color)) {
    stop("Color CSS inv\u00e1lido.", call. = FALSE)
  }
  as.vector(color, mode = "character")
}

#' Servidor del módulo CajaValor
#' @param id ID local del módulo.
#' @param valor_r Reactivo que devuelve el valor numérico.
#' @param formato Formato registrado en racafeShiny::DefinirFormato.
#' @param meta Meta numérica opcional; NA desactiva el color condicional.
#' @param empty Texto opcional para valor NULL/NA; NULL conserva la suspensión del output anterior.
#' @param color Color explícito opcional del valor; NULL hereda color_kpi de la UI. meta conserva su color condicional.
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
cajaValor_server <- function(id, valor_r, formato, meta = NA, empty = NULL, color = NULL) {
  if (!is.null(color)) color <- .cv_color(color)
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
      racafeShiny::FormatearNumero(value, formato, meta = meta, color = if (is.null(color)) "inherit" else color)
    })

    list(
      click_detalle = shiny::reactive(input$detalle)
    )
  })
}
