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
#' @param color Color de acento para borde lateral, icono y botón; no indica cumplimiento del KPI.
#' @param color_kpi,color_footer Overrides opcionales; NULL usa negro para el KPI y gris para el footer.
#' @param footer Texto o contenido Shiny opcional del pie.
#' @param label_boton,icono_boton Etiqueta e icono del botón situado abajo a la derecha.
#' @param mostrar_kpi Mostrar el valor reactivo; FALSE permite tarjetas informativas sin servidor.
#' @param id_boton ID local de la acción; detalle conserva la API anterior.
#' @details Conserva los argumentos posicionales e IDs del módulo trasladado desde racafeShiny.
#'   Desde 0.5.0 usa borde lateral y botón inferior opcional, manteniendo IDs y click_detalle.
#'   Los colores específicos prevalecen sobre color. La aplicación decide qué hacer con el evento.
#' @examples
#' cajaValor_ui(shiny::NS("resumen")("ventas"), texto = "Ventas")
#' @export
#' @param tooltip Texto accesible y hover del botón; NULL conserva la etiqueta anterior.
#' @param button_size normal o icon; icon utiliza un botón compacto de 22 px.
#' @param footer_position inside conserva el pie interior; outside lo coloca fuera de la tarjeta.
#' @param nota_fn Función de presentación de la nota externa.
cajaValor_ui <- function(id, icono = "chart-line", texto = "",
                         mostrar_boton = TRUE, color_borde = NULL, color_icono = NULL,
                         detalle = NULL, compact = FALSE, color = "#DA291C", color_kpi = NULL,
                         color_footer = NULL, footer = NULL, label_boton = "Ver detalle",
                         icono_boton = "arrow-right", mostrar_kpi = TRUE, id_boton = "detalle",
                         tooltip = NULL, button_size = c("normal", "icon"),
                         footer_position = c("inside", "outside"), nota_fn = .vancouver_nota) {
  button_size <- match.arg(button_size)
  footer_position <- match.arg(footer_position)
  footer_externo <- if (footer_position == "outside") footer else NULL
  if (footer_position == "outside") footer <- NULL
  for (flag in list(compact, mostrar_boton, mostrar_kpi)) {
    if (!is.logical(flag) || length(flag) != 1L || is.na(flag)) stop("Estado l\u00f3gico inv\u00e1lido.", call. = FALSE)
  }
  color <- .cv_color(color)
  color_borde <- if (is.null(color_borde)) color else .cv_color(color_borde)
  color_icono <- if (is.null(color_icono)) color else .cv_color(color_icono)
  color_kpi <- if (is.null(color_kpi)) "#000000" else .cv_color(color_kpi)
  color_footer <- if (is.null(color_footer)) "#667085" else .cv_color(color_footer)
  .cv_button_id(id_boton)
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
      if (mostrar_boton) shiny::actionButton(ns(id_boton), label = label_boton, icon = shiny::icon(icono_boton),
        class = if (button_size == "icon") "rmcv-button rmcv-button--icon" else "rmcv-button",
        title = if (!is.null(tooltip)) tooltip else if (is.null(label_boton) || !nzchar(label_boton)) {
          "Ver detalle"
        } else label_boton)))
  if (!is.null(tooltip)) {
    card <- htmltools::tagQuery(card)$find(".rmcv-button")$addAttrs(`aria-label` = tooltip)$allTags()
  }
  if (footer_position == "outside") {
    card <- shiny::tags$div(class = "rmcv-with-note", card, nota_fn(footer_externo))
  }
  htmltools::attachDependencies(card, htmltools::htmlDependency("racafe-caja-valor", "0.6.0",
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
#' @param color Color explícito opcional del valor; NULL hereda el negro de UI o un color_kpi explícito.
#' @param prop TRUE: mayor o igual a meta es mejor; FALSE: menor o igual es mejor.
#' @param id_boton ID local de la acción, igual al declarado en UI.
#' @details Sin meta no se aplica rojo/verde. La comparación usa las unidades originales del valor y la meta.
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
#' @param formatear Callback opcional del valor; NULL conserva el formato y meta anteriores.
cajaValor_server <- function(id, valor_r, formato, meta = NA, empty = NULL, color = NULL,
                             prop = TRUE, id_boton = "detalle", formatear = NULL) {
  .cv_button_id(id_boton)
  if (!is.null(formatear) && !is.function(formatear)) stop("formatear debe ser una función.", call. = FALSE)
  if (!is.logical(prop) || length(prop) != 1L || is.na(prop)) stop("prop debe ser l\u00f3gico.", call. = FALSE)
  if (length(meta) != 1L || !(is.numeric(meta) || (is.logical(meta) && is.na(meta))) ||
      (!is.na(meta) && !is.finite(meta))) {
    stop("meta debe ser num\u00e9rica escalar o NA.", call. = FALSE)
  }
  if (!is.null(color)) color <- .cv_color(color)
  if (!is.null(empty) && (!is.character(empty) || length(empty) != 1L || is.na(empty))) {
    stop("empty debe ser un texto escalar.", call. = FALSE)
  }
  shiny::moduleServer(id, function(input, output, session) {

    render_valor <- if (is.null(formatear)) shiny::renderUI else racafeShiny::racafeRenderUI
    output$valor_display <- render_valor({
      value <- valor_r()
      if (!is.null(formatear)) return(formatear(value))
      if (!is.null(empty) && (!length(value) || (length(value) == 1L && is.na(value)))) {
        return(shiny::tags$span(as.vector(empty, mode = "character")))
      }
      shiny::req(value)
      kpi_color <- if (is.null(color)) "inherit" else color
      if (!is.na(meta)) {
        cumple <- if (prop) value >= meta else value <= meta
        kpi_color <- if (cumple) "#1A7A5E" else "#C0392B"
      }
      racafeShiny::FormatearNumero(value, formato, color = kpi_color)
    })

    list(
      click_detalle = shiny::reactive(input[[id_boton]])
    )
  })
}

# ID LOCAL DE LA ACCION ----
.cv_button_id <- function(id_boton) {
  if (!is.character(id_boton) || length(id_boton) != 1L || is.na(id_boton) || !nzchar(id_boton)) {
    stop("id_boton debe ser un texto no vac\u00edo.", call. = FALSE)
  }
  invisible(id_boton)
}
