# Tarjeta reactiva con detalle opcional ----
.caja_modal_ui <- function(id, texto, icono, footer = NULL, mostrar_boton = FALSE,
                           id_boton = "detalle", tooltip = "Ver detalle", color_fondo = NULL) {
  caja <- cajaValor_ui(id, texto = texto, icono = icono, footer = footer, compact = TRUE,
    mostrar_boton = mostrar_boton, label_boton = NULL, id_boton = id_boton,
    tooltip = tooltip, button_size = "icon", footer_position = "outside")
  if (!is.null(color_fondo)) {
    caja <- htmltools::tagQuery(caja)$find(".rmcv-card")$
      addAttrs(style = paste0("background:", color_fondo, ";"))$allTags()
  }
  caja
}

#' cajaValorModal: componente reutilizable del ecosistema Racafé
#' @export
cajaValorModal <- function(id, valor, formato = "numero", texto = "", icono = "info-circle", colores = NULL,
                      color_fondo_hex = NULL, mostrar_boton = TRUE, contenido_modal = NULL, titulo_modal = "",
                      icono_modal = NULL, tamano_modal = "ventana", footer = NULL, footer_class = NULL,
                           ui_fn = .caja_modal_ui, formatear = NULL) {
  session <- shiny::getDefaultReactiveDomain()
  output <- session$output
  .valor <- function(x) if (shiny::is.reactive(x)) x() else x
  accion <- isTRUE(mostrar_boton) && !is.null(contenido_modal)
  output[[paste0(id, "_tarjeta")]] <- racafeShiny::racafeRenderUI({
    ui_fn(session$ns(id), texto = .valor(texto), icono = .valor(icono), footer = .valor(footer),
                  mostrar_boton = accion, id_boton = "btn_abrir", tooltip = .valor(titulo_modal),
                  color_fondo = .valor(color_fondo_hex))
  })
  modulo <- cajaValor_server(id, valor_r = shiny::reactive(.valor(valor)), formato = formato,
                                          empty = "—", id_boton = "btn_abrir", formatear = formatear)
  if (accion) {
    shiny::observeEvent(modulo$click_detalle(), {
      contenido <- if (is.function(contenido_modal)) contenido_modal() else contenido_modal
      shiny::showModal(racafeForms::racafeModalDetail(
                        title = shiny::tagList(.valor(titulo_modal)), contenido,
                        size = tamano_modal, easy_close = TRUE,
                        footer = racafeForms::racafeModalFooter()))
    }, ignoreInit = TRUE)
  }
  invisible(modulo)
}
