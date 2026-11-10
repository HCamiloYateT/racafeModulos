# Acciones flotantes de tablas y tarjetas, sin reglas de negocio ----
#' MenuAccionesDependencies: componente reutilizable del ecosistema Racafé
#' @export
MenuAccionesDependencies <- function() {
  htmltools::htmlDependency("racafe-action-menus", "0.6.0",
    src = c(file = system.file("action-menus", package = "racafeModulos")), script = "action-menus.js",
    stylesheet = "action-menus.css")
}
#' MenuAccionesUI: componente reutilizable del ecosistema Racafé
#' @export
MenuAccionesUI <- function(codigo, acciones, config = list(), ns = identity,
                           id_evento = "AccionSeleccionada", id_grupo = NULL) {
  if (is.null(id_grupo)) id_grupo <- ns(paste0("acc_", gsub("[^A-Za-z0-9]", "", codigo)))
  botones <- lapply(names(acciones), function(clave) {
    cfg <- config[[clave]]
    color <- .cv_color(if (is.null(cfg$color)) "#C11007" else cfg$color)
    icono <- if (is.null(cfg$icono)) "ellipsis" else cfg$icono
    shiny::tags$button(type = "button", title = acciones[[clave]], `data-rm-action` = clave,
      `data-rm-input` = ns(id_evento), `data-rm-code` = as.character(codigo),
      class = "rm-action-option", style = paste0("--rm-action-color:", color, ";"), shiny::icon(icono))
  })
  htmltools::attachDependencies(shiny::tags$span(
    shiny::tags$button(type = "button", title = "Acciones", class = "rc-boton-acciones rm-action-trigger",
      `data-rm-panel` = id_grupo,
      shiny::icon("ellipsis")),
    shiny::tags$div(id = id_grupo, class = "rc-panel-acciones rm-action-panel",
      botones)),
    MenuAccionesDependencies())
}
