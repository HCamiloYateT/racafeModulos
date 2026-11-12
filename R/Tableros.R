.tableros_dependencies <- function() {
  htmltools::htmlDependency("racafe-tableros", "0.6.3",
    src = c(file = system.file("tableros", package = "racafeModulos")), stylesheet = "tableros.css")
}
# Presentación de tareas y Kanban con acciones delegadas ----
#' TableroTareasTarjetas: componente reutilizable del ecosistema Racafé
#' @export
TableroTareasTarjetas <- function(datos, prioridades, actividades_fn, ns = identity,
                                  campos = c(id = "Id", titulo = "Titulo", descripcion = "Descripcion",
                                    prioridad = "Prioridad", responsable = "Responsable", tipo = "Tipo",
                                    cumplido = "Cumplido", archivado = "Archivado", vencimiento = "FechaCumplimiento"),
                                  nota_fn = .vancouver_nota, colores = PaletaPrioridadesTareas()) {
  claves <- c(id = "cons", titulo = "titulo", descripcion = "Descripcion", prioridad = "Prioridad",
    responsable = "Responsable", tipo = "Tipo", cumplido = "Cumplido", archivado = "Archivado",
    vencimiento = "FechaCumplimiento")
  fuente <- datos
  for (clave in names(claves)) fuente[[claves[[clave]]]] <- datos[[campos[[clave]]]]
  columnas <- lapply(prioridades, function(prioridad) {
    filas <- fuente[fuente$Prioridad == prioridad, , drop = FALSE]
    tarjetas <- lapply(seq_len(nrow(filas)), function(i) {
      fila <- filas[i, , drop = FALSE]
      actividades <- actividades_fn(fila)
      texto <- trimws(as.character(fila$Descripcion[[1]]))
      if (is.na(texto) || !nzchar(texto)) texto <- "Sin descripción"
      if (nchar(texto) > 180) texto <- paste0(substr(texto, 1, 177), "…")
      boton <- racafeForms::racafeButton(paste0("abrir_", fila$cons[[1]]), "Ver detalle", icon = "arrow-right",
        variant = "secondary", size = "xs", ns = ns)
      # Un solo evento por tablero conserva la clave y evita observers por tarjeta.
      boton <- shiny::tagAppendAttributes(boton, onclick = paste0("Shiny.setInputValue(",
        jsonlite::toJSON(ns("abrir"), auto_unbox = TRUE), ",",
        jsonlite::toJSON(as.character(fila$cons[[1]]), auto_unbox = TRUE), ",{priority:'event'});"))
      responsables <- as.character(fila$Responsable[[1]])
      responsables <- if (is.na(responsables)) character() else {
        unique(trimws(strsplit(responsables, "|", fixed = TRUE)[[1]]))
      }
      responsables <- responsables[nzchar(responsables)]
      if (!length(responsables)) responsables <- "Sin responsable"
      badges <- lapply(responsables, function(nombre) {
        shiny::tagAppendAttributes(racafeForms::racafeStatusBadge(nombre, "neutral", size = "xs"),
          title = paste("Responsable:", nombre))
      })
      n_actividades <- length(actividades$textos)
      racafeForms::racafeCard(padding = "compact",
        shiny::tags$details(class = "rm-task-detail",
          shiny::tags$summary(
            shiny::tags$strong(as.character(fila$titulo[[1]])),
            shiny::tags$span(class = "rm-task-badges", badges)),
          shiny::tags$div(class = "rm-task-expanded",
            shiny::tags$div(class = "rm-task-badges",
              racafeForms::racafeStatusBadge(
                if (identical(as.character(fila$Tipo[[1]]), "list")) "Tarea" else "Nota", "neutral", size = "xs"),
              racafeForms::racafeStatusBadge(if (n_actividades) {
                paste0("Actividades: ", sum(actividades$cumplidas), "/", n_actividades)
              } else "Sin actividades", "neutral", size = "xs"),
              if (isTRUE(as.integer(fila$Cumplido[[1]]) == 1L)) {
                racafeForms::racafeStatusBadge("Cumplida", "success", size = "xs")
              },
              if (isTRUE(as.integer(fila$Archivado[[1]]) == 1L)) {
                racafeForms::racafeStatusBadge("Archivada", "neutral", size = "xs")
              }),
            shiny::tags$p(texto),
            racafeForms::racafeInfoItem("Vencimiento", racafeForms::racafeDateLabel(fila$FechaCumplimiento[[1]])),
            boton)))
    })
    color <- unname(colores[prioridad])
    if (!length(color) || is.na(color)) color <- "#6C757D"
    ColumnaKanbanUI(prioridad, cantidad = nrow(filas), color = color, header_filled = FALSE,
      cuerpo = if (!length(tarjetas)) nota_fn("Sin registros en esta prioridad.") else shiny::tagList(tarjetas))
  })
  tablero <- TableroKanbanUI(columnas, ns = ns)
  shiny::tagAppendAttributes(tablero, class = "rm-task-board-shared")
}

#' ColumnaKanbanUI: componente reutilizable del ecosistema Racafé
#' @export
ColumnaKanbanUI <- function(titulo, cuerpo, cantidad, color = "#495057", badges = NULL, acciones = NULL,
                            header_filled = TRUE) {
  color <- .cv_color(color)
  shiny::tags$div(class = paste("kpb-col", if (!header_filled) "kpb-col-outline"),
    style = paste0("--kanban-color:", color, ";"),
    shiny::tags$div(class = "kpb-col-header", style = if (header_filled) paste0("background:", color, ";") else
      paste0("background:#fff;border-bottom:3px solid ", color, ";"),
      shiny::tags$div(class = "kpb-col-header-inner",
        shiny::tags$span(class = "kpb-col-titulo", titulo),
        shiny::tags$div(class = "kpb-col-badges",
          shiny::tags$span(class = "kpb-col-count", as.character(cantidad)), badges, acciones))),
    shiny::tags$div(class = "kpb-col-body", cuerpo))
}
#' TableroKanbanUI: componente reutilizable del ecosistema Racafé
#' @export
TableroKanbanUI <- function(..., id = NULL, ns = identity) {
  htmltools::attachDependencies(shiny::tags$div(id = if (!is.null(id)) ns(id),
    `data-kanban-ns` = ns(""), class = "kpb-board", ...),
    list(MenuAccionesDependencies(), .tableros_dependencies()))
}

#' Colores compartidos de prioridad para tableros, tarjetas y convenciones
#' @return Vector de colores nombrado por prioridad.
#' @export
PaletaPrioridadesTareas <- function() {
  c(URGENTE = "#7A2028", IMPORTANTE = "#9B7160", MEDIA = "#C8A77D", BAJA = "#6C757D")
}
