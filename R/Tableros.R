# Tarjeta compacta compartida sin reglas de negocio ----
#' Tarjeta compacta para tableros con acciones y arrastre delegado
#' @param titulo Título de texto de la tarjeta.
#' @param responsables Badges o contenido Shiny de responsables.
#' @param contexto Texto auxiliar compacto.
#' @param cuerpo Contenido visible al expandir.
#' @param menu Menú de acciones creado por la aplicación.
#' @param color Color del borde izquierdo.
#' @param codigo Clave del registro; NULL desactiva el arrastre.
#' @param origen Columna actual.
#' @param destinos Columnas permitidas por la aplicación.
#' @param busqueda Texto usado para buscar la tarjeta.
#' @return Tarjeta Shiny con detalle colapsado y eventos delegados por el tablero.
#' @export
TarjetaKanbanUI <- function(titulo, responsables = NULL, contexto = NULL, cuerpo = NULL, menu = NULL,
                            color = "#495057", codigo = NULL, origen = NULL, destinos = character(), busqueda = NULL) {
  tarjeta <- shiny::tags$div(class = "kpb-card kpb-card-compact",
    draggable = if (!is.null(codigo) && length(destinos)) "true" else "false",
    `data-cod-contacto` = codigo, `data-kb-source` = origen, `data-kb-targets` = paste(destinos, collapse = " "),
    `data-kb-search` = busqueda, style = paste0("border-left:3px solid ", .cv_color(color), ";"),
    shiny::tags$details(shiny::tags$summary(shiny::tags$strong(titulo),
      shiny::tags$span(class = "kpb-responsables", responsables),
      if (!is.null(contexto)) shiny::tags$span(class = "kb-contexto", title = contexto, contexto)),
      shiny::tags$div(class = "kpb-card-body", cuerpo)),
    shiny::tags$div(class = "kpb-card-menu", menu))
  htmltools::attachDependencies(tarjeta, .tableros_dependencies())
}

.tableros_dependencies <- function() {
  htmltools::htmlDependency("racafe-tableros", "0.6.4",
    src = c(file = system.file("tableros", package = "racafeModulos")), stylesheet = "tableros.css")
}
# Presentación de tareas y Kanban con acciones delegadas ----
#' TableroTareasTarjetas: componente reutilizable del ecosistema Racafé
#' @export
TableroTareasTarjetas <- function(datos, prioridades, actividades_fn, ns = identity,
                                  campos = c(id = "Id", titulo = "Titulo", descripcion = "Descripcion",
                                    prioridad = "Prioridad", responsable = "Responsable", tipo = "Tipo",
                                    cumplido = "Cumplido", archivado = "Archivado", vencimiento = "FechaCumplimiento"),
                                  nota_fn = .vancouver_nota, colores = PaletaPrioridadesTareas(),
                                  acciones_fn = NULL, arrastrable = FALSE) {
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
      acciones <- if (is.null(acciones_fn)) c(abrir = "Ver detalle") else acciones_fn(fila)
      iconos <- c(abrir = "eye", editar = "pen", archivar = "archive", desarchivar = "box-open",
        cumplir = "check", eliminar = "trash")
      config <- lapply(names(acciones),
        function(a) list(icono = if (a %in% names(iconos)) unname(iconos[a]) else "ellipsis", color = "#495057"))
      names(config) <- names(acciones)
      menu <- MenuAccionesUI(as.character(fila$cons[[1]]), acciones, config = config, ns = ns,
        id_evento = "AccionTarea", id_grupo = ns(paste0("acciones_tarea_", fila$cons[[1]])))
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
      color <- unname(colores[prioridad])
      if (!length(color) || is.na(color)) color <- "#6C757D"
      TarjetaKanbanUI(as.character(fila$titulo[[1]]), responsables = shiny::tagList(badges),
        contexto = paste(if (identical(as.character(fila$Tipo[[1]]), "list")) "TAREA" else "NOTA",
          if (n_actividades) paste0(sum(actividades$cumplidas), "/", n_actividades,
            " ACTIVIDADES") else "SIN ACTIVIDADES"),
        cuerpo = shiny::tagList(
          if (isTRUE(as.integer(fila$Cumplido[[1]]) == 1L)) racafeForms::racafeStatusBadge("CUMPLIDA",
            "success", size = "xs"),
          if (isTRUE(as.integer(fila$Archivado[[1]]) == 1L)) racafeForms::racafeStatusBadge("ARCHIVADA",
            "neutral", size = "xs"),
          shiny::tags$p(texto),
          racafeForms::racafeInfoItem("Vencimiento", racafeForms::racafeDateLabel(fila$FechaCumplimiento[[1]]))),
        menu = menu, color = color, codigo = if (arrastrable) as.character(fila$cons[[1]]) else NULL,
        origen = prioridad, destinos = if (arrastrable) setdiff(prioridades, prioridad) else character())

    })
    color <- unname(colores[prioridad])
    if (!length(color) || is.na(color)) color <- "#6C757D"
    shiny::tags$div(class = "kpb-slot", `data-kb-stage` = prioridad,
      ColumnaKanbanUI(prioridad, cantidad = nrow(filas), color = color, header_filled = FALSE,
        acciones = shiny::tags$button(type = "button", class = "kb-collapse-toggle", `data-kb-toggle` = "true",
          title = paste("Colapsar", prioridad), `aria-expanded` = "true", shiny::icon("chevron-down")),
        cuerpo = if (!length(tarjetas)) nota_fn("Sin registros en esta prioridad.") else shiny::tagList(tarjetas)))

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
