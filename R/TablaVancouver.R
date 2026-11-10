# Tablas Vancouver interactivas con adaptadores de presentación ----
.vancouver_nota <- function(texto) {
  if (is.null(texto)) return(NULL)
  racafeForms::racafeNoticeList(texto, variant = "footnote", font_size = 11,
    text_color = "#495057", accent_color = "#495057")
}
.vancouver_tabla <- function(datos, ...) {
  racafeShiny::racafeTableVancouver(datos, header_color = "#495057", header_width = 1.5,
    decimals = NULL, ...)
}
#' TablaVancouverUI: componente reutilizable del ecosistema Racafé
#' @export
TablaVancouverUI <- function(id, titulo = NULL, subtitulo = NULL, footer = NULL, footer_tipo = "info",
                          sortable = TRUE, mostrar_nota = TRUE, estilo = "minimal", searchable = TRUE,
                          nota_fn = .vancouver_nota) {
  ns <- shiny::NS(id)
  shiny::tagList(
    if (!is.null(titulo)) shiny::tags$strong(titulo),
    if (!is.null(subtitulo)) shiny::tags$div(subtitulo),
    if (searchable) shiny::tags$div(style = "max-width:220px;margin-left:auto;",
      racafeForms::racafeInputText("buscar", NULL, placeholder = "Buscar...", ns = ns)),
    shiny::uiOutput(ns("tabla")),
    shiny::uiOutput(ns("paginacion")),
    if (!is.null(footer)) nota_fn(footer) else shiny::uiOutput(paste0(id, "_nota")))
}

#' TablaVancouverDatos: componente reutilizable del ecosistema Racafé
#' @export
TablaVancouverDatos <- function(datos, columnas = NULL, col_specs = NULL, col_labels = NULL,
                             on_row = NULL, on_sort = NULL, indices = seq_len(nrow(datos)),
                             preparar_fn = NULL, etiquetas_fn = function(x) stats::setNames(as.list(x), x),
                             tabla_fn = .vancouver_tabla,
                             texto_fn = identity) {
  etiquetas_nit <- col_labels
  for (nombre in names(col_specs)) {
    if (!is.null(col_specs[[nombre]]$name)) etiquetas_nit[[nombre]] <- col_specs[[nombre]]$name
  }
  if (!is.null(preparar_fn)) datos <- preparar_fn(datos, etiquetas_nit)
  visibles <- if (is.null(columnas)) names(datos) else intersect(columnas, names(datos))
  visibles <- visibles[!vapply(visibles, function(n) identical(col_specs[[n]]$show, FALSE), logical(1))]
  tabla <- tabla_fn(datos[visibles], font_size = 11, row_padding = 2)
  etiquetas <- etiquetas_fn(visibles)
  for (nombre in visibles) {
    spec <- col_specs[[nombre]]
    if (!is.null(spec$name)) etiquetas[[nombre]] <- spec$name
    if (nombre %in% names(col_labels)) etiquetas[[nombre]] <- col_labels[[nombre]]
    formato <- spec$format
    if (!is.null(formato$cell)) formato <- formato$cell
    if (is.numeric(datos[[nombre]])) {
      digitos <- if (is.null(formato$digits)) 2 else formato$digits
      if (isTRUE(formato$percent)) {
        tabla <- do.call(gt::fmt_percent, list(data = tabla, columns = nombre, decimals = digitos))
      } else {
        tabla <- do.call(gt::fmt_number, list(data = tabla, columns = nombre, decimals = digitos,
          use_seps = is.null(formato$separators) || isTRUE(formato$separators),
          pattern = paste0(if (!is.null(formato$prefix)) formato$prefix, "{x}", formato$suffix)))
      }
    }
    if (is.function(spec$cell) && !grepl("nit", paste(nombre, etiquetas[[nombre]]), ignore.case = TRUE)) {
      valores <- vapply(seq_len(nrow(datos)), function(i) as.character(spec$cell(datos[[nombre]][i])), character(1))
      tabla <- gt::text_transform(tabla, locations = gt::cells_body(columns = tidyselect::all_of(nombre)),
        fn = local({ texto <- valores; function(x) htmltools::htmlEscape(texto) }))
    }
    if (is.function(spec$style)) {
      for (i in seq_len(nrow(datos))) {
        estilo <- spec$style(datos[[nombre]][i])
        if (identical(estilo$color, "inherit")) estilo$color <- NULL
        estilos <- list()
        if (!is.null(estilo$background)) estilos <- c(estilos, list(gt::cell_fill(color = estilo$background)))
        if (!is.null(estilo$color) || !is.null(estilo$fontWeight)) {
          estilos <- c(estilos, list(gt::cell_text(color = estilo$color, weight = estilo$fontWeight)))
        }
        if (length(estilos)) tabla <- gt::tab_style(tabla, style = estilos,
          locations = gt::cells_body(columns = tidyselect::all_of(nombre), rows = i))
      }
    }
  }
  if (!is.null(on_row) && length(visibles)) {
    primera <- visibles[[1]]
    enlaces <- vapply(seq_len(nrow(datos)), function(i) as.character(shiny::tags$button(type = "button",
      class = "btn btn-link crm-tabla-enlace", style = "padding:0;font:inherit;color:inherit;text-align:left;",
      onclick = on_row(indices[[i]]),
      texto_fn(as.character(datos[[primera]][[i]])))), character(1))
    tabla <- gt::text_transform(tabla, locations = gt::cells_body(columns = tidyselect::all_of(primera)),
      fn = local({ texto <- enlaces; function(x) texto }))
  }
  if (!is.null(on_sort)) etiquetas <- lapply(names(etiquetas), function(nombre) gt::html(as.character(
    shiny::tags$button(type = "button", class = "btn btn-link crm-tabla-enlace",
      style = "padding:0;font:inherit;color:inherit;text-align:inherit;", onclick = on_sort(nombre),
      etiquetas[[nombre]])))) |> stats::setNames(names(etiquetas))
  tabla <- gt::cols_label(tabla, .list = etiquetas)
  gt::sub_missing(tabla, missing_text = "—")
}

#' TablaVancouverServer: componente reutilizable del ecosistema Racafé
#' @export
TablaVancouverServer <- function(id, data, columnas = NULL, col_specs = NULL, col_labels = NULL,
                                   id_col = NULL, modo_seleccion = "ninguno", sortable = TRUE,
                                   searchable = TRUE, page_size = 15, titulo = NULL, subtitulo = NULL, ...,
                                   preparar_fn = NULL,
                                   etiquetas_fn = function(x) stats::setNames(as.list(x), x),
                                   tabla_fn = .vancouver_tabla, texto_fn = identity, nota_fn = .vancouver_nota) {
  shiny::moduleServer(id, function(input, output, session) {
    seleccion <- shiny::reactiveVal(NULL)
    orden <- shiny::reactiveVal(list(col = NULL, desc = FALSE))
    pagina <- shiny::reactiveVal(1L)
    .val <- function(x) if (shiny::is.reactive(x)) x() else x
    .datos <- shiny::reactive({ .val(data) })
    .filas <- shiny::reactive({
      datos <- .datos()
      shiny::req(is.data.frame(datos))
      indices <- seq_len(nrow(datos))
      consulta <- if (searchable) input$buscar else NULL
      if (!is.null(consulta) && nzchar(trimws(consulta))) {
        visibles <- if (is.null(columnas)) names(datos) else intersect(columnas, names(datos))
        coincide <- vapply(indices, function(i) any(vapply(datos[visibles], function(col) {
          grepl(tolower(consulta), tolower(as.character(col[[i]])), fixed = TRUE)
        }, logical(1)), na.rm = TRUE), logical(1))
        indices <- indices[coincide]
      }
      actual <- orden()
      if (!is.null(actual$col) && actual$col %in% names(datos)) {
        indices <- indices[order(datos[[actual$col]][indices], decreasing = actual$desc, na.last = TRUE)]
      }
      if (!is.null(id_col) && id_col %in% names(datos)) {
        total <- !is.na(datos[[id_col]][indices]) & as.character(datos[[id_col]][indices]) == "TOTAL"
        indices <- c(indices[!total], indices[total])
      }
      indices
    })
    shiny::observeEvent(list(.datos(), input$buscar), { pagina(1L); seleccion(NULL) }, ignoreInit = TRUE)
    shiny::observeEvent(input$ordenar, {
      shiny::req(sortable, input$ordenar %in% names(.datos()))
      anterior <- orden()
      orden(list(col = input$ordenar, desc = identical(anterior$col, input$ordenar) && !anterior$desc))
      pagina(1L)
    })
    .paginas <- shiny::reactive(max(1L, ceiling(length(.filas()) / page_size)))
    shiny::observeEvent(input$anterior, pagina(max(1L, pagina() - 1L)))
    shiny::observeEvent(input$siguiente, pagina(min(.paginas(), pagina() + 1L)))
    .evento <- function(id, valor) paste0("Shiny.setInputValue(",
      jsonlite::toJSON(session$ns(id), auto_unbox = TRUE), ",", jsonlite::toJSON(valor, auto_unbox = TRUE),
      ",{priority:'event'});")
    output$tabla <- racafeShiny::racafeRenderUI({
      datos <- .datos()
      filas <- .filas()
      desde <- (min(pagina(), .paginas()) - 1L) * page_size
      indices <- head(filas[seq_along(filas) > desde], page_size)
      if (!length(indices)) return(nota_fn("Sin registros para los filtros seleccionados."))
      shiny::tagList(
        if (!is.null(titulo)) shiny::tags$strong(.val(titulo)),
        if (!is.null(subtitulo)) shiny::tags$div(.val(subtitulo)),
        TablaVancouverDatos(datos[indices, , drop = FALSE], columnas, col_specs, col_labels,
          on_row = if (modo_seleccion == "fila") function(i) .evento("seleccionar", i),
          on_sort = if (sortable) function(col) .evento("ordenar", col), indices = indices,
          preparar_fn = preparar_fn, etiquetas_fn = etiquetas_fn, tabla_fn = tabla_fn, texto_fn = texto_fn))
    })
    output$paginacion <- shiny::renderUI({
      if (.paginas() <= 1L) return(NULL)
      shiny::tags$div(style = "display:flex;gap:8px;align-items:center;justify-content:flex-end;",
        racafeForms::racafeButton("anterior", NULL, icon = "angle-left", variant = "ghost", size = "xs",
          title = "Página anterior", ns = session$ns),
        shiny::tags$small(paste("Página", min(pagina(), .paginas()), "de", .paginas())),
        racafeForms::racafeButton("siguiente", NULL, icon = "angle-right", variant = "ghost", size = "xs",
          title = "Página siguiente", ns = session$ns))
    })
    shiny::observeEvent(input$seleccionar, {
      fila <- suppressWarnings(as.integer(input$seleccionar))
      shiny::req(modo_seleccion == "fila", length(fila) == 1L, fila %in% .filas())
      datos <- .datos()
      seleccion(list(modo = "fila", row = fila, id = if (!is.null(id_col)) datos[[id_col]][fila] else fila,
        data = datos[fila, , drop = FALSE]))
    })
    list(seleccion = shiny::reactive(seleccion()), modo = modo_seleccion, limpiar = function() seleccion(NULL))
  })
}

#' TablaVancouver: componente reutilizable del ecosistema Racafé
#' @export
TablaVancouver <- function(..., footer = NULL, footer_tipo = "info", modal_titulo_fn = NULL, modal_contenido_fn = NULL,
                                modal_pre_fn = NULL,
                                modal_size = "xl", modal_icon = NULL, modal_footer = NULL,
                                modal_footer_tipo = "info", nota_fn = .vancouver_nota) {
  argumentos <- list(...)
  id <- if (!is.null(argumentos[["id", exact = TRUE]])) argumentos[["id", exact = TRUE]] else argumentos[[1]]
  session <- shiny::getDefaultReactiveDomain()
  output <- session$output
  output[[paste0(id, "_nota")]] <- racafeShiny::racafeRenderUI({
    nota <- if (shiny::is.reactive(footer)) footer() else footer
    if (is.null(nota)) return(NULL)
    tipo <- if (footer_tipo == "danger") "error" else if (footer_tipo == "warning") "warning" else "info"
    nota_fn(nota)
  })
  tabla <- TablaVancouverServer(..., nota_fn = nota_fn)
  if (!is.null(modal_contenido_fn)) {
    shiny::observeEvent(tabla$seleccion(), {
      seleccion <- tabla$seleccion()
      shiny::req(seleccion)
      if (!is.null(modal_pre_fn)) modal_pre_fn(seleccion)
      titulo <- if (is.function(modal_titulo_fn)) modal_titulo_fn(seleccion) else modal_titulo_fn
      shiny::showModal(racafeForms::racafeModalDetail(
                        title = shiny::tagList(titulo), modal_contenido_fn(seleccion),
                        size = modal_size, easy_close = TRUE,
                        footer = racafeForms::racafeModalFooter(
                          left = if (!is.null(modal_footer)) {
                            nota_fn(modal_footer)
                          }, right = shiny::modalButton("Cerrar"))))
      tabla$limpiar()
    }, ignoreInit = TRUE)
  }
  tabla
}

#' TablaVancouverCeldas: componente reutilizable del ecosistema Racafé
#' @export
TablaVancouverCeldas <- function(id, data, titulo = NULL, subtitulo = NULL, footer = NULL,
                                col_specs = NULL, id_col = NULL, modal_titulo_fn = NULL,
                                modal_pre_fn = NULL, modal_contenido_fn = NULL, modal_size = "ventana",
                                datos_fn = TablaVancouverDatos, nota_fn = .vancouver_nota,
                                seleccion_permitida = function(sel) !is.null(sel)) {
  shiny::moduleServer(id, function(input, output, session) {
    .val <- function(x) if (shiny::is.reactive(x)) x() else x
    output$tabla <- racafeShiny::racafeRenderUI({
      datos <- .val(data)
      if (!nrow(datos)) return(nota_fn("Sin registros para los filtros seleccionados."))
      tabla <- datos_fn(datos, col_specs = col_specs)
      for (col in setdiff(names(datos), id_col)) {
        botones <- vapply(seq_len(nrow(datos)), function(i) {
          valor <- as.character(datos[[col]][i])
          if (!seleccion_permitida(list(id = as.character(datos[[id_col]][i]), col = col))) {
            return(htmltools::htmlEscape(valor))
          }
          evento <- paste0("Shiny.setInputValue(", jsonlite::toJSON(session$ns("celda"), auto_unbox = TRUE),
            ",", jsonlite::toJSON(list(id = as.character(datos[[id_col]][i]), col = col), auto_unbox = TRUE),
            ",{priority:'event'});")
          as.character(shiny::tags$button(type = "button", class = "btn btn-link crm-tabla-enlace",
            style = "padding:0;font:inherit;color:inherit;", onclick = evento,
            `aria-label` = paste(datos[[id_col]][i], col, valor), valor))
        }, character(1))
        tabla <- gt::text_transform(tabla, locations = gt::cells_body(columns = tidyselect::all_of(col)),
          fn = local({ texto <- botones; function(x) texto }))
      }
      shiny::tagList(if (!is.null(titulo)) shiny::tags$strong(.val(titulo)),
        if (!is.null(subtitulo)) shiny::tags$div(.val(subtitulo)), tabla, nota_fn(.val(footer)))
    })
    shiny::observeEvent(input$celda, {
      datos <- .val(data)
      sel <- input$celda
      shiny::req(is.list(sel), is.character(sel$id), length(sel$id) == 1L,
        is.character(sel$col), length(sel$col) == 1L, seleccion_permitida(sel),
        sel$id %in% as.character(datos[[id_col]]), sel$col %in% setdiff(names(datos), id_col))
      if (!is.null(modal_pre_fn)) modal_pre_fn(sel)
      shiny::showModal(racafeForms::racafeModalDetail(title = modal_titulo_fn(sel), modal_contenido_fn(sel),
        size = modal_size, easy_close = TRUE, footer = racafeForms::racafeModalFooter()))
    }, ignoreInit = TRUE)
  })
}
