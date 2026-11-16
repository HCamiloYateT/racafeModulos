# Eventos documentados con fechas reales y contenido opcional de acciones ----
#' Línea de tiempo vertical reutilizable
#' @param eventos Data frame con Fecha y Evento; Descripcion, Usuario, Referencia y Estado son opcionales.
#' @param contenido Lista opcional de controles o contenido Shiny, uno por fila original.
#' @param agrupar Agrupar por día y tipo; conserva cada evento dentro del detalle expandible.
#' @param max_height Altura máxima del área desplazable en píxeles.
#' @return Contenedor Shiny con eventos ordenados; los eventos sin fecha aparecen al final.
#' @export
TimelineUI <- function(eventos, contenido = NULL, max_height = 420, agrupar = FALSE) {
  if (!is.data.frame(eventos) || !all(c("Fecha", "Evento") %in% names(eventos))) {
    stop("TimelineUI requiere las columnas Fecha y Evento.")
  }
  if (!is.null(contenido) && length(contenido) != nrow(eventos)) stop("Contenido debe corresponder a cada evento.")
  if (length(max_height) != 1L || !is.finite(max_height) || max_height <= 0) stop("Altura inválida.")
  if (!nrow(eventos)) return(shiny::tags$p("No hay eventos documentados."))
  fechas <- if (inherits(eventos$Fecha, "Date")) {
    as.POSIXct(as.character(eventos$Fecha), tz = "America/Bogota")
  } else as.POSIXct(eventos$Fecha, tz = "America/Bogota")
  indices <- order(fechas, seq_len(nrow(eventos)), na.last = TRUE)
  campo <- function(nombre, i) {
    if (!nombre %in% names(eventos)) return(NULL)
    valor <- as.character(eventos[[nombre]][i])
    if (!is.na(valor) && nzchar(valor)) valor else NULL
  }
  articulo <- function(i) shiny::tags$article(role = "listitem", class = "rm-timeline-evento",
    style = "border-left:2px solid #C8A77D;margin:0 0 5px;padding:0 0 4px 9px;",
    shiny::tags$small(style = "color:#6C757D;", if (is.na(fechas[i])) "Fecha no documentada" else {
      format(fechas[i], if (inherits(eventos$Fecha, "Date")) "%d/%m/%Y" else "%d/%m/%Y %H:%M",
        tz = "America/Bogota")
    }),
    shiny::tags$div(shiny::tags$strong(campo("Evento", i))),
    shiny::tags$div(campo("Descripcion", i)),
    shiny::tags$small(paste(Filter(Negate(is.null), lapply(c("Usuario", "Referencia", "Estado"),
      function(nombre) campo(nombre, i))), collapse = " · ")),
    if (!is.null(contenido)) contenido[[i]])
  elementos <- if (!agrupar) lapply(indices, articulo) else {
    dia <- format(fechas, "%Y-%m-%d", tz = "America/Bogota")
    llave <- paste(dia, eventos$Evento, sep = "|")
    llave[is.na(fechas)] <- paste0("SIN_FECHA_", which(is.na(fechas)))
    grupos <- split(indices, factor(llave[indices], levels = unique(llave[indices])))
    lapply(grupos, function(filas) {
      if (length(filas) == 1L) return(articulo(filas))
      identificadores <- lapply(c(Linea = "Líneas", Lote = "Lotes"), function(etiqueta) {
        nombre <- names(c(Linea = "Líneas", Lote = "Lotes"))[match(etiqueta, c("Líneas", "Lotes"))]
        if (!nombre %in% names(eventos)) return(NULL)
        valores <- unique(as.character(eventos[[nombre]][filas]))
        valores <- valores[!is.na(valores) & nzchar(valores) & valores != "0"]
        if (!length(valores)) return(NULL)
        paste0(etiqueta, ": ", paste(head(valores, 5L), collapse = ", "),
          if (length(valores) > 5L) paste0(" y ", length(valores) - 5L, " más"))
      })
      shiny::tags$details(class = "rm-timeline-grupo", style = "margin-bottom:6px;",
        shiny::tags$summary(
          shiny::tags$small(format(fechas[filas[1]], "%d/%m/%Y", tz = "America/Bogota")), " · ",
          shiny::tags$strong(campo("Evento", filas[1])), " · ", length(filas), " registros",
          shiny::tags$div(paste(Filter(Negate(is.null), identificadores), collapse = " · "))),
        lapply(filas, articulo))
    })
  }
  shiny::tags$div(class = "rm-timeline", role = "list",
    style = paste0("max-height:", max_height, "px;overflow-y:auto;padding:4px 8px;"), elementos)
}
