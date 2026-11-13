# Eventos documentados con fechas reales y contenido opcional de acciones ----
#' Línea de tiempo vertical reutilizable
#' @param eventos Data frame con Fecha y Evento; Descripcion, Usuario, Referencia y Estado son opcionales.
#' @param contenido Lista opcional de controles o contenido Shiny, uno por fila original.
#' @param max_height Altura máxima del área desplazable en píxeles.
#' @return Contenedor Shiny con eventos ordenados; los eventos sin fecha aparecen al final.
#' @export
TimelineUI <- function(eventos, contenido = NULL, max_height = 420) {
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
  shiny::tags$div(class = "rm-timeline", role = "list",
    style = paste0("max-height:", max_height, "px;overflow-y:auto;padding:4px 12px;"),
    lapply(indices, function(i) shiny::tags$article(role = "listitem",
      style = "border-left:2px solid #C8A77D;margin:0 0 10px;padding:0 0 8px 12px;",
      shiny::tags$small(style = "color:#6C757D;", if (is.na(fechas[i])) "Fecha no documentada" else {
        format(fechas[i], if (inherits(eventos$Fecha, "Date")) "%d/%m/%Y" else "%d/%m/%Y %H:%M",
          tz = "America/Bogota")
      }),
      shiny::tags$div(shiny::tags$strong(campo("Evento", i))),
      shiny::tags$div(campo("Descripcion", i)),
      shiny::tags$small(paste(Filter(Negate(is.null), lapply(c("Usuario", "Referencia", "Estado"),
        function(nombre) campo(nombre, i))), collapse = " · ")),
      if (!is.null(contenido)) contenido[[i]])))
}
