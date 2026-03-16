# TablaReactable helpers ----

#' Operador interno para fallback de valores NULL
#' @keywords internal
`%||%` <- function(x, y) if (!is.null(x)) x else y

#' Convierte un vector R en un arreglo JSON string para JS
#' @keywords internal
.to_json_arr <- function(v) {
  if (is.null(v) || length(v) == 0) return("[]")
  paste0('["', paste(as.character(v), collapse = '","'), '"]')
}

#' Genera JS para click por fila
#' @keywords internal
.js_fila <- function(input_id, filas_bloqueadas_json = "[]") {
  paste0(
    "function(rowInfo) {",
    "  var bloq = ", filas_bloqueadas_json, ";",
    "  if (bloq.length > 0) {",
    "    var vals = Object.values(rowInfo.values).map(function(v) {",
    "      return String(v).trim().toUpperCase();",
    "    });",
    "    var bup = bloq.map(function(b) { return String(b).trim().toUpperCase(); });",
    "    if (vals.some(function(v) { return bup.indexOf(v) >= 0; })) return;",
    "  }",
    "  Shiny.setInputValue('", input_id, "', {",
    "    row_index : rowInfo.index,",
    "    values    : rowInfo.values,",
    "    nonce     : Math.random()",
    "  }, {priority: 'event'});",
    "}"
  )
}

#' Genera JS para click por celda
#' @keywords internal
.js_celda <- function(input_id,
                      cols_activos_json = "[]",
                      filas_bloqueadas_json = "[]") {
  paste0(
    "function(rowInfo, colInfo) {",
    "  var cols = ", cols_activos_json, ";",
    "  if (cols.length > 0 && cols.indexOf(colInfo.id) < 0) return;",
    "  var bloq = ", filas_bloqueadas_json, ";",
    "  if (bloq.length > 0) {",
    "    var vals = Object.values(rowInfo.values).map(function(v) {",
    "      return String(v).trim().toUpperCase();",
    "    });",
    "    var bup = bloq.map(function(b) { return String(b).trim().toUpperCase(); });",
    "    if (vals.some(function(v) { return bup.indexOf(v) >= 0; })) return;",
    "  }",
    "  Shiny.setInputValue('", input_id, "', {",
    "    row_index : rowInfo.index,",
    "    values    : rowInfo.values,",
    "    col       : colInfo.id,",
    "    valor     : rowInfo.values[colInfo.id],",
    "    nonce     : Math.random()",
    "  }, {priority: 'event'});",
    "}"
  )
}

#' Genera JS para click por columna
#' @keywords internal
.js_columna <- function(input_id, cols_activos_json = "[]") {
  paste0(
    "function(rowInfo, colInfo) {",
    "  var cols = ", cols_activos_json, ";",
    "  if (cols.length > 0 && cols.indexOf(colInfo.id) < 0) return;",
    "  Shiny.setInputValue('", input_id, "', {",
    "    col   : colInfo.id,",
    "    nonce : Math.random()",
    "  }, {priority: 'event'});",
    "}"
  )
}

#' Genera JS de click en encabezado para sort server-side
#' Solo emite la columna clicada; el toggle asc/desc lo resuelve R en sort_estado
#' @keywords internal
.js_header_sort <- function(sort_input_id, col_name) {
  paste0(
    "function(column) {",
    "  Shiny.setInputValue('", sort_input_id, "', {",
    "    col   : '", col_name, "',",
    "    nonce : Math.random()",
    "  }, {priority: 'event'});",
    "}"
  )
}

#' Genera nota de interaccion automatica con informacion de restricciones activas
#' @keywords internal
.nota_interaccion <- function(
    modo_seleccion,
    sortable,
    cols_activos = NULL,
    filas_bloqueadas = NULL,
    filas_seleccionables = NULL
) {
  # Texto principal segun modo de seleccion y restricciones activas
  txt_modo <- switch(
    modo_seleccion,
    fila = {
      if (!is.null(filas_bloqueadas) && length(filas_bloqueadas) > 0) {
        bloq <- paste(filas_bloqueadas, collapse = ", ")
        paste0(
          "\u2139\ufe0f  Clic en una fila para ver su detalle. ",
          "Las filas ", bloq, " no son seleccionables."
        )
      } else if (!is.null(filas_seleccionables)) {
        paste0(
          "\u2139\ufe0f  Clic en una fila para ver su detalle. ",
          "Solo algunas filas estan habilitadas para seleccion."
        )
      } else {
        "\u2139\ufe0f  Clic en una fila para ver su detalle."
      }
    },
    celda = {
      if (!is.null(cols_activos) && length(cols_activos) > 0) {
        cols <- paste(cols_activos, collapse = ", ")
        if (!is.null(filas_bloqueadas) && length(filas_bloqueadas) > 0) {
          bloq <- paste(filas_bloqueadas, collapse = ", ")
          paste0(
            "\u2139\ufe0f  Clic en las columnas ", cols, " para ver el detalle. ",
            "Las filas ", bloq, " no son seleccionables."
          )
        } else {
          paste0("\u2139\ufe0f  Clic en las columnas ", cols, " para ver el detalle.")
        }
      } else {
        if (!is.null(filas_bloqueadas) && length(filas_bloqueadas) > 0) {
          bloq <- paste(filas_bloqueadas, collapse = ", ")
          paste0(
            "\u2139\ufe0f  Clic en una celda para ver su detalle. ",
            "Las filas ", bloq, " no son seleccionables."
          )
        } else {
          paste0(
            "\u2139\ufe0f  Clic en una celda para ver su detalle; ",
            "el encabezado identifica la columna seleccionada."
          )
        }
      }
    },
    columna = {
      if (!is.null(cols_activos) && length(cols_activos) > 0) {
        cols <- paste(cols_activos, collapse = ", ")
        paste0("\u2139\ufe0f  Clic en las columnas ", cols, " para explorar sus valores.")
      } else {
        "\u2139\ufe0f  Clic en cualquier celda de una columna para explorar sus valores."
      }
    },
    ninguno = NULL
  )

  # Texto de ordenamiento
  txt_orden <- if (sortable) {
    "\u21c5  Clic en el encabezado de una columna para ordenar la tabla."
  } else {
    NULL
  }

  if (is.null(txt_modo) && is.null(txt_orden)) return(NULL)

  lineas <- Filter(Negate(is.null), list(txt_modo, txt_orden))

  shiny::tags$div(
    class = "caja-modal-footer",
    lapply(lineas, function(l) shiny::tags$div(l))
  )
}

#' Construye bloque footer opcional
#' @keywords internal
.footer_bloque <- function(footer, footer_tipo) {
  if (is.null(footer)) return(NULL)

  css_class <- switch(
    footer_tipo,
    info    = "caja-modal-footer",
    warning = "caja-modal-footer-warning",
    dark    = "caja-modal-footer-dark",
    danger  = "caja-modal-footer-danger",
    "caja-modal-footer"
  )

  if (inherits(footer, "shiny.tag") || inherits(footer, "shiny.tag.list")) {
    shiny::tags$div(class = css_class, footer)
  } else {
    shiny::tags$div(class = css_class, as.character(footer))
  }
}

#' @keywords internal
.FORMATOS_RACAFE <- c(
  "coma", "numero", "dinero", "dolares", "miles",
  "porcentaje", "cientifico", "millones", "entero",
  "tiempo", "kwh", "log"
)

#' Resuelve formateador de celda para reactable::colDef
#' @keywords internal
.resolver_fmt_celda <- function(formato) {
  formato <- tolower(trimws(formato))

  if (formato == "fecha") {
    return(function(v) {
      if (is.na(v)) return("\u2014")
      tryCatch(format(as.Date(v), "%d/%m/%Y"), error = function(e) as.character(v))
    })
  }

  if (!formato %in% .FORMATOS_RACAFE) {
    stop(paste0(
      "TablaReactable col_specs: formato '", formato, "' no reconocido. ",
      "Use uno de: ", paste(.FORMATOS_RACAFE, collapse = ", "), " o 'fecha'."
    ))
  }

  formateador <- racafe::DefinirFormato(formato)

  function(v) {
    if (is.na(v)) return("\u2014")
    formateador(v)
  }
}

#' Aplica sort server-side manteniendo filas fijas siempre al final
#' Separa filas normales de fijas, ordena solo las normales y las recombina
#' @keywords internal
.aplicar_sort <- function(df, sort_col, sort_dir, filas_fijas_al_final) {
  if (is.null(sort_col) || !sort_col %in% names(df)) return(df)

  fijas_up <- toupper(trimws(as.character(filas_fijas_al_final %||% character(0))))

  # Separar filas fijas detectando coincidencia en cualquier celda de la fila
  if (length(fijas_up) > 0) {
    es_fija <- vapply(
      seq_len(nrow(df)),
      function(i) {
        vals <- toupper(trimws(as.character(unlist(df[i, , drop = TRUE]))))
        any(vals %in% fijas_up)
      },
      logical(1L)
    )
    df_normal <- df[!es_fija, , drop = FALSE]
    df_fijas  <- df[es_fija,  , drop = FALSE]
  } else {
    df_normal <- df
    df_fijas  <- df[0, , drop = FALSE]
  }

  # Ordenar solo filas normales; NAs siempre al final
  col_vals <- df_normal[[sort_col]]
  ord <- if (sort_dir == "desc") {
    order(col_vals, decreasing = TRUE, na.last = TRUE)
  } else {
    order(col_vals, decreasing = FALSE, na.last = TRUE)
  }

  rbind(df_normal[ord, , drop = FALSE], df_fijas)
}

#' Construye colDefs con header JS para sort server-side
#' Cuando sortable = FALSE construye coldefs estaticos sin header interactivo
#' El icono de direccion activa se muestra en el label del encabezado
#' @keywords internal
.coldefs_default <- function(
    data,
    columnas_override,
    col_specs,
    id_col,
    col_header_n,
    sortable_flag,
    sort_input_id = NULL,
    sort_col_actual = NULL,
    sort_dir_actual = NULL
) {
  nms            <- names(data)
  col_header_idx <- seq_len(col_header_n)
  col_specs      <- col_specs %||% list()

  if (!is.list(col_specs)) col_specs <- as.list(col_specs)

  normalizar_spec <- function(x) {
    if (is.null(x)) return(list())
    if (is.list(x)) return(x)
    x_list <- as.list(x)
    if (!is.null(names(x_list)) && any(nzchar(names(x_list)))) return(x_list)
    if (length(x_list) == 1L && is.atomic(x)) return(list(formato = as.character(x[[1L]])))
    list()
  }

  # Construir label con icono de sort: activo muestra direccion, inactivo muestra neutro
  .header_label <- function(label, col_name) {
    if (!sortable_flag || is.null(sort_input_id)) return(label)
    icono <- if (!is.null(sort_col_actual) && sort_col_actual == col_name) {
      if (identical(sort_dir_actual, "asc")) " \u25b2" else " \u25bc"
    } else {
      " \u25b2\u25bc"
    }
    paste0(label, icono)
  }

  defaults <- stats::setNames(
    lapply(seq_along(nms), function(i) {
      nm    <- nms[[i]]
      specs <- normalizar_spec(col_specs[[nm]])

      if (!is.null(id_col) && nm == id_col) return(reactable::colDef(show = FALSE))

      extra_class  <- if (i %in% col_header_idx) "rt-col-header" else NULL
      label        <- specs$label    %||% nm
      formato      <- specs$formato  %||% "numero"
      min_width    <- specs$min_width %||% NULL
      max_width    <- specs$max_width %||% NULL
      color_fn     <- specs$color_fn
      alinear      <- specs$alinear  %||% NULL
      header_label <- .header_label(label, nm)

      # Header JS para captura de click de sort; NULL cuando sortable = FALSE
      header_fn <- if (sortable_flag && !is.null(sort_input_id)) {
        reactable::JS(.js_header_sort(sort_input_id, nm))
      } else {
        NULL
      }

      if (is.numeric(data[[nm]])) {
        fmt_fn <- .resolver_fmt_celda(formato)
        reactable::colDef(
          name     = header_label,
          header   = header_fn,
          class    = extra_class,
          sortable = FALSE,
          minWidth = min_width,
          maxWidth = max_width,
          align    = alinear,
          cell     = fmt_fn,
          style    = color_fn
        )
      } else if (inherits(data[[nm]], c("Date", "POSIXct", "POSIXlt"))) {
        fmt_fn <- .resolver_fmt_celda(specs$formato %||% "fecha")
        reactable::colDef(
          name     = header_label,
          header   = header_fn,
          class    = extra_class,
          sortable = FALSE,
          minWidth = min_width,
          maxWidth = max_width,
          cell     = fmt_fn,
          style    = color_fn
        )
      } else {
        reactable::colDef(
          name     = header_label,
          header   = header_fn,
          class    = extra_class,
          sortable = FALSE,
          minWidth = min_width,
          maxWidth = max_width,
          align    = alinear,
          style    = color_fn
        )
      }
    }),
    nms
  )

  if (!is.null(columnas_override)) {
    for (nm in names(columnas_override)) defaults[[nm]] <- columnas_override[[nm]]
  }

  defaults
}

#' @keywords internal
.js_row_class <- function() {
  reactable::JS("function(rowInfo) {
    var vals = Object.values(rowInfo.values).map(function(v) {
      return String(v).trim().toUpperCase();
    });
    if (vals.some(function(v) { return v === 'TOTAL'; })) return 'rt-fila-total';
    if (vals.some(function(v) { return v === 'OTROS'; })) return 'rt-fila-otros';
    return '';
  }")
}

#' @keywords internal
.normalizar_seleccion <- function(click, modo, df, id_col) {
  if (!is.list(click)) return(NULL)

  row_index <- suppressWarnings(as.integer(click[["row_index"]]))
  values    <- click[["values"]]

  switch(
    modo,
    fila = {
      id_val  <- if (!is.null(id_col) && !is.null(values)) values[[id_col]] else row_index
      fila_df <- if (!is.null(id_col) && !is.null(id_val)) {
        df[df[[id_col]] == id_val, , drop = FALSE]
      } else if (!is.na(row_index)) {
        df[row_index + 1L, , drop = FALSE]
      } else {
        df[0, , drop = FALSE]
      }
      list(modo = "fila", id = id_val, fila = fila_df)
    },
    celda = {
      id_val  <- if (!is.null(id_col) && !is.null(values)) values[[id_col]] else row_index
      fila_df <- if (!is.null(id_col) && !is.null(id_val)) {
        df[df[[id_col]] == id_val, , drop = FALSE]
      } else if (!is.na(row_index)) {
        df[row_index + 1L, , drop = FALSE]
      } else {
        df[0, , drop = FALSE]
      }
      list(
        modo  = "celda",
        id    = id_val,
        col   = click[["col"]],
        valor = click[["valor"]],
        fila  = fila_df
      )
    },
    columna = {
      col <- click[["col"]]
      list(
        modo = "columna",
        col  = col,
        data = if (!is.null(col) && col %in% names(df)) {
          df[, col, drop = FALSE]
        } else {
          df[0, , drop = FALSE]
        }
      )
    }
  )
}

#' @keywords internal
.modal_titulo_tag <- function(sel, modal_titulo_fn, modal_icon) {
  titulo_txt <- modal_titulo_fn(sel)
  if (!is.null(modal_icon) && nzchar(modal_icon)) {
    shiny::tagList(shiny::icon(modal_icon), " ", titulo_txt)
  } else {
    titulo_txt
  }
}

#' @keywords internal
.modal_footer_bloque <- function(modal_footer, modal_footer_tipo) {
  botones <- shiny::tagList(shiny::modalButton("Cerrar"))
  if (is.null(modal_footer)) return(botones)

  css <- switch(
    modal_footer_tipo,
    info    = "caja-modal-footer",
    warning = "caja-modal-footer-warning",
    dark    = "caja-modal-footer-dark",
    danger  = "caja-modal-footer-danger",
    "caja-modal-footer"
  )

  shiny::tagList(
    shiny::tags$div(
      class = css,
      if (inherits(modal_footer, "shiny.tag") ||
          inherits(modal_footer, "shiny.tag.list")) {
        modal_footer
      } else {
        as.character(modal_footer)
      }
    ),
    botones
  )
}

#' UI del modulo TablaReactable
#'
#' @param id ID del modulo Shiny.
#' @param titulo Titulo principal. `NULL` para omitir.
#' @param subtitulo Subtitulo secundario. `NULL` para omitir.
#' @param footer Contenido del footer: `NULL` | string | `tagList`.
#' @param footer_tipo Estilo del footer: `"info"` | `"warning"` | `"dark"` | `"danger"`.
#' @param sortable Activar estilos de encabezado ordenable. Debe coincidir con el server.
#'
#' @return `shiny.tag` contenedor con `reactableOutput`, nota y badge.
#' @export
TablaReactableUI <- function(
    id,
    titulo = NULL,
    subtitulo = NULL,
    footer = NULL,
    footer_tipo = c("info", "warning", "dark", "danger"),
    sortable = TRUE
) {
  ns <- shiny::NS(id)
  footer_tipo <- match.arg(footer_tipo)

  clases_contenedor <- trimws(paste(
    "rt-contenedor reactable-wrap",
    if (sortable) "rt-sortable" else ""
  ))

  header_bloque <- if (!is.null(titulo) && nzchar(titulo)) {
    shiny::tagList(
      shiny::tags$div(class = "rt-titulo", titulo),
      if (!is.null(subtitulo) && nzchar(subtitulo)) {
        shiny::tags$div(class = "rt-subtitulo", subtitulo)
      }
    )
  }

  shiny::tags$div(
    class = clases_contenedor,
    header_bloque,
    reactable::reactableOutput(ns("tabla")),
    shiny::uiOutput(ns("nota_interaccion")),
    .footer_bloque(footer, footer_tipo),
    shiny::uiOutput(ns("badge_seleccion"))
  )
}

#' Server del modulo TablaReactable
#'
#' Tabla reactable con seleccion configurable por fila, celda o columna.
#' El sort es manejado en R (server-side) para garantizar que las filas
#' definidas en `filas_fijas_al_final` siempre queden al final de la tabla
#' independientemente del ordenamiento activo.
#'
#' @param id ID del modulo Shiny.
#' @param data `reactive()` que retorna un `data.frame`.
#' @param columnas `list()` de `reactable::colDef` para override completo selectivo.
#' @param col_specs Named list con `label`, `formato`, `min_width`, `max_width`,
#'   `color_fn`, `alinear` por columna.
#' @param modo_seleccion `"fila"` | `"celda"` | `"columna"` | `"ninguno"`.
#' @param id_col Columna identificadora primaria. `NULL` usa `row_index`.
#' @param col_header_n Numero de columnas iniciales con clase `.rt-col-header`.
#' @param sortable Habilita ordenamiento server-side por encabezado.
#' @param searchable Habilita buscador global.
#' @param page_size Filas por pagina.
#' @param compact Modo compacto.
#' @param mostrar_badge Muestra resumen del ultimo click.
#' @param modal_titulo_fn Funcion `function(sel)` para titulo del modal.
#' @param modal_contenido_fn Funcion `function(sel)` para cuerpo del modal.
#' @param modal_pre_fn Hook previo sincronico antes de `showModal()`.
#' @param modal_size Tamano del modal (`"s"`, `"m"`, `"l"`, `"xl"`).
#' @param modal_icon Icono FontAwesome del titulo modal.
#' @param modal_footer Footer adicional del modal.
#' @param modal_footer_tipo Estilo del footer del modal.
#' @param cols_activos Vector de columnas habilitadas para seleccion en JS.
#' @param filas_bloqueadas Vector de valores bloqueados por fila en JS.
#' @param filas_seleccionables Restriccion en R (`vector` de IDs o funcion).
#' @param filas_fijas_al_final Vector de valores que siempre se anclan al final
#'   de la tabla sin importar el ordenamiento activo. Case-insensitive, deteccion
#'   por valor en cualquier celda de la fila. Default `c("OTROS", "TOTAL")`.
#'
#' @return Lista con `seleccion` (reactive), `modo` (string) y `limpiar` (function).
#' @export
TablaReactable <- function(
    id,
    data,
    columnas = NULL,
    col_specs = NULL,
    modo_seleccion = c("fila", "celda", "columna", "ninguno"),
    id_col = NULL,
    col_header_n = 1L,
    sortable = TRUE,
    searchable = TRUE,
    page_size = 15,
    compact = TRUE,
    mostrar_badge = FALSE,
    modal_titulo_fn = NULL,
    modal_contenido_fn = NULL,
    modal_pre_fn = NULL,
    modal_size = "xl",
    modal_icon = NULL,
    modal_footer = NULL,
    modal_footer_tipo = c("info", "warning", "dark", "danger"),
    cols_activos = NULL,
    filas_bloqueadas = NULL,
    filas_seleccionables = NULL,
    filas_fijas_al_final = c("OTROS", "TOTAL")
) {
  modo_seleccion    <- match.arg(modo_seleccion)
  modal_footer_tipo <- match.arg(modal_footer_tipo)
  col_header_n      <- max(1L, as.integer(col_header_n))

  cols_activos_json     <- .to_json_arr(cols_activos)
  filas_bloqueadas_json <- .to_json_arr(filas_bloqueadas)
  usar_modal            <- !is.null(modal_titulo_fn) && !is.null(modal_contenido_fn)

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Estado de seleccion y estado de sort activo
    seleccion_r <- shiny::reactiveVal(NULL)
    sort_estado <- shiny::reactiveVal(list(col = NULL, dir = "asc"))

    # Captura de click en encabezado; toggle asc/desc calculado en R
    shiny::observeEvent(input$sort_header, {
      shiny::req(!is.null(input$sort_header))
      col_nuevo <- input$sort_header[["col"]]
      if (is.null(col_nuevo) || !nzchar(col_nuevo)) return()
      st_actual <- sort_estado()
      # Misma columna: invertir direccion; columna nueva: iniciar en asc
      nueva_dir <- if (!is.null(st_actual$col) && st_actual$col == col_nuevo) {
        if (identical(st_actual$dir, "asc")) "desc" else "asc"
      } else {
        "asc"
      }
      sort_estado(list(col = col_nuevo, dir = nueva_dir))
    }, ignoreNULL = TRUE)

    # Dataframe ordenado: sort en R garantiza filas fijas siempre al final
    df_ordenado <- shiny::reactive({
      shiny::req(data())
      st <- sort_estado()
      df <- data()
      if (sortable && !is.null(st$col)) {
        .aplicar_sort(df, st$col, st$dir, filas_fijas_al_final)
      } else {
        df
      }
    })

    on_click <- switch(
      modo_seleccion,
      fila = reactable::JS(
        .js_fila(ns("click"), filas_bloqueadas_json = filas_bloqueadas_json)
      ),
      celda = reactable::JS(
        .js_celda(
          ns("click"),
          cols_activos_json     = cols_activos_json,
          filas_bloqueadas_json = filas_bloqueadas_json
        )
      ),
      columna = reactable::JS(
        .js_columna(ns("click"), cols_activos_json = cols_activos_json)
      ),
      ninguno = NULL
    )

    output$tabla <- reactable::renderReactable({
      shiny::req(df_ordenado())
      df <- df_ordenado()
      st <- sort_estado()

      # Coldefs con header JS para captura de sort e icono de direccion activa
      coldefs <- .coldefs_default(
        data              = df,
        columnas_override = columnas,
        col_specs         = col_specs,
        id_col            = id_col,
        col_header_n      = col_header_n,
        sortable_flag     = sortable,
        sort_input_id     = if (sortable) ns("sort_header") else NULL,
        sort_col_actual   = st$col,
        sort_dir_actual   = st$dir
      )

      reactable::reactable(
        data            = df,
        columns         = coldefs,
        onClick         = on_click,
        rowClass        = .js_row_class(),
        sortable        = FALSE,
        highlight       = modo_seleccion != "ninguno",
        searchable      = searchable,
        defaultPageSize = page_size,
        compact         = compact,
        bordered        = TRUE,
        striped         = FALSE,
        language = reactable::reactableLang(
          searchPlaceholder = "Buscar...",
          noData            = "Sin resultados",
          pageInfo          = "{rowStart}\u2013{rowEnd} de {rows} registros",
          pagePrevious      = "Anterior",
          pageNext          = "Siguiente"
        ),
        theme = reactable::reactableTheme(
          headerStyle = list(
            fontWeight = "600",
            fontSize   = "12px",
            cursor     = if (sortable) "pointer" else "default"
          ),
          cellStyle = list(
            fontSize = "12px",
            padding  = "3px 6px",
            cursor   = if (modo_seleccion != "ninguno") "pointer" else "default"
          )
        )
      )
    })

    # Nota de interaccion con restricciones activas
    output$nota_interaccion <- shiny::renderUI({
      .nota_interaccion(
        modo_seleccion       = modo_seleccion,
        sortable             = sortable,
        cols_activos         = cols_activos,
        filas_bloqueadas     = filas_bloqueadas,
        filas_seleccionables = filas_seleccionables
      )
    })

    shiny::observeEvent(input$click, {
      shiny::req(!is.null(input$click))

      # Normalizar contra data() original para mantener consistencia de IDs
      sel <- .normalizar_seleccion(input$click, modo_seleccion, data(), id_col)
      if (is.null(sel)) return()

      if (!is.null(filas_seleccionables) && sel$modo %in% c("fila", "celda")) {
        permitida <- if (is.function(filas_seleccionables)) {
          isTRUE(filas_seleccionables(sel$fila))
        } else {
          isTRUE(as.character(sel$id) %in% as.character(filas_seleccionables))
        }
        if (!permitida) return()
      }

      seleccion_r(sel)

      if (usar_modal) {
        if (!is.null(modal_pre_fn)) modal_pre_fn(sel)
        shiny::showModal(shiny::modalDialog(
          title     = .modal_titulo_tag(sel, modal_titulo_fn, modal_icon),
          size      = modal_size,
          easyClose = TRUE,
          footer    = .modal_footer_bloque(modal_footer, modal_footer_tipo),
          modal_contenido_fn(sel)
        ))
      }
    }, ignoreNULL = TRUE)

    output$badge_seleccion <- shiny::renderUI({
      shiny::req(mostrar_badge, !is.null(seleccion_r()))
      sel <- seleccion_r()
      txt <- switch(
        sel$modo,
        fila    = paste0("Fila: ", sel$id),
        celda   = paste0("Celda [", sel$col, "] = ", sel$valor, "  \u2014  ID: ", sel$id),
        columna = paste0("Columna: ", sel$col)
      )
      shiny::tags$p(
        shiny::icon("mouse-pointer"),
        shiny::tags$span(txt, style = "font-size:11px; color:#555; margin-left:4px;"),
        style = "margin-top:6px; margin-bottom:0;"
      )
    })

    list(
      seleccion = shiny::reactive(seleccion_r()),
      modo      = modo_seleccion,
      limpiar   = function() seleccion_r(NULL)
    )
  })
}
