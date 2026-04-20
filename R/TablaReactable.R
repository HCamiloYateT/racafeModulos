# TablaReactable helpers ----

#' Convierte un vector R en un arreglo JSON string para JS
#' @keywords internal
.to_json_arr <- function(x) {
  if (is.null(x) || length(x) == 0) return("[]")
  paste0('["', paste(as.character(x), collapse = '","'), '"]')
}

#' Bloque footer con clase CSS segun tipo semantico
#' @keywords internal
.footer_bloque <- function(footer, footer_tipo = "info") {
  if (is.null(footer) || !nzchar(as.character(footer))) return(NULL)
  clase <- switch(
    footer_tipo,
    warning = "caja-modal-footer-warning",
    dark = "caja-modal-footer-dark",
    danger = "caja-modal-footer-danger",
    "caja-modal-footer"
  )
  shiny::tags$div(class = clase, footer)
}

#' Footer del modal: nota metodologica + boton cerrar
#' @keywords internal
.modal_footer_bloque <- function(footer, footer_tipo = "info") {
  if (is.null(footer)) return(shiny::modalButton("Cerrar"))
  shiny::tagList(.footer_bloque(footer, footer_tipo), shiny::modalButton("Cerrar"))
}

#' Titulo del modal con icono opcional
#' @keywords internal
.modal_titulo_tag <- function(sel, modal_titulo_fn, modal_icon) {
  txt <- if (is.function(modal_titulo_fn)) modal_titulo_fn(sel) else "Detalle"
  if (!is.null(modal_icon)) shiny::tagList(shiny::icon(modal_icon), txt) else txt
}

#' Nota de interaccion automatica debajo de la tabla
#' @keywords internal
.nota_interaccion <- function(modo_seleccion, sortable) {
  if (modo_seleccion == "ninguno" && !isTRUE(sortable)) return(NULL)
  txts <- c(
    if (modo_seleccion == "fila") "Clic en fila para seleccionar",
    if (modo_seleccion == "celda") "Clic en celda para seleccionar",
    if (modo_seleccion == "columna") "Clic en celda para capturar columna",
    if (isTRUE(sortable)) "Encabezados ordenables"
  )
  shiny::tags$div(class = "caja-modal-footer", paste(txts, collapse = " • "))
}

#' JS handler: click de fila; respeta lista de indices bloqueados
#' @keywords internal
.js_fila <- function(input_id, filas_bloqueadas_json = "[]") {
  paste0(
    "function(rowInfo){",
    "var b=", filas_bloqueadas_json, ",i=String(rowInfo.index);",
    "if(b.length&&b.indexOf(i)>=0)return;",
    "Shiny.setInputValue('", input_id, "',",
    "{modo:'fila',index:rowInfo.index,values:rowInfo.values},",
    "{priority:'event'});}"
  )
}

#' JS handler: click de celda; filtra columnas activas y filas bloqueadas
#' @keywords internal
.js_celda <- function(input_id, cols_activos_json = "[]", filas_bloqueadas_json = "[]") {
  paste0(
    "function(rowInfo,colInfo){",
    "var a=", cols_activos_json, ",b=", filas_bloqueadas_json, ";",
    "if(a.length&&a.indexOf(colInfo.id)<0)return;",
    "if(b.length&&b.indexOf(String(rowInfo.index))>=0)return;",
    "Shiny.setInputValue('", input_id, "',",
    "{modo:'celda',index:rowInfo.index,col:colInfo.id,values:rowInfo.values},",
    "{priority:'event'});}"
  )
}

#' JS handler: click de columna; captura nombre de columna activa
#' @keywords internal
.js_columna <- function(input_id, cols_activos_json = "[]") {
  paste0(
    "function(rowInfo,colInfo){",
    "var a=", cols_activos_json, ";",
    "if(a.length&&a.indexOf(colInfo.id)<0)return;",
    "Shiny.setInputValue('", input_id, "',",
    "{modo:'columna',col:colInfo.id},{priority:'event'});}"
  )
}

#' JS rowClass: asigna clase CSS a filas 'otros' y 'total' via .row_type
#' @keywords internal
.js_row_class <- function() {
  reactable::JS(paste0(
    "function(rowInfo){",
    "var t=rowInfo.values['.row_type'];",
    "if(t==='total')return 'rt-fila-total';",
    "if(t==='otros')return 'rt-fila-otros';",
    "return '';}"
  ))
}

#' Ordena df manteniendo filas 'otros'/'total' siempre al fondo
#' @keywords internal
.ordenar_con_pin <- function(df, col = NULL, desc = FALSE) {
  tiene_tipo <- ".row_type" %in% names(df)
  if (!tiene_tipo) {
    if (!is.null(col) && col %in% names(df)) {
      return(df[order(df[[col]], decreasing = desc, na.last = TRUE), , drop = FALSE])
    }
    return(df)
  }

  mask_esp <- df$.row_type %in% c("otros", "total")
  df_norm <- df[!mask_esp, , drop = FALSE]
  df_esp <- df[mask_esp, , drop = FALSE]

  if (!is.null(col) && col %in% names(df_norm) && col != ".row_type") {
    df_norm <- df_norm[order(df_norm[[col]], decreasing = desc, na.last = TRUE), , drop = FALSE]
  }

  if (nrow(df_esp) > 0) {
    df_esp <- df_esp[
      order(match(df_esp$.row_type, c("otros", "total")), na.last = TRUE),
      ,
      drop = FALSE
    ]
  }

  rbind(df_norm, df_esp)
}

#' Construye colDefs con sort R-side y headers custom; oculta .row_type
#' @keywords internal
.coldefs_default <- function(
    data,
    columnas_override,
    col_specs,
    id_col,
    col_header_n,
    sort_col = NULL,
    sort_desc = FALSE,
    ns_sort = NULL
) {
  nms_todos <- names(data)
  nms_visibles <- if (!is.null(columnas_override)) columnas_override else nms_todos
  nms_visibles <- nms_visibles[!nms_visibles %in% ".row_type"]

  coldefs <- stats::setNames(
    lapply(nms_todos, function(nm) reactable::colDef(show = FALSE)),
    nms_todos
  )

  for (i in seq_along(nms_visibles)) {
    local({
      nm_i <- nms_visibles[[i]]
      sc <- sort_col
      sd <- sort_desc
      nss <- ns_sort
      cd <- if (!is.null(col_specs) && !is.null(col_specs[[nm_i]])) {
        col_specs[[nm_i]]
      } else {
        reactable::colDef()
      }

      cd$sortable <- FALSE
      if (i <= col_header_n) {
        cd$class <- trimws(paste(if (!is.null(cd$class)) cd$class else "", "rt-col-header"))
      }

      if (!is.null(nss)) {
        cd$header <- function(value) {
          ind <- if (!is.null(sc) && nm_i == sc) {
            shiny::tags$span(
              if (sd) "▼" else "▲",
              style = "font-size:9px;color:#d90429;margin-left:3px;vertical-align:middle;"
            )
          }

          shiny::tags$div(
            style = "cursor:pointer;display:inline-flex;align-items:center;gap:2px;",
            onclick = sprintf(
              "event.stopPropagation();Shiny.setInputValue('%s',{col:'%s'},{priority:'event'});",
              nss,
              nm_i
            ),
            value,
            ind
          )
        }
      }

      coldefs[[nm_i]] <<- cd
    })
  }

  coldefs
}

#' Interpolacion heatmap azul claro a azul oscuro
#' @keywords internal
.hm_bg <- function(n) {
  sprintf(
    "rgb(%d,%d,%d)",
    as.integer(230L - n * 226L),
    as.integer(241L - n * 197L),
    as.integer(251L - n * 168L)
  )
}

#' @keywords internal
.hm_txt <- function(n) if (n > 0.55) "#E6F1FB" else "#042C53"

#' Inyecta estilo heatmap en colDef
#' @keywords internal
.overlay_heatmap <- function(cd, col_vals) {
  rng <- range(col_vals, na.rm = TRUE)
  if (!is.finite(diff(rng)) || diff(rng) == 0) return(cd)
  mn <- rng[1]
  mx <- rng[2]

  cd$style <- function(value) {
    if (!is.numeric(value) || is.na(value)) return(list())
    n <- (value - mn) / (mx - mn)
    list(background = .hm_bg(n), color = .hm_txt(n), fontWeight = "500")
  }

  cd
}

#' Inyecta color positivo/negativo en colDef segun umbral
#' @keywords internal
.overlay_valor_color <- function(cd, umbral = 0) {
  u <- umbral
  cd$style <- function(value) {
    if (!is.numeric(value) || is.na(value)) return(list())
    if (value > u) return(list(color = "#3B6D11", fontWeight = "500"))
    if (value < u) return(list(color = "#A32D2D", fontWeight = "500"))
    list(color = "inherit")
  }
  cd
}

#' Resuelve columnas de estilo desde spec
#' @keywords internal
.resolver_cols_estilo <- function(spec, df) {
  if (is.null(spec)) return(character(0))
  if (identical(spec, "auto")) return(names(df)[vapply(df, is.numeric, logical(1))])
  intersect(as.character(spec), names(df))
}

#' @keywords internal
.aplicar_heatmap <- function(coldefs, cols, df) {
  for (col in cols) {
    base <- if (!is.null(coldefs[[col]])) coldefs[[col]] else reactable::colDef()
    coldefs[[col]] <- .overlay_heatmap(base, df[[col]])
  }
  coldefs
}

#' @keywords internal
.aplicar_valor_color <- function(coldefs, cols, umbral = 0) {
  for (col in cols) {
    base <- if (!is.null(coldefs[[col]])) coldefs[[col]] else reactable::colDef()
    coldefs[[col]] <- .overlay_valor_color(base, umbral)
  }
  coldefs
}

#' Normaliza payload JS a estructura estandar de seleccion R
#' @keywords internal
.normalizar_seleccion <- function(click, modo_seleccion, df, id_col) {
  if (is.null(click)) return(NULL)
  if (click$modo == "columna") return(list(modo = "columna", col = click$col))

  idx <- as.integer(click$index) + 1L
  if (is.na(idx) || idx < 1L || idx > nrow(df)) return(NULL)

  fila_data <- as.list(df[idx, , drop = FALSE])
  id_val <- if (!is.null(id_col) && id_col %in% names(df)) df[[id_col]][idx] else idx

  if (click$modo == "fila") {
    return(list(modo = "fila", id = id_val, index = idx, fila = fila_data))
  }

  if (click$modo == "celda") {
    col_nm <- click$col
    valor <- if (!is.null(col_nm) && col_nm %in% names(df)) df[[col_nm]][idx] else NA
    return(list(
      modo = "celda",
      id = id_val,
      col = col_nm,
      valor = valor,
      index = idx,
      fila = fila_data
    ))
  }

  NULL
}

#' UI del modulo TablaReactable
#'
#' @param id ID del modulo Shiny.
#' @param titulo Titulo principal. `NULL` para omitir.
#' @param subtitulo Subtitulo secundario. `NULL` para omitir.
#' @param footer Contenido del footer: `NULL` | string | `tagList`.
#' @param footer_tipo Estilo del footer: `"info"` | `"warning"` | `"dark"` | `"danger"`.
#' @param sortable Activar estilos de encabezado ordenable. Debe coincidir con el server.
#' @param mostrar_nota Mostrar nota de interaccion debajo de la tabla.
#' @param estilo Estilo visual de la tabla.
#'
#' @return `shiny.tag` contenedor con `reactableOutput`, nota y badge.
#' @export
TablaReactableUI <- function(
    id,
    titulo = NULL,
    subtitulo = NULL,
    footer = NULL,
    footer_tipo = c("info", "warning", "dark", "danger"),
    sortable = TRUE,
    mostrar_nota = TRUE,
    estilo = c("analitica", "minimal", "striped", "financial", "accent", "ghost")
) {
  ns <- shiny::NS(id)
  footer_tipo <- match.arg(footer_tipo)
  estilo <- match.arg(estilo)

  clases_contenedor <- trimws(paste(
    "rt-contenedor reactable-wrap",
    paste0("rt-estilo-", estilo),
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
    if (isTRUE(mostrar_nota)) shiny::uiOutput(ns("nota_interaccion")),
    .footer_bloque(footer, footer_tipo),
    shiny::uiOutput(ns("badge_seleccion"))
  )
}

#' Server del modulo TablaReactable
#'
#' @param id ID del modulo Shiny.
#' @param data `reactive()` que retorna un `data.frame`.
#' @param columnas Vector de columnas visibles. Si `NULL`, usa todas.
#' @param col_specs Named list con overrides de `reactable::colDef` por columna.
#' @param modo_seleccion `"fila"` | `"celda"` | `"columna"` | `"ninguno"`.
#' @param id_col Columna identificadora primaria. `NULL` usa `row_index`.
#' @param col_header_n Numero de columnas iniciales con clase `.rt-col-header`.
#' @param sortable Habilita ordenamiento por encabezado (R-side).
#' @param searchable Habilita buscador global.
#' @param page_size Filas por pagina.
#' @param compact Modo compacto.
#' @param mostrar_badge Muestra resumen del ultimo click.
#' @param mostrar_nota Mostrar nota de interaccion debajo de la tabla.
#' @param modal_titulo_fn Funcion `function(sel)` para titulo del modal.
#' @param modal_contenido_fn Funcion `function(sel)` para cuerpo del modal.
#' @param modal_pre_fn Hook previo sincronico antes de `showModal()`.
#' @param modal_size Tamano del modal (`"s"`, `"m"`, `"l"`, `"xl"`).
#' @param modal_icon Icono FontAwesome del titulo modal.
#' @param modal_footer Footer adicional del modal.
#' @param modal_footer_tipo Estilo del footer del modal.
#' @param cols_activos Vector de columnas habilitadas para seleccion en JS.
#' @param filas_bloqueadas Vector de indices bloqueados en JS.
#' @param filas_seleccionables Restriccion en R (`vector` de IDs o funcion).
#' @param cols_heatmap Columnas para overlay heatmap (`NULL`, `"auto"` o vector).
#' @param cols_valor_color Columnas para color positivo/negativo (`NULL`, `"auto"` o vector).
#' @param umbral_valor_color Umbral para `cols_valor_color`.
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
    mostrar_nota = TRUE,
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
    cols_heatmap = NULL,
    cols_valor_color = NULL,
    umbral_valor_color = 0
) {
  modo_seleccion <- match.arg(modo_seleccion)
  modal_footer_tipo <- match.arg(modal_footer_tipo)
  col_header_n <- max(1L, as.integer(col_header_n))

  cols_activos_json <- .to_json_arr(cols_activos)
  filas_bloqueadas_json <- .to_json_arr(filas_bloqueadas)
  usar_modal <- !is.null(modal_titulo_fn) && !is.null(modal_contenido_fn)

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    seleccion_r <- shiny::reactiveVal(NULL)
    sort_state_r <- shiny::reactiveVal(list(col = NULL, desc = FALSE))

    shiny::observeEvent(input$sort_click, {
      shiny::req(!is.null(input$sort_click$col))
      cur <- sort_state_r()
      nueva_col <- input$sort_click$col
      nueva_desc <- if (!is.null(cur$col) && cur$col == nueva_col) !cur$desc else FALSE
      sort_state_r(list(col = nueva_col, desc = nueva_desc))
    })

    df_sorted_r <- shiny::reactive({
      srt <- sort_state_r()
      .ordenar_con_pin(data(), srt$col, isTRUE(srt$desc))
    })

    on_click <- switch(
      modo_seleccion,
      fila = reactable::JS(.js_fila(
        ns("click"), filas_bloqueadas_json = filas_bloqueadas_json
      )),
      celda = reactable::JS(.js_celda(
        ns("click"),
        cols_activos_json = cols_activos_json,
        filas_bloqueadas_json = filas_bloqueadas_json
      )),
      columna = reactable::JS(.js_columna(
        ns("click"), cols_activos_json = cols_activos_json
      )),
      ninguno = NULL
    )

    output$tabla <- reactable::renderReactable({
      shiny::req(data())
      df <- df_sorted_r()
      srt <- sort_state_r()

      coldefs <- .coldefs_default(
        data = df,
        columnas_override = columnas,
        col_specs = col_specs,
        id_col = id_col,
        col_header_n = col_header_n,
        sort_col = srt$col,
        sort_desc = isTRUE(srt$desc),
        ns_sort = if (isTRUE(sortable)) ns("sort_click") else NULL
      )

      cols_hm <- .resolver_cols_estilo(cols_heatmap, df)
      coldefs <- .aplicar_heatmap(coldefs, cols_hm, df)
      cols_vc <- .resolver_cols_estilo(cols_valor_color, df)
      coldefs <- .aplicar_valor_color(coldefs, cols_vc, umbral_valor_color)

      reactable::reactable(
        data = df,
        columns = coldefs,
        onClick = on_click,
        rowClass = .js_row_class(),
        sortable = FALSE,
        highlight = modo_seleccion != "ninguno",
        searchable = searchable,
        defaultPageSize = page_size,
        compact = compact,
        bordered = TRUE,
        striped = FALSE,
        language = reactable::reactableLang(
          searchPlaceholder = "Buscar...",
          noData = "Sin resultados",
          pageInfo = "{rowStart}–{rowEnd} de {rows} registros",
          pagePrevious = "Anterior",
          pageNext = "Siguiente"
        ),
        theme = reactable::reactableTheme(
          headerStyle = list(fontWeight = "600", fontSize = "12px"),
          cellStyle = list(
            fontSize = "12px",
            padding = "3px 6px",
            cursor = if (modo_seleccion != "ninguno") "pointer" else "default"
          )
        )
      )
    })

    output$nota_interaccion <- shiny::renderUI({
      if (!isTRUE(mostrar_nota)) return(NULL)
      .nota_interaccion(modo_seleccion, sortable)
    })

    shiny::observeEvent(input$click, {
      shiny::req(!is.null(input$click))
      sel <- .normalizar_seleccion(input$click, modo_seleccion, df_sorted_r(), id_col)
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
          title = .modal_titulo_tag(sel, modal_titulo_fn, modal_icon),
          size = modal_size,
          easyClose = TRUE,
          footer = .modal_footer_bloque(modal_footer, modal_footer_tipo),
          modal_contenido_fn(sel)
        ))
      }
    }, ignoreNULL = TRUE)

    output$badge_seleccion <- shiny::renderUI({
      shiny::req(mostrar_badge, !is.null(seleccion_r()))
      sel <- seleccion_r()
      txt <- switch(
        sel$modo,
        fila = paste0("Fila: ", sel$id),
        celda = paste0("Celda [", sel$col, "] = ", sel$valor, "  —  ID: ", sel$id),
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
      modo = modo_seleccion,
      limpiar = function() seleccion_r(NULL)
    )
  })
}
