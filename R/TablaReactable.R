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
#' Componente de interfaz para renderizar una tabla `reactable` con header
#' opcional (titulo/subtitulo), nota de interaccion, footer semantico y badge
#' reactivo de seleccion.
#'
#' @param id ID del modulo Shiny.
#' @param titulo Titulo principal. `NULL` para omitir.
#' @param subtitulo Subtitulo secundario. `NULL` para omitir.
#' @param footer Contenido del footer: `NULL` | string | `tagList`.
#' @param footer_tipo Estilo del footer: `"info"` | `"warning"` | `"dark"` | `"danger"`.
#' @param sortable Activar estilos de encabezado ordenable. Debe coincidir con el server.
#' @param mostrar_nota Mostrar nota de interaccion debajo de la tabla.
#' @param estilo Estilo visual de la tabla: `"minimal"` o `"minimal2"`.
#'
#' @return `shiny.tag` contenedor con `reactableOutput`, nota y badge.
#' @export
#'
#' @examples
#' TablaReactableUI(
#'   id = "tabla_ventas",
#'   titulo = "Ventas por segmento",
#'   subtitulo = "Haz clic para ver detalle",
#'   estilo = "minimal2"
#' )
TablaReactableUI <- function(
    id,
    titulo = NULL,
    subtitulo = NULL,
    footer = NULL,
    footer_tipo = c("info", "warning", "dark", "danger"),
    sortable = TRUE,
    mostrar_nota = TRUE,
    estilo = c("minimal", "minimal2")
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
#' Renderiza una `reactable` con seleccion normalizada para consumo en el
#' modulo padre. Opcionalmente dispara un modal con contenido custom y soporta
#' estilos condicionales por columna (heatmap y color por umbral).
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
#' @return Lista con:
#' * `seleccion`: `reactive()` con lista normalizada (`modo`, `id`, `fila`, etc.).
#' * `modo`: string con el modo activo de seleccion.
#' * `limpiar`: funcion para resetear seleccion y badge.
#' @export
#'
#' @examples
#' # En UI:
#' # TablaReactableUI("tabla_demo", titulo = "Clientes")
#' #
#' # En server:
#' # out <- TablaReactable(
#' #   id = "tabla_demo",
#' #   data = reactive(df_clientes),
#' #   modo_seleccion = "fila",
#' #   id_col = "cliente_id",
#' #   modal_titulo_fn = function(sel) paste("Detalle", sel$id),
#' #   modal_contenido_fn = function(sel) shiny::tags$pre(str(sel))
#' # )
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



#' DemoTablaReactable: app de demostracion del modulo TablaReactable
#'
#' Ejecuta una aplicacion Shiny autocontenida que recorre escenarios de uso
#' comunes de [TablaReactable()]:
#' \itemize{
#'   \item Estilos `minimal` y `minimal2`.
#'   \item Seleccion por fila con modal integrado.
#'   \item Seleccion por celda y columna.
#'   \item Acciones explicitas con boton embebido en celda.
#' }
#'
#' Esta demo incluye bloques colapsables con codigo de referencia para UI y
#' server, utiles para acelerar integraciones en aplicaciones reales.
#'
#' @return Ejecuta `shiny::shinyApp()` (sin valor de retorno).
#' @export
#'
#' @examples
#' \dontrun{
#' DemoTablaReactable()
#' }

DemoTablaReactable <- function() {
  # Dependencia explicita para ejecucion interactiva de la demo
  library(shiny)

  # DATOS ----

  # Dataset base: .row_type es obligatorio para pinear filas especiales
  # Valores validos: "" (normal), "otros", "total"
  .dat <- tibble::tibble(
    segmento  = c("Microempresa", "Pequena empresa", "Mediana empresa",
                  "Gran empresa", "Corporativo", "Otros", "TOTAL"),
    creditos  = c(4821L, 2103L, 847L, 412L, 198L, 312L, 8693L),
    cartera   = c(12.4, 28.7, 54.1, 87.3, 142.6, 6.8, 331.9),
    mora_pct  = c(3.2, 1.8, 0.9, 0.4, 0.2, 4.1, 1.6),
    delta_mom = c(0.3, -0.1, -0.2, 0.4, -0.3, 0.5, 0.1),
    .row_type = c("", "", "", "", "", "otros", "total")
  )

  # Dataset con columna boton para tab de accion explicita
  .dat_btn <- tibble::tibble(
    segmento  = c("Microempresa", "Pequena empresa", "Mediana empresa",
                  "Gran empresa", "Corporativo", "Otros", "TOTAL"),
    creditos  = c(4821L, 2103L, 847L, 412L, 198L, 312L, 8693L),
    cartera   = c(12.4, 28.7, 54.1, 87.3, 142.6, 6.8, 331.9),
    mora_pct  = c(3.2, 1.8, 0.9, 0.4, 0.2, 4.1, 1.6),
    delta_mom = c(0.3, -0.1, -0.2, 0.4, -0.3, 0.5, 0.1),
    ver       = "Ver detalle",
    .row_type = c("", "", "", "", "", "otros", "total")
  )

  # Filas reales (sin pinadas) habilitadas para seleccion
  .filas_ok <- c("Microempresa", "Pequena empresa", "Mediana empresa",
                 "Gran empresa", "Corporativo")

  # HELPERS UI ----

  # Bloque de texto explicativo con icono y nota sobre .row_type
  .bloque_info <- function(..., nota_row_type = TRUE) {
    shiny::tags$div(
      style = paste0(
        "background:#EFF6FF;border-left:4px solid #1D4ED8;",
        "border-radius:4px;padding:12px 16px;margin-bottom:14px;",
        "font-size:12.5px;line-height:1.6;color:#1E3A5F;"
      ),
      ...,
      if (isTRUE(nota_row_type)) shiny::tags$p(
        style = "margin-top:8px;margin-bottom:0;font-size:11.5px;color:#555;",
        shiny::icon("triangle-exclamation"),
        shiny::tags$strong(" Requisito del dataset: "),
        "la columna ",
        shiny::tags$code(".row_type"),
        ' debe existir con valores ',
        shiny::tags$code('""'),
        ' (fila normal), ',
        shiny::tags$code('"otros"'),
        ' y ',
        shiny::tags$code('"total"'),
        ". Las filas marcadas se pinean siempre al fondo independientemente del orden."
      )
    )
  }

  # Tarjeta colapsable con bloque de codigo fuente
  .card_codigo <- function(titulo, codigo) {
    bs4Dash::bs4Card(
      width = 12, collapsible = TRUE, collapsed = TRUE, status = "white",
      title = shiny::tagList(shiny::icon("code"), paste(" Codigo:", titulo)),
      shiny::tags$pre(
        style = paste0(
          "background:#F8F9FA;border:1px solid #DEE2E6;border-radius:4px;",
          "padding:12px;font-size:11px;overflow-x:auto;margin:0;"
        ),
        shiny::tags$code(codigo)
      )
    )
  }

  # MODULO AUXILIAR: KPIs de fila en modal ----

  # UI: delega render a uiOutput interno
  #' UI del modulo interno de detalle de fila
  #'
  #' Crea un `uiOutput` namespaced para renderizar los KPIs de la fila
  #' seleccionada dentro de un modal.
  #'
  #' @param id String. ID del modulo Shiny.
  #'
  #' @return `shiny.tag` con `uiOutput` del modulo.
  #' @keywords internal
  .DetalleFilaUI <- function(id) shiny::uiOutput(shiny::NS(id)("contenido"))

  # Server: construye KPIs desde reactivo fila_r
  #' Server del modulo interno de detalle de fila
  #'
  #' Recibe una fila reactiva y renderiza un bloque de KPIs resumidos
  #' (creditos, cartera y mora) para mostrar en modal.
  #'
  #' @param id String. ID del modulo Shiny.
  #' @param fila_r Reactive que retorna la fila seleccionada.
  #'
  #' @return Nada (side-effects de `moduleServer`).
  #' @keywords internal
  .DetalleFila <- function(id, fila_r) {
    shiny::moduleServer(id, function(input, output, session) {
      output$contenido <- shiny::renderUI({
        f <- fila_r()
        shiny::req(!is.null(f))
        shiny::tagList(
          shiny::tags$h5(
            shiny::icon("layer-group"),
            shiny::tags$strong(f$segmento[[1]])
          ),
          shiny::fluidRow(
            shiny::column(4,
                          shiny::tags$div(class = "caja-modal-footer",
                                          shiny::tags$div("Creditos"),
                                          shiny::tags$div(
                                            shiny::tags$strong(format(f$creditos[[1]], big.mark = ","))
                                          )
                          )
            ),
            shiny::column(4,
                          shiny::tags$div(class = "caja-modal-footer",
                                          shiny::tags$div("Cartera MM COP"),
                                          shiny::tags$div(shiny::tags$strong(f$cartera[[1]]))
                          )
            ),
            shiny::column(4,
                          shiny::tags$div(class = "caja-modal-footer",
                                          shiny::tags$div("Mora %"),
                                          shiny::tags$div(
                                            shiny::tags$strong(paste0(f$mora_pct[[1]], "%"))
                                          )
                          )
            )
          )
        )
      })
    })
  }

  # colDef para columna boton HTML; filas pinadas muestran guion
  .coldef_boton <- function() {
    reactable::colDef(
      name    = "",
      width   = 110,
      html    = TRUE,
      cell    = function(value, index, name) {
        if (is.na(value) || !nzchar(trimws(as.character(value)))) return("\u2014")
        as.character(shiny::tags$button(
          style = paste0(
            "background:#1D4ED8;color:#fff;border:none;border-radius:4px;",
            "padding:2px 10px;font-size:11px;cursor:pointer;"
          ),
          shiny::icon("circle-info"), " Ver"
        ))
      }
    )
  }

  # UI ----
  ui <- bs4Dash::bs4DashPage(
    title   = "Demo TablaReactable",  dark = NULL, scrollToTop = TRUE, help = NULL,
    header  = bs4Dash::bs4DashNavbar(title = "Demo TablaReactable"),
    sidebar = bs4Dash::bs4DashSidebar(status = "danger", expandOnHover = FALSE,
      bs4Dash::bs4SidebarMenu(
        id = "tabs_demo",
        bs4Dash::bs4SidebarMenuItem("Estilos",               tabName = "tab_estilos", icon = shiny::icon("palette")),
        bs4Dash::bs4SidebarMenuItem("Seleccion fila + modal", tabName = "tab_fila",    icon = shiny::icon("hand-pointer")),
        bs4Dash::bs4SidebarMenuItem("Seleccion celda",        tabName = "tab_celda",   icon = shiny::icon("table-cells")),
        bs4Dash::bs4SidebarMenuItem("Boton en celda",         tabName = "tab_btn",     icon = shiny::icon("circle-info"))
      )
    ),
    footer  = bs4Dash::bs4DashFooter(),
    body    = bs4Dash::bs4DashBody(
      includeCSS("https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/Styles/style.css"),
      includeScript("https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/Scripts/scripts"),
      bs4Dash::bs4TabItems(
        # TAB 1: estilos minimal / minimal2 ----
        bs4Dash::bs4TabItem(tabName = "tab_estilos",
                            shiny::br(),
                            shiny::fluidRow(
                              bs4Dash::bs4Card(width = 12, collapsible = FALSE,
                                               .bloque_info(
                                                 shiny::tags$p(
                                                   style = "margin:0 0 4px 0;",
                                                   shiny::icon("palette"),
                                                   shiny::tags$strong(" Estilos visuales: minimal y minimal2")
                                                 ),
                                                 shiny::tags$p(
                                                   style = "margin:0;",
                                                   "El parametro ", shiny::tags$code("estilo"),
                                                   " en ", shiny::tags$code("TablaReactableUI()"),
                                                   " controla la apariencia general de la tabla.",
                                                   " ", shiny::tags$strong("minimal"),
                                                   " usa fondo blanco con bordes sutiles.",
                                                   " ", shiny::tags$strong("minimal2"),
                                                   " agrega encabezado con fondo coloreado.",
                                                   " Ambos soportan heatmap en columnas numericas y",
                                                   " coloreado positivo/negativo via ",
                                                   shiny::tags$code("cols_valor_color"), "."
                                                 )
                                               )
                              )
                            ),
                            shiny::fluidRow(
                              bs4Dash::bs4Card(
                                width = 6, collapsible = FALSE, solidHeader = TRUE, status = "white",
                                title = "Estilo: minimal",
                                TablaReactableUI("t_est_minimal",
                                                 titulo    = "Cartera por segmento",
                                                 subtitulo = "Heatmap cartera • Color delta mom",
                                                 estilo    = "minimal"
                                )
                              ),
                              bs4Dash::bs4Card(
                                width = 6, collapsible = FALSE, solidHeader = TRUE, status = "white",
                                title = "Estilo: minimal2",
                                TablaReactableUI("t_est_minimal2",
                                                 titulo    = "Cartera por segmento",
                                                 subtitulo = "Heatmap cartera • Color delta mom",
                                                 estilo    = "minimal2"
                                )
                              )
                            ),
                            shiny::fluidRow(
                              .card_codigo("estilos minimal y minimal2", paste0(
                                '# minimal\n',
                                'TablaReactableUI("t_est_minimal", titulo = "Cartera por segmento",\n',
                                '  subtitulo = "Heatmap cartera", estilo = "minimal")\n\n',
                                'TablaReactable("t_est_minimal", data = reactive(.dat),\n',
                                '  id_col = "segmento", modo_seleccion = "ninguno",\n',
                                '  cols_heatmap = "cartera", cols_valor_color = "delta_mom")\n\n',
                                '# minimal2: identico, cambia estilo = "minimal2" en UI'
                              ))
                            )
        ),

        # TAB 2: seleccion de fila con modal integrado ----
        bs4Dash::bs4TabItem(tabName = "tab_fila",
                            shiny::br(),
                            shiny::fluidRow(
                              bs4Dash::bs4Card(width = 12, collapsible = FALSE,
                                               .bloque_info(
                                                 shiny::tags$p(
                                                   style = "margin:0 0 4px 0;",
                                                   shiny::icon("hand-pointer"),
                                                   shiny::tags$strong(" Seleccion de fila con modal integrado")
                                                 ),
                                                 shiny::tags$p(
                                                   style = "margin:0;",
                                                   "Al pasar ", shiny::tags$code("modal_titulo_fn"),
                                                   " y ", shiny::tags$code("modal_contenido_fn"),
                                                   " al server, cada click de fila dispara automaticamente",
                                                   " un ", shiny::tags$code("showModal()"), ".",
                                                   " El parametro ", shiny::tags$code("filas_seleccionables"),
                                                   " restringe que filas pueden abrir el modal;",
                                                   " las filas ", shiny::tags$code('"otros"'), " y ",
                                                   shiny::tags$code('"total"'), " quedan bloqueadas.",
                                                   " El modulo auxiliar vive fuera del closure del modal",
                                                   " para evitar reinstanciacion en cada apertura."
                                                 )
                                               )
                              )
                            ),
                            shiny::fluidRow(
                              bs4Dash::bs4Card(
                                width = 8, collapsible = FALSE, solidHeader = TRUE, status = "white",
                                title = "Click en fila — Otros y TOTAL no abren modal",
                                TablaReactableUI("t_fila",
                                                 titulo    = "Cartera por segmento",
                                                 subtitulo = "Solo segmentos reales activan el modal",
                                                 footer    = "Heatmap: cartera • Color: delta mom",
                                                 estilo    = "minimal"
                                )
                              ),
                              bs4Dash::bs4Card(
                                width = 4, collapsible = FALSE, title = "Seleccion activa",
                                shiny::verbatimTextOutput("sel_fila")
                              )
                            ),
                            shiny::fluidRow(
                              .card_codigo("seleccion fila + modal integrado", paste0(
                                'fila_modal_rv <- reactiveVal(NULL)\n\n',
                                'mod <- TablaReactable(\n',
                                '  id                   = "t_fila",\n',
                                '  data                 = reactive(.dat),\n',
                                '  id_col               = "segmento",\n',
                                '  modo_seleccion       = "fila",\n',
                                '  cols_heatmap         = "cartera",\n',
                                '  cols_valor_color     = "delta_mom",\n',
                                '  filas_seleccionables = c("Microempresa","Pequena empresa",\n',
                                '                           "Mediana empresa","Gran empresa","Corporativo"),\n',
                                '  modal_icon           = "chart-bar",\n',
                                '  modal_size           = "m",\n',
                                '  modal_titulo_fn      = function(sel) paste0("Segmento: ", sel$fila$segmento[[1]]),\n',
                                '  modal_pre_fn         = function(sel) fila_modal_rv(sel$fila),\n',
                                '  modal_contenido_fn   = function(sel) .DetalleFilaUI("mod_detalle")\n',
                                ')\n',
                                '# Modulo fuera del closure del modal:\n',
                                '.DetalleFila("mod_detalle", reactive(fila_modal_rv()))'
                              ))
                            )
        ),

        # TAB 3: seleccion de celda con cols_activos ----
        bs4Dash::bs4TabItem(tabName = "tab_celda",
                        shiny::br(),
                        shiny::fluidRow(
                          bs4Dash::bs4Card(width = 12, collapsible = FALSE,
                                           .bloque_info(
                                             shiny::tags$p(
                                               style = "margin:0 0 4px 0;",
                                               shiny::icon("table-cells"),
                                               shiny::tags$strong(" Seleccion de celda con columnas activas restringidas")
                                             ),
                                             shiny::tags$p(
                                               style = "margin:0;",
                                               "Con ", shiny::tags$code('modo_seleccion = "celda"'),
                                               " el click retorna fila + columna + valor.",
                                               " El parametro ", shiny::tags$code("cols_activos"),
                                               " filtra en JS que columnas disparan el evento;",
                                               " clicks en columnas no listadas son ignorados silenciosamente.",
                                               " El badge muestra la ultima celda seleccionada",
                                               " via ", shiny::tags$code("mostrar_badge = TRUE"), "."
                                             )
                                           )
                          )
                        ),
                        shiny::fluidRow(
                          bs4Dash::bs4Card(
                            width = 8, collapsible = FALSE, solidHeader = TRUE, status = "white",
                            title = "Click en celda — activas: creditos, cartera, mora_pct, delta_mom",
                            TablaReactableUI("t_celda",
                                             titulo    = "Cartera por segmento",
                                             subtitulo = "Solo columnas numericas responden al click",
                                             estilo    = "minimal2"
                            )
                          ),
                          bs4Dash::bs4Card(
                            width = 4, collapsible = FALSE, title = "Seleccion activa",
                            shiny::verbatimTextOutput("sel_celda")
                          )
                        ),
                        shiny::fluidRow(
                          .card_codigo("seleccion celda + cols_activos", paste0(
                            'TablaReactable(\n',
                            '  id               = "t_celda",\n',
                            '  data             = reactive(.dat),\n',
                            '  id_col           = "segmento",\n',
                            '  modo_seleccion   = "celda",\n',
                            '  cols_activos     = c("creditos","cartera","mora_pct","delta_mom"),\n',
                            '  cols_heatmap     = "cartera",\n',
                            '  cols_valor_color = "delta_mom",\n',
                            '  mostrar_badge    = TRUE\n',
                            ')'
                          ))
                        )
        ),

        # TAB 4: boton en celda abre modal externo ----
        bs4Dash::bs4TabItem(tabName = "tab_btn",
                        shiny::br(),
                        shiny::fluidRow(
                          bs4Dash::bs4Card(width = 12, collapsible = FALSE,
                                           .bloque_info(
                                             shiny::tags$p(
                                               style = "margin:0 0 4px 0;",
                                               shiny::icon("circle-info"),
                                               shiny::tags$strong(" Columna boton HTML que dispara modal externo")
                                             ),
                                             shiny::tags$p(
                                               style = "margin:0;",
                                               "Se agrega una columna ", shiny::tags$code("ver"),
                                               " al dataset con valor ", shiny::tags$code('"Ver detalle"'),
                                               " en filas normales y vacio en pinadas.",
                                               " El ", shiny::tags$code("colDef"),
                                               " renderiza un ", shiny::tags$code("<​button>"),
                                               " HTML via ", shiny::tags$code("html = TRUE"), ".",
                                               " El modulo usa ", shiny::tags$code('modo_seleccion = "celda"'),
                                               " con ", shiny::tags$code('cols_activos = "ver"'),
                                               " para que solo esa columna dispare el evento.",
                                               " El server observa la seleccion y llama ",
                                               shiny::tags$code("showModal()"), " manualmente,",
                                               " lo que permite logica adicional antes de abrir el modal."
                                             )
                                           )
                          )
                        ),
                        shiny::fluidRow(
                          bs4Dash::bs4Card(
                            width = 8, collapsible = FALSE, solidHeader = TRUE, status = "white",
                            title = "Columna boton — click en Ver abre modal externo",
                            TablaReactableUI("t_btn",
                                             titulo    = "Cartera por segmento",
                                             subtitulo = "Clic en Ver para abrir detalle del segmento",
                                             estilo    = "minimal"
                            )
                          ),
                          bs4Dash::bs4Card(
                            width = 4, collapsible = FALSE, title = "Seleccion activa",
                            shiny::verbatimTextOutput("sel_btn")
                          )
                        ),
                        shiny::fluidRow(
                          .card_codigo("boton en celda + modal externo", paste0(
                            '# colDef del boton:\n',
                            '.coldef_boton <- function() {\n',
                            '  reactable::colDef(\n',
                            '    name = "", width = 110, html = TRUE,\n',
                            '    cell = function(value, index, name) {\n',
                            '      if (!nzchar(trimws(as.character(value)))) return("\\u2014")\n',
                            '      as.character(tags$button(\n',
                            '        style = "background:#1D4ED8;color:#fff;...",\n',
                            '        icon("circle-info"), " Ver"\n',
                            '      ))\n',
                            '    }\n',
                            '  )\n',
                            '}\n\n',
                            '# Server:\n',
                            'fila_btn_rv <- reactiveVal(NULL)\n',
                            'mod_btn <- TablaReactable(\n',
                            '  id             = "t_btn",\n',
                            '  data           = reactive(.dat_btn),\n',
                            '  id_col         = "segmento",\n',
                            '  modo_seleccion = "celda",\n',
                            '  cols_activos   = "ver",\n',
                            '  col_specs      = list(ver = .coldef_boton()),\n',
                            '  cols_heatmap   = "cartera",\n',
                            '  cols_valor_color = "delta_mom"\n',
                            ')\n',
                            'observeEvent(mod_btn$seleccion(), {\n',
                            '  sel <- mod_btn$seleccion()\n',
                            '  req(!is.null(sel), sel$col == "ver")\n',
                            '  fila_btn_rv(sel$fila)\n',
                            '  showModal(modalDialog(\n',
                            '    title = paste0("Detalle: ", sel$fila$segmento[[1]]),\n',
                            '    .DetalleFilaUI("mod_detalle_btn"), easyClose = TRUE\n',
                            '  ))\n',
                            '})'
                          ))
                        )
        )
      )
    )
  )

  # SERVER ----

  #' Server de la app demo de TablaReactable
  #'
  #' Inicializa todos los escenarios de interaccion de la demo:
  #' visualizacion, seleccion por fila/celda y accion explicita por boton.
  #'
  #' @param input,output,session Parametros estandar de servidor Shiny.
  #'
  #' @return Nada (side-effects de `shinyServer`).
  #' @keywords internal
  server <- function(input, output, session) {

    # Tab 1: solo visualizacion, sin seleccion ----
    TablaReactable(
      id               = "t_est_minimal",
      data             = shiny::reactive(.dat),
      id_col           = "segmento",
      modo_seleccion   = "ninguno",
      cols_heatmap     = "cartera",
      cols_valor_color = "delta_mom"
    )
    TablaReactable(
      id               = "t_est_minimal2",
      data             = shiny::reactive(.dat),
      id_col           = "segmento",
      modo_seleccion   = "ninguno",
      cols_heatmap     = "cartera",
      cols_valor_color = "delta_mom"
    )

    # Tab 2: fila + modal integrado via modal_pre_fn ----
    fila_modal_rv <- shiny::reactiveVal(NULL)
    mod_fila <- TablaReactable(
      id                   = "t_fila",
      data                 = shiny::reactive(.dat),
      id_col               = "segmento",
      modo_seleccion       = "fila",
      cols_heatmap         = "cartera",
      cols_valor_color     = "delta_mom",
      filas_seleccionables = .filas_ok,
      modal_icon           = "chart-bar",
      modal_size           = "m",
      modal_titulo_fn      = function(sel) paste0("Segmento: ", sel$fila$segmento[[1]]),
      modal_pre_fn         = function(sel) fila_modal_rv(sel$fila),
      modal_contenido_fn   = function(sel) .DetalleFilaUI("mod_detalle")
    )
    .DetalleFila("mod_detalle", shiny::reactive(fila_modal_rv()))
    output$sel_fila <- shiny::renderPrint({
      sel <- mod_fila$seleccion()
      if (is.null(sel)) cat("(sin seleccion)\n") else utils::str(sel)
    })

    # Tab 3: celda + cols_activos + badge ----
    mod_celda <- TablaReactable(
      id               = "t_celda",
      data             = shiny::reactive(.dat),
      id_col           = "segmento",
      modo_seleccion   = "celda",
      cols_activos     = c("creditos", "cartera", "mora_pct", "delta_mom"),
      cols_heatmap     = "cartera",
      cols_valor_color = "delta_mom",
      mostrar_badge    = TRUE
    )
    output$sel_celda <- shiny::renderPrint({
      sel <- mod_celda$seleccion()
      if (is.null(sel)) cat("(sin seleccion)\n") else utils::str(sel)
    })

    # Tab 4: boton en celda + modal externo manual ----
    fila_btn_rv <- shiny::reactiveVal(NULL)
    mod_btn <- TablaReactable(
      id               = "t_btn",
      data             = shiny::reactive(.dat_btn),
      id_col           = "segmento",
      modo_seleccion   = "celda",
      cols_activos     = "ver",
      col_specs        = list(ver = .coldef_boton()),
      cols_heatmap     = "cartera",
      cols_valor_color = "delta_mom"
    )
    # Observa seleccion en columna "ver" y abre modal manualmente
    shiny::observeEvent(mod_btn$seleccion(), {
      sel <- mod_btn$seleccion()
      shiny::req(!is.null(sel), sel$col == "ver")
      fila_btn_rv(sel$fila)
      shiny::showModal(shiny::modalDialog(
        title     = paste0("Detalle: ", sel$fila$segmento[[1]]),
        size      = "m",
        easyClose = TRUE,
        footer    = shiny::modalButton("Cerrar"),
        .DetalleFilaUI("mod_detalle_btn")
      ))
    }, ignoreNULL = TRUE)
    .DetalleFila("mod_detalle_btn", shiny::reactive(fila_btn_rv()))
    output$sel_btn <- shiny::renderPrint({
      sel <- mod_btn$seleccion()
      if (is.null(sel)) cat("(sin seleccion)\n") else utils::str(sel)
    })
  }

  # APP ----

  shiny::shinyApp(ui = ui, server = server)
}
