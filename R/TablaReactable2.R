TablaReactable2UI <- function(id, estilo = c("minimal", "minimal2")) {
  ns    <- shiny::NS(id)
  estilo <- match.arg(estilo)
  shiny::tags$div(
    class = paste("rt-contenedor reactable-wrap rt-sortable", paste0("rt-estilo-", estilo)),
    shiny::uiOutput(ns("header_bloque")),
    reactable::reactableOutput(ns("tabla")),
    shiny::uiOutput(ns("nota_interaccion")),
    shiny::uiOutput(ns("footer_bloque_ui")),
    shiny::uiOutput(ns("badge_seleccion"))
  )
}
TablaReactable2 <- function(id,
    data ,
    titulo = NULL,
    subtitulo = NULL,
    footer = NULL,
    footer_tipo = c("info", "warning", "dark", "danger"),
    col_labels = NULL,
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

  modo_seleccion    <- match.arg(modo_seleccion)
  col_header_n      <- max(1L, as.integer(col_header_n))

  .to_json_arr      <- racafeModulos:::.to_json_arr
  .ordenar_con_pin  <- racafeModulos:::.ordenar_con_pin
  .coldefs_default  <- racafeModulos:::.coldefs_default
  .resolver_cols_estilo <- racafeModulos:::.resolver_cols_estilo
  .aplicar_heatmap  <- racafeModulos:::.aplicar_heatmap
  .aplicar_valor_color  <- racafeModulos:::.aplicar_valor_color
  .js_fila          <- racafeModulos:::.js_fila
  .js_celda         <- racafeModulos:::.js_celda
  .js_columna       <- racafeModulos:::.js_columna
  .js_row_class     <- racafeModulos:::.js_row_class
  .normalizar_seleccion <- racafeModulos:::.normalizar_seleccion
  .modal_titulo_tag <- racafeModulos:::.modal_titulo_tag
  .modal_footer_bloque  <- racafeModulos:::.modal_footer_bloque
  .footer_bloque    <- racafeModulos:::.footer_bloque

  cols_activos_json     <- .to_json_arr(cols_activos)
  filas_bloqueadas_json <- .to_json_arr(filas_bloqueadas)

  usar_modal            <- !is.null(modal_titulo_fn) && !is.null(modal_contenido_fn)

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    .val <- function(x) if (shiny::is.reactive(x)) x() else x

    seleccion_r  <- shiny::reactiveVal(NULL)
    sort_state_r <- shiny::reactiveVal(list(col = NULL, desc = FALSE))

    output$header_bloque <- shiny::renderUI({
      tit <- .val(titulo)
      sub <- .val(subtitulo)
      if (is.null(tit) || !nzchar(tit)) return(NULL)
      shiny::tagList(
        shiny::tags$div(class = "rt-titulo", tit),
        if (!is.null(sub) && nzchar(sub)) shiny::tags$div(class = "rt-subtitulo", sub)
      )
    })

    output$footer_bloque_ui <- shiny::renderUI({
      ft <- .val(footer_tipo)
      ft <- match.arg(ft, c("info", "warning", "dark", "danger"))
      .footer_bloque(.val(footer), ft)
    })

    shiny::observeEvent(input$sort_click, {
      shiny::req(!is.null(input$sort_click$col))
      cur        <- sort_state_r()
      nueva_col  <- input$sort_click$col
      nueva_desc <- if (!is.null(cur$col) && cur$col == nueva_col) !cur$desc else FALSE
      sort_state_r(list(col = nueva_col, desc = nueva_desc))
    })

    df_sorted_r <- shiny::reactive({
      srt <- sort_state_r()
      .ordenar_con_pin(data(), srt$col, isTRUE(srt$desc))
    })

    on_click <- switch(
      modo_seleccion,
      fila    = reactable::JS(.js_fila(ns("click"),
                                       filas_bloqueadas_json = filas_bloqueadas_json)),
      celda   = reactable::JS(.js_celda(ns("click"),
                                        cols_activos_json     = cols_activos_json,
                                        filas_bloqueadas_json = filas_bloqueadas_json)),
      columna = reactable::JS(.js_columna(ns("click"),
                                          cols_activos_json = cols_activos_json)),
      ninguno = NULL
    )

    output$tabla <- reactable::renderReactable({
      shiny::req(data())
      df  <- df_sorted_r()
      srt <- sort_state_r()

      coldefs <- .coldefs_default(
        data              = df,
        columnas_override = columnas,
        col_specs         = col_specs,
        id_col            = id_col,
        col_header_n      = col_header_n,
        sort_col          = srt$col,
        sort_desc         = isTRUE(srt$desc),
        ns_sort           = if (isTRUE(sortable)) ns("sort_click") else NULL
      )

      lbl <- .val(col_labels)
      if (!is.null(lbl) && length(lbl) > 0) {
        for (col in names(lbl)) {
          if (col %in% names(coldefs)) coldefs[[col]]$name <- lbl[[col]]
          else coldefs[[col]] <- reactable::colDef(name = lbl[[col]])
        }
      }

      cols_hm <- .resolver_cols_estilo(cols_heatmap, df)
      coldefs <- .aplicar_heatmap(coldefs, cols_hm, df)
      cols_vc <- .resolver_cols_estilo(cols_valor_color, df)
      coldefs <- .aplicar_valor_color(coldefs, cols_vc, umbral_valor_color)

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
        language        = reactable::reactableLang(
          searchPlaceholder = "Buscar...",
          noData            = "Sin resultados",
          pageInfo          = "{rowStart}–{rowEnd} de {rows} registros",
          pagePrevious      = "Anterior",
          pageNext          = "Siguiente"
        ),
        theme = reactable::reactableTheme(
          headerStyle = list(fontWeight = "600", fontSize = "12px"),
          cellStyle   = list(
            fontSize = "12px", padding = "3px 6px",
            cursor   = if (modo_seleccion != "ninguno") "pointer" else "default"
          )
        )
      )
    })

    output$nota_interaccion <- shiny::renderUI({
      if (!isTRUE(.val(mostrar_nota))) return(NULL)
      racafeModulos:::.nota_interaccion(modo_seleccion, sortable)
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
      txt <- switch(sel$modo,
                    fila    = paste0("Fila: ", sel$id),
                    celda   = paste0("Celda [", sel$col, "] = ", sel$valor, "  —  ID: ", sel$id),
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
