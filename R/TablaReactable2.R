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



DemoTablaReactable2 <- function() {

  .dat <- tibble::tibble(
    segmento  = c("Microempresa", "Pequeña empresa", "Mediana empresa",
                  "Gran empresa", "Corporativo", "Otros", "TOTAL"),
    creditos  = c(4821L, 2103L, 847L, 412L, 198L, 312L, 8693L),
    cartera   = c(12.4, 28.7, 54.1, 87.3, 142.6, 6.8, 331.9),
    mora_pct  = c(3.2, 1.8, 0.9, 0.4, 0.2, 4.1, 1.6),
    delta_mom = c(0.3, -0.1, -0.2, 0.4, -0.3, 0.5, 0.1),
    .row_type = c("", "", "", "", "", "otros", "total")
  )
  .filas_ok <- c("Microempresa", "Pequeña empresa", "Mediana empresa", "Gran empresa", "Corporativo")

  .bloque_info <- function(..., nota_row_type = TRUE) {
    shiny::tags$div(
      style = paste0(
        "background:#EFF6FF;border-left:4px solid #1D4ED8;border-radius:4px;",
        "padding:12px 16px;margin-bottom:14px;font-size:12.5px;",
        "line-height:1.6;color:#1E3A5F;"
      ),
      ...,
      if (isTRUE(nota_row_type)) shiny::tags$p(
        style = "margin-top:8px;margin-bottom:0;font-size:11.5px;color:#555;",
        shiny::icon("triangle-exclamation"),
        shiny::tags$strong(" Requisito del dataset: "),
        "la columna ", shiny::tags$code(".row_type"),
        " debe existir con valores ", shiny::tags$code("\"\""),
        " (fila normal), ", shiny::tags$code("\"otros\""),
        " y ", shiny::tags$code("\"total\""), "."
      )
    )
  }

  .card_codigo <- function(titulo, codigo) {
    bs4Dash::bs4Card(
      width = 12, collapsible = TRUE, collapsed = TRUE, status = "white",
      title = shiny::tagList(shiny::icon("code"), paste(" Código:", titulo)),
      shiny::tags$pre(
        style = paste0(
          "background:#F8F9FA;border:1px solid #DEE2E6;border-radius:4px;",
          "padding:12px;font-size:11px;overflow-x:auto;margin:0;"
        ),
        shiny::tags$code(codigo)
      )
    )
  }

  .DetalleFilaUI <- function(id) shiny::uiOutput((shiny::NS(id))("contenido"))

  .DetalleFila <- function(id, fila_r) {
    shiny::moduleServer(id, function(input, output, session) {
      output$contenido <- shiny::renderUI({
        f <- fila_r()
        shiny::req(!is.null(f))
        shiny::tagList(
          shiny::tags$h5(shiny::icon("layer-group"), shiny::tags$strong(f$segmento[[1]])),
          shiny::fluidRow(
            shiny::column(4, shiny::tags$div(class = "caja-modal-footer",
              shiny::tags$div("Créditos"),
              shiny::tags$div(shiny::tags$strong(format(f$creditos[[1]], big.mark = ","))))),
            shiny::column(4, shiny::tags$div(class = "caja-modal-footer",
              shiny::tags$div("Cartera MM COP"),
              shiny::tags$div(shiny::tags$strong(f$cartera[[1]])))),
            shiny::column(4, shiny::tags$div(class = "caja-modal-footer",
              shiny::tags$div("Mora %"),
              shiny::tags$div(shiny::tags$strong(paste0(f$mora_pct[[1]], "%")))))
          )
        )
      })
    })
  }

  ui <- bs4Dash::bs4DashPage(
    title       = "Demo TablaReactable2",
    dark        = NULL,
    scrollToTop = TRUE,
    help        = NULL,
    header  = bs4Dash::bs4DashNavbar(title = "Demo TablaReactable2"),
    sidebar = bs4Dash::bs4DashSidebar(
      status = "danger", expandOnHover = FALSE,
      bs4Dash::bs4SidebarMenu(
        id = "tabs_demo",
        bs4Dash::bs4SidebarMenuItem("Params en server", tabName = "tab_server", icon = shiny::icon("server")),
        bs4Dash::bs4SidebarMenuItem("Notas dinámicas", tabName = "tab_notas", icon = shiny::icon("comment-dots")),
        bs4Dash::bs4SidebarMenuItem("Col labels", tabName = "tab_col_labels", icon = shiny::icon("tags")),
        bs4Dash::bs4SidebarMenuItem("Selección fila", tabName = "tab_fila", icon = shiny::icon("hand-pointer")),
        bs4Dash::bs4SidebarMenuItem("Selección celda", tabName = "tab_celda", icon = shiny::icon("table-cells"))
      )
    ),
    footer = bs4Dash::bs4DashFooter(),
    body   = bs4Dash::bs4DashBody(
      shiny::includeCSS("https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/Styles/style.css"),
      shiny::includeScript("https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/Scripts/scripts"),
      bs4Dash::bs4TabItems(
        bs4Dash::bs4TabItem(tabName = "tab_server", shiny::br(),
          shiny::fluidRow(bs4Dash::bs4Card(width = 12, collapsible = FALSE,
            .bloque_info(
              shiny::tags$p(style = "margin:0 0 4px 0;", shiny::icon("server"), shiny::tags$strong(" UI mínima — todo el contenido en server")),
              shiny::tags$p(style = "margin:0;", shiny::tags$code("TablaReactable2UI()"), " solo recibe ", shiny::tags$code("id"), " y ", shiny::tags$code("estilo"), "."),
              nota_row_type = FALSE
            ))),
          shiny::fluidRow(shiny::column(12, shiny::tags$div(style = "max-width:420px;margin-bottom:16px;",
            shiny::textInput("srv_titulo", "Título reactivo (se propaga a ambas tablas):", value = "Cartera por segmento", width = "100%")))),
          shiny::fluidRow(
            bs4Dash::bs4Card(width = 6, collapsible = FALSE, solidHeader = TRUE, status = "white", title = "Estilo: minimal", TablaReactable2UI("t_srv_minimal", estilo = "minimal")),
            bs4Dash::bs4Card(width = 6, collapsible = FALSE, solidHeader = TRUE, status = "white", title = "Estilo: minimal2", TablaReactable2UI("t_srv_minimal2", estilo = "minimal2"))
          )
        ),
        bs4Dash::bs4TabItem(tabName = "tab_notas", shiny::br(),
          shiny::fluidRow(
            shiny::column(4, bs4Dash::bs4Card(width = 12, collapsible = FALSE, title = "Controles reactivos",
              shiny::textInput("notas_titulo", "Título:", value = "Cartera por segmento", width = "100%"),
              shiny::textInput("notas_subtitulo", "Subtítulo:", value = "Vista interactiva — modo fila", width = "100%"),
              shiny::textInput("notas_footer", "Footer:", value = "Fuente: sistema de cartera", width = "100%"),
              shiny::selectInput("notas_footer_tipo", "Tipo de footer:", choices = c("info", "warning", "dark", "danger"), width = "100%"),
              shiny::checkboxInput("notas_mostrar_nota", "Mostrar nota de interacción", value = TRUE)
            )),
            shiny::column(8, bs4Dash::bs4Card(width = 12, collapsible = FALSE, solidHeader = TRUE, status = "white", title = "Resultado en tiempo real", TablaReactable2UI("t_notas", estilo = "minimal")))
          )
        ),
        bs4Dash::bs4TabItem(tabName = "tab_col_labels", shiny::br(),
          shiny::fluidRow(shiny::column(12, shiny::tags$div(style = "max-width:300px;margin-bottom:16px;",
            shiny::selectInput("cl_formato", "Formato de encabezados:",
              choices = c("Español completo" = "es_full", "Español abreviado" = "es_short", "English" = "en"), width = "100%")))) ,
          shiny::fluidRow(bs4Dash::bs4Card(width = 12, collapsible = FALSE, solidHeader = TRUE,
            status = "white", title = "Mismos datos — encabezados según formato seleccionado", TablaReactable2UI("t_col_labels", estilo = "minimal2")))
        ),
        bs4Dash::bs4TabItem(tabName = "tab_fila", shiny::br(),
          shiny::fluidRow(
            bs4Dash::bs4Card(width = 8, collapsible = FALSE, solidHeader = TRUE, status = "white", title = "Click en fila — Otros y TOTAL no abren modal", TablaReactable2UI("t_fila", estilo = "minimal")),
            bs4Dash::bs4Card(width = 4, collapsible = FALSE, title = "Selección activa", shiny::verbatimTextOutput("sel_fila"))
          )
        ),
        bs4Dash::bs4TabItem(tabName = "tab_celda", shiny::br(),
          shiny::fluidRow(
            bs4Dash::bs4Card(width = 8, collapsible = FALSE, solidHeader = TRUE, status = "white", title = "Click en celda — activas: creditos, cartera, mora_pct, delta_mom", TablaReactable2UI("t_celda", estilo = "minimal2")),
            bs4Dash::bs4Card(width = 4, collapsible = FALSE, title = "Selección activa", shiny::verbatimTextOutput("sel_celda"))
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    TablaReactable2("t_srv_minimal", data = shiny::reactive(.dat), titulo = shiny::reactive(input$srv_titulo), subtitulo = "Heatmap cartera • Color delta mom", id_col = "segmento", modo_seleccion = "ninguno", cols_heatmap = "cartera", cols_valor_color = "delta_mom")
    TablaReactable2("t_srv_minimal2", data = shiny::reactive(.dat), titulo = shiny::reactive(input$srv_titulo), subtitulo = "Heatmap cartera • Color delta mom", id_col = "segmento", modo_seleccion = "ninguno", cols_heatmap = "cartera", cols_valor_color = "delta_mom")
    TablaReactable2("t_notas", data = shiny::reactive(.dat), titulo = shiny::reactive(input$notas_titulo), subtitulo = shiny::reactive(input$notas_subtitulo), footer = shiny::reactive(input$notas_footer), footer_tipo = shiny::reactive(input$notas_footer_tipo), mostrar_nota = shiny::reactive(input$notas_mostrar_nota), id_col = "segmento", modo_seleccion = "fila", cols_heatmap = "cartera", cols_valor_color = "delta_mom")

    cl_r <- shiny::reactive({
      switch(input$cl_formato,
        es_full = c(segmento = "Segmento de cliente", creditos = "Créditos activos", cartera = "Cartera (MM COP)", mora_pct = "Índice de mora (%)", delta_mom = "Variación MoM"),
        es_short = c(segmento = "Segmento", creditos = "Créditos", cartera = "Cartera MM", mora_pct = "Mora %", delta_mom = "Δ MoM"),
        en = c(segmento = "Segment", creditos = "Active credits", cartera = "Portfolio", mora_pct = "Delinquency (%)", delta_mom = "MoM delta")
      )
    })
    TablaReactable2("t_col_labels", data = shiny::reactive(.dat), titulo = shiny::reactive(paste("Vista:", input$cl_formato)), col_labels = cl_r, id_col = "segmento", modo_seleccion = "ninguno", cols_heatmap = "cartera", cols_valor_color = "delta_mom")

    fila_modal_rv <- shiny::reactiveVal(NULL)
    mod_fila <- TablaReactable2(
      id = "t_fila", data = shiny::reactive(.dat), titulo = "Cartera por segmento", subtitulo = "Solo segmentos reales activan el modal",
      footer = "Heatmap: cartera • Color: delta mom", id_col = "segmento", modo_seleccion = "fila", cols_heatmap = "cartera",
      cols_valor_color = "delta_mom", filas_seleccionables = .filas_ok, modal_icon = "chart-bar", modal_size = "m",
      modal_titulo_fn = function(sel) paste0("Segmento: ", sel$fila$segmento[[1]]),
      modal_pre_fn = function(sel) fila_modal_rv(sel$fila), modal_contenido_fn = function(sel) .DetalleFilaUI("mod_detalle")
    )
    .DetalleFila("mod_detalle", shiny::reactive(fila_modal_rv()))
    output$sel_fila <- shiny::renderPrint({ sel <- mod_fila$seleccion(); if (is.null(sel)) cat("(sin selección)\n") else utils::str(sel) })

    mod_celda <- TablaReactable2(
      id = "t_celda", data = shiny::reactive(.dat), titulo = "Cartera por segmento", subtitulo = "Solo columnas numéricas responden al click",
      id_col = "segmento", modo_seleccion = "celda", cols_activos = c("creditos", "cartera", "mora_pct", "delta_mom"),
      cols_heatmap = "cartera", cols_valor_color = "delta_mom", mostrar_badge = TRUE
    )
    output$sel_celda <- shiny::renderPrint({ sel <- mod_celda$seleccion(); if (is.null(sel)) cat("(sin selección)\n") else utils::str(sel) })
  }

  shiny::shinyApp(ui = ui, server = server)
}
