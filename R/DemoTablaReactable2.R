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
