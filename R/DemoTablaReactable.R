DemoTablaReactable <- function() {
  
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
  .DetalleFilaUI <- function(id) shiny::uiOutput(shiny::NS(id)("contenido"))
  
  # Server: construye KPIs desde reactivo fila_r
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
