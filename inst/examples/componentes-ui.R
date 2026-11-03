# Demostración autocontenida: indicadores, secciones y módulo en modal ----
.form_ui <- function(id) {
  ns <- shiny::NS(id)
  racafeForms::racafeInputText("nombre", "Nombre del registro", value = "Pedido de prueba", ns = ns)
}
.form_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    guardados <- shiny::reactiveVal(0L)
    shiny::observeEvent(input$cancelar, shiny::removeModal(session))
    shiny::observeEvent(input$guardar, {
      shiny::req(nzchar(trimws(input$nombre)))
      guardados(guardados() + 1L)
      shiny::removeModal(session)
    })
    guardados
  })
}
.board_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Detalle corporativo reutilizable"),
    shiny::actionButton(ns("incrementar"), "Actualizar valores"),
    shiny::actionButton(ns("abrir"), "Abrir información"),
    shiny::actionButton(ns("cerrar"), "Cerrar información"),
    shiny::actionButton(ns("nativo"), "Modal sin footer corporativo"),
    shiny::tags$div(class = "rcm-tarjeta-grid", lapply(1:6, function(i) {
      racafeModulos::TarjetaUI(ns(paste0("kpi", i)))
    })),
    racafeShiny::racafeCollapsible(ns("detalle"), "Detalle de pedidos, lotes y fechas", open = TRUE,
      shiny::tableOutput(ns("pedidos"))),
    racafeShiny::racafeCollapsible(ns("comercial"), "Información comercial (editable)", open = FALSE,
      shiny::p("Cualquier módulo o formulario Shiny puede incluirse aquí."),
      racafeForms::racafeInputText("observacion", "Observación", ns = ns),
      shiny::textOutput(ns("oculto")), shiny::plotOutput(ns("grafico"), height = "180px")),
    shiny::textOutput(ns("estados")), shiny::textOutput(ns("eventos")))
}
.board_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    valor <- shiny::reactiveVal(2)
    ns <- session$ns
    guardados <- .form_server("edicion")
    labels <- c("Pedidos", "Lotes", "Producción (sacos)", "Despachos (sacos)",
                "Sacos asignados", "Facturación (sacos)")
    tarjetas <- lapply(seq_along(labels), function(i) {
      racafeModulos::Tarjeta(paste0("kpi", i), titulo = labels[[i]], valor = valor,
        descripcion = if (i == 1L) "Creación: 20/10/2026" else if (i == 6L) "Última: 23/10/2026",
        boton = i <= 2L)
    })
    lapply(1:2, function(i) {
      shiny::observeEvent(tarjetas[[i]]$accion(), {
        child_ns <- shiny::NS(ns("edicion"))
        shiny::showModal(shiny::modalDialog(title = paste("Detalle de", labels[[i]]),
          .form_ui(ns("edicion")), easyClose = FALSE,
          footer = racafeShiny::racafeModalFooter(cerrar = FALSE, acciones = shiny::tagList(
            racafeForms::racafeButtonCancel("cancelar", "Cancelar", ns = child_ns),
            racafeForms::racafeButtonSave("guardar", "Guardar", ns = child_ns)))))
      })
    })
    shiny::observeEvent(input$incrementar, valor(valor() + 1))
    shiny::observeEvent(input$abrir, racafeShiny::racafeUpdateCollapsible(session, "comercial", TRUE))
    shiny::observeEvent(input$cerrar, racafeShiny::racafeUpdateCollapsible(session, ns("comercial"), FALSE))
    shiny::observeEvent(input$nativo, {
      shiny::showModal(shiny::modalDialog("Control nativo sin estilos corporativos",
                                         footer = shiny::modalButton("Cerrar")))
    })
    output$oculto <- shiny::renderText(paste("Valor oculto:", valor()))
    output$grafico <- shiny::renderPlot(graphics::plot(1:3, c(valor(), valor() + 1, valor() + 2),
                                                     xlab = "Periodo", ylab = "Indicador"))
    output$pedidos <- shiny::renderTable(data.frame(Pedido = c(46907, 46684), Sacos = c(1, 1)))
    output$estados <- shiny::renderText(paste("Detalle:", input$detalle, "Comercial:", input$comercial))
    output$eventos <- shiny::renderText({
      first <- tarjetas[[1]]$accion(); second <- tarjetas[[2]]$accion()
      paste("Pedidos:", if (is.null(first)) 0 else first, "Lotes:", if (is.null(second)) 0 else second,
            "Guardados:", guardados())
    })
  })
}
.content <- .board_ui("tablero")
if (!identical(Sys.getenv("RACAFE_UI_BS4DASH"), "0")) {
  ui <- bs4Dash::dashboardPage(header = bs4Dash::dashboardHeader(title = "Racafé UI"),
    sidebar = bs4Dash::dashboardSidebar(disable = TRUE), body = bs4Dash::dashboardBody(.content))
  ui <- shiny::tagList(htmltools::suppressDependencies("ionicons", "googlefonts"), ui)
} else ui <- shiny::fluidPage(.content)
server <- function(input, output, session) .board_server("tablero")
shiny::shinyApp(ui, server)
