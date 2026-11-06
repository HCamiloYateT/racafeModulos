# TARJETAS INFORMATIVAS Y KPI SIN BASES DE DATOS ----
library(shiny)
library(bs4Dash)
ui <- bs4DashPage(title = "Tarjetas Racafé", header = bs4DashNavbar(title = "Tarjetas KPI"),
  sidebar = bs4DashSidebar(bs4SidebarMenu(id = "tabs",
    bs4SidebarMenuItem("Tarjetas", tabName = "tarjetas", icon = icon("chart-line")),
    bs4SidebarMenuItem("Destino", tabName = "destino", icon = icon("arrow-right")))),
  body = bs4DashBody(
    racafeForms::racafeSharedAssets(system.file("gallery-assets", package = "racafeForms"), global_css = FALSE),
    bs4TabItems(
      bs4TabItem(tabName = "tarjetas",
        h3("Etapas"),
        racafeForms::racafeInfoGrid(columns = 4,
          racafeModulos::cajaValor_ui("ejecucion", texto = "1. Ejecución 2026", icono = "clock-rotate-left",
            mostrar_boton = FALSE, mostrar_kpi = FALSE,
            detalle = paste("Cuando existe información contable, los períodos cerrados se toman directamente",
              "de la ejecución registrada y permanecen como referencia de solo lectura.")),
          racafeModulos::cajaValor_ui("proyeccion", texto = "2. Proyección", icono = "chart-line",
            mostrar_boton = FALSE, mostrar_kpi = FALSE,
            detalle = paste("En los módulos que lo requieren, los meses pendientes del año vigente pueden ser",
              "ajustados para construir el cierre proyectado.")),
          racafeModulos::cajaValor_ui("presupuesto", texto = "3. Presupuesto 2027", icono = "pen-to-square",
            mostrar_boton = FALSE, mostrar_kpi = FALSE,
            detalle = paste("Los valores presupuestales se ingresan directamente en las celdas habilitadas.",
              "Algunas matrices permiten trabajar de forma mensual o ingresar primero el valor anual.")),
          racafeModulos::cajaValor_ui("confirmacion", texto = "4. Confirmación", icono = "circle-check",
            mostrar_boton = FALSE, mostrar_kpi = FALSE,
            detalle = paste("Una vez revisada la información, la tabla puede confirmarse. La confirmación queda",
              "registrada y bloquea la edición hasta que el presupuesto sea reabierto."))),
        h3("Indicadores y acciones"),
        racafeForms::racafeInfoGrid(columns = 3,
          racafeModulos::cajaValor_ui("modal", texto = "Pedidos", icono = "cart-shopping", color = "#DA291C",
            label_boton = "Ver detalle", footer = "KPI del período", compact = TRUE),
          racafeModulos::cajaValor_ui("nav", texto = "Proyección", icono = "chart-line", color = "#245A81",
            label_boton = "Ir al destino", footer = "Cierre esperado", compact = TRUE),
          racafeModulos::cajaValor_ui("readonly", texto = "Cumplimiento", icono = "circle-check", color = "#1F7A55",
            color_kpi = "#344054", color_footer = "#667085", mostrar_boton = FALSE,
            footer = "Meta registrada", compact = TRUE)),
        numericInput("valor", "Valor de prueba", 1234.56), verbatimTextOutput("state")),
      bs4TabItem(tabName = "destino", h3("Navegación recibida"), textOutput("destination")))))
ui <- tagList(htmltools::suppressDependencies("googlefonts", "ionicons"), ui)
server <- function(input, output, session) {
  detail <- racafeModulos::cajaValor_server("modal", reactive(input$valor), "numero")
  nav <- racafeModulos::cajaValor_server("nav", reactive(input$valor * 2), "numero")
  racafeModulos::cajaValor_server("readonly", reactive(input$valor / 10), "numero")
  observeEvent(detail$click_detalle(), showModal(racafeForms::racafeModalDetail("Detalle de pedidos",
    p("Evento gestionado por la aplicación"))))
  observeEvent(nav$click_detalle(), bs4Dash::updateTabItems(session, "tabs", "destino"))
  output$destination <- renderText(paste("Eventos de navegación:", nav$click_detalle()))
  output$state <- renderPrint(list(modal = detail$click_detalle(), nav = nav$click_detalle()))
}
shinyApp(ui, server)
