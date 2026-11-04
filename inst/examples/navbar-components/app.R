# GALERIA CORPORATIVA SIN PERSISTENCIA ----
.header_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    racafeModulos::MenuHeaderUI(ns("tasks"), shiny::icon("list-check"), title = paste(id, "Tareas"),
      min_width = "360px", keep_open_on_select = TRUE),
    racafeModulos::MenuHeaderUI(ns("clients"), shiny::icon("users"), title = paste(id, "Clientes"),
      min_width = "360px", keep_open_on_select = FALSE),
    racafeModulos::MenuHeaderUI(ns("direct"), shiny::icon("circle-info"), title = paste(id, "Acceso directo"),
      direct = TRUE))
}
.body_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::h3(paste("Módulo", id)),
    racafeForms::racafeForm(variant = "plain",
      racafeForms::racafeButton("add", "Añadir registro", ns = ns),
      racafeForms::racafeButton("outside", "Acción fuera del overlay", ns = ns),
      racafeForms::racafeButton("load", "Consulta de prueba", ns = ns),
      racafeForms::racafeButton("query_confirm", "Consulta con confirmación", ns = ns),
      racafeForms::racafeInputCheckbox("fail", "Simular error", ns = ns)),
    racafeShiny::racafeOverlayLoader("panel", ns = ns, triggers = c("load", "logical"),
      racafeForms::racafePanel("Panel de consulta", min_height = "180px",
        shiny::textOutput(ns("load_state")), shiny::actionButton(ns("inside"), "Dentro del panel"),
        shiny::uiOutput(ns("html_table")))),
    racafeShiny::racafeListGroup(
      racafeShiny::racafeListItem("Selección por teclado", id = "standalone", value = "keyboard", ns = ns),
      racafeShiny::racafeListItem("Elemento deshabilitado", id = "standalone", value = "disabled", ns = ns,
        disabled = TRUE)),
    gt::gt_output(ns("gt_table")), shiny::verbatimTextOutput(ns("state")))
}
.module <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    count <- shiny::reactiveVal(2L)
    loads <- shiny::reactiveVal(0L)
    status <- shiny::reactiveVal("Disponible")
    outside <- shiny::reactiveVal(0L)
    busy <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(input$add, { count(count() + 1L) })
    shiny::observeEvent(input$outside, { outside(outside() + 1L) })
    list_items <- shiny::reactive({
      do.call(racafeShiny::racafeListGroup, lapply(seq_len(count()), function(i) {
        racafeShiny::racafeListItem(paste("Tarea", i, "Café & <texto>"), subtitle = "Revisión de ejemplo",
          metadata = c(Responsable = "Ana", Fecha = "04/11/2026", Avance = "1/3"),
          badges = racafeForms::racafeStatusBadge("Alta", "warning"), value = list(key = i))
      }))
    })
    detail <- function(selection) shiny::tagList(
      racafeForms::racafeInfoGrid(racafeForms::racafeInfoItem("Registro", selection$key)),
      racafeForms::racafeDateLabel(as.Date("2026-11-04")),
      racafeForms::racafeDataBadge("Responsable", "Ana"),
      racafeForms::racafeDetailSection("Actividades",
        racafeForms::racafeChecklist("activities", c(review = "Revisar", confirm = "Confirmar"),
          checked = c(confirm = TRUE, review = FALSE), ns = session$ns)),
      racafeShiny::racafeCompactTable(data.frame(Columna = "Texto extenso con tildes y ñ", Total = "1,234.56"), "html"))
    tasks <- racafeModulos::MenuHeaderServer("tasks", items_r = list_items, badgeStatus_r = "warning",
      headerText = "Tareas", modal_titulo_fn = function(selection) "Detalle de tarea", modal_size = "subventana2",
      modal_contenido_fn = detail)
    clients <- racafeModulos::MenuHeaderServer("clients", items_r = shiny::reactive(
      racafeShiny::racafeListGroup(racafeShiny::racafeListItem("Compañía Café & Cía.",
        metadata = c(NIT = "900123456", Línea = "Nacional"), value = list(key = "c1")))),
      badgeStatus_r = "info", headerText = "Clientes", modal_titulo_fn = function(selection) "Detalle de cliente",
      modal_contenido_fn = detail, modal_size = "subventana2")
    direct <- racafeModulos::MenuHeaderServer("direct", data.frame(key = "direct", label = "Acceso directo"),
      key_cols = "key", modal_titulo_fn = function(selection) "Detalle directo", modal_contenido_fn = detail,
      modal_size = "aviso")
    output$html_table <- shiny::renderUI(racafeShiny::racafeCompactTable(
      data.frame(Cliente = "<script>texto escapado</script>", Total = "1,234.56"), "html", align = c(Total = "right")))
    output$gt_table <- gt::render_gt(racafeShiny::racafeCompactTable(
      data.frame(Descripción = c("Tabla gt sin bordes ni fondo", "Segunda fila"), Valor = c(12.5, 1.2)),
      "gt", align = c(Valor = "right")))
    .run_query <- function() {
      if (isTRUE(shiny::isolate(busy()))) return(invisible(NULL))
      busy(TRUE)
      status("Consultando")
      racafeShiny::racafeUpdateOverlayLoader(session, "panel", TRUE)
      # Diferir permite interactuar con el segundo módulo durante la consulta simulada.
      later::later(function() shiny::withReactiveDomain(session, {
        if (session$isClosed()) return(invisible(NULL))
        tryCatch({
          if (isTRUE(shiny::isolate(input$fail))) stop("Error simulado")
          loads(shiny::isolate(loads()) + 1L); status("Consulta terminada")
        }, error = function(error) status(conditionMessage(error)), finally = {
          busy(FALSE); racafeShiny::racafeUpdateOverlayLoader(session, "panel", FALSE)
        })
      }), delay = 1.5)
    }
    shiny::observeEvent(input$load, { .run_query() })
    shiny::observeEvent(input$query_confirm, {
      racafeForms::racafeConfirm(session, "logical", "Consultar", "Ejecutar consulta de demostración", ns = session$ns)
    })
    shiny::observeEvent(input$logical, { if (isTRUE(input$logical)) .run_query() })
    output$load_state <- shiny::renderText(status())
    output$state <- shiny::renderPrint(list(task = tasks$seleccion(), client = clients$seleccion(),
      direct = direct$seleccion(), standalone = input$standalone, n = tasks$n(),
      activities = input$activities, loads = loads(), outside = outside()))
  })
}
ui <- bs4Dash::dashboardPage(title = "Componentes navbar Racafé",
  header = bs4Dash::dashboardHeader(title = "Galería de navbar",
    rightUi = do.call(shiny::tagList, c(.header_ui("one"), .header_ui("two")))),
  sidebar = bs4Dash::dashboardSidebar(disable = TRUE),
  body = bs4Dash::dashboardBody(racafeForms::racafeSharedAssets(
    system.file("gallery-assets", package = "racafeForms"), global_css = FALSE),
    shiny::fluidRow(shiny::column(6, .body_ui("one")), shiny::column(6, .body_ui("two")))))
ui <- shiny::tagList(htmltools::suppressDependencies("googlefonts", "ionicons"), ui)
server <- function(input, output, session) { .module("one"); .module("two") }
shiny::shinyApp(ui, server)
