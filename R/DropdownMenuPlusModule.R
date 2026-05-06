# DropdownMenuPlusUI ----

#' UI del dropdown de navegacion reactivo
#'
#' Placeholder `uiOutput` que debe ubicarse en el `rightUi` o `leftUi` del
#' navbar. El contenido lo renderiza [dropdownMenuPlusServer()].
#'
#' @param id String. ID del modulo.
#'
#' @return `uiOutput` con namespace aplicado.
#' @export
#'
#' @examples
#' \dontrun{
#' # En UI (dentro de bs4DashNavbar rightUi):
#' dropdownMenuPlusUI("menu_alertas")
#'
#' # En server:
#' dropdownMenuPlusServer("menu_alertas", items_r = reactive({ list(...) }))
#' }
dropdownMenuPlusUI <- function(id) {
  shiny::uiOutput(shiny::NS(id, "menu"), inline = TRUE)
}


# DropdownMenuPlusServer ----

#' Server del dropdown de navegacion reactivo
#'
#' Gestiona la reactividad del dropdown. Los parametros `items_r`,
#' `numItems_r`, `badgeStatus_r` y `headerText` aceptan tanto un
#' `reactive({})` como un valor estatico; la funcion resuelve cual es cual
#' internamente.
#'
#' La funcion base `dropdownMenuPlus()` ya existe en `racafe`; este modulo solo
#' crea el placeholder y lo renderiza reactivamente dentro de un navbar.
#'
#' Jerarquia de resolucion:
#' \itemize{
#'   \item `icon`: explicito > derivado de `type` > generico, delegado en
#'     `racafe::dropdownMenuPlus()`.
#'   \item `headerText`: explicito > derivado de `type` > `NULL`, delegado en
#'     `racafe::dropdownMenuPlus()`.
#'   \item `numItems`: `numItems_r` resuelto > `length(items_r)`.
#'   \item `badgeStatus`: `badgeStatus_r` resuelto en cada invalidacion.
#' }
#'
#' @param id String. ID del modulo.
#' @param items_r Reactive o lista. Items del dropdown.
#' @param numItems_r Reactive, entero o `NULL`. Conteo para el badge.
#' @param badgeStatus_r Reactive, string o `NULL`. Color del badge.
#' @param headerText Reactive, string o `NULL`. Texto del header.
#' @param type String opcional. Uno de `"messages"`, `"notifications"` o `"tasks"`.
#' @param icon Icono opcional generado con `shiny::icon()`.
#' @param href String opcional. URL del footer.
#' @param footerText String. Etiqueta del footer. Default: `"Ver mas"`.
#' @param showBadge Logical. Default: `TRUE`.
#' @param showHeader Logical. Default: `TRUE`.
#' @param menuClass String opcional. Clases CSS adicionales.
#'
#' @return Nada (side-effects de Shiny moduleServer).
#' @export
#'
#' @examples
#' \dontrun{
#' dropdownMenuPlusServer(
#'   id            = "menu_alertas",
#'   icon          = shiny::icon("exclamation-triangle"),
#'   items_r       = reactive({ purrr::pmap(alertas_r(), ~ .alerta_item(..1, ..2)) }),
#'   numItems_r    = reactive(nrow(alertas_r())),
#'   badgeStatus_r = reactive(if (any(alertas_r()$nivel == "alto")) "danger" else "warning"),
#'   headerText    = reactive(sprintf("%d alerta(s)", nrow(alertas_r())))
#' )
#' }
dropdownMenuPlusServer <- function(id,
                                   items_r,
                                   numItems_r    = NULL,
                                   badgeStatus_r = NULL,
                                   headerText    = NULL,
                                   type          = NULL,
                                   icon          = NULL,
                                   href          = NULL,
                                   footerText    = "Ver mas",
                                   showBadge     = TRUE,
                                   showHeader    = TRUE,
                                   menuClass     = NULL) {
  .resolve <- function(x) if (shiny::is.reactive(x)) x() else x

  shiny::moduleServer(id, function(input, output, session) {
    output$menu <- shiny::renderUI({
      items <- .resolve(items_r)
      num <- .dropdown_menu_plus_coalesce(.resolve(numItems_r), length(items))
      badge_st <- .resolve(badgeStatus_r)
      header_tx <- .resolve(headerText)

      if (!is.null(badge_st)) .dropdown_menu_plus_validate_status(badge_st)

      racafe::dropdownMenuPlus(
        type        = type,
        badgeStatus = badge_st,
        icon        = icon,
        headerText  = header_tx,
        .list       = items,
        href        = href,
        footerText  = footerText,
        showBadge   = showBadge,
        showHeader  = showHeader,
        numItems    = num,
        menuClass   = menuClass
      )
    })
  })
}


# DemoDropdownMenuPlus ----

#' Demo del modulo dropdownMenuPlus
#'
#' App autocontenida que cubre todas las variantes del modulo:
#' \itemize{
#'   \item Estaticos: indicadores de mercado, paleta corporativa, glosario.
#'   \item Tipificados reactivos: notificaciones, mensajes, tareas.
#'   \item Libre reactivo: alertas con badge y color dinamicos.
#' }
#'
#' Los controles del sidebar permiten manipular el estado reactivo en tiempo
#' real para verificar que badge, contenido y color responden correctamente.
#'
#' @return Ejecuta `shiny::shinyApp()` (sin valor de retorno).
#' @export
#'
#' @examples
#' \dontrun{
#' DemoDropdownMenuPlus()
#' }
DemoDropdownMenuPlus <- function() {
  .gen_alertas <- function(n_alto, n_medio, n_bajo) {
    dplyr::bind_rows(
      if (n_alto > 0L) tibble::tibble(mensaje = paste0("Alerta critica #", seq_len(n_alto)), nivel = "alto"),
      if (n_medio > 0L) tibble::tibble(mensaje = paste0("Advertencia #", seq_len(n_medio)), nivel = "medio"),
      if (n_bajo > 0L) tibble::tibble(mensaje = paste0("Aviso menor #", seq_len(n_bajo)), nivel = "bajo")
    )
  }

  .gen_notificaciones <- function(n) {
    if (n == 0L) return(tibble::tibble(texto = character(), status = character()))
    tipos <- c("Nuevo pedido", "Carga completada", "Error pipeline", "Reporte listo")
    estados <- c("success", "info", "danger", "info")
    idx <- ((seq_len(n) - 1L) %% length(tipos)) + 1L
    tibble::tibble(texto = paste0(tipos[idx], " #", seq_len(n)), status = estados[idx])
  }

  .gen_mensajes <- function(n) {
    if (n == 0L) return(tibble::tibble(from = character(), message = character()))
    remitentes <- c("Carlos Ruiz", "Ana Gomez", "Pedro Lopez", "Maria Torres")
    cuerpos <- c("¿Revisas el informe?", "Reunion a las 3pm.", "Datos actualizados.", "Solicitud pendiente.")
    idx <- ((seq_len(n) - 1L) %% length(remitentes)) + 1L
    tibble::tibble(from = remitentes[idx], message = cuerpos[idx])
  }

  .gen_tareas <- function(n) {
    if (n == 0L) return(tibble::tibble(nombre = character(), pct_avance = integer()))
    nombres <- c("Validar carga mensual", "Actualizar dashboard", "Revisar alertas", "Cerrar sprint", "Publicar reporte", "Auditar accesos")
    idx <- ((seq_len(n) - 1L) %% length(nombres)) + 1L
    tibble::tibble(nombre = nombres[idx], pct_avance = sample(10:95, n, replace = TRUE))
  }

  .alerta_item <- function(mensaje, nivel) {
    color <- switch(nivel, alto = "danger", medio = "warning", "info")
    shiny::tags$div(
      class = "dropdown-item d-flex align-items-start py-2",
      shiny::tags$span(class = sprintf("badge badge-%s mr-2 mt-1", color), toupper(nivel)),
      shiny::tags$span(mensaje, style = "font-size:13px;white-space:normal;")
    )
  }

  .swatch <- function(nombre, hex) {
    shiny::tags$div(
      class = "d-flex align-items-center px-3 py-1",
      shiny::tags$div(style = sprintf(
        "width:22px;height:22px;border-radius:4px;background:%s;margin-right:10px;flex-shrink:0;",
        hex
      )),
      shiny::tags$span(nombre, style = "font-size:13px;"),
      shiny::tags$span(hex, style = "font-size:11px;color:#999;margin-left:auto;")
    )
  }

  .termino <- function(term, def) {
    shiny::tags$div(
      class = "px-3 py-2 border-bottom",
      shiny::tags$p(class = "mb-0 font-weight-bold", style = "font-size:13px;", term),
      shiny::tags$p(class = "mb-0 text-muted", style = "font-size:11px;", def)
    )
  }

  .panel_log <- function(output_id, titulo, status = "secondary") {
    bs4Dash::bs4Card(
      title = titulo, width = 12, solidHeader = TRUE, status = status,
      shiny::verbatimTextOutput(output_id)
    )
  }

  .bloque_codigo <- function(codigo_ui, codigo_server = NULL) {
    items <- list(
      shiny::tags$p(
        class = "mb-1",
        style = "font-size:11px;font-weight:600;color:#888;text-transform:uppercase;",
        shiny::icon("desktop"), " UI"
      ),
      shiny::tags$pre(
        class = "mb-0",
        style = paste("background:#f8f9fa;border:1px solid #e9ecef;border-radius:4px;",
                      "padding:10px;font-size:11px;white-space:pre-wrap;color:#212529;"),
        codigo_ui
      )
    )
    if (!is.null(codigo_server)) {
      items <- c(items, list(
        shiny::tags$p(
          class = "mt-3 mb-1",
          style = "font-size:11px;font-weight:600;color:#888;text-transform:uppercase;",
          shiny::icon("server"), " Server"
        ),
        shiny::tags$pre(
          class = "mb-0",
          style = paste("background:#f8f9fa;border:1px solid #e9ecef;border-radius:4px;",
                        "padding:10px;font-size:11px;white-space:pre-wrap;color:#212529;"),
          codigo_server
        )
      ))
    }
    bs4Dash::bs4Card(
      title = "Codigo", width = 12, solidHeader = TRUE, status = "secondary",
      collapsible = TRUE,
      shiny::tagList(items)
    )
  }

  .cod <- list(
    alertas = list(
      ui = 'dropdownMenuPlusUI("menu_alertas")',
      server = 'dropdownMenuPlusServer(
  id = "menu_alertas",
  icon = shiny::icon("exclamation-triangle"),
  href = "#", footerText = "Ver historial de alertas",
  items_r = shiny::reactive(purrr::pmap(alertas_r(), ~ .alerta_item(..1, ..2))),
  numItems_r = shiny::reactive(nrow(alertas_r())),
  badgeStatus_r = shiny::reactive({
    df <- alertas_r()
    if (nrow(df) == 0L) return(NULL)
    if (any(df$nivel == "alto")) return("danger")
    if (any(df$nivel == "medio")) return("warning")
    "info"
  }),
  headerText = shiny::reactive(
    if (nrow(alertas_r()) == 0L) "Sin alertas activas"
    else sprintf("%d alerta(s) activa(s)", nrow(alertas_r()))
  )
)'
    ),
    notif = list(
      ui = 'dropdownMenuPlusUI("menu_notificaciones")',
      server = 'dropdownMenuPlusServer(
  id = "menu_notificaciones", type = "notifications", badgeStatus_r = "danger",
  href = "#", footerText = "Ver todas las notificaciones",
  items_r = shiny::reactive(purrr::pmap(notificaciones_r(), function(texto, status) {
    bs4Dash::notificationItem(text = texto, status = status)
  })),
  numItems_r = shiny::reactive(nrow(notificaciones_r()))
)'
    ),
    msg = list(
      ui = 'dropdownMenuPlusUI("menu_mensajes")',
      server = 'dropdownMenuPlusServer(
  id = "menu_mensajes", type = "messages", badgeStatus_r = "primary",
  href = "#", footerText = "Ver todos los mensajes",
  items_r = shiny::reactive(purrr::pmap(mensajes_r(), function(from, message) {
    bs4Dash::messageItem(from = from, message = message)
  })),
  numItems_r = shiny::reactive(nrow(mensajes_r()))
)'
    ),
    tasks = list(
      ui = 'dropdownMenuPlusUI("menu_tareas")',
      server = 'dropdownMenuPlusServer(
  id = "menu_tareas", type = "tasks", href = "#", footerText = "Ver todas las tareas",
  items_r = shiny::reactive(purrr::pmap(tareas_r(), function(nombre, pct_avance) {
    bs4Dash::taskItem(text = nombre, value = pct_avance)
  })),
  numItems_r = shiny::reactive(nrow(tareas_r())),
  badgeStatus_r = shiny::reactive(if (nrow(tareas_r()) == 0L) NULL else "warning")
)'
    ),
    indicadores = list(ui = 'racafe::dropdownMenuPlus(icon = shiny::icon("chart-line"), headerText = "Indicadores de mercado", badgeStatus = "success", numItems = 3L, ...)'),
    paleta = list(ui = 'racafe::dropdownMenuPlus(icon = shiny::icon("palette"), showBadge = FALSE, headerText = "Paleta corporativa", .list = list(...))'),
    glosario = list(ui = 'racafe::dropdownMenuPlus(icon = shiny::icon("book-open"), showBadge = FALSE, headerText = "Glosario", .list = list(...))')
  )

  ui <- bs4Dash::bs4DashPage(
    dark = FALSE,
    title = "Demo — dropdownMenuPlus",
    header = bs4Dash::bs4DashNavbar(
      status = "white", border = FALSE,
      title = bs4Dash::bs4DashBrand(title = "dropdownMenuPlus", color = "white"),
      rightUi = shiny::tagList(
        dropdownMenuPlusUI("menu_alertas"),
        dropdownMenuPlusUI("menu_notificaciones"),
        dropdownMenuPlusUI("menu_mensajes"),
        dropdownMenuPlusUI("menu_tareas"),
        racafe::dropdownMenuPlus(
          icon = shiny::icon("chart-line"), headerText = "Indicadores de mercado",
          badgeStatus = "success", numItems = 3L,
          shiny::tags$div(
            class = "px-3 py-2", style = "min-width:260px;",
            shiny::tags$table(
              class = "table table-sm table-borderless mb-0",
              shiny::tags$tbody(
                shiny::tags$tr(
                  shiny::tags$td("TRM"),
                  shiny::tags$td(class = "text-right font-weight-bold", "$ 4.182,50"),
                  shiny::tags$td(class = "text-success", shiny::icon("arrow-up"))
                ),
                shiny::tags$tr(
                  shiny::tags$td("Cafe (lb)"),
                  shiny::tags$td(class = "text-right font-weight-bold", "USD 2,34"),
                  shiny::tags$td(class = "text-danger", shiny::icon("arrow-down"))
                ),
                shiny::tags$tr(
                  shiny::tags$td("IBR 90d"),
                  shiny::tags$td(class = "text-right font-weight-bold", "9,82 %"),
                  shiny::tags$td(class = "text-secondary", shiny::icon("minus"))
                )
              )
            )
          )
        ),
        racafe::dropdownMenuPlus(
          icon = shiny::icon("palette"), showBadge = FALSE, headerText = "Paleta corporativa",
          .list = list(
            .swatch("Primario", "#1A3A5C"),
            .swatch("Secundario", "#2E8B57"),
            .swatch("Acento", "#E8A020"),
            .swatch("Alerta", "#C0392B"),
            .swatch("Texto base", "#333333")
          )
        ),
        racafe::dropdownMenuPlus(
          icon = shiny::icon("book-open"), showBadge = FALSE, headerText = "Glosario",
          footerText = "Ver glosario completo", href = "#",
          .list = list(
            .termino("Rotacion", "Ventas / Inventario promedio del periodo."),
            .termino("Cobertura", "Dias de inventario sobre demanda media."),
            .termino("Cumplimiento", "Ventas reales sobre presupuesto vigente (%)."),
            .termino("Reactivacion", "Cliente sin compra >90d que retoma actividad.")
          )
        )
      )
    ),
    sidebar = bs4Dash::bs4DashSidebar(
      bs4Dash::bs4SidebarMenu(
        bs4Dash::bs4SidebarMenuItem("Alertas", tabName = "tab_alertas", icon = shiny::icon("exclamation-triangle")),
        bs4Dash::bs4SidebarMenuItem("Notificaciones", tabName = "tab_notif", icon = shiny::icon("bell")),
        bs4Dash::bs4SidebarMenuItem("Mensajes", tabName = "tab_msg", icon = shiny::icon("comments")),
        bs4Dash::bs4SidebarMenuItem("Tareas", tabName = "tab_tasks", icon = shiny::icon("list-check")),
        bs4Dash::bs4SidebarMenuItem("Estaticos", tabName = "tab_estaticos", icon = shiny::icon("code"))
      )
    ),
    body = bs4Dash::bs4DashBody(
      bs4Dash::bs4TabItems(
        bs4Dash::bs4TabItem(
          tabName = "tab_alertas",
          shiny::fluidRow(
            shiny::column(4, bs4Dash::bs4Card(
              title = "Controles — Alertas", width = 12, solidHeader = TRUE, status = "danger",
              shiny::sliderInput("n_alto", "Alertas criticas (danger):", 0, 5, 2, step = 1),
              shiny::sliderInput("n_medio", "Advertencias (warning):", 0, 5, 1, step = 1),
              shiny::sliderInput("n_bajo", "Avisos menores (info):", 0, 5, 1, step = 1)
            )),
            shiny::column(8, .panel_log("log_alertas", "Estado reactivo — Alertas", "danger"))
          ),
          shiny::fluidRow(shiny::column(12, .bloque_codigo(.cod$alertas$ui, .cod$alertas$server)))
        ),
        bs4Dash::bs4TabItem(
          tabName = "tab_notif",
          shiny::fluidRow(
            shiny::column(4, bs4Dash::bs4Card(
              title = "Controles — Notificaciones", width = 12, solidHeader = TRUE, status = "warning",
              shiny::sliderInput("n_notif", "Numero de notificaciones:", 0, 8, 3, step = 1)
            )),
            shiny::column(8, .panel_log("log_notif", "Estado reactivo — Notificaciones", "warning"))
          ),
          shiny::fluidRow(shiny::column(12, .bloque_codigo(.cod$notif$ui, .cod$notif$server)))
        ),
        bs4Dash::bs4TabItem(
          tabName = "tab_msg",
          shiny::fluidRow(
            shiny::column(4, bs4Dash::bs4Card(
              title = "Controles — Mensajes", width = 12, solidHeader = TRUE, status = "primary",
              shiny::sliderInput("n_msg", "Numero de mensajes:", 0, 6, 2, step = 1)
            )),
            shiny::column(8, .panel_log("log_msg", "Estado reactivo — Mensajes", "primary"))
          ),
          shiny::fluidRow(shiny::column(12, .bloque_codigo(.cod$msg$ui, .cod$msg$server)))
        ),
        bs4Dash::bs4TabItem(
          tabName = "tab_tasks",
          shiny::fluidRow(
            shiny::column(4, bs4Dash::bs4Card(
              title = "Controles — Tareas", width = 12, solidHeader = TRUE, status = "success",
              shiny::sliderInput("n_tasks", "Numero de tareas:", 0, 6, 3, step = 1)
            )),
            shiny::column(8, .panel_log("log_tasks", "Estado reactivo — Tareas", "success"))
          ),
          shiny::fluidRow(shiny::column(12, .bloque_codigo(.cod$tasks$ui, .cod$tasks$server)))
        ),
        bs4Dash::bs4TabItem(
          tabName = "tab_estaticos",
          shiny::fluidRow(
            shiny::column(4, .bloque_codigo(.cod$indicadores$ui)),
            shiny::column(4, .bloque_codigo(.cod$paleta$ui)),
            shiny::column(4, .bloque_codigo(.cod$glosario$ui))
          )
        )
      )
    ),
    footer = bs4Dash::bs4DashFooter(left = "Demo — dropdownMenuPlus", right = "racafeModulos")
  )

  server <- function(input, output, session) {
    alertas_r <- shiny::reactive(.gen_alertas(input$n_alto, input$n_medio, input$n_bajo))
    notificaciones_r <- shiny::reactive(.gen_notificaciones(input$n_notif))
    mensajes_r <- shiny::reactive(.gen_mensajes(input$n_msg))
    tareas_r <- shiny::reactive(.gen_tareas(input$n_tasks))

    dropdownMenuPlusServer(
      id = "menu_alertas",
      icon = shiny::icon("exclamation-triangle"),
      href = "#", footerText = "Ver historial de alertas",
      items_r = shiny::reactive(purrr::pmap(alertas_r(), ~ .alerta_item(..1, ..2))),
      numItems_r = shiny::reactive(nrow(alertas_r())),
      badgeStatus_r = shiny::reactive({
        df <- alertas_r()
        if (nrow(df) == 0L) return(NULL)
        if (any(df$nivel == "alto")) return("danger")
        if (any(df$nivel == "medio")) return("warning")
        "info"
      }),
      headerText = shiny::reactive(
        if (nrow(alertas_r()) == 0L) "Sin alertas activas"
        else sprintf("%d alerta(s) activa(s)", nrow(alertas_r()))
      )
    )

    dropdownMenuPlusServer(
      id = "menu_notificaciones",
      type = "notifications",
      badgeStatus_r = "danger",
      href = "#", footerText = "Ver todas las notificaciones",
      items_r = shiny::reactive(purrr::pmap(notificaciones_r(), function(texto, status) {
        bs4Dash::notificationItem(text = texto, status = status)
      })),
      numItems_r = shiny::reactive(nrow(notificaciones_r()))
    )

    dropdownMenuPlusServer(
      id = "menu_mensajes",
      type = "messages",
      badgeStatus_r = "primary",
      href = "#", footerText = "Ver todos los mensajes",
      items_r = shiny::reactive(purrr::pmap(mensajes_r(), function(from, message) {
        bs4Dash::messageItem(from = from, message = message)
      })),
      numItems_r = shiny::reactive(nrow(mensajes_r()))
    )

    dropdownMenuPlusServer(
      id = "menu_tareas",
      type = "tasks",
      href = "#", footerText = "Ver todas las tareas",
      items_r = shiny::reactive(purrr::pmap(tareas_r(), function(nombre, pct_avance) {
        bs4Dash::taskItem(text = nombre, value = pct_avance)
      })),
      numItems_r = shiny::reactive(nrow(tareas_r())),
      badgeStatus_r = shiny::reactive(if (nrow(tareas_r()) == 0L) NULL else "warning")
    )

    output$log_alertas <- shiny::renderPrint({
      df <- alertas_r()
      cat(sprintf(
        "Total: %d | Alto: %d | Medio: %d | Bajo: %d\n",
        nrow(df), sum(df$nivel == "alto"), sum(df$nivel == "medio"), sum(df$nivel == "bajo")
      ))
      if (nrow(df) > 0L) print(df) else cat("(sin alertas)\n")
    })

    output$log_notif <- shiny::renderPrint({
      df <- notificaciones_r()
      cat(sprintf("Total notificaciones: %d\n", nrow(df)))
      if (nrow(df) > 0L) print(df) else cat("(sin notificaciones)\n")
    })

    output$log_msg <- shiny::renderPrint({
      df <- mensajes_r()
      cat(sprintf("Total mensajes: %d\n", nrow(df)))
      if (nrow(df) > 0L) print(df) else cat("(sin mensajes)\n")
    })

    output$log_tasks <- shiny::renderPrint({
      df <- tareas_r()
      cat(sprintf(
        "Total tareas: %d | Avance promedio: %s\n",
        nrow(df),
        if (nrow(df) > 0L) sprintf("%.1f%%", mean(df$pct_avance)) else "N/A"
      ))
      if (nrow(df) > 0L) print(df) else cat("(sin tareas)\n")
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}


# Helpers internos ----

.dropdown_menu_plus_coalesce <- function(x, y) {
  if (is.null(x)) y else x
}

.dropdown_menu_plus_validate_status <- function(status) {
  valid_status <- c("primary", "secondary", "success", "info", "warning", "danger", "light", "dark")
  if (!is.character(status) || length(status) != 1L || !status %in% valid_status) {
    stop(
      "`badgeStatus_r` debe resolver a uno de: ",
      paste(valid_status, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(status)
}
