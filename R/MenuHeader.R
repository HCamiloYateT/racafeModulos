# Helpers internos ----

.mh_validate_status <- function(status) {
  validos <- c(
    "primary", "secondary", "success", "info", "warning", "danger",
    "light", "dark", "white", "gray", "gray-dark",
    "indigo", "lightblue", "navy", "purple", "fuchsia", "pink",
    "maroon", "orange", "lime", "teal", "olive"
  )
  if (!status %in% validos) {
    stop(sprintf(
      "status '%s' no válido. Opciones: %s", status, paste(validos, collapse = ", ")
    ), call. = FALSE)
  }
}

# Serializa columnas clave de una fila a objeto JS {k:"v", ...}
.mh_keys_js <- function(fila, cols) {
  pares <- mapply(function(k, v) {
    sprintf('"%s":"%s"', k, gsub('"', '\\"', as.character(v), fixed = TRUE))
  }, cols, fila[cols], SIMPLIFY = TRUE)
  paste0("{", paste(pares, collapse = ","), "}")
}

# Detecta el modo de despacho del contenido según tipo del objeto
.mh_tipo_items <- function(items) {
  if (is.null(items)) return("vacio")
  if (is.data.frame(items)) return("df")
  if (inherits(items, c("shiny.tag", "shiny.tag.list"))) return("tag")
  if (is.list(items)) return("lista")
  "vacio"
}


# Modulo ----

#' UI del módulo MenuHeader
#'
#' Crea un item de navbar tipo dropdown para `bs4Dash`, con trigger estático,
#' badge reactivo y panel con header, contenido y footer renderizados por
#' [MenuHeaderServer()].
#'
#' @param id String. ID del módulo.
#' @param icon Icono o tag que se mostrará como trigger del dropdown.
#' @param min_width String CSS para el ancho mínimo del panel. Default:
#'   `"280px"`.
#' @param title String opcional para mostrar un tooltip nativo del navegador
#'   al hacer hover sobre el trigger.
#'
#' @return `shiny.tag` con un `<li>` de navegación dropdown.
#' @export
#'
#' @examples
#' \dontrun{
#' MenuHeaderUI("menu_segmentos", icon = shiny::icon("layer-group"))
#' }
MenuHeaderUI <- function(id, icon, min_width = "280px", title = NULL) {
  ns <- shiny::NS(id)
  shiny::tags$li(
    class = "nav-item dropdown",
    # Trigger — title agrega tooltip nativo del navegador al hacer hover ----
    shiny::tags$a(
      class = "nav-link", `data-toggle` = "dropdown",
      href = "#", `aria-expanded` = "false",
      title = title,
      icon,
      shiny::uiOutput(ns("badge"), inline = TRUE)
    ),
    # stopPropagation evita que Bootstrap cierre el menú al hacer click interno ----
    shiny::tags$div(
      class = "dropdown-menu dropdown-menu-lg",
      style = paste0("min-width:", min_width, ";"),
      onclick = "event.stopPropagation();",
      shiny::uiOutput(ns("header_ui")),
      shiny::uiOutput(ns("contenido_ui")),
      shiny::uiOutput(ns("footer_ui"))
    )
  )
}

#' Server del módulo MenuHeader
#'
#' Renderiza un menú dropdown de navbar cuyo contenido puede provenir de un
#' `data.frame`, una lista de tags, un tag Shiny o `NULL`. Para `data.frame`,
#' genera items clicables y expone la selección como reactivo; opcionalmente
#' puede abrir un modal al seleccionar un item.
#'
#' @param id String. ID del módulo.
#' @param items_r Valor o reactive con items del menú. Acepta `data.frame`,
#'   lista, `shiny.tag`, `shiny.tag.list` o `NULL`.
#' @param key_cols Character. Columnas clave para serializar la selección en
#'   modo `data.frame`.
#' @param badgeStatus_r Valor o reactive con el status del badge. Si es `NULL`
#'   se oculta.
#' @param headerText Valor o reactive con el texto del header.
#' @param href String opcional para el footer. Si es `NULL`, no se muestra.
#' @param footerText Texto del footer. Default: `"Ver más"`.
#' @param showHeader Logical. Si `FALSE`, oculta el header.
#' @param modal_titulo_fn Function opcional que recibe la selección y retorna
#'   el título del modal.
#' @param modal_pre_fn Function opcional ejecutada antes de abrir el modal.
#' @param modal_contenido_fn Function opcional que recibe la selección y retorna
#'   la UI del modal.
#' @param modal_size String. Tamaño de `shiny::modalDialog()`. Default: `"l"`.
#' @param modal_icon String opcional para `shiny::icon()` en el título.
#'
#' @return Lista con reactivos `seleccion` y `n`.
#' @export
#'
#' @examples
#' \dontrun{
#' MenuHeaderServer(
#'   "menu_segmentos",
#'   items_r = shiny::reactive(segmentos),
#'   key_cols = "segmento",
#'   badgeStatus_r = "info"
#' )
#' }
MenuHeaderServer <- function(id, items_r = NULL, key_cols = NULL,
                             badgeStatus_r = NULL, headerText = NULL,
                             href = NULL, footerText = "Ver más",
                             showHeader = TRUE, modal_titulo_fn = NULL,
                             modal_pre_fn = NULL, modal_contenido_fn = NULL,
                             modal_size = "l", modal_icon = NULL,
                             badge_n_r = NULL) {
  
  .resolve <- function(x) if (shiny::is.reactive(x)) x() else x
  usar_modal <- !is.null(modal_titulo_fn) && !is.null(modal_contenido_fn)

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    seleccion_r <- shiny::reactiveVal(NULL)

    # Conteo inferido — solo df y lista producen n > 0 ----
    n_r <- shiny::reactive({
      override <- .resolve(badge_n_r)
      if (!is.null(override)) return(as.integer(override))
      items <- .resolve(items_r)
      switch(.mh_tipo_items(items), df = nrow(items), lista = length(items), 0L)
    })

    # Badge — oculto si n == 0 o badgeStatus es NULL ----
    output$badge <- shiny::renderUI({
      badge_st <- .resolve(badgeStatus_r)
      if (is.null(badge_st) || n_r() == 0L) return(NULL)
      .mh_validate_status(badge_st)
      shiny::tags$span(class = paste0("badge badge-", badge_st, " navbar-badge"), n_r())
    })

    # Header + divisor condicional ----
    output$header_ui <- shiny::renderUI({
      if (!showHeader) return(NULL)
      ht <- .resolve(headerText)
      if (is.null(ht) || !nzchar(as.character(ht))) return(NULL)
      shiny::tagList(
        shiny::tags$span(class = "dropdown-item dropdown-header", ht),
        shiny::tags$div(class = "dropdown-divider")
      )
    })

    # Contenido — despacha según tipo ----
    output$contenido_ui <- shiny::renderUI({
      items <- .resolve(items_r)
      tipo <- .mh_tipo_items(items)
      switch(tipo,
        tag = items,
        lista = do.call(shiny::tagList, items),
        df = {
          shiny::req(!is.null(key_cols), all(key_cols %in% names(items)))
          cols_vis <- setdiff(names(items), key_cols)
          filas_ui <- lapply(seq_len(nrow(items)), function(i) {
            fila <- items[i, , drop = FALSE]
            badge_item <- if ("status" %in% cols_vis && nzchar(as.character(fila$status))) {
              st <- as.character(fila$status)
              .mh_validate_status(st)
              shiny::tags$span(class = paste0("badge badge-", st, " mr-2"), st)
            }
            shiny::tags$a(
              class = "dropdown-item d-flex align-items-center",
              href = "javascript:void(0);",
              style = "white-space:normal;",
              onclick = sprintf(
                "Shiny.setInputValue('%s', %s, {priority:'event'});",
                ns("item_click"), .mh_keys_js(fila, key_cols)
              ),
              badge_item,
              shiny::tags$span(as.character(fila$label), style = "font-size:13px;")
            )
          })
          do.call(shiny::tagList, filas_ui)
        },
        NULL
      )
    })

    # Footer ----
    output$footer_ui <- shiny::renderUI({
      if (is.null(href)) return(NULL)
      shiny::tags$a(class   = "dropdown-item dropdown-footer",
                    href    = "javascript:void(0);",
                    onclick = sprintf(
                      "Shiny.setInputValue('%s', Date.now(), {priority:'event'});",
                      ns("footer_click")
                    ),
                    footerText
                   )
    })

    # Click handler — resuelve selección y dispara modal si está configurado ----
    shiny::observeEvent(input$item_click, {
      sel <- input$item_click
      seleccion_r(sel)
      if (!usar_modal) return()
      if (!is.null(modal_pre_fn)) modal_pre_fn(sel)
      titulo <- if (!is.null(modal_icon)) {
        shiny::tagList(shiny::icon(modal_icon), " ", modal_titulo_fn(sel))
      } else {
        modal_titulo_fn(sel)
      }
      shiny::showModal(shiny::modalDialog(
        title = titulo, size = modal_size, easyClose = TRUE,
        footer = shiny::modalButton("Cerrar"),
        modal_contenido_fn(sel)
      ))
    }, ignoreNULL = TRUE)

   list(
     seleccion    = shiny::reactive(seleccion_r()),
     n            = shiny::reactive(n_r()),
     footer_click = shiny::reactive(input$footer_click)
   )
  })
}


# DemoMenuHeader ----

#' Demo del módulo MenuHeader
#'
#' App autocontenida que muestra el módulo en tres modos: `data.frame` con
#' modal `gt`, lista de tags KPI y contenido embebido con `reactable` dentro
#' del panel del dropdown.
#'
#' @return Ejecuta `shiny::shinyApp()`.
#' @export
#'
#' @examples
#' \dontrun{
#' DemoMenuHeader()
#' }
DemoMenuHeader <- function() {

  # Datos ----

  .clientes <- tibble::tibble(
    segmento = c("A", "A", "A", "B", "B", "C", "C", "C", "C"),
    cliente = c(
      "Ana Gómez", "Luis Torres", "Paula Díaz",
      "Carlos Ruiz", "María López",
      "Juan Pérez", "Pedro Silva", "Diana Castro", "Rosa Medina"
    ),
    saldo = c(21000, 15600, 9100, 12500, 4800, 8300, 6700, 11200, 3400),
    mora_pct = c(0.2, 0.5, 0.8, 1.2, 2.1, 3.4, 1.8, 0.9, 4.2),
    sacos = c(840L, 620L, 380L, 510L, 210L, 320L, 280L, 450L, 190L)
  )

  .segmentos <- tibble::tibble(
    segmento = c("A", "B", "C"),
    label = c(
      "Segmento A — 3 clientes", "Segmento B — 2 clientes",
      "Segmento C — 4 clientes"
    ),
    status = c("success", "info", "warning")
  )


  # Módulo: ResumenSegmento — tabla gt renderizada en modal ----
  ResumenSegmentoUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
      shiny::uiOutput(ns("subtitulo")),
      gt::gt_output(ns("tabla"))
    )
  }
  ResumenSegmentoServer <- function(id, sel_r, data) {
    shiny::moduleServer(id, function(input, output, session) {
      df_r <- shiny::reactive({
        sel <- sel_r()
        shiny::req(!is.null(sel))
        dplyr::filter(data, segmento == sel$segmento) |>
          dplyr::select(-segmento)
      })
      output$subtitulo <- shiny::renderUI({
        shiny::req(!is.null(sel_r()))
        shiny::tags$p(
          style = "font-size:12px; color:#666; margin-bottom:8px;",
          shiny::icon("users"),
          sprintf(" %d cliente(s) en el segmento.", nrow(df_r()))
        )
      })
      output$tabla <- gt::render_gt({
        df_r() |>
          gt::gt() |>
          gt::cols_label(
            cliente ~ "Cliente",
            saldo ~ "Saldo (COP)",
            mora_pct ~ "Mora %",
            sacos ~ "Sacos"
          ) |>
          gt::fmt_number(columns = saldo, decimals = 0, sep_mark = ",") |>
          gt::fmt_number(columns = mora_pct, decimals = 1, suffix = "%") |>
          gt::fmt_integer(columns = sacos, sep_mark = ",") |>
          gt::tab_style(
            style = gt::cell_fill(color = "#FFF8EC"),
            locations = gt::cells_body(rows = mora_pct > 2)
          ) |>
          gt::tab_style(
            style = gt::cell_fill(color = "#FEF2F2"),
            locations = gt::cells_body(rows = mora_pct > 3)
          ) |>
          gt::tab_options(
            table.font.size = 13,
            column_labels.font.weight = "bold",
            table.width = gt::pct(100)
          )
      })
    })
  }

  # Módulo: TablaClientes — reactable con filtro embebido en el dropdown ----
  TablaClientesUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
      shiny::tags$div(
        style = "padding:6px 12px 2px;",
        shiny::selectInput(ns("seg"),
          label = NULL,
          choices = c("Todos" = "", "A", "B", "C"),
          width = "100%"
        )
      ),
      shiny::tags$div(
        style = "max-height:260px; overflow-y:auto;",
        reactable::reactableOutput(ns("tabla"), width = "100%")
      ),
      shiny::uiOutput(ns("resumen"))
    )
  }
  TablaClientesServer <- function(id, data) {
    shiny::moduleServer(id, function(input, output, session) {

      df_r <- shiny::reactive({
        seg <- if (is.null(input$seg)) "" else input$seg
        if (nzchar(seg)) dplyr::filter(data, segmento == seg) else data
      })

      output$tabla <- reactable::renderReactable({
        reactable::reactable(
          df_r(), compact = TRUE, pagination = FALSE, bordered = TRUE,
          columns = list(
            segmento = reactable::colDef(name = "Seg.", maxWidth = 55),
            cliente = reactable::colDef(name = "Cliente", minWidth = 110),
            saldo = reactable::colDef(name = "Saldo", maxWidth = 90, cell = function(v) format(v, big.mark = ",")),
            mora_pct = reactable::colDef(name = "Mora %", maxWidth = 70, style = function(v) {
              if (v > 3) {
                list(color = "#A32D2D", fontWeight = "600")
              } else if (v > 1.5) {
                list(color = "#B45309")
              } else {
                list(color = "#1A8754")
              }
            }),
            sacos = reactable::colDef(name = "Sacos", maxWidth = 70)
          ),
          theme = reactable::reactableTheme(
            headerStyle = list(fontWeight = "600", fontSize = "11px"),
            cellStyle = list(fontSize = "12px", padding = "3px 6px")
          )
        )
      })

      output$resumen <- shiny::renderUI({
        shiny::tags$p(
          style = "font-size:11px; color:#888; padding:4px 12px 6px; margin:0;",
          shiny::icon("circle-info"),
          sprintf(" %d de %d clientes", nrow(df_r()), nrow(data))
        )
      })

      list(n = shiny::reactive(nrow(df_r())))
    })
  }


  # Helper — ítem KPI para modo lista ----
  .item_kpi <- function(label, valor, delta, status) {
    color <- switch(status,
      success = "#1A8754", danger = "#A32D2D", warning = "#B45309", "#1A5276"
    )
    shiny::tags$div(
      class = "dropdown-item",
      style = "padding:8px 16px; border-bottom:1px solid #F0F0F0;",
      shiny::tags$div(
        style = "display:flex; justify-content:space-between; align-items:center;",
        shiny::tags$span(label, style = "font-size:13px; color:#555;"),
        shiny::tags$span(
          shiny::tags$strong(valor, style = paste0("color:", color, ";")),
          shiny::tags$small(delta, style = paste0("color:", color, "; margin-left:6px;"))
        )
      )
    )
  }

  ui <- bs4Dash::bs4DashPage(
    title = "Demo MenuHeader", dark = NULL,
    header = bs4Dash::bs4DashNavbar(
      status = "white", border = FALSE,
      title = bs4Dash::bs4DashBrand(title = "Demo MenuHeader", color = "white"),
      rightUi = shiny::tagList(
        MenuHeaderUI("menu_segmentos", icon = shiny::icon("layer-group"), min_width = "300px"),
        MenuHeaderUI("menu_kpis", icon = shiny::icon("chart-bar"), min_width = "280px"),
        MenuHeaderUI("menu_tabla", icon = shiny::icon("users"), min_width = "360px")
      )
    ),
    sidebar = bs4Dash::bs4DashSidebar(
      bs4Dash::bs4SidebarMenu(
        bs4Dash::bs4SidebarMenuItem("df + modal gt", tabName = "tab_df", icon = shiny::icon("layer-group")),
        bs4Dash::bs4SidebarMenuItem("Lista de tags", tabName = "tab_lista", icon = shiny::icon("chart-bar")),
        bs4Dash::bs4SidebarMenuItem("Tabla en dropdown", tabName = "tab_tabla", icon = shiny::icon("users"))
      )
    ),
    body = bs4Dash::bs4DashBody(
      bs4Dash::bs4TabItems(
        ## Tab — df + modal gt ----
        bs4Dash::bs4TabItem(
          tabName = "tab_df", shiny::br(),
          shiny::fluidRow(
            shiny::column(
              6,
              bs4Dash::bs4Card(
                title = "Segmentos disponibles en el menú",
                width = 12, solidHeader = TRUE, status = "white",
                reactable::reactableOutput("tabla_segmentos")
              )
            ),
            shiny::column(
              6,
              bs4Dash::bs4Card(
                title = "res_seg$seleccion()",
                width = 12, solidHeader = TRUE, status = "white",
                shiny::tags$p(
                  class = "text-muted", style = "font-size:12px;",
                  shiny::icon("hand-pointer"),
                  " Haz click en el menú ", shiny::icon("layer-group"),
                  " para abrir el modal con tabla gt."
                ),
                shiny::verbatimTextOutput("sel_segmento")
              )
            )
          )
        ),
        ## Tab — lista de tags ----
        bs4Dash::bs4TabItem(
          tabName = "tab_lista", shiny::br(),
          shiny::fluidRow(
            shiny::column(
              12,
              bs4Dash::bs4Card(
                title = "Modo lista de tags — KPIs visuales",
                width = 12, solidHeader = TRUE, status = "white",
                shiny::tags$p(
                  class = "text-muted",
                  "El menú ", shiny::tags$strong(shiny::icon("chart-bar")),
                  " muestra 3 KPIs como tags personalizados.",
                  " Badge = ", shiny::tags$code("length(list)"), " = 3.",
                  shiny::tags$br(),
                  shiny::tags$code("seleccion()"), " siempre es ",
                  shiny::tags$code("NULL"), " — la interacción la define cada tag."
                )
              )
            )
          )
        ),
        ## Tab — tabla embebida en el dropdown ----
        bs4Dash::bs4TabItem(
          tabName = "tab_tabla", shiny::br(),
          shiny::fluidRow(
            shiny::column(
              12,
              bs4Dash::bs4Card(
                title = "Tabla reactable directamente en el panel del dropdown",
                width = 12, solidHeader = TRUE, status = "white",
                shiny::tags$p(
                  class = "text-muted",
                  "El menú ", shiny::tags$strong(shiny::icon("users")),
                  " contiene el módulo ", shiny::tags$code("TablaClientesUI"),
                  " completo — filtro de segmento y reactable con color por mora.",
                  shiny::tags$br(),
                  "El módulo se registra en el server del padre normalmente.",
                  " Su UI se pasa como ", shiny::tags$code("items_r"),
                  " al tipo ", shiny::tags$code('"tag"'), " de MenuHeaderServer.",
                  shiny::tags$br(),
                  "Badge oculto: n no es inferible desde un ",
                  shiny::tags$code("shiny.tag.list"), "."
                ),
                shiny::verbatimTextOutput("n_tabla")
              )
            )
          )
        )
      )
    ),
    # Footer ----
    footer = bs4Dash::bs4DashFooter(left = "Demo MenuHeader", right = "racafeModulos")
  )

  # Server ----

  server <- function(input, output, session) {

    # rv para pasar la selección al módulo auxiliar antes de showModal ----
    sel_seg_rv <- shiny::reactiveVal(NULL)

    # Módulos auxiliares — registrados antes del primer click ----
    ResumenSegmentoServer("mod_resumen",
      sel_r = shiny::reactive(sel_seg_rv()),
      data = .clientes
    )
    mod_tabla <- TablaClientesServer("mod_tabla_drop", data = .clientes)

    # Módulo — modo data.frame + modal gt ----
    res_seg <- MenuHeaderServer(
      id = "menu_segmentos",
      items_r = shiny::reactive(.segmentos),
      key_cols = "segmento",
      badgeStatus_r = "info",
      headerText = "Segmentos de clientes",
      href = "#", footerText = "Ver reporte completo",
      modal_icon = "layer-group",
      modal_size = "m",
      modal_titulo_fn = function(sel) paste0("Detalle — Segmento ", sel$segmento),
      modal_pre_fn = function(sel) sel_seg_rv(sel),
      modal_contenido_fn = function(sel) ResumenSegmentoUI("mod_resumen")
    )

    # Módulo — modo lista de tags ----
    MenuHeaderServer(
      id = "menu_kpis",
      items_r = shiny::reactive(list(
        .item_kpi("Retención", "78.4 %", "+2.1 pp", "success"),
        .item_kpi("Pérdida", "12.3 %", "-0.8 pp", "danger"),
        .item_kpi("Sacos", "42.150", "+3.2 %", "info")
      )),
      badgeStatus_r = "success",
      headerText = "Indicadores clave"
    )

    # Módulo — tabla reactable embebida en el panel del dropdown ----
    # TablaClientesUI retorna shiny.tag.list → tipo "tag" en MenuHeaderServer
    MenuHeaderServer(
      id = "menu_tabla",
      items_r = TablaClientesUI("mod_tabla_drop"),
      headerText = "Clientes",
      href = "#", footerText = "Ver todos"
    )

    # Outputs de demo ----
    output$tabla_segmentos <- reactable::renderReactable({
      reactable::reactable(.segmentos,
        compact = TRUE, bordered = TRUE, pagination = FALSE,
        columns = list(
          segmento = reactable::colDef(name = "Clave", maxWidth = 80),
          label = reactable::colDef(name = "Descripción"),
          status = reactable::colDef(
            name = "Estado", maxWidth = 90,
            cell = function(v) shiny::tags$span(class = paste0("badge badge-", v), v)
          )
        )
      )
    })
    output$sel_segmento <- shiny::renderPrint({
      sel <- res_seg$seleccion()
      if (is.null(sel)) cat("(sin selección — haz click en el menú de segmentos)\n")
      else utils::str(sel)
    })
    output$n_tabla <- shiny::renderPrint({
      cat(sprintf("Clientes visibles en el dropdown: %d\n", mod_tabla$n()))
    })
  }

  shiny::shinyApp(ui, server)
}
