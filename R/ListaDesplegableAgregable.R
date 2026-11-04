#' UI del módulo ListaDesplegableAgregable
#' @param id Identificador del módulo Shiny, también admite un namespace padre.
#' @return Contenedores picker_ui y nuevo_ui; conserva los IDs existentes del módulo.
#' @export
ListaDesplegableAgregableUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::uiOutput(ns("picker_ui")), shiny::uiOutput(ns("nuevo_ui")))
}

# Comparación corporativa sin modificar códigos ni etiquetas del catálogo ----
.lda_trim <- function(value) trimws(as.character(value), whitespace = "[\\h\\v]")
.lda_key <- function(value) {
  value <- gsub("[\\h\\v]+", " ", .lda_trim(value), perl = TRUE)
  value <- chartr("áéíóúüÁÉÍÓÚÜ", "aeiouuAEIOUU", value)
  toupper(gsub("[\u0301\u0308]", "", value, perl = TRUE))
}
.lda_values <- function(choices) unname(as.character(unlist(choices, use.names = FALSE)))
.lda_keys <- function(choices) .lda_key(c(.lda_values(choices), names(choices)))
.lda_picker_choices <- function(choices, multiple, fem) {
  empty <- if (!multiple && !"" %in% .lda_values(choices)) stats::setNames("", if (fem) "Ninguna" else "Ninguno")
  c(stats::setNames("AGREGAR...", "AGREGAR..."), empty, choices)
}
.lda_capture_ui <- function(ns) {
  racafeForms::racafeForm(id = "nuevo_form", ns = ns, variant = "plain",
    racafeForms::racafeFormRow(actions = TRUE,
      shiny::tagList(racafeForms::racafeInputText("nuevo_valor", "Nueva opción", required = TRUE,
                                                placeholder = "Escriba la nueva opción...", ns = ns),
                      shiny::uiOutput(ns("error_valor"))),
      racafeForms::racafeButton("guardar_nuevo", "Guardar", icon = "floppy-disk", ns = ns)))
}

#' Lista desplegable con creación de opciones en la sesión
#' @description Utiliza racafeForms y mantiene el catálogo exclusivamente en memoria.
#' @param id Identificador del módulo Shiny.
#' @param label Etiqueta del control.
#' @param choices Vector de opciones iniciales; puede usar etiquetas con nombres y códigos distintos.
#' @param selected Valores seleccionados inicialmente; NULL no selecciona opciones.
#' @param multiple Permitir selección múltiple.
#' @param fem Usar etiquetas femeninas.
#' @param uppercase Presentación visual en mayúsculas; FALSE conserva la capitalización original.
#' @param confirm Solicitar racafeConfirm antes de incorporar una opción válida; TRUE conserva la confirmación anterior.
#' @param confirmation alert conserva SweetAlert; summary utiliza el modal con resumen de racafeForms.
#' @return Lista con reactivos seleccion y choices. seleccion nunca incluye AGREGAR...
#' @details La opción reservada AGREGAR... ocupa siempre el primer lugar. Los duplicados
#'   se comparan ignorando mayúsculas, espacios redundantes y tildes agudas/diéresis de vocales,
#'   incluidas las formas combinantes. La ñ se distingue de la n. Se comparan códigos y etiquetas.
#'   Solo se recortan los espacios extremos del valor nuevo; no se cambia su capitalización.
#'   No muestra errores hasta guardar. Revalida el valor confirmado contra el catálogo vigente
#'   inmediatamente antes de incorporarlo y consume cada confirmación una sola vez.
#'   No modifica bases de datos ni persistencia. Mantiene los IDs sel, nuevo_valor, guardar_nuevo,
#'   error_valor, ConfirmarNuevo y CancelarNuevo. ConfirmarNuevo recibe ahora un lógico de SweetAlert.
#' @export
ListaDesplegableAgregable <- function(id, label = NULL, choices, selected = choices, multiple = TRUE, fem = FALSE,
                                      uppercase = TRUE, confirm = TRUE, confirmation = c("alert", "summary")) {
  if (!"uppercase" %in% names(formals(racafeForms::racafeInputSelect))) {
    stop("Actualice racafeForms a la revisión con selectores corporativos y validación inline.")
  }
  confirmation <- match.arg(confirmation)
  if (confirmation == "summary" && !"racafeConfirmation" %in% getNamespaceExports("racafeForms")) {
    stop("Actualice racafeForms a 0.6.0 para utilizar la confirmación con resumen.")
  }
  for (flag in list(multiple, fem, uppercase, confirm)) {
    if (!is.logical(flag) || length(flag) != 1L || is.na(flag)) stop("Los estados deben ser lógicos escalares.")
  }
  if (any(.lda_keys(choices) == "AGREGAR...")) stop("AGREGAR... es una opción reservada, no un valor del catálogo.")
  shiny::moduleServer(id, function(input, output, session) {
    choices_r <- shiny::reactiveVal(choices)
    mostrar_nuevo_r <- shiny::reactiveVal(FALSE)
    attempted_r <- shiny::reactiveVal(FALSE)
    pending_r <- shiny::reactiveVal(NULL)
    bound_r <- shiny::reactiveVal(FALSE)
    initial <- intersect(as.character(selected), .lda_values(choices))
    if (!multiple) initial <- if (length(initial)) initial[[1]] else ""
    selection_r <- shiny::reactiveVal(initial)
    ns <- session$ns
    rules <- list(nuevo_valor = function(value, values) {
      if (.lda_key(value) %in% c(.lda_keys(choices_r()), "AGREGAR...")) {
        "Esta opción ya existe en el catálogo."
      } else TRUE
    })
    .validate <- function(value) racafeForms::racafeValidateForm(list(nuevo_valor = value),
      .lda_capture_ui(ns), rules = rules, ns = ns)
    result <- shiny::reactive(.validate(input$nuevo_valor))

    # El picker conserva su binding; las incorporaciones usan el actualizador ----
    output$picker_ui <- shiny::renderUI({
      picker <- racafeForms::racafeInputSelect("sel", label,
        .lda_picker_choices(shiny::isolate(choices_r()), multiple, fem),
        selected = shiny::isolate(selection_r()), multiple = multiple, empty = FALSE,
        gender = if (fem) "f" else "m", uppercase = uppercase, ns = ns)
      picker <- shiny::tagAppendAttributes(picker, `data-rf-action-value` = "AGREGAR...")
      racafeForms::racafeForm(variant = "plain", picker)
    })
    shiny::observeEvent(input$sel, {
      if (is.null(input$sel) && !bound_r()) return()
      bound_r(TRUE)
      selected_now <- as.character(input$sel)
      clean <- intersect(setdiff(selected_now, "AGREGAR..."), .lda_values(choices_r()))
      if ("AGREGAR..." %in% selected_now) {
        if (!multiple) clean <- selection_r()
        if (!mostrar_nuevo_r()) attempted_r(FALSE)
        mostrar_nuevo_r(TRUE)
        racafeForms::racafeUpdateSelect(session, "sel", selected = clean)
      }
      if (!multiple) clean <- if (length(clean)) clean[[1]] else ""
      selection_r(clean)
    }, ignoreNULL = FALSE)
    output$nuevo_ui <- shiny::renderUI({
      if (mostrar_nuevo_r()) .lda_capture_ui(ns)
    })
    output$error_valor <- shiny::renderUI({
      if (!mostrar_nuevo_r() || !attempted_r()) return(NULL)
      errors <- result()$errors
      if ("nuevo_valor" %in% names(errors)) {
        racafeForms::racafeFormMessage(errors[["nuevo_valor"]], "error", size = "compact")
      }
    })

    # Consumir primero la solicitud y revalidar contra el catálogo vigente ----
    .commit <- function() {
      pending <- pending_r()
      if (is.null(pending)) return(invisible(NULL))
      pending_r(NULL)
      racafeForms::racafeUpdateButton(session, "guardar_nuevo", loading = FALSE)
      if (!.validate(pending)$valid) return(invisible(NULL))
      updated <- c(choices_r(), pending)
      selected_now <- if (multiple) unique(c(selection_r(), pending)) else pending
      choices_r(updated)
      selection_r(selected_now)
      racafeForms::racafeUpdateSelect(session, "sel", choices = .lda_picker_choices(updated, multiple, fem),
                                     selected = selected_now)
      shiny::updateTextInput(session, "nuevo_valor", value = "")
      mostrar_nuevo_r(FALSE)
      attempted_r(FALSE)
      racafeForms::racafeNotify(session, "Opción agregada", paste0('Se agregó "', pending, '" al catálogo.'),
                                type = "success", duration = 3)
    }
    shiny::observeEvent(input$guardar_nuevo, {
      if (!mostrar_nuevo_r() || !is.null(pending_r())) return()
      attempted_r(TRUE)
      if (!result()$valid) return()
      pending_r(.lda_trim(input$nuevo_valor))
      if (!confirm) { .commit(); return() }
      racafeForms::racafeUpdateButton(session, "guardar_nuevo", loading = TRUE)
      if (confirmation == "summary") {
        shiny::showModal(racafeForms::racafeConfirmation("ConfirmarNuevo", list(`Nueva opción` = pending_r()),
          title = "Confirmar nueva opción", ns = ns, cancel_id = "CancelarNuevo"))
      } else racafeForms::racafeConfirm(session, "ConfirmarNuevo", "Confirmar nueva opción",
        paste0('¿Desea agregar "', pending_r(), '" a la lista?'), ns = ns)
    })
    shiny::observeEvent(input$ConfirmarNuevo, {
      accepted <- if (confirmation == "summary") is.numeric(input$ConfirmarNuevo) &&
        length(input$ConfirmarNuevo) == 1L && input$ConfirmarNuevo > 0 else isTRUE(input$ConfirmarNuevo)
      if (accepted) {
        if (confirmation == "summary") shiny::removeModal()
        .commit()
      } else {
        pending_r(NULL)
        racafeForms::racafeUpdateButton(session, "guardar_nuevo", loading = FALSE)
      }
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$CancelarNuevo, {
      pending_r(NULL)
      racafeForms::racafeUpdateButton(session, "guardar_nuevo", loading = FALSE)
    })
    list(seleccion = shiny::reactive(setdiff(selection_r(), "AGREGAR...")), choices = shiny::reactive(choices_r()))
  })
}

#' Demo de ListaDesplegableAgregable
#'
#' Ejecuta una aplicación autocontenida con versiones múltiple y única del
#' módulo, además de una salida que permite inspeccionar selección y catálogo.
#'
#' @return Una aplicación `shiny.appobj`.
#' @export
#' @examples
#' \dontrun{DemoListaDesplegableAgregable()}
DemoListaDesplegableAgregable <- function() {
  .card_codigo <- function(titulo, codigo) {
    bs4Dash::bs4Card(
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      status = "white",
      title = shiny::tagList(shiny::icon("code"), paste(" Código:", titulo)),
      shiny::tags$pre(shiny::tags$code(codigo))
    )
  }

  codigo_multiple <- paste(
    'ListaDesplegableAgregableUI("seg_multiple")',
    "",
    'ListaDesplegableAgregable(',
    '  "seg_multiple", label = "Segmento",',
    '  choices = c("A la medida", "Convencionales", "Premium"),',
    '  selected = NULL, multiple = TRUE',
    ')',
    sep = "\n"
  )
  codigo_single <- paste(
    'ListaDesplegableAgregableUI("cat_single")',
    "",
    'ListaDesplegableAgregable(',
    '  "cat_single", label = "Categoría",',
    '  choices = c("Café", "Azúcar", "Panela"),',
    '  selected = "Café", multiple = FALSE, fem = TRUE',
    ')',
    sep = "\n"
  )

  ui <- bs4Dash::bs4DashPage(
    title = "Demo ListaDesplegableAgregable",
    header = bs4Dash::bs4DashNavbar(title = "Demo ListaDesplegableAgregable"),
    sidebar = bs4Dash::bs4DashSidebar(disable = TRUE),
    footer = bs4Dash::bs4DashFooter(),
    controlbar = bs4Dash::dashboardControlbar(
      racafeForms::racafeForm(variant = "plain",
        ListaDesplegableAgregableUI("bar_single"), ListaDesplegableAgregableUI("bar_multiple")),
      id = "demo_controlbar", width = 440, pinned = FALSE),
    body = bs4Dash::bs4DashBody(
      racafeForms::racafeFormsDependencies(),
      racafeForms::racafeButton("demo_modal", "Probar modal y footer corporativo"),
      racafeForms::racafeSharedAssets(system.file("gallery-assets", package = "racafeForms"),
                                      global_css = FALSE, prefix = "racafe-modulos-demo"),
      shiny::fluidRow(
        bs4Dash::bs4Card(
          width = 6,
          collapsible = FALSE,
          title = "Segmento (múltiple)",
          ListaDesplegableAgregableUI("seg_multiple")
        ),
        bs4Dash::bs4Card(
          width = 6,
          collapsible = FALSE,
          title = "Categoría (única)",
          ListaDesplegableAgregableUI("cat_single")
        )
      ),
      shiny::fluidRow(
        bs4Dash::bs4Card(
          width = 12,
          collapsible = FALSE,
          title = "Selección y catálogo en vivo",
          shiny::verbatimTextOutput("resultado")
        )
      ),
      shiny::fluidRow(
        .card_codigo("Picker múltiple con opción de agregar", codigo_multiple),
        .card_codigo("Picker único con opción de agregar", codigo_single)
      )
    )
  )

  server <- function(input, output, session) {
    res_bar_single <- ListaDesplegableAgregable("bar_single", "Categoría en controlbar", c("Café", "Té"),
                                               selected = "Café", multiple = FALSE, fem = TRUE,
                                               confirmation = "summary")
    res_bar_multiple <- ListaDesplegableAgregable("bar_multiple", "Segmentos en controlbar", c("Café", "Premium"),
                                                 selected = "Premium", multiple = TRUE, confirmation = "summary")
    shiny::observeEvent(input$demo_modal, {
      shiny::showModal(racafeForms::racafeModal("Sin persistencia ni datos empresariales", title = "Modal de prueba",
        size = "aviso", footer = racafeForms::racafeModalFooter(shiny::modalButton("Cancelar"),
          shiny::actionButton("demo_confirm", "Confirmar"))))
    })
    shiny::observeEvent(input$demo_confirm, { shiny::removeModal() })
    res_multiple <- ListaDesplegableAgregable(
      "seg_multiple",
      label = "Segmento",
      choices = c("A la medida", "Convencionales", "Premium"),
      selected = NULL,
      multiple = TRUE
    )
    res_single <- ListaDesplegableAgregable(
      "cat_single",
      label = "Categoría",
      choices = c("Café", "Azúcar", "Panela"),
      selected = "Café",
      multiple = FALSE,
      fem = TRUE
    )

    output$resultado <- shiny::renderPrint({
      list(
        seg_seleccion = res_multiple$seleccion(),
        seg_catalogo = res_multiple$choices(),
        cat_seleccion = res_single$seleccion(),
        cat_catalogo = res_single$choices(),
        bar_simple = res_bar_single$seleccion(), bar_multiple = res_bar_multiple$seleccion()
      )
    })
  }

  ui <- shiny::tagList(htmltools::suppressDependencies("googlefonts", "ionicons"), ui)
  shiny::shinyApp(ui = ui, server = server)
}
