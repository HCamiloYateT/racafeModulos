test_that("CajaValor conserva UI y namespaces anidados", {
  ns <- shiny::NS("resumen")
  html <- as.character(cajaValor_ui(ns("ventas"), texto = "Café <script>", mostrar_boton = TRUE))
  expect_match(html, 'id="resumen-ventas-detalle"', fixed = TRUE)
  expect_match(html, 'id="resumen-ventas-valor_display"', fixed = TRUE)
  expect_match(html, "Café &lt;script&gt;", fixed = TRUE)
  expect_false(grepl('id="resumen-ventas-detalle"',
    as.character(cajaValor_ui(ns("ventas"), mostrar_boton = FALSE)), fixed = TRUE))
})

test_that("CajaValor transmite detalle y actualiza valores, incluido cero", {
  valor <- shiny::reactiveVal(1234.56)
  shiny::testServer(cajaValor_server, args = list(valor_r = valor, formato = "numero", meta = 100), {
    session$flushReact()
    expect_match(output$valor_display$html, "1,234.56", fixed = TRUE)
    expect_identical(shiny::isolate(session$returned$click_detalle()), NULL)
    session$setInputs(detalle = 1)
    expect_equal(shiny::isolate(session$returned$click_detalle()), 1)
    valor(0)
    session$flushReact()
    expect_match(output$valor_display$html, "0.00", fixed = TRUE)
    expect_identical(as.character(output$valor_display$html),
      as.character(racafeShiny::FormatearNumero(0, "numero", meta = 100)))
  })
})

test_that("Dos instancias de CajaValor se mantienen independientes", {
  parent <- function(id) shiny::moduleServer(id, function(input, output, session) {
    one <- cajaValor_server("one", shiny::reactive(12), "numero")
    two <- cajaValor_server("two", shiny::reactive(34), "numero")
    list(one = one, two = two)
  })
  shiny::testServer(parent, {
    session$setInputs(`one-detalle` = 2, `two-detalle` = 7)
    expect_equal(shiny::isolate(session$returned$one$click_detalle()), 2)
    expect_equal(shiny::isolate(session$returned$two$click_detalle()), 7)
  })
})

test_that("CajaValor admite borde e icono corporativos sin alterar el valor", {
  html <- as.character(cajaValor_ui("cliente", icono = "industry", mostrar_boton = FALSE,
    color_borde = "#DA291C", color_icono = "#DA291C", detalle = "Creación: 06/11/2026"))
  expect_match(html, "--rmcv-border:#DA291C", fixed = TRUE)
  expect_match(html, "--rmcv-icon:#DA291C", fixed = TRUE)
  expect_match(html, "fa-industry", fixed = TRUE)
  expect_match(html, "Creación: 06/11/2026", fixed = TRUE)
  expect_error(cajaValor_ui("bad", color_borde = "red;display:none"), "Color")
})

test_that("CajaValor distingue cantidades desconocidas y cero cuando se solicita empty", {
  valor <- shiny::reactiveVal(NA_real_)
  shiny::testServer(cajaValor_server, args = list(valor_r = valor, formato = "numero", empty = "Sin dato"), {
    session$flushReact()
    expect_match(output$valor_display$html, "Sin dato", fixed = TRUE)
    valor(0)
    session$flushReact()
    expect_match(output$valor_display$html, "0.00", fixed = TRUE)
  })
})

test_that("La meta respeta indicadores directos e inversos, incluida igualdad y cero", {
  cases <- data.frame(valor = c(9, 10, 11, 9, 10, 11, 0), meta = c(rep(10, 6), 0),
    prop = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    color = c("#C0392B", "#1A7A5E", "#1A7A5E", "#1A7A5E", "#1A7A5E", "#C0392B", "#1A7A5E"))
  for (i in seq_len(nrow(cases))) {
    item <- cases[i, ]
    shiny::testServer(cajaValor_server, args = list(valor_r = shiny::reactive(item$valor), formato = "numero",
      meta = item$meta, prop = item$prop), {
      session$flushReact()
      expect_match(output$valor_display$html, item$color, fixed = TRUE)
    })
  }
})

test_that("El ID de acción configurable conserva el evento público", {
  html <- as.character(cajaValor_ui("validacion", id_boton = "btn_abrir"))
  expect_match(html, 'id="validacion-btn_abrir"', fixed = TRUE)
  shiny::testServer(cajaValor_server, args = list(valor_r = shiny::reactive(2), formato = "entero",
    id_boton = "btn_abrir"), {
    session$setInputs(btn_abrir = 1)
    expect_equal(shiny::isolate(session$returned$click_detalle()), 1)
  })
})
