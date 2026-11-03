test_that("la tarjeta compacta conserva precisión y escapa datos", {
  expect_equal(.tarjeta_value(2, NULL), "2")
  expect_equal(.tarjeta_value(1234.56, NULL), "1,234.56")
  expect_equal(.tarjeta_value(-1234567.89, NULL), "-1,234,567.89")
  expect_equal(.tarjeta_value(.125, NULL), "0.125")
  expect_equal(.tarjeta_value(NA_real_, NULL), "—")
  expect_equal(.tarjeta_value(1234.56, "numero"), "1,234.56")
  expect_equal(.tarjeta_value(2, function(x) paste0(x, " sacos")), "2 sacos")
  expect_error(.tarjeta_value(1:2, NULL))
  ui <- TarjetaUI(shiny::NS("padre")("pedidos"))
  expect_match(as.character(ui), 'id="padre-pedidos-contenido"', fixed = TRUE)
  expect_length(htmltools::htmlDependencies(ui), 1L)
})

test_that("los valores reactivos no recrean el botón ni generan eventos", {
  value <- shiny::reactiveVal(2)
  title <- shiny::reactiveVal("Pedidos")
  shiny::testServer(Tarjeta, args = list(titulo = title, valor = value, descripcion = "Creación: 20/10/2026",
                                       boton = TRUE), {
    session$flushReact()
    expect_null(session$getReturned()$accion())
    expect_equal(session$getReturned()$valor(), 2)
    before <- output$boton$html
    session$setInputs(accion = 1)
    expect_equal(session$getReturned()$accion(), 1L)
    value(3)
    title("Nuevos pedidos")
    session$flushReact()
    expect_equal(session$getReturned()$valor(), 3)
    expect_equal(session$getReturned()$accion(), 1L)
    expect_identical(output$boton$html, before)
    expect_match(output$contenido$html, "Nuevos pedidos", fixed = TRUE)
    session$setInputs(accion = 2)
    expect_equal(session$getReturned()$accion(), 2L)
  })
})

test_that("sin botón no se emiten acciones y los datos externos son texto", {
  shiny::testServer(Tarjeta, args = list(titulo = htmltools::HTML("<script>no</script>"), valor = 2), {
    session$flushReact()
    expect_null(output$boton)
    expect_match(output$contenido$html, "&lt;script&gt;", fixed = TRUE)
    session$setInputs(accion = 1)
    expect_null(session$getReturned()$accion())
  })
  expect_error(Tarjeta("id", "Título", 1, boton = NA))
})
