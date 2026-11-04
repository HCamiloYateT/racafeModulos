test_that("navbar UI keeps its li contract and legacy defaults without application IDs", {
  tag <- MenuHeaderUI("parent-menu", shiny::icon("users"))
  expect_identical(tag$name, "li")
  expect_match(tag$attribs$class, "rmh-menu", fixed = TRUE)
  expect_match(tag$attribs$style, "280px", fixed = TRUE)
  expect_identical(tag$attribs$`data-rmh-keep`, "legacy")
  expect_false(grepl("onclick", as.character(tag), fixed = TRUE))
  expect_identical(MenuHeaderUI("m", shiny::icon("users"), keep_open_on_select = TRUE)$attribs$`data-rmh-keep`, "true")
  expect_error(MenuHeaderUI("m", shiny::icon("users"), button_size = "invalid"))
})
test_that("dataframe keys and list item payloads are reactive without constructed onclick", {
  shiny::testServer(MenuHeaderServer, args = list(items_r = data.frame(code = "c'1", label = "Café"), key_cols = "code"), {
    session$flushReact()
    html <- output$contenido_ui$html
    expect_match(html, "data-rsh-value", fixed = TRUE)
    expect_false(grepl("onclick", html, fixed = TRUE))
    expect_identical(session$getReturned()$n(), 1L)
    session$setInputs(item_click = list(code = "c'1"))
    expect_identical(session$getReturned()$seleccion(), list(code = "c'1"))
  })
  items <- shiny::reactiveVal(racafeShiny::racafeListGroup(
    racafeShiny::racafeListItem("Uno", value = "one"), racafeShiny::racafeListItem("Dos", value = "two")))
  shiny::testServer(MenuHeaderServer, args = list(items_r = items), {
    session$flushReact()
    expect_identical(session$getReturned()$n(), 2L)
    expect_match(output$contenido_ui$html, session$ns("item_click"), fixed = TRUE)
    items(racafeShiny::racafeListGroup(racafeShiny::racafeListItem("Tres", value = "three")))
    session$flushReact()
    expect_identical(session$getReturned()$n(), 1L)
    session$setInputs(item_click = "three")
    expect_identical(session$getReturned()$seleccion(), "three")
  })
})
test_that("direct access resolves the single record without another dropdown system", {
  shiny::testServer(MenuHeaderServer, args = list(items_r = data.frame(code = 7, label = "Directo"), key_cols = "code"), {
    session$flushReact()
    session$setInputs(open = 1)
    expect_identical(session$getReturned()$seleccion(), list(code = "7"))
  })
})
test_that("the integrated gallery supplies direct li children to bs4Dash navbar", {
  expect_s3_class(DemoMenuHeader(corporate = TRUE), "shiny.appobj")
})
