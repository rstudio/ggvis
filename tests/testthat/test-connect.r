context("connect")

library(shiny)
df <- data.frame(x = 1:10, y = 1:10)
asis <- props(x ~ x, y ~ y)

test_that("sluicing data sources returns data frame", {
  sources <- list(
    reactive = datasource(reactive(df)),
    eager = datasource(df)
  )

  for(nm in names(sources)) {
    source <- sources[[nm]]
    expect_equal(sluice(source, props()), df, label = paste0("sluice ", nm))
  }

})

test_that("reactive source responds to changes", {
  v <- reactiveValues(n = 1)
  src <- datasource(reactive(df[1:v$n, ]))

  r <- connect(src, props())
  expect_equal(isolate(nrow(r())), 1L)

  v$n <- 2
  expect_equal(isolate(nrow(r())), 2L)
})

test_that("reactive transform responds to changes in parameters", {
  v <- reactiveValues(add = 1)
  pipe <- pipeline(df, transform_scale(reactive(v$add)))
  r <- connect(pipe, asis)
  expect_equal(isolate(r()$x), 2:11)

  v$add <- 0
  expect_equal(isolate(r()$x), 1:10)
})

test_that("reactive transform responds to changes in reactive source", {
  v <- reactiveValues(n = 1, add = 1)
  d_updated <- 0
  t_updated <- 0

  ds <- reactive({
    d_updated <<- d_updated + 1
    df[1:v$n, ]
  })
  ts <- transform_scale(reactive({
    t_updated <<- t_updated + 1
    v$add
  }))

  pipe <- pipeline(ds, ts)
  r <- connect(pipe, asis)
  expect_equal(isolate(r()$x), 2)
  expect_equal(d_updated, 1)
  expect_equal(t_updated, 1)

  v$n <- 5
  expect_equal(isolate(r()$x), 2:6)
  expect_equal(d_updated, 2)
  expect_equal(t_updated, 1)

  v$add <- 0
  expect_equal(isolate(r()$x), 1:5)
  expect_equal(d_updated, 2)
  expect_equal(t_updated, 2)

})
