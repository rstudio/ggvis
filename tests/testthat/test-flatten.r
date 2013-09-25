context("Flatten")

test_that("props inherited from parent", {
  p <- ggvis(data.frame(),
    props(x := 1),
    branch(
      props(y := 2),
      mark_line(props(x := 3))
    )
  )
  nodes <- flatten(p)

  expect_equal(length(nodes), 1)
  props <- nodes[[1]]$props

  expect_equal(sort(names(props)), c("x.update", "y.update"))
  expect_equal(props$x.update$value, 3)
  expect_equal(props$y.update$value, 2)
})

test_that("data flows through pipeline", {
  df <- data.frame(x = 1, y = 2)
  p <- ggvis(df, props(x = ~x, y = ~y),
    branch(branch(branch(branch(branch(mark_line()))))))
  nodes <- flatten(p)

  expect_equal(length(nodes), 1)
  pipeline <- nodes[[1]]$pipeline

  expect_equal(isolate(pipeline()), df)
})

test_that("no data is an error", {
  p <- ggvis(NULL, props(x = ~x, y = ~y),
    branch(branch(branch(branch(branch(mark_line()))))))
  expect_error(flatten(p), "parent has no data")
})

test_that("reactive source data only run once", {
  library("shiny")
  runs <- 0
  df <- data.frame(x = 1, y = 2)
  rdf <- reactive({
    runs <<- runs + 1
    df
  })

  p <- ggvis(rdf, props(x = ~x, y = ~y),
    mark_line(),
    mark_symbol())
  nodes <- flatten(p)

  expect_equal(length(nodes), 2)

  expect_equal(isolate(nodes[[1]]$pipeline()), df)
  expect_equal(isolate(nodes[[2]]$pipeline()), df)
  expect_equal(runs, 1)
})
