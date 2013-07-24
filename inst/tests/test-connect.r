context("connect")

library(shiny)

df <- data.frame(x = 1:10, y = 1:10)
asis <- props(x ~ x, y ~ y)

test_that("sluicing data sources returns data frame", {
  sources <- list(
    reactive = source_reactive(reactive(df)),
    lazy = source_lazy("df"),
    eager = source_eager(df),
    fun = source_function(function() df)
  )
  
  for(nm in names(sources)) {
    source <- sources[[nm]]
    expect_equal(sluice(source, props()), df, label = paste0("sluice ", nm))
  }
  
})