library(httpuv)
library(Rook)
library(ggplot2)
library(gg2v)
library(RJSONIO)

plotToScript <- function(id, plotObj) {
  json <- toJSON(plot_spec(plotObj, embed_data=TRUE))
  paste(
    sep = '\n',
    '<script>',
    sprintf('var spec = %s;', spec),
    'vg.parse.spec(spec, function(chart) {',
    sprintf('  chart({el:"#%s"}).update();', id),
    '});',
    '</script>'
  )
}

headFunc <- function() {
  plotToScript('vis', qplot(speed, dist, data=cars))
}

bodyFunc <- function() {
  '<div id="vis"></div>'
}



rootDir <- getwd()

# Serves up files in the www directory
libServer <- File$new(file.path(rootDir, 'inst/www'))

template <- paste(readLines(file.path(rootDir, 'inst/index.html')), collapse='\n')

app <- list(
  call = function(req) {
    
    resp <- suppressWarnings(libServer$call(req))
    if (resp$status == 200)
      return(resp)
    
    switch (
      req$PATH_INFO,
      '/' = {
        page <- template
        page <- gsub('$HEAD$', headFunc(), page, fixed = TRUE)
        page <- gsub('$BODY$', bodyFunc(), page, fixed = TRUE)
        list(status = 200L,
             headers = list('Content-Type' = 'text/html'),
             body = page)
      },
      list(status = 404L,
           headers = list('Content-Type' = 'text/plain'),
           body = 'Not found')
    )
  },
  onWSOpen = function(ws) {
    
  }
)

runServer('0.0.0.0', 8101, app)