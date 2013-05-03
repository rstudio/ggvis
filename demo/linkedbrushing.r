library(httpuv)
library(Rook)
library(ggplot2)
library(gg2v)
library(RJSONIO)
library(whisker)

# Convert a ggplot2 plot object to a JS vega spec with related gigvis calls
plotToScript <- function(id, plotObj) {
  ps <- plot_spec(plotObj, embed_data=TRUE)
  
  # Do some monkeypatching of the generated vega plotspec. The monkeypatching
  # is very crude, assumes only 1 data item and 1 mark.
  
  # Add a data source that will represent the brush
  ps$data[[2]] <- list(
    name = "brush",
    values = list(
      list(
        x = 0, y = 0, width = 0, height = 0
      )
    )
  )
  
  # Change the mark specification to make it look a little nicer
  ps$marks[[1]]$properties$update$size <- list(value = 20)
  ps$marks[[1]]$properties$update$stroke <- list(value = "transparent")
  ps$marks[[1]]$properties$update$opacity <- list(value = 0.5)
  
  # Add a mark property set for selected (linked) data points
  ps$marks[[1]]$properties$selected <- list(
    fill=list(
      value="#00CC00"
    )
  )
  
  # Add a mark for the brushing rectangle
  ps$marks[[2]] <- list(
    type = "rect",
    from = list(data = "brush"),
    properties = list(
      update = list(
        x = list(field = "data.x"),
        y = list(field = "data.y"),
        width = list(field = "data.width"),
        height = list(field = "data.height"),
        stroke = list(value = "#CCC"),
        fill = list(value = "transparent"),
        opacity = list(value = 1.0)
      )
    )
  )
  
  json <- toJSON(ps)
  whisker.render("<script>
vg.parse.spec({{{json}}}, function(chart) {
  gigvis.addChart('{{id}}', chart({el: '#{{id}}'}).update());
})
</script>")
}

# Custom content for the <head> of the webpage
headFunc <- function() {
  diamonds <- diamonds[sample(nrow(diamonds), 500),]
  paste(
    sep = '\n',
    plotToScript('vis', qplot(carat, price, data=diamonds)),
    plotToScript('vis2', qplot(clarity, price, data=diamonds)),
    plotToScript('vis3', qplot(color, price, data=diamonds)),
    plotToScript('vis4', qplot(cut, price, data=diamonds))
  )
}

# Custom content for the <body> of the webpage
bodyFunc <- function() {
  paste('<div class="plots">',
        '<div id="vis"></div>',
        '<div id="vis2"></div>',
        '<div id="vis3"></div>',
        '<div id="vis4"></div>',
        '</div>')
}



rootDir <- system.file('', package='gigvis')

# Serves up files in the www directory
libServer <- Rook::File$new(file.path(rootDir, 'www'))

template <- paste(readLines(file.path(rootDir, 'index.html')), collapse='\n')

# This is our rook/httpuv app
app <- list(
  call = function(req) {
    
    # Try using static resources in www to fulfill the request
    resp <- suppressWarnings(libServer$call(req))
    if (resp$status == 200)
      return(resp)
    
    # Nothing static found; handle it dynamically if possible
    
    switch (
      req$PATH_INFO,
      '/' = {
        cat("Rendering page...\n")
        print(system.time({
          response <- list(status = 200L,
                           headers = list('Content-Type' = 'text/html'),
                           body = whisker.render(template,
                                                 list(head = headFunc(),
                                                      body = bodyFunc())))
        }))
        response
      },
      list(status = 404L,
           headers = list('Content-Type' = 'text/plain'),
           body = 'Not found')
    )
  },
  onWSOpen = function(ws) {
    ws$onMessage(function(binary, message) {
      if (binary) {
        warning("Binary websocket message was not expected")
        ws$close()
        return()
      }
      
      msg <- fromJSON(message)
      str(msg)
    })
    ws$onClose(function() {
      
    })
  }
)

runServer('0.0.0.0', 8101, app)