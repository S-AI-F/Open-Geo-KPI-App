jscode <- "shinyjs.refresh = function() { history.go(0); }"

ui <- navbarPage(
  useShinyjs(),
  extendShinyjs(text = jscode),
  textInput("text", "Text"),
  actionButton("refresh", "Refresh app")
)

server <- function(input, output, session) {
  observeEvent(input$refresh, {
    js$refresh();
  })
}

shinyApp(ui = ui, server = server)