library(shiny)
library(shinyBS)

counterButton <- function(id, label = "Counter") {
  ns <- NS(id)
  tagList(
    actionButton(ns("button"), label = label),
    bsTooltip(id = ns("button"), title = "Info about the button"),
    verbatimTextOutput(ns("out")),
    actionLink(ns("link"), "Help")
  )
}

counter <- function(input, output, session) {
  
  ns <- session$ns
  
  count <- reactiveVal(0)
  observeEvent(input$button, {
    count(count() + 1)
  })
  
  output$out <- renderText({
    count()
  })
  
  addPopover(session, 
             id = ns("out"), 
             title = "Info", 
             content = "More info about the counter field", 
             trigger = "hover")
  
  addPopover(session=session, 
             id=ns("link"), 
             title="", 
             content="Testing.", 
             placement = "bottom",
             trigger = "click",
             options = NULL)
  
  count
}

ui <- fluidPage(
  counterButton("counter1", "Counter #1")
)

server <- function(input, output, session) {
  callModule(counter, "counter1")
}

shinyApp(ui, server)