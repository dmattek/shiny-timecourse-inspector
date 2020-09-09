library(shiny)
library(shinyBS)


ui = fluidPage(
    actionButton("button", label = "Counter"),
    bsTooltip(id = "button", title = "Info about the button"),
    verbatimTextOutput("out"),
    actionLink("link", "Help")
)

server = function(input, output, session) {
  count <- reactiveVal(0)
  
  observeEvent(input$button, {
    count(count() + 1)
  })
  
  output$out <- renderText({
    count()
  })
  
  addPopover(session, 
             id = "out", 
             title = "Info", 
             content = "More info about the counter field", 
             trigger = "hover")
  
  addPopover(session=session, id="link", title="", 
             content="Testing.", placement = "bottom",
             trigger = "click", options = NULL)
}

shinyApp(ui, server)