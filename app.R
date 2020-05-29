library(shiny)
library(arules)
library(arulesViz)




# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Employee Attrition - ARM"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         radioButtons("rhs", "Attrition Status:", choices = c("Attrition=Yes","Attrition=No"),
                      selected = "Attrition=Yes"),
         numericInput("support", "Support:", 0.03, min = 0, max = 1, step = 0.01),
         numericInput("confidence", "Confidence:", 0.6, min = 0, max = 1, step = 0.01),
         numericInput("minlen", "Minimum Length of Rules:",
                      4, min = 2, max = 31, step = 1),
         numericInput("maxlen", "Minimum Length of Rules:",
                      8, min = 2, max = 31, step = 1),
         numericInput("nrules", "Number of Rules:",
                      5, min = 1, max = 10, step = 1),
         selectInput("parameter", "Sorting Parameter",
                     choices = c("lift", "support", "confidence"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Rulesets", tableOutput("results")),
          tabPanel("Paracoord Plot", plotOutput("rulesPlot1")),
          tabPanel("Grouped Plot", plotOutput("rulesPlot2"))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$results <- renderTable({
    rules_sorted <- sort(apriori(empmat, 
                                 parameter=list(support=input$support,confidence=input$confidence,
                                                minlen = input$minlen, maxlen = input$maxlen),
                                 appearance = list(default="lhs", rhs=input$rhs)), by=input$parameter)
    validate(need(length(rules_sorted) != 0, "Unfortunately, No rules for this support and confidence values"))
    DATAFRAME(head(rules_sorted, input$nrules))
  })
  
   output$rulesPlot1 <- renderPlot({
     rules_sorted <- sort(apriori(empmat, 
                                  parameter=list(support=input$support,confidence=input$confidence,
                                                 minlen = input$minlen, maxlen = input$maxlen),
                                  appearance = list(default="lhs", rhs=input$rhs)), by=input$parameter)
     plot(head(rules_sorted, input$nrules), method = "paracoord", control=list(col = rainbow(4)))
   })
   
   output$rulesPlot2 <- renderPlot({
     rules_sorted <- sort(apriori(empmat, 
                                  parameter=list(support=input$support,confidence=input$confidence,
                                                 minlen = input$minlen, maxlen = input$maxlen),
                                  appearance = list(default="lhs", rhs=input$rhs)), by=input$parameter)
     plot(head(rules_sorted, input$nrules), method = "graph", control=list(edgeCol=2, type="items"))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)