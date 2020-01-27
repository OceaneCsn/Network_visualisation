#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
source("D:/These/CombinatoireRNASeqFeNCO2/DEFunctions.R")
setwd("D:/These/Network_visualisation/")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),

    # Application title
    titlePanel("Network visualisation"),
    hr(),
    fluidRow(
        column(
            width = 12,
            visNetworkOutput("network"),
            
        )
    ),
    
    hr(),
    fluidRow(
        column(
            width = 12,
            div(h3("You selected"), align = "center"),
            #verbatimTextOutput("SelectedGene"),
            DT::dataTableOutput("Ontologies")
            
        )
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    load("./DataNetworkGenieCO2Clusters.RData")
    
    output$network <- renderVisNetwork({
        visNetwork(nodes = data$nodes, edges = data$edges, height = "10%", width = "100%") %>%
            visEdges(smooth = FALSE) %>% visPhysics(solver = "forceAtlas2Based", timestep = 1, minVelocity=10, maxVelocity = 10, stabilization = F)%>%
            visOptions(selectedBy = "group", 
                       highlightNearest = TRUE, 
                       nodesIdSelection  = TRUE, collapse = TRUE)
        
        
    })
    
    output$SelectedGene <- renderPrint({print(input$mynetwork_selected)})
    

    output$Ontologies <- DT::renderDataTable(data$nodes[c("description", "Ontology")])
    

}

# Run the application 
shinyApp(ui = ui, server = server)

