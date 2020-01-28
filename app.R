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
library(DT)
library(visNetwork)
source("./DEFunctions.R")
setwd("~/Documents/Network_visualisation/")

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
    ),
    
    hr(),
    fluidRow(
        column(
            width = 12,
            div(h3("Gene expression accros all conditions"), align = "center"),
            #verbatimTextOutput("SelectedGene"),
            plotOutput("expression_plot")
            
        )
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    load("./DataNetworkGenieCO2Clusters.RData")
    load("./normalized.count_At.RData")
    
    output$network <- renderVisNetwork({
        visNetwork(nodes = data$nodes, edges = data$edges, height = "10%", width = "100%") %>%
            visEdges(smooth = FALSE) %>% visPhysics(solver = "forceAtlas2Based", timestep = 1, minVelocity=10, maxVelocity = 10, stabilization = F)%>%
            visOptions(selectedBy = "group", 
                       highlightNearest = TRUE, 
                       nodesIdSelection  = TRUE, collapse = TRUE)%>% visEvents(click = "function(nodes){
                  Shiny.onInputChange('click', nodes.nodes[0]);
                  ;}"
                       )
        
        
    })
    
    output$SelectedGene <- renderPrint({
        print(input$click)
    })
    
    #output$SelectedGene <- renderPrint({print(input$mynetwork_selectedBy)})
    

    output$Ontologies <- DT::renderDataTable({
        if(is.null(input$click)){
            data$nodes[c("description", "Ontology")]
        }
        else{
            data$nodes[input$click,c("description", "Ontology")]
        }})
    
    output$expression_plot <- renderPlot({
        getExpression(input$click)
        
        
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)

