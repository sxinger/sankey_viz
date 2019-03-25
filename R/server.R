library(shiny)
library(networkD3)
library(leaflet)

shinyServer(function(input, output) {

    output$sankey<-renderSankeyNetwork({
      sankey_dat<-readRDS("../data/sankey_dat.rda")
      sankeyNetwork(Links = sankey_dat$links,
                    Nodes = sankey_dat$nodes,
                    Source = "source",
                    Target = "target",
                    Value = "value",
                    NodeID = "key",
                    colourScale = 'd3.scaleOrdinal() 
                                  .domain(["sympt","both","0","1","2","3","4","5"]) 
                                  .range(["#ffd7ff", "#5fd7ff","#4eaa5f","#99db7f","#f1ff75","#f9956d","#f77676","#e04e4e"])',
                    LinkGroup = "group",
                    NodeGroup = "group",
                    fontSize= 15,
                    nodeWidth = 30,
                    width = 2000,
                    height=1500,
                    sinksRight=input$sinksRight)
      
      }) 
})
