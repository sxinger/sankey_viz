library(shiny)
library(networkD3)

shinyServer(function(input, output) {

    output$sankey<-renderSankeyNetwork({
      data("sankey_dat")
      sankeyNetwork(Links = sankey_dat$links,
                    Nodes = sankey_dat$nodes,
                    Source = "source",
                    Target = "target",
                    Value = "value",
                    NodeID = "key",
                    colourScale = 'd3.scaleOrdinal()
                    .domain(["sympt", "both", "treat","0","1","2","3","4"])
                    .range(["#ffd7ff", "#5fd7ff", "#5fd7ff","#98d19e","#d1f2cc","#e57373","#ef5350","#f44346"])',
                    LinkGroup = "group",
                    NodeGroup = "group",
                    fontSize= 15,
                    nodeWidth = 30,
                    width = 2000,
                    height=1500,
                    sinksRight=input$sinksRight)
      })
})
