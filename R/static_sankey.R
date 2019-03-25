####plot sankey diagram####
rm(list=ls())
gc()

source("./R/util.R")
require_libraries(c("networkD3",
                    "tidyr",
                    "dplyr",
                    "magrittr"))

#load data
sankey_dat<-readRDS("./data/sankey_dat.rda")

#customize link and node colors
my_color<-'d3.scaleOrdinal() 
          .domain(["sympt","both","0","1","2","3","4","5"]) 
          .range(["#ffd7ff", "#5fd7ff","#4eaa5f","#99db7f","#f1ff75","#f9956d","#f77676","#e04e4e"])'


#generate basic plot (not print)
sn<-networkD3::sankeyNetwork(Links = sankey_dat$links,
                             Nodes = sankey_dat$nodes,
                             Source = "source",
                             Target = "target",
                             Value = "value",
                             NodeID = "key",
                             colourScale = my_color,
                             LinkGroup = "group",
                             NodeGroup = "group",
                             fontSize= 15, nodeWidth = 30,
                             width = 2000,height=1500)

#add more info to links
links_name<-sankey_dat$links %>%
  inner_join(sankey_dat$nodes %>% dplyr::select(key) %>% mutate(code=1:n()-1),
             by=c("source"="code")) %>%
  dplyr::rename(source_name = key) %>%
  inner_join(sankey_dat$nodes %>% dplyr::select(key) %>% mutate(code=1:n()-1),
             by=c("target"="code")) %>%
  dplyr::rename(target_name = key)

#customize tooltip label
sn$x$links$label<-paste(paste(links_name$source_name,links_name$target_name,sep="->"),
                        paste(sankey_dat$links$value,sankey_dat$links$label),sep="\n")
htmlwidgets::onRender(
  sn,
  '
  //user-defined label
  function(el,x){
  d3.selectAll(".link")
  .select("title")
  .text(function(d) { return d.label; });
  
  //move position of node text
  d3.select(el)
  .selectAll(".node text")
  .attr("text-anchor","begin")
  .attr("x",5)
  }
  '
)




