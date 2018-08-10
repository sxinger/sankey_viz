####plot sankey diagram####
rm(list=ls())
gc()

#install packages
package_list<-c("dplyr",
                "tidyr",
                "networkD3")
new_packages<-package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(new_packages)>0){
  install.packages(new_packages) 
}
library(dplyr)
library(tidyr)

#load data
data("sankey_dat")

#customize link and node colors
my_color<-'d3.scaleOrdinal() 
           .domain(["sympt", "both", "treat","0","1","2","3","4"]) 
           .range(["#ffd7ff", "#5fd7ff", "pink","#98d19e","#d1f2cc","#e57373","#ef5350","#f44346"])'

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
                        paste0(sankey_dat$links$value,sankey_dat$links$label),sep="\n")
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




