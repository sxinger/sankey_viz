####Process Model####
rm(list=ls())
gc()

setwd("E:/Sepsis Treatment FS PM")

# trace(utils:::unpackPkgZip, edit=TRUE)
# #Sys.sleep(2)

# install.packages("plotly")
install.packages("networkD3")
library(dplyr)
library(tidyr)
library(magrittr)
# library(plotly)
library(networkD3)

#load data
clinical_pathway<-read.csv("./data/clinical_pathways.csv",
                           stringsAsFactors = F)
load("./data/sepsis_target_trt3hr.Rdata")

#summarize clinical pathway
alt_pathway<-clinical_pathway %>%
  dplyr::select(ENCOUNTER_NUM,
                SI_SINCE_TRIAGE,
                SIRS1_SINCE_TRIAGE,
                SIRS2_SINCE_TRIAGE,
                SIRS3_SINCE_TRIAGE,
                SIRS4_SINCE_TRIAGE,
                OD1_SINCE_TRIAGE,
                OD2_SINCE_TRIAGE,
                OD3_SINCE_TRIAGE,
                OD4_SINCE_TRIAGE,
                OD5_SINCE_TRIAGE,
                OD6_SINCE_TRIAGE,
                OD7_SINCE_TRIAGE) %>%
  gather(key,HOUR_SINCE_TRIAGE,-ENCOUNTER_NUM) %>%
  arrange(ENCOUNTER_NUM) %>%
  dplyr::mutate(key = gsub("_.*","",key)) %>%
  left_join(clinical_pathway %>%
              dplyr::select(ENCOUNTER_NUM,
                            SIRS1_TYPE,
                            SIRS2_TYPE,
                            SIRS3_TYPE,
                            SIRS4_TYPE,
                            OD1_TYPE,
                            OD2_TYPE,
                            OD3_TYPE,
                            OD4_TYPE,
                            OD5_TYPE,
                            OD6_TYPE,
                            OD7_TYPE) %>%
      gather(key,ALERT_TYPE,-ENCOUNTER_NUM) %>%
      dplyr::mutate(key = gsub("_.*","",key)) %>%
      arrange(ENCOUNTER_NUM),
            by=c("ENCOUNTER_NUM","key")) %>%
  dplyr::mutate(ALERT_TYPE= ifelse(key=="SI",key,ALERT_TYPE)) %>%
  dplyr::filter(!is.na(HOUR_SINCE_TRIAGE) & !is.na(ALERT_TYPE))  %>%
  arrange(ENCOUNTER_NUM, HOUR_SINCE_TRIAGE) %>%
  dplyr::filter(key %in% c("SI","SIRS2","OD1")) %>%
  mutate(key = ifelse(key=="OD1",paste0(key,"_",ALERT_TYPE),key)) %>%
  mutate(alt_ind = 1)

outcome<-clinical_pathway %>% 
  dplyr::mutate(SEPSIS_SINCE_TRIAGE=pmax(SI_SINCE_TRIAGE,SIRS2_SINCE_TRIAGE,OD1_SINCE_TRIAGE)) %>%
  dplyr::select(ENCOUNTER_NUM,SEPSIS_SINCE_TRIAGE) %>%
  mutate(HOUR_SINCE_TRIAGE=ifelse(is.na(SEPSIS_SINCE_TRIAGE),48,SEPSIS_SINCE_TRIAGE),
         key=ifelse(is.na(SEPSIS_SINCE_TRIAGE),"x_NoSepsis_x","x_Sepsis_x"),
         ALERT_TYPE = "Outcome",
         alt_ind = ifelse(is.na(SEPSIS_SINCE_TRIAGE),0,1)) %>%
  dplyr::select(ENCOUNTER_NUM,key,HOUR_SINCE_TRIAGE,ALERT_TYPE,alt_ind)


trt_pathway<-trt3hr %>%
  dplyr::select(ENCOUNTER_NUM,
                BLOOD_C,
                ABX,
                LAC1,
                BOLUS_BEGIN,
                BOLUS_END) %>%
  gather(key,HOUR_SINCE_TRIAGE,-ENCOUNTER_NUM) %>%
  arrange(ENCOUNTER_NUM, HOUR_SINCE_TRIAGE) %>%
  mutate(alt_ind = 0)

#blood/abx culture time adjustment
trt_pathway %<>%
  left_join(alt_pathway %>% dplyr::select(ENCOUNTER_NUM,HOUR_SINCE_TRIAGE,ALERT_TYPE) %>% 
              unique %>% filter(grepl("SI+$",ALERT_TYPE)),
            by=c("ENCOUNTER_NUM","HOUR_SINCE_TRIAGE")) %>%
  dplyr::mutate(HOUR_SINCE_TRIAGE=ifelse(grepl("(BLOOD_C)|(ABX)",key),
                                         HOUR_SINCE_TRIAGE+0.01,HOUR_SINCE_TRIAGE)) %>%
  dplyr::select(-ALERT_TYPE)


#lactate time
trt_pathway %<>%
  left_join(alt_pathway %>% dplyr::select(ENCOUNTER_NUM,HOUR_SINCE_TRIAGE,ALERT_TYPE) %>% 
              unique %>% filter(grepl("OD:IL",ALERT_TYPE)),
            by=c("ENCOUNTER_NUM","HOUR_SINCE_TRIAGE")) %>%
  dplyr::mutate(HOUR_SINCE_TRIAGE=ifelse(grepl("(LAC1)",key),
                                         HOUR_SINCE_TRIAGE+0.01,HOUR_SINCE_TRIAGE)) %>%
  dplyr::select(-ALERT_TYPE)


#bolus time
trt_pathway %<>%
  left_join(alt_pathway %>% dplyr::select(ENCOUNTER_NUM,HOUR_SINCE_TRIAGE,ALERT_TYPE) %>% 
              unique %>% filter(grepl("OD:Hypotension",ALERT_TYPE)),
            by=c("ENCOUNTER_NUM","HOUR_SINCE_TRIAGE")) %>%
  dplyr::mutate(HOUR_SINCE_TRIAGE=ifelse(grepl("(BOLUS)",key),
                                         HOUR_SINCE_TRIAGE+0.01,HOUR_SINCE_TRIAGE)) %>%
  dplyr::select(-ALERT_TYPE)


#put alert and treatment together
trt_alt<-alt_pathway %>% 
  dplyr::select(ENCOUNTER_NUM,key,HOUR_SINCE_TRIAGE,alt_ind) %>%
  bind_rows(outcome %>% dplyr::select(-ALERT_TYPE)) %>%
  bind_rows(trt_pathway) %>%
  arrange(ENCOUNTER_NUM, HOUR_SINCE_TRIAGE,alt_ind) %>%
  dplyr::filter(!is.na(HOUR_SINCE_TRIAGE)) %>%
  left_join(outcome %>% dplyr::select(ENCOUNTER_NUM,HOUR_SINCE_TRIAGE) %>%
              dplyr::rename(time_bd = HOUR_SINCE_TRIAGE),
            by="ENCOUNTER_NUM") %>%
  dplyr::filter(HOUR_SINCE_TRIAGE <= time_bd) %>%
  dplyr::select(-time_bd)

trt_alt %<>%
  group_by(ENCOUNTER_NUM,HOUR_SINCE_TRIAGE) %>% arrange(alt_ind,key) %>%
  dplyr::mutate(key=paste0(key,collapse = "+"),
                alt_ind=paste0(alt_ind,collapse = "")) %>%
  ungroup %>% unique %>%
  group_by(ENCOUNTER_NUM) %>% 
  dplyr::mutate(key_ord = rank(HOUR_SINCE_TRIAGE)) %>%
  ungroup %>% unique
  
#sanity check
# trt_alt %>% filter(grepl("x_Sepsis_x",key) & key_ord==1) %>% View
# clinical_pathway %>% filter(ENCOUNTER_NUM == 7606025) %>% View #inspect questionable cases

trt_alt %<>%
  dplyr::mutate(key = paste0(key,"@",key_ord)) %>%
  arrange(ENCOUNTER_NUM,HOUR_SINCE_TRIAGE) %>%
  group_by(ENCOUNTER_NUM) %>%
  dplyr::mutate(key_last=lag(key),
                hour_last=lag(HOUR_SINCE_TRIAGE),
                alt_ind_last = lag(alt_ind)) %>%
  dplyr::mutate(key_last = ifelse(is.na(key_last),"Triage@0",key_last),
                hour_last = ifelse(is.na(hour_last),0,hour_last),
                alt_ind_last = ifelse(is.na(alt_ind_last),alt_ind,alt_ind_last)) %>%
  # dplyr::mutate(time_lapse = HOUR_SINCE_TRIAGE - hour_last) %>%
  dplyr::mutate(time_lapse = HOUR_SINCE_TRIAGE) %>%
  ungroup %>%
  dplyr::rename(key=key_last,key_next=key,
                alt_ind=alt_ind_last,alt_ind_next=alt_ind) %>%
  dplyr::select(ENCOUNTER_NUM,
                key,key_next,time_lapse,
                alt_ind,alt_ind_next) %>%
  unique


#distinct nodes and links
length(unique(trt_alt$key)) #267
nrow(trt_alt %>% dplyr::select(key, key_next) %>% unique) #1615

#initial event
nrow(trt_alt %>% dplyr::select(key) %>% unique %>%
       filter(grepl("@1",key))) #88

trt_alt %>% dplyr::select(key,ENCOUNTER_NUM) %>% unique %>%
  filter(grepl("@1",key)) %>% group_by(key) %>%
  dplyr::summarize(enc_cnt = length(unique(ENCOUNTER_NUM))) %>% 
  ungroup %>% arrange(desc(enc_cnt)) %>% View

#encounters of clear starting alert
enc_clear_st<-trt_alt %>%
  filter(key %in% c( "SI@1"
                    ,"SIRS2@1"
                    # ,"OD1_OD:Hypotension@1"
                    # ,"OD1_OD:AMS@1"
                    # ,"OD1_OD:AC@1"
                    # ,"OD1_OD:KF@1"
                    # ,"OD1_OD:Hypoxemia@1"
                    # ,"OD1_OD:IL@1"
                    # ,"OD1_OD:LF@1"
                    )) %>%
  dplyr::select(ENCOUNTER_NUM) %>%
  unique

trt_alt %<>% semi_join(enc_clear_st, by="ENCOUNTER_NUM")

#distinct nodes and links
length(unique(trt_alt$key)) #135
nrow(trt_alt %>% dplyr::select(key, key_next) %>% unique) #897

#initial event
nrow(trt_alt %>% dplyr::select(key) %>% unique %>%
       filter(grepl("@1",key))) #9

#distinct pathways
path_cnt<-trt_alt %>% group_by(ENCOUNTER_NUM) %>%
  dplyr::summarize(pathways=paste0(key_next,collapse = "+")) %>%
  ungroup %>% unique
length(unique(path_cnt$pathways))

#encode the nodes and collect node and link summaries
enc_nodes<-trt_alt %>% dplyr::select(ENCOUNTER_NUM,key,key_next,alt_ind) %>%
  semi_join(outcome %>% filter(alt_ind==1),by = "ENCOUNTER_NUM") %>%
  group_by(ENCOUNTER_NUM) %>%
  slice(1) %>% ungroup %>%
  group_by(key,alt_ind) %>%
  dplyr::summarize(enc_cnt = length(unique(ENCOUNTER_NUM))) %>%
  ungroup %>% arrange(desc(enc_cnt)) %>%
  bind_rows(trt_alt %>% dplyr::select(ENCOUNTER_NUM,key_next,alt_ind_next) %>%
              group_by(ENCOUNTER_NUM) %>%
              dplyr::mutate(event_ord = 1:n()) %>% 
              ungroup %>% arrange(event_ord) %>%
              group_by(event_ord,key_next,alt_ind_next) %>%
              dplyr::summarize(enc_cnt = length(unique(ENCOUNTER_NUM))) %>%
              arrange(desc(enc_cnt)) %>% ungroup %>%
              dplyr::select(key_next,alt_ind_next,enc_cnt) %>% 
              dplyr::rename(key=key_next,
                            alt_ind=alt_ind_next) %>%
              unique) %>%
  arrange(desc(enc_cnt)) %>% unique %>%
  mutate(key_cd=1:n()-1) %>%
  dplyr::mutate(node_grp=case_when(grepl("0",alt_ind)&grepl("1",alt_ind) ~ "treat",
                                   grepl("0",alt_ind)&(!grepl("1",alt_ind)) ~ "both",
                                   (!grepl("0",alt_ind))&grepl("1",alt_ind) ~ "sympt")) %>%
  dplyr::mutate(group=as.factor(node_grp))

enc_links<-trt_alt %>% dplyr::select(ENCOUNTER_NUM,key,key_next) %>%
  group_by(key,key_next) %>%
  dplyr::summarize(enc_cnt = length(unique(ENCOUNTER_NUM))) %>%
  ungroup

trt_alt %<>%
  left_join(enc_nodes %>% dplyr::select(-alt_ind,-enc_cnt),
            by="key") %>%
  dplyr::rename(source=key_cd) %>%
  left_join(enc_nodes %>% dplyr::select(-alt_ind,-enc_cnt),
            by=c("key_next"="key")) %>%
  dplyr::rename(target=key_cd) %>%
  left_join(enc_links,by=c("key","key_next")) %>%
  dplyr::rename(value=enc_cnt) %>%
  inner_join(outcome %>% dplyr::select(ENCOUNTER_NUM,alt_ind) %>%
               dplyr::rename(sepsis_ind=alt_ind),
             by = "ENCOUNTER_NUM")
  
trt_alt_summ<-trt_alt %>% 
  group_by(source,target,value) %>%
  arrange(time_lapse) %>%
  dplyr::summarize(time_lapse_q1=round(quantile(time_lapse,probs=0.25,na.rm=T),2),
                   time_lapse_med=round(median(time_lapse,na.rm=T),2),
                   time_lapse_q3=round(quantile(time_lapse,probs=0.75,na.rm=T),2),
                   sepsis_prop=round(mean(sepsis_ind),2)) %>%
  ungroup %>% unique %>%
  dplyr::mutate(link_grp=case_when(round(time_lapse_med)==0 ~ 0,
                                   round(time_lapse_med)>0&round(time_lapse_med)<=2 ~ 1,
                                   round(time_lapse_med)>2&round(time_lapse_med)<=5 ~ 2,
                                   round(time_lapse_med)>5&round(time_lapse_med)<=24 ~ 3,
                                   round(time_lapse_med)>24 ~ 4),
                link_grp2=case_when(sepsis_prop>0&sepsis_prop<=0.25 ~ 0,
                                    sepsis_prop>0.25&sepsis_prop<=0.5 ~ 1,
                                    sepsis_prop>0.5&sepsis_prop<=0.75 ~ 2,
                                    sepsis_prop>0.5&sepsis_prop<=0.90 ~ 3,
                                    sepsis_prop>0.90 ~ 4)) %>%
  dplyr::mutate(group=as.factor(link_grp2)) %>%
  dplyr::mutate(label=paste0("[",time_lapse_q1,",",time_lapse_med,",",time_lapse_q3,"]"))
  # dplyr::mutate(label=paste0(time_lapse_med,"hr"))


#shrink the size
incld<-40
# incld<-length(unique(trt_alt$key))+1
nodes_restr<-enc_nodes %>% dplyr::filter(key_cd <= incld)
links_restr<-trt_alt_summ %>% dplyr::filter(source <= incld & target <= incld)

#collect info for sankey nodes --networkD3 implementation
nodes<-as.data.frame(nodes_restr %>%
                       dplyr::select(key,group))
links<-as.data.frame(links_restr %>%
                       dplyr::select(source,target,value,group))

#give a color for each group
unique_color<-unique(c(enc_nodes$node_grp,trt_alt_summ$link_grp))
my_color<-'d3.scaleOrdinal() .domain(["sympt", "both", "treat","0","1","2","3","4"]) .range(["#ffd7ff", "#5fd7ff", "pink","#98d19e","#d1f2cc","#e57373","#ef5350","#f44346"])'

sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "key",
              colourScale = my_color,
              LinkGroup = "group",
              NodeGroup = "group",
              fontSize= 28, fontFamily = "sans-serif",
              nodeWidth = 30,
              width = 2000,height=800)



#sample codes for sankey plot
trace(utils:::unpackPkgZip, edit=TRUE)
#Sys.sleep(2)

install.packages("rjson")
library(rjson)
json_file <- "https://raw.githubusercontent.com/plotly/plotly.js/master/test/image/mocks/sankey_energy.json"
json_data <-fromJSON(paste(readLines(json_file), collapse=""))

p <- plot_ly(
  type = "sankey",
  domain = c(
    x =  c(0,1),
    y =  c(0,1)
  ),
  orientation = "h",
  valueformat = ".0f",
  valuesuffix = "TWh",
  
  node = list(
    label = json_data$data[[1]]$node$label,
    color = json_data$data[[1]]$node$color,
    pad = 15,
    thickness = 15,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = json_data$data[[1]]$link$source,
    target = json_data$data[[1]]$link$target,
    value =  json_data$data[[1]]$link$value,
    label =  json_data$data[[1]]$link$label
  )
) %>% 
  layout(
    title = "Energy forecast for 2050<br>Source: Department of Energy & Climate Change, Tom Counsell via <a href='https://bost.ocks.org/mike/sankey/'>Mike Bostock</a>",
    font = list(
      size = 10
    ),
    xaxis = list(showgrid = F, zeroline = F),
    yaxis = list(showgrid = F, zeroline = F)
  )
p
#not work!


#collect info for sankey nodes --plotly implementation
p <- plot_ly(
  type = "sankey",
  domain = c(
    x =  c(0,1),
    y =  c(0,1)
  ),
  orientation = "h",
  valueformat = ".0f",
  valuesuffix = "TWh",
  
  node = list(
    label = enc_nodes$key_cd,
    color =  "blue",
    pad = 15,
    thickness = 15,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = trt_alt_summ$source,
    target = trt_alt_summ$target,
    value =  trt_alt_summ$target_enc,
    label =  trt_alt_summ$label
  )
) %>% 
  layout(
    title = "Sepsis Alert and 3-hour Bundle Trajectory",
    font = list(
      size = 10
    ),
    xaxis = list(showgrid = F,zeroline = F,showline=F,showticklabels=F),
    yaxis = list(showgrid = F,zeroline = F,showline=F,showticklabels=F)
  )

p

# publish online
# https://plot.ly/r/getting-started

Sys.setenv("plotly_username"="sxinger")
Sys.setenv("plotly_api_key"="RjoEMXjqPXE3sY1llsR9")

chart_link = api_create(p,filename = "sankey_sepsis_process3",fileopt = "overwrite")
chart_link


