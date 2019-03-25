####Process Model####
rm(list=ls())
gc()

# trace(utils:::unpackPkgZip, edit=TRUE)
# #Sys.sleep(2)

source("./R/util.R")
require_libraries(c("networkD3",
                    "tidyr",
                    "dplyr",
                    "magrittr",
                    "leaflet"))

#load data
clinical_pathway<-readRDS("./data/alerts.rda")
#--SIRS_X_Type
#--SIRS_X_Date
#--SIRS_X_Since_Triage
#--OD_X_Type
#--OD_X_Date
#--OD_X_Since_Triage
#--Sepsis_Onset
#--Sepsis_Since_Triage

trt3hr<-readRDS("./data/trt3hr.rda")
#treatnebt table required elements:
#--BLOOD_C (Hour_Since_Triage)
#--ABX (Hour_Since_Triage)
#--LAC1 (Hour_Since_Triage)
#--BOLUS_TRIGGER (Hour_Since_Triage)
#--TRIGGER TYPE 
#--BOLUS_BEGIN (Hour_Since_Triage)
#--BOLUS_END (Hour_Since_Triage)


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
length(unique(trt_alt$key)) #233
nrow(trt_alt %>% dplyr::select(key, key_next) %>% unique) #1382

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
                    ,"OD1_OD:Hypotension@1"
                    ,"OD1_OD:AMS@1"
                    ,"OD1_OD:AC@1"
                    ,"OD1_OD:KF@1"
                    ,"OD1_OD:Hypoxemia@1"
                    ,"OD1_OD:IL@1"
                    ,"OD1_OD:LF@1"
                    )) %>%
  dplyr::select(ENCOUNTER_NUM) %>%
  unique

trt_alt %<>% semi_join(enc_clear_st, by="ENCOUNTER_NUM")

#distinct nodes and links
length(unique(trt_alt$key)) #143
nrow(trt_alt %>% dplyr::select(key, key_next) %>% unique) #983

#initial event
nrow(trt_alt %>% dplyr::select(key) %>% unique %>%
       filter(grepl("@1",key))) #9

#distinct pathways
path_cnt<-trt_alt %>% group_by(ENCOUNTER_NUM) %>%
  dplyr::summarize(pathways=paste0(key_next,collapse = "+")) %>%
  ungroup %>% unique
length(unique(path_cnt$pathways)) #2270

pathway_dist<-path_cnt %>% group_by(pathways) %>%
  dplyr::summarise(cnt=length(unique(ENCOUNTER_NUM))) %>%
  ungroup

path_cnt %<>%
  semi_join(pathway_dist %>% dplyr::filter(cnt>=50),by="pathways")

trt_alt %<>%
  semi_join(path_cnt,by="ENCOUNTER_NUM")

#encode the nodes and collect node and link summaries
enc_nodes<-trt_alt %>% 
  dplyr::select(ENCOUNTER_NUM,key,key_next,alt_ind) %>%
  left_join(outcome %>% 
              dplyr::select(ENCOUNTER_NUM,alt_ind) %>%
              dplyr::rename(sepsis_ind=alt_ind),
            by = "ENCOUNTER_NUM") %>%
  group_by(ENCOUNTER_NUM) %>%
  dplyr::slice(1:1) %>% ungroup %>%
  group_by(key,alt_ind) %>%
  dplyr::summarize(enc_cnt = length(unique(ENCOUNTER_NUM)),
                   sepsis_cnt=length(unique(ENCOUNTER_NUM*sepsis_ind))-1) %>%
  ungroup %>% arrange(desc(enc_cnt)) %>%
  dplyr::mutate(sepsis_prop_node=round(sepsis_cnt/enc_cnt,4)) %>%
  bind_rows(trt_alt %>% 
              dplyr::select(ENCOUNTER_NUM,key_next,alt_ind_next) %>%
              left_join(outcome %>%
                          dplyr::select(ENCOUNTER_NUM,alt_ind) %>%
                          dplyr::rename(sepsis_ind=alt_ind),
                        by = "ENCOUNTER_NUM") %>%
              group_by(ENCOUNTER_NUM) %>%
              dplyr::mutate(event_ord = 1:n()) %>% 
              ungroup %>% arrange(event_ord) %>%
              group_by(event_ord,key_next,alt_ind_next) %>%
              dplyr::summarize(enc_cnt = length(unique(ENCOUNTER_NUM)),
                               sepsis_cnt=length(unique(ENCOUNTER_NUM*sepsis_ind))-1) %>%
              arrange(desc(enc_cnt)) %>% ungroup %>%
              dplyr::mutate(sepsis_prop_node=round(sepsis_cnt/enc_cnt,4)) %>%
              dplyr::select(key_next,alt_ind_next,enc_cnt,sepsis_cnt,sepsis_prop_node) %>% 
              dplyr::rename(key=key_next,
                            alt_ind=alt_ind_next) %>%
              unique) %>%
  arrange(desc(enc_cnt)) %>% unique %>%
  mutate(key_cd=1:n()-1) %>%
  dplyr::mutate(node_grp=case_when(grepl("0",alt_ind)&grepl("1",alt_ind) ~ "treat",
                                   grepl("0",alt_ind)&(!grepl("1",alt_ind)) ~ "both",
                                   (!grepl("0",alt_ind))&grepl("1",alt_ind) ~ "sympt")) %>%
  dplyr::mutate(group=as.factor(node_grp))

enc_links<-trt_alt %>% 
  dplyr::select(ENCOUNTER_NUM,key,key_next) %>%
  left_join(outcome %>%
              dplyr::select(ENCOUNTER_NUM,alt_ind) %>%
              dplyr::rename(sepsis_ind=alt_ind),
            by = "ENCOUNTER_NUM") %>%
  group_by(key,key_next) %>%
  dplyr::summarize(enc_cnt = length(unique(ENCOUNTER_NUM)),
                   sepsis_cnt=length(unique(ENCOUNTER_NUM*sepsis_ind))-1) %>%
  ungroup %>%
  dplyr::mutate(sepsis_prop_link=round(sepsis_cnt/enc_cnt,4))

trt_alt %<>%
  left_join(enc_nodes %>% dplyr::select(-alt_ind),
            by="key") %>%
  dplyr::rename(source=key_cd) %>%
  left_join(enc_nodes %>% dplyr::select(-alt_ind),
            by=c("key_next"="key")) %>%
  dplyr::rename(target=key_cd) %>%
  left_join(enc_links,by=c("key","key_next")) %>%
  dplyr::rename(value=enc_cnt)
  
trt_alt_summ<-trt_alt %>% 
  group_by(source,target,value) %>%
  arrange(time_lapse) %>%
  dplyr::summarize(time_lapse_q1=round(quantile(time_lapse,probs=0.25,na.rm=T),2),
                   time_lapse_med=round(median(time_lapse,na.rm=T),2),
                   time_lapse_q3=round(quantile(time_lapse,probs=0.75,na.rm=T),2),
                   sepsis_oddsrt=round(mean((sepsis_prop_link/(1-sepsis_prop_link))/(sepsis_prop_node.x/(1-sepsis_prop_node.x))),2)) %>%
  ungroup %>% unique %>%
  dplyr::mutate(sepsis_oddsrt=ifelse(is.nan(sepsis_oddsrt),0,sepsis_oddsrt)) %>%
  dplyr::mutate(link_grp=case_when(round(time_lapse_med)==0 ~ 0,
                                   round(time_lapse_med)>0&round(time_lapse_med)<=2 ~ 1,
                                   round(time_lapse_med)>2&round(time_lapse_med)<=5 ~ 2,
                                   round(time_lapse_med)>5&round(time_lapse_med)<=24 ~ 3,
                                   round(time_lapse_med)>24 ~ 4),
                link_grp2=case_when(sepsis_oddsrt>=0&sepsis_oddsrt<=0.5 ~ 0,
                                    sepsis_oddsrt>0.5&sepsis_oddsrt<=1 ~ 1,
                                    sepsis_oddsrt>1&sepsis_oddsrt<=2 ~ 2,
                                    sepsis_oddsrt>2&sepsis_oddsrt<=4 ~ 3,
                                    sepsis_oddsrt>4&sepsis_oddsrt<=10 ~ 4,
                                    sepsis_oddsrt>10 ~ 5)) %>%
  dplyr::mutate(group=as.factor(link_grp2)) %>%
  dplyr::mutate(label=paste0("[",time_lapse_q1,",",time_lapse_med,",",time_lapse_q3,"](",sepsis_oddsrt,")"))
  # dplyr::mutate(label=paste0(time_lapse_med,"hr"))


#shrink the size
incld<-100
# incld<-length(unique(trt_alt$key))+1
nodes_restr<-enc_nodes %>% dplyr::filter(key_cd <= incld)
links_restr<-trt_alt_summ %>% dplyr::filter(source <= incld & target <= incld)

#collect info for sankey nodes --networkD3 implementation
nodes<-as.data.frame(nodes_restr %>%
                       dplyr::select(key,group))
links<-as.data.frame(links_restr %>%
                       dplyr::select(source,target,value,group,label))

sankey_dat<-list(links=links,
                 nodes=nodes)
saveRDS(sankey_dat,file="./data/sankey_dat.rda")


#give a color for each group
unique_color<-unique(c(enc_nodes$node_grp,trt_alt_summ$link_grp))
my_color<-'d3.scaleOrdinal() .domain(["sympt","both","0","1","2","3","4","5"]) .range(["#ffd7ff", "#5fd7ff","#4eaa5f","#99db7f","#f1ff75","#f9956d","#f77676,"#e04e4e"])'

sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "key",
              colourScale = my_color,
              LinkGroup = "group",
              NodeGroup = "group",
              fontSize= 28, fontFamily = "sans-serif",
              nodeWidth = 30,
              width = 2000,height=800)


