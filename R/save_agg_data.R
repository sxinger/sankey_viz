####read local data file and save to /data####

dat_path<-"E:/Sepsis Treatment FS PM/sankey_dat.Rdata"
dat<-load(dat_path)

save(dat,file="./data/sankey_dat.rda")

#test
rm(list=ls());gc()
dat<-data("sankey_dat")
head(dat)

