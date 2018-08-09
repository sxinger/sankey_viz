####read local data file and save to /data####
dat_path<-"E:/Sepsis Treatment FS PM/sankey_dat.Rdata"
load(dat_path)
save(sankey_dat,file="./data/sankey_dat.rda")


#test
rm(list=ls());gc()
data()
data("sankey_dat")
head(sankey_dat)


