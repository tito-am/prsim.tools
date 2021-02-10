#' Création des fichiers CSV de pointe à partir des simulations PRSIM
#'
#' Parallélisation de la génération des volumes. Fournit les intrants pour le calcul des quantiles.
#'
#' @param path Répertoire où se retrouvent les fichiers rdata produits par PRSIM (sims_final)
#' 
#' @import reshape2
#' @import readr
#' @import zoo
#' @import dplyr
#' @import roll
#' @import parallel
#' @import MASS
#' @import foreach
#' @import doParallel
#' @exemples
#' @export
prsim_rdata_to_csv<-function(path){
  
  require(reshape2)
  require(readr)
  require(parallel)
  require(MASS)
  require(foreach)
  require(doParallel)
  

  numCores <- detectCores()
  numCores
  
  registerDoParallel(numCores) 
  
  #path<-'/media/tito/TIIGE/PRSIM/0.9997_harm_mb/'
  setwd(path)
  fichiers<-list.files(paste0(path,'sims_final/'))
  numeros_r<-c("r1","r2","r3","r4","r5","r6","r7","r8","r9","r10")
  
  n_fichiers<-length(fichiers)
  i=0
  foreach (n=1:n_fichiers) %do% {
    fichier<-fichiers[n]
    try(rm(stoch_sim))
    i=i+1
    load(paste(path,'sims_final/',fichier,sep=""))
    stoch_sim<-out
    numero_gen<-as.character(i)
    bvs<-names(stoch_sim)
    mainDir<-paste0(path,'bv_csv/')
    for(bv in bvs){
      test<-stoch_sim[bv][[1]]$simulation
      
      #length(test$r1)#24455/365 = 67 annees
      #unique(test$YYYY)
      
      
      
      
      #jours juliens
      tmp <- as.Date(test$timestamp, format = "%y-%m-%d")
      
      
      test['julian_day']<-format(tmp, "%j")
      test['gen_number']<-rep(i,nrow(test))
      
      test$season <- "winter"
      test$season[which(test$MM%in%c(3,4,5))] <- "spring"
      test$season[which(test$MM%in%c(6,7,8))] <- "summer"
      test$season[which(test$MM%in%c(9,10,11))] <- "fall"
      
      dir.create(file.path(mainDir, bv), showWarnings = FALSE)
      setwd(file.path(mainDir, bv))
      #melt matrix
      for(numero_r in numeros_r){
        
        test2<-melt(data=test,id.vars=c("julian_day","YYYY","season","gen_number"),measure.vars=numero_r)#c("r1","r2","r3","r4","r5","r6","r7","r8","r9","r10")
        
        filename<-paste(mainDir,'/',bv,'/',numero_gen,'-',bv,'-',numero_r,'.csv',sep='')
        write_csv(test2,filename)
        
        
        
      }
      
    }
    
  }
  
}
#' Création des fichiers CSV de volume à partir des simulations PRSIM
#'
#' Parallélisation de la génération des volumes. Fournit les intrants pour le calcul des quantiles.
#'
#' @param path A data.frame containing at least one numeric column
#' 
#' @import reshape2
#' @import readr
#' @import zoo
#' @import dplyr
#' @import roll
#' @import parallel
#' @import MASS
#' @import foreach
#' @import doParallel
#' @exemples
#' @export
prsim_rdata_to_csv_volume<-function(path){
  
  
  
  
  
  
  
  numCores <- detectCores()
  numCores
  
  registerDoParallel(numCores) 
  
  rm(list=ls())
  
  #path<-'/media/tito/TIIGE/PRSIM/0.9995/'
  
  fichiers<-list.files(paste0(path,'sims_final//'))
  numeros_r<-c("r1","r2","r3","r4","r5","r6","r7","r8","r9","r10")
  
  n_fichiers<-length(fichiers)
  i=0
  foreach(n=1:n_fichiers)%do%{
    fichier<-fichiers[n]
    try(rm(stoch_sim))
    i=i+1
    load(paste(path,'sims_final/',fichier,sep=""))
    numero_gen<-as.character(i)
    bvs<-names(stoch_sim)
    mainDir<-paste0(path,'/bv_csv_volume/')
    for(bv in bvs){
      test<-stoch_sim[bv][[1]]$simulation
      
      #length(test$r1)#24455/365 = 67 annees
      #unique(test$YYYY)
      
      
      
      
      #jours juliens
      tmp <- as.Date(test$timestamp, format = "%y-%m-%d")
      
      
      test['julian_day']<-format(tmp, "%j")
      test['gen_number']<-rep(i,nrow(test))
      
      test$season <- "winter"
      test$season[which(test$MM%in%c(3,4,5))] <- "spring"
      test$season[which(test$MM%in%c(6,7,8))] <- "summer"
      test$season[which(test$MM%in%c(9,10,11))] <- "fall"
      
      dir.create(file.path(mainDir, bv), showWarnings = FALSE)
      setwd(file.path(mainDir, bv))
      #melt matrix
      for(numero_r in numeros_r){
        
        test2<-melt(data=test,id.vars=c("julian_day","YYYY","season","gen_number"),measure.vars=numero_r)#c("r1","r2","r3","r4","r5","r6","r7","r8","r9","r10")
        
        res=test2%>%filter(season %in% 'spring')%>%group_by(YYYY,gen_number,variable)%>%mutate(vol =value*(1e-6*60*60*24) )%>%
          mutate(roll_sum = roll_sum(vol, 70))  %>%summarize(max=max(roll_sum,na.rm = TRUE)) %>%collect()
        filename<-paste(mainDir,'/',bv,'/',numero_gen,'-',bv,'-',numero_r,'-volume_prt_max_annuel.csv',sep='')
        write_csv(res,filename)
        
        #filename<-paste(mainDir,'/',bv,'/',numero_gen,'-',bv,'-',numero_r,'-parquet',sep='')
        
        #final <- copy_to(sc, final,overwrite = TRUE)
        #Parquet = spark_write_parquet(final, filename, mode = "overwrite")
        
        
      }
      
    }
    
  }}
prsim_rdata_to_hecressim_csv<-function(path){
  
  require(sparklyr)
  require(reshape2)
  require(readr)
  require(stringr)
  options(scipen = 999)
  #rm(list=ls())
  
  
  #path<-'/media/tito/TIIGE/PRSIM/0.9997_harm/'
  fichiers<-list.files(paste0(path,'sims_final/'))
  numeros_r<-c("r1","r2","r3","r4","r5","r6","r7","r8","r9","r10")
  
  #Preallouer la matrice avec les annees
  load(paste0(path,"sims_final/",fichiers[1]))
  pre_test<-stoch_sim['Bark Lake'][[1]]$simulation
  
  i=0
  for(fichier in fichiers){
    try(rm(stoch_sim))
    
    load(paste(path,'sims_final/',fichier,sep=""))
    bvs<-names(stoch_sim)
    mainDir<-paste0(path,'bv_csv_hecressim')
    for(numero_r in numeros_r){
      conc_total<-pre_test$YYYY
      
      for(bv in bvs){
        test<-stoch_sim[bv][[1]]$simulation
        #conc_total<-test$YYYY
        
        conc<-test[numero_r]
        colnames(conc)<-bv
        conc_total<-cbind(conc_total,conc)
      }
      for(year in unique(conc_total$conc_total)){
        hydros_per_sim_per_year<-conc_total[conc_total$conc_total==year,]
        i=i+1
        filename<-paste(path,'bv_csv_hecressim/',str_pad(i, 7, pad = "0"),'.csv',sep='')
        hydros_per_sim_per_year <- hydros_per_sim_per_year[,-1] 
        write.csv(x = hydros_per_sim_per_year[complete.cases(hydros_per_sim_per_year),],filename,row.names = FALSE,quote = FALSE)
      }
    }
  }}

