


#' ecriture_prsim_vers_csv
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' mainDir<-'/media/tito/TIIGE/PRSIM/0.9995/bv_csv_hecressim'

ecriture_prsim_vers_csv_angliers<-function(path){
  require(tidyverse)
  require(readxl)
  require(reshape2)
  bvs_angliers<-c("Dozois","Lac Victoria et lac Granet","Rapide-7","Rapide-2","Riviere Kinojevis","Lac des Quinze")


  mainDir2<-'/media/tito/TIIGE/PRSIM/0.9995/'
  subDir<-'angliers_sum'
  dir.create(file.path(mainDir2, subDir), showWarnings = FALSE)
  setwd(path)
  fichiers<-list.files()

  for(fichier in fichiers){
    df<-read_csv(fichier)
    df<-df[bvs_angliers]
    angliers_row_sum<-rowSums(df)
    plot(angliers_row_sum,type='l')
    filename<-paste0(mainDir2,subDir,'/angliers_sum_',fichier)
    df<-as.data.frame(cbind(rep(fichier,length(angliers_row_sum)),seq(1,length(angliers_row_sum)),angliers_row_sum))
    colnames(df)<-c('sim_number','julian_day','angliers_sum')
    write_csv(as.data.frame(df),filename)
  }

}


#' ecriture_prsim_vers_csv_temiscamingue
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' mainDir<-'/media/tito/TIIGE/PRSIM/0.9995/bv_csv_hecressim'

ecriture_prsim_vers_csv_temiscamingue<-function(path){
  require(tidyverse)
  require(readxl)
  require(reshape2)
  bvs_temiscamingue<-c( "Dozois","Lac Victoria et lac Granet","Rapide-7","Rapide-2","Riviere Kinojevis","Lac des Quinze","Mistinikon","Lady Evelyn","Lower Notch et Indian Chute","Rabbit Lake", "Kipawa",
                        "Lac Temiscamingue a Angliers","Riviere Blanche" )


  mainDir2<-'/media/tito/TIIGE/PRSIM/0.9995/'
  subDir<-'temiscamingue_sum'
  dir.create(file.path(mainDir2, subDir), showWarnings = FALSE)
  setwd(path)
  fichiers<-list.files()

  for(fichier in fichiers){
    df<-read_csv(fichier)
    df<-df[bvs_temiscamingue]
    temiscamingue_row_sum<-rowSums(df)
    plot(temiscamingue_row_sum,type='l')
    filename<-paste0(mainDir2,subDir,'/temiscamingue_sum_',fichier)
    df<-as.data.frame(cbind(rep(fichier,length(temiscamingue_row_sum)),seq(1,length(temiscamingue_row_sum)),temiscamingue_row_sum))
    colnames(df)<-c('sim_number','julian_day','temiscamingue_sum')
    write_csv(as.data.frame(df),filename)
  }

}


#' calcul_prsim_vers_statistiques_sommaires_temiscamingue
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
calcul_prsim_vers_statistiques_sommaires_angliers<-function(mainDir2){

  require(sparklyr)
  require(dplyr)
  require(tidyr)

  config <- spark_config()

  config$`sparklyr.shell.driver-memory` <- "4G"
  config$`sparklyr.shell.executor-memory` <- "4G"
  config$`spark.yarn.executor.memoryOverhead` <- "512"

  # Connect to local cluster with custom configuration
  sc <- spark_connect(master = "local", config = config)

  spec_with_r <- sapply(read.csv('/media/tito/TIIGE/PRSIM/0.9995/angliers_sum/angliers_sum_0000001.csv', nrows = 1), class)

  subDir<-'angliers_sum'
  testo<-spark_read_csv(sc = sc,path = paste0(mainDir2,subDir),columns=spec_with_r,memory = FALSE)
  src_tbls(sc)
  testo$ops$vars
  #testo %>% filter(carillon_sum > 2000)
  start_time = Sys.time()
  df_mean_per_julian_day = testo %>% group_by(julian_day) %>% summarise(AvgQ=mean(angliers_sum,na.rm = TRUE))%>% collect()
  end_time = Sys.time()
  end_time - start_time
  df_max_per_julian_day = testo %>% group_by(julian_day) %>% summarise(MaxQ=max(angliers_sum,na.rm = TRUE))%>% collect()
  df_min_per_julian_day = testo %>% group_by(julian_day) %>% summarise(MinQ=min(angliers_sum,na.rm = TRUE))%>% collect()

  #mettre en ordre les statistiques sommaires de l'hydrogramme
  df_mean_per_julian_day_ordered<-df_mean_per_julian_day[order(df_mean_per_julian_day$julian_day),]
  df_max_per_julian_day_ordered<-df_max_per_julian_day[order(df_max_per_julian_day$julian_day),]
  df_min_per_julian_day_ordered<-df_min_per_julian_day[order(df_min_per_julian_day$julian_day),]

  final_prsim_angliers<-c(df_mean_per_julian_day_ordered,df_max_per_julian_day_ordered,df_min_per_julian_day_ordered)
  save(final_prsim_angliers,file='~/Documents/github/prsim/outaouais_sup_lynda/final_prsim_angliers.RData')

  #ggplot des statistiques sommaires des simulations PRSIM
  plot(df_max_per_julian_day_ordered,type='l',ylim=c(0,4000))
  points(df_mean_per_julian_day_ordered,type='l')
  points(df_min_per_julian_day_ordered,type='l')
  #ajout des informations de debit de la premiere etude
  #points(qobs_mean_per_day$AvgQ,type='l',col='red')
  #points(qobs_max_per_day$MaxQ,type='l',col='red')
  #points(qobs_min_per_day$MinQ,type='l',col='red')
  #sdf_pivot(testo, sim_number ~ carillon_sum)

  #calcul de la pointe a carillon
  #ajouter les saisons
  #res=testo%>%filter(season %in% target)%>%group_by(sim_number)%>%summarize(max=max(carillon_sum))%>%collect()

  #maximun par annee
  res=testo%>%group_by(sim_number)%>%summarize(max=max(angliers_sum))%>%collect()
  #ecdf cunnane
  ecdf_cunnane<-function (x)
  {
    x <- sort(x)
    n <- length(x)
    if (n < 1)
      stop("'x' must have 1 or more non-missing values")
    vals <- unique(x)
    rval <- approxfun(vals, cumsum(tabulate(match(x, vals))-0.4)/(n+0.2),
                      method = "constant", yleft = 0, yright = 1, f = 0, ties = "ordered")
    class(rval) <- c("ecdf", "stepfun", class(rval))
    assign("nobs", n, envir = environment(rval))
    attr(rval, "call") <- sys.call()
    rval
  }
  ecdf_max_year<-ecdf_cunnane(res$max)
  plot(ecdf_max_year)
  Fn<- ecdf_cunnane(res$max)
  #prendre les codes pour cunnane
  quantiles_qinter<-data.frame(quantiles=quantile(Fn, prob = c((1-(1/10000)),(1-(1/2000)),(1-(1/1000)),(1-(1/200)),(1-(1/100)),(1-(1/50)),(1-(1/20)),(1-(1/10)),(1-(1/2))), names = FALSE),row.names=c(10000,2000,1000,200,100,50,20,10,2))

  quantiles_qinter_2<-data.frame(quantiles=quantile(ecdf_max_year, prob = c((1-(1/10000)),(1-(1/2000)),(1-(1/1000)),(1-(1/200)),(1-(1/100)),(1-(1/50)),(1-(1/20)),(1-(1/10)),(1-(1/2))), names = FALSE),row.names=c(10000,2000,1000,200,100,50,20,10,2))

  #
  res<-c(final_prsim_angliers,quantiles_qinter)
  spark_disconnect(sc)

  return(res)

}
