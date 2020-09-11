
#' Comparaison de la somme des apports intermediaires avec les simulations de prsim
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' path<-'/home/tito/Documents/github/prsim/outaouais_sup_lynda/Frequentielles/Apports Latéraux Methode extrême PRT/Q10000/'
#' sum_temiscamingue<-Comparaison_Temiscamingue(path)

Comparaison_Temiscamingue<-function(path){

  require(readxl)
  require(tidyverse)
  require(readr)




  setwd(path)

  files<-list.files(pattern = '.csv')


  total_temiscamingue<-list()

  for(file in files){

    df<-readr::read_csv(file)

    #somme jusqu'a temiscamingue

    bvs_temiscamingue<-c( "Dozois","Lac Victoria et lac Granet","Rapide 7","Rapide 2","Riviere Kinojevis","Lac des Quinze","Mistinikon","Lady Evelyn","Lower Notch et Indian Chute","Rabbit Lake", "Kipawa",
                          "Lac Temiscamingue a Angliers","Riviere Blanche" )

    x<-df[bvs_temiscamingue]

    sum_temiscamingue<-rowSums (x, na.rm = FALSE, dims = 1)

    sum_carillon<-rowSums (df, na.rm = FALSE, dims = 1)

    total_temiscamingue[[file]]<-sum_temiscamingue
  }

  df_test<-do.call(cbind,total_temiscamingue)

  matplot(df_test,type='l',col='blue',lty=1,xlab = 'jours juliens',ylab='Q', main="Scénarios de crue 1:10000 à Témiscamingue de l''étude de l''Outaouais Supérieur")
  grid()
  return(sum_temiscamingue)

}


#' Comparaison de la somme des apports intermediaires avec les simulations de prsim
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' path<-'/home/tito/Documents/github/prsim/outaouais_sup_lynda/Frequentielles/Apports Latéraux Methode extrême PRT/Q10000/'
#' sum_angliers<-Comparaison_Angliers(path)

Comparaison_Angliers<-function(path){

  require(readxl)
  require(tidyverse)
  require(readr)




  setwd(path)

  files<-list.files(pattern = '.csv')


  total_angliers<-list()

  for(file in files){

    df<-readr::read_csv(file)

    #somme jusqu'a temiscamingue

    bvs_angliers<-c( "Dozois","Lac Victoria et lac Granet","Rapide 7","Rapide 2","Riviere Kinojevis","Lac des Quinze")

    x<-df[bvs_angliers]

    sum_angliers<-rowSums (x, na.rm = FALSE, dims = 1)

    sum_carillon<-rowSums (df, na.rm = FALSE, dims = 1)

    total_angliers[[file]]<-sum_angliers
  }

  df_test<-do.call(cbind,total_angliers)

  matplot(df_test,type='l',col='blue',lty=1,xlab = 'jours juliens',ylab='Q', main="Scénarios de crue 1:10000 à Angliers de l''étude de l''Outaouais Supérieur")
  grid()
  return(sum_angliers)

}
