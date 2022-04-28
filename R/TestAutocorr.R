#' TestAutocorr
#'
#' @param var série stationnaire (d=0); type timeSeries
#' @param p ordre partie autorégressive à tester
#' @param q ordre partie moyenne mobile à tester
#'
#' @return lag=i et p-value du test de Portemanteau (test d'indépendance des résidus -> white noise ? ; le test vérifie sur un nb de lag donné si les autocorrélations entre résidus sont nuls) avec statistique de Ljung-Box. p-value <0.05 on rejette l'hypothèse nulle au seuil de 95% i.e. rejette l'hypothèse d'indépendance des résidus.
#' @export

TestAutocorr<-function(var,p,q){
  autocorL<-list()
  fitdf<-p+q
  serie<-arima(var,order=c(p,0,q),include.mean=FALSE)
  rej<-0
  for (i in c(1:20)){
    if (i<=fitdf) {NA}
    else {
      cat("lag=",i,"pvalue=",Box.test(residuals(serie),lag=i,type="Ljung-Box",fitdf = fitdf)$p.value,"\n")  #type="Ljung-Box",
      rej<-rej+(Box.test(residuals(serie),lag=i,fitdf = fitdf)$p.value<0.05)}
  }
  if (rej>0) cat("=> Modele non valide : l'absence d'autocorrelation des residus est rejetee au seuil de 95%.\n")
  else cat("=> Modele valide\n")
}
