#' TestAutocorr
#'
#' @param var série stationnaire (d=0); type timeSeries
#' @param p ordre partie autorégressive à tester
#' @param q ordre partie moyenne mobile à tester
#' @param lagmax lag max à tester pour les autocorrélations des résidus (statistique de Ljung-Box) > hypothèse forte résidus suivent un bruit blanc, test jusqu'à lag_max
#' @param w_mean include mean
#' @param w_drift include drift
#'
#' @return lag=i et p-value du test de Portemanteau (test d'indépendance des résidus -> white noise ? ; le test vérifie sur un nb de lag donné si les autocorrélations entre résidus sont nuls) avec statistique de Ljung-Box. p-value <0.05 on rejette l'hypothèse nulle au seuil de 95% i.e. rejette l'hypothèse d'indépendance des résidus.
#' @export

TestAutocorr<-function(var,p,q,lagmax, w_mean, w_drift){
  autocorL<-list()
  fitdf<-p+q
  serie<-Arima(var,order=c(p,0,q), include.mean = w_mean, include.drift = w_drift)
  rej<-0
  for (i in c(1:lagmax)){
    if (i<=fitdf) {NA}
    else {
      cat("lag=",i,"pvalue=",Box.test(residuals(serie),lag=i,type="Ljung-Box",fitdf = fitdf)$p.value,"\n")  #type="Ljung-Box",
      rej<-rej+(Box.test(residuals(serie),lag=i,fitdf = fitdf)$p.value<0.05)}
  }
  if (rej>0) cat("=> Modele non valide : l'absence d'autocorrelation des residus est rejetee au seuil de 95%.\n")
  else cat("=> Modele valide\n")
}
