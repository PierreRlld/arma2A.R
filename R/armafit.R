#' Title
#'
#' @param serie série Stationnaire d'entrée
#' @param p ordre partie autorégressive à tester
#' @param q ordre partie moyenne mobile à tester
#' @param lagmax lag max à tester pour les autocorrélations des résidus (statistique de Ljung-Box) > hypothèse forte résidus suivent un bruit blanc, test jusqu'à lag_max
#'
#' @return Synthese de TestAutocorr et TestSignificativite
#' @export
#'
#' @importFrom stats arima AIC BIC
armafit<-function(serie,p,q,lagmax){
  cat("Try ARMA(",p,",",q,")\n")
  cat("\n")
  cat("1/ Test d'absence absence d'autocorrelation des residus [Ljung-Box] :\n")
  TestAutocorr(serie,p,q,lagmax)
  cat(" \n")
  cat("2/ Test de nullite des coefficients des des ordres les plus eleves :\n")
  TestSignificatif(serie,p,q)
  cat(" \n")
  cat("3/ Criteres d'informations :\n")
  mod <- arima(serie, order=c(p,0,q), include.mean = FALSE)
  cat(rbind('AIC: ', AIC(mod),'---', 'BIC: ', BIC(mod)))
}
