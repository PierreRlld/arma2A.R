#' Title
#'
#' @param serie série stationnaire d'entrée
#' @param p ordre partie autorégressive à tester
#' @param q ordre partie moyenne mobile à tester
#'
#' @return TestAutocorr et TestSignificativite
#' @export
#'
armafit<-function(serie,p,q){
  cat("Try ARMA(",p,",",q,")\n")
  cat("\n")
  cat("1/ Test d'absence absence d'autocorrelation des residus [Ljung-Box] :\n")
  TestAutocorr(serie,p,q)
  cat(" \n")
  cat("2/ Test de nullite des coefficients des des ordres les plus eleves :\n")
  TestSignificatif(serie,p,q)
}