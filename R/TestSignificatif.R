#' TestSignificatif
#'
#' @param serie série stationnaire (d=0)
#' @param p ordre partie autorégressive à tester
#' @param q ordre partie moyenne mobile à tester
#' @param w_mean include mean
#' @param w_drift include drift
#'
#' @return Significativité au seuil de 95% des coefficients des ordres les plus élevés
#' @export
#'
#' @importFrom stats Box.test arima pnorm residuals
TestSignificatif <- function(serie,p,q, w_mean, w_drift){
  var<-Arima(serie,order=c(p,0,q), include.mean = w_mean, include.drift = w_drift)
  coef<-var$coef
  se<-sqrt(diag(var$var.coef))
  t<-abs(coef/se)
  t2<-t>1.96             #on rejette l'hypothèse nulle du test de Student ?
  sig<-c(t2[p],t2[p+q])  #On regarde si les coefficients des plus grands ordres sont significatifs
  if (p==0 & q!=0 ){
    res<-t2[q]
    nb_param<-1}
  else if(q==0 & p!=0){
    res<-t2[p]
    nb_param<-1}
  else {
    res<-t2[p]+t2[p+q]
    nb_param<-2} #(p!=0 & q!=0)


  #Résultats d'intérêt
  coef_est<-c(coef[p],coef[p+q])
  se_est<-c(se[p],se[p+q])
  tstat<-c(t[p],t[p+q])
  pval <- (1-pnorm(abs(tstat)))*2

  print(rbind(coef_est,se_est,tstat,pval))
  cat(" \n")
  cat("Significativite au seuil de 95% \n")
  print(sig)
  if (res!=nb_param){cat("=> Modele rejete : au moins l'un des coefficients des ordres les plus eleves n'est pas signififcatif au seuil de 95%\n")} else{cat("=> Modele bien ajuste\n")}
}
