#' Lorenz curve of education
#'
#' \code{elc} is a function to graph the Lorenz curve of education for any group of
#' countries using the set of estimates developed in Jorda and Alonso (2017).
#'
#' @inheritParams edcdf
#' @param M size of the simulated sample (default \code{M = 5000}).
#' @return \code{elc} returns a graph of the evolution of the Lorenz curve of education
#' over the specified period.
#' @seealso \code{\link[flexsurv]{GenGamma.orig}}, \code{\link[ineq]{Lc}},
#'   \code{\link{data_country}}. Visit \url{http://www.educationdata.unican.es}
#'   for more information on the constructoin of the dataset and the available
#'   \href{http://www.educationdata.unican.es/countries}{countries}.
#' @details We use the set of estimates developed in Jorda and Alonso (2017), where
#' the generalized gamma distribution (Stacy, 1962) is used to model the time that
#' individuals attend school until they complete the educational cycle or decide to
#' drop out. To accommodate time and country varying parameters, the distribution of education
#' of each country and year is estimated by non-linear least squares (see, Jorda and
#' Alonso (2017) for further description on the estimation strategy).The Lorenz curve
#' is computed from a synthetic sample of size \code{M} of the distribution of
#' education of the specified group of countries.
#' The sample is obtained by Monte Carlo simulation using the mixture of the national
#' distributions, weighted by their population shares.
#' @references Cowell, F. (2011). Measuring inequality. Oxford University Press.
#'
#' Jorda, V. and Alonso, J.M. (2017). New estimates on educational
#'  attainment using a continuous approach (1970-2010), World Development,
#'  90, 281 - 293. \url{http://www.sciencedirect.com/science/article/pii/S0305750X16305010}
#'
#'  Stacy, E. W. (1962). A generalization of the gamma distribution. Annals of
#'  Mathematical Statistics, 33, 1187 - 1192.
#'
#' @export
#' @examples
#' elc(countries = c("CAN","USA"), init.y = 1985, final.y = 1985,
#'   database = "female25", M = 300)
#'
#' @importFrom graphics abline
elc <- function(countries, init.y, final.y, database, M = 5000) {
  if (init.y < 1970){init.y = 1970}
  if (final.y > 2010){final.y = 2010}
  if (final.y < init.y){
    print("Initial year must be earlier than final year.")
    stop()
  }
  if((init.y/5)%%1 != 0 | init.y == "" ) {
    print("Starting year incorrectly specified")
    stop()
  }
  if (final.y == "" | (final.y/5)%%1 != 0 ) {
    print("Final year incorrectly specified")
    stop()
  }
  if (database != "total15" & database != "total25" &
      database != "male15" & database != "male25" &
      database != "female15" & database != "female25"|
      database == "") {
    print("Database incorrectly specified. Use total15, total25, male15, male25, female15 or female25.")
    stop()
  }
  if (database == "total15") {
    dataset <- estim_total15
  }
  if (database == "total25") {
    dataset <- estim_total25
  }
  if (database == "male15") {
    dataset <- estim_male15
  }
  if (database == "male25") {
    dataset <- estim_male25
  }
  if (database == "female15") {
    dataset <- estim_female15
  }
  if (database == "female25") {
    dataset <- estim_female25
  }
  if(any(countries %in% levels(data_countries$Region))){
    if (length(which(countries %in% levels(data_countries$Region)))<2) {
      countries<-data_countries$Code[data_countries$Region ==
          countries[which(countries %in% levels(data_countries$Region))]]
    }
    else{
      print("More than two regions used as countries.")
      stop()
    }
  }
  if(any(countries == "all")){
    countries<-data_countries$Code
  }
  countries = as.data.frame(countries)
  ok.data = merge(x = dataset, y = countries, by.x = "code", by.y = "countries")
  if (nrow(ok.data) == 0) {
    print("Countries are incorrectly specified. Check the list of countries.")
    stop()
  }
  if (length(unique(ok.data$country)) != nrow(countries)) {
    print("Warning: Some countries are incorrectly specified. Check the list of countries.")
  }
  time <- seq(init.y, final.y, 5)
  qED <- matrix(NA, M, length(time))
  set.seed(7)
  sim <- runif(M)
  for(k in 1:length(time)){
    a.x<-ok.data$parA[ok.data$year==time[k]]
    p.x<-ok.data$parP[ok.data$year==time[k]]
    b.x<-ok.data$parB[ok.data$year==time[k]]
    w <- ok.data$pop[ok.data$year==time[k]]/sum(ok.data$pop[ok.data$year==time[k]])
    for(m in 1:M){
      solveED<-function(x){
        w%*%pgengamma.orig(x,a.x,b.x,p.x) - sim[m]
      }
      unite <- try(uniroot(solveED, c(0, 2000)))
      if('try-error'%in%class(unite)) next
      else{
        qED[m,k] <- unite$root
      }
    }
  }
  plot(c(0, ineq::Lc(qED[, 1])$p, 1), c(0, ineq::Lc(qED[, 1])$L, 1), xlab = "Proportion of population", ylab = "Proportion of education", panel.first = grid(col="gray78"), xlim = c(0, 1), type = "l", pch = 20, col = 2)
  box(lwd = 2)
  if (length(time)>1){
    for(j in 2:ncol(qED)){
      points(c(0, ineq::Lc(qED[, j])$p, 1), c(0, ineq::Lc(qED[, j])$L, 1), col = j+1, type = "l")
    }
  }
  abline(0,1, col="Black")
  legendtext <- time
  legend("topleft", legend = legendtext, cex = 0.7,
    lty = 1, col = 2:(ncol(qED)+1), ncol = 2)
  list(countries = unique(ok.data$country))
}
