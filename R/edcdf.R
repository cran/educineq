#' Cumulative distribution function of time of schooling
#'
#' \code{edcdf} is a function to graph the CDF of time of schooling for any group of
#' countries using the set of estimates developed in Jorda and Alonso (2017).
#'
#' @param countries character vector with the country codes of the countries
#'   to be used. Some macro-regions are already defined and can be used
#'   instead of the country codes: \code{South Asia, Europe and Central Asia,
#'   Middle East and North Africa, Latin America and the Caribbean, Advanced
#'   Economies, Sub-Saharan Africa, East Asia and the Pacific}.
#'   (see \code{data_country}).
#' @param init.y the first year in which the function is calculated. Available
#'   years are 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010.
#' @param final.y the last year in which the function is calculated Available
#'   years are 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010.
#' @param database population subgrup for which the function is calculated.
#'   The following options are available:
#'   \enumerate{
#'     \item \code{"total15"}: Total population aged over-15.
#'     \item \code{"total25"}: Total population aged over-25.
#'     \item \code{"male15"}: Male population aged over-15.
#'     \item \code{"male25"}: Male population aged over-25.
#'     \item \code{"female15"}: Female population aged over-15.
#'     \item \code{"female25"}: Female population aged over-25.
#'   }
#' @return \code{edcdf} returns a graph of the evolution of the CDF of education
#' over the specified period.
#' @seealso \code{\link[flexsurv]{GenGamma.orig}}, \code{\link{data_country}}.
#'   Visit \url{http://www.educationdata.unican.es}for more information on
#'   the constructoin of the dataset and the available
#'   \href{http://www.educationdata.unican.es/countries}{countries}.
#' @details We use the set of estimates developed in Jorda and Alonso (2017), where
#' the generalized gamma distribution (Stacy, 1962) is used to model the time that
#' individuals attend school until they complete the educational cycle or decide to
#' drop out. The reason is twofold; first, the generalized gamma distribution is a
#' parsimonious model that nests most of the parametric assumptions described in the
#' literature (see, Marshall and Olkin, 2007). Second, the generalized gamma distribution
#' is able to model one- and zero-mode distributions and to represent several types of
#' hazard rates.The flexibility of this model to consider such heterogeneity, makes it
#' an outstanding candidate to model the distribution of education. It is important to
#' highlight that this parametric model includes as particular cases most of the
#' distributions commonly used in survival analysis, including the Weibull, the
#' exponential, and the gamma distributions, so it would converge to any of its special
#' cases if needed.
#'
#'  To accommodate time and country varying parameters, the distribution of education
#'  of each country and year is estimated by non-linear least squares (see, Jorda and
#'  Alonso (2017) for further description on the estimation strategy).The distribution
#'  of education of a particular group or region of countries is simply defined as a
#'  mixture of the national distributions, weighted by their population shares.
#'
#' @references Jorda, V. and Alonso, J.M. (2017). New estimates on educational
#'  attainment using a continuous approach (1970-2010), World Development,
#'  90, 281 - 293. \url{http://www.sciencedirect.com/science/article/pii/S0305750X16305010}
#'
#'  Marshall, A. W. and Olkin, I. (2007). Life distributions. Structure of nonparametric,
#'  semiparametric, and parametric families. New York: Springer.
#'
#'  Stacy, E. W. (1962). A generalization of the gamma distribution. Annals of
#'  Mathematical Statistics, 33, 1187 - 1192.
#'
#' @export
#' @examples
#' edcdf(countries = "South Asia", init.y = 1980, final.y = 1990, database = "female25")
#' edcdf(countries = c("DNK", "FIN", "ISL", "NOR", "SWE"),init.y = 1995,
#' final.y = 2010, database = "male25")
#' @importFrom graphics plot grid box legend points
edcdf <- function(countries, init.y, final.y, database) {
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
  x.axis<-seq(0.0000001,30,0.01)
  qED <- matrix(NA, length(x.axis), length(time))
  for(k in 1:length(time)){
    a.x<-ok.data$parA[ok.data$year==time[k]]
    p.x<-ok.data$parP[ok.data$year==time[k]]
    b.x<-ok.data$parB[ok.data$year==time[k]]
    w <- ok.data$pop[ok.data$year==time[k]]/sum(ok.data$pop[ok.data$year==time[k]])
    pdfED<-function(x){
      w%*%pgengamma.orig(x,a.x,b.x,p.x)
    }
    for (i in 1:length(x.axis)){
      qED[i,k] <- pdfED(x.axis[i])
    }
  }
  plot(x.axis, qED[, 1], xlab = "Years of schooling", ylab = "Probability", panel.first = grid(col="gray78"), ylim=c(0,1),xlim = c(0.5, 30), type = "l", pch = 20, col = 1)
  box(lwd = 2)
  if (length(time)>1){
    for(j in 2:ncol(qED)){
      points(x.axis, qED[, j], col = j, type = "l")
    }
  }
  legendtext <- time
  legend("bottomright", legend = legendtext, cex = 0.7,
    lty = 1, col = 1:ncol(qED), ncol = 2)
  list(countries = unique(ok.data$country))
}
