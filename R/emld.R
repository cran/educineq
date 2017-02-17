#' Mean log deviation (MLD) of education
#'
#' \code{emld} function computes the MLD of education, with for any group of
#' countries included in the dataset developed in Jorda and Alonso (2017).
#' The function also provides a decomposition of this index in between-county
#' and within-country inequality.
#'
#' @inheritParams emean
#' @return \code{emld} returns a list with the following objects:
#'   \enumerate{
#'   \item \code{MLD}: evolution of the MLD of education
#'     from the initial to the last year, decomposed in between-country
#'     and within-country inequality.
#'   \item \code{countries}: countries used to compute the MLD.
#'   \item If \code{plot = TRUE}, graphical representation of the numerical results.
#'   }
#' @seealso \code{\link{data_country}}. Visit \url{http://www.educationdata.unican.es}
#'   for more information on the constructoin of the dataset and the available
#'   \href{http://www.educationdata.unican.es/countries}{countries}.
#' @details The estimates of the MLD for the specified group of countries
#' can be easily derived by taking advantage of the decomposition of this family.
#' It is computed as the sum of the following terms, which correspond to within-
#' country and between, country inequality respectively (see, e.g., Cowell, 2011):
#'
#' \eqn{MLD_W=\sum_{i=1}^{N} p_i MLD_i;}
#'
#' \eqn{MLD_B=\sum_{i=1}^{N} p_i log(\mu / \mu_i),}
#'
#' where \emph{N} is the number of countries, \eqn{MLD_i} and \eqn{p_i} denote, respectively, the MDL
#' and the population weight of the country \emph{i}.
#'
#' @references Cowell, F. (2011). Measuring inequality. Oxford University Press.
#'
#' Jorda, V. and Alonso, J.M. (2017). New estimates on educational
#'  attainment using a continuous approach (1970-2010), World Development,
#'  90, 281 - 293.
#'  \url{http://www.sciencedirect.com/science/article/pii/S0305750X16305010}
#' @export
#' @examples
#' emld(countries = "East Asia and the Pacific", init.y = 1980,
#'      final.y = 2000, database = "female25")
#' emld(countries = c("DNK", "FIN", "ISL", "NOR", "SWE"), init.y = 1980,
#'      final.y = 2000, database = "total25")
emld <- function(countries, init.y, final.y, database, plot = TRUE) {
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
    dataset <- ineq_total15
  }
  if (database == "total25") {
    dataset <- ineq_total25
  }
  if (database == "male15") {
    dataset <- ineq_male15
  }
  if (database == "male25") {
    dataset <- ineq_male25
  }
  if (database == "female15") {
    dataset <- ineq_female15
  }
  if (database == "female25") {
    dataset <- ineq_female25
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
  theil <- matrix(NA, 3, length(time))
  for(k in 1:length(time)){
    x<-ok.data$mys[ok.data$year==time[k]]
    i.x<-ok.data$mld[ok.data$year==time[k]]
    w <- ok.data$pop[ok.data$year==time[k]]/sum(ok.data$pop[ok.data$year==time[k]])
    m <- sum(w*x)
    theil[1, k] <- log(m)-sum(w*log(x))
    theil[2, k] <- sum(w*i.x)
    theil[3, k] <-  theil[1, k]+ theil[2, k]
  }
  if (plot==TRUE){
    plot(time, theil[3,], xlab = "Year", ylab = "Inequality", panel.first = grid(col="gray78"),
      xlim = c(min(time), max(time)), ylim = c(0, 1.5*max(theil[3,])), type = "o", pch = 20,col ="Blue")
    box(lwd = 2)
    points(time, theil[1,], col="Red", type="o", pch = 20)
    points(time, theil[2,], col="green4", type="o", pch = 20)
    legendtext <- c("MLD", "Between-country", "Within-country")
    legend("top", legend = legendtext, cex = 0.8, ncol = 3,
      lty = 1, col = c("Blue", "Red", "green4"))
  }
  colnames(theil) <- time
  row.names(theil) <- c("Between","Within","MLD")
  list(MLD = theil, countries = unique(ok.data$country))
}
