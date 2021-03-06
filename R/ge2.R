#' Generalized entropy measure of education
#'
#' \code{ege2} function computes the generalized entropy measure of education, with
#' the sensitivity parameter set to 2, for any group of countries included
#' in the dataset developed in Jorda and Alonso (2017). The function also
#' provides a decomposition of this index in between-county and within-
#' country inequality.
#'
#' @inheritParams emean
#' @return \code{ege2} returns a list with the following objects:
#'   \enumerate{
#'   \item \code{GE_2}: evolution of the generalized entropy measure of education
#'     from the initial to the last year, decomposed in between-country
#'     and within-country inequality.
#'   \item \code{countries}: countries used to compute the generalized entropy measure.
#'   \item If \code{plot = TRUE}, graphical representation of the numerical results.
#'   }
#' @seealso \code{\link{data_country}}. Visit \url{http://www.educationdata.unican.es}
#'   for more information on the constructoin of the dataset and the available
#'   \href{http://www.educationdata.unican.es/countries}{countries}.
#' @details The estimates of the generalized entropy measure for the specified group of countries
#' can be easily derived by taking advantage of the decomposition of this family.
#' It is computed as the sum of the following terms, which correspond to within-
#' country and between, country inequality respectively (see, e.g., Cowell, 2011):
#'
#' \eqn{GE(2)_W=\sum_{i=1}^{N} s_i^2 p_i^{-1} GE(2)_i;}
#'
#' \eqn{GE(2)_B= 0.5 \sum_{i=1}^{N} p_i (\mu_i / \mu)^2 -1,}
#'
#' where \emph{N} is the number of countries, \eqn{GE(2)_i} and \eqn{p_i} denote,
#' respectively, the generalized entropy measure and the population weight of the
#' country \emph{i}, and \eqn{s_i} stands for the proportion of mean income of the
#' country \emph{i} in the overall mean of the group:
#' \eqn{s_i=\lambda_i \mu_i / \sum_{i=1}^{N} \lambda_i \mu_i}.
#'
#' @references Cowell, F. (2011). Measuring inequality. Oxford University Press.
#'
#' Jorda, V. and Alonso, J.M. (2017). New estimates on educational
#'  attainment using a continuous approach (1970-2010), World Development,
#'  90, 281 - 293.
#'  \url{http://www.sciencedirect.com/science/article/pii/S0305750X16305010}
#' @export
#' @examples
#' ege2(countries = "all", init.y = 1980, final.y = 2000,
#'      database = "total25")
#' ege2(countries = c("DNK", "FIN", "ISL", "NOR", "SWE"), init.y = 1980,
#'      final.y = 2000, database = "female15")
ege2 <- function(countries, init.y, final.y, database, plot = TRUE) {
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
    i.x<-ok.data$ge2[ok.data$year==time[k]]
    w <- ok.data$pop[ok.data$year==time[k]]/sum(ok.data$pop[ok.data$year==time[k]])
    m <- sum(w*x)
    si<-w*x/m
    theil[1, k] <- 1/2*(sum(w*(x/m)^2)-1)
    theil[2, k] <- sum(w^-1*si^2*i.x)
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
  row.names(theil) <- c("Between","Within","Overall")
  list(GE_2 = theil, countries = unique(ok.data$country))
}
