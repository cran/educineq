#' Mean years of schooling
#'
#' \code{emean} is a function to compute mean years of schooling for any group of
#' countries included in the dataset developed in Jorda and Alonso (2017).
#' It is computed as the average of the national years of schooling
#' weighted by population weights.
#'
#' @param countries character vector with the country codes of the countries
#'   to be used. Some macro-regions are already defined and can be used
#'   instead of the country codes: \code{South Asia, Europe and Central Asia,
#'   Middle East and North Africa, Latin America and the Caribbean, Advanced
#'   Economies, Sub-Saharan Africa, East Asia and the Pacific} and \code{all}
#'   for the 142 counrties included in the dataset.(see \code{data_country}).
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
#' @param plot if \code{TRUE} (the default), displays a graph of the results.
#' @return \code{emean} returns a list with the following objects:
#'   \enumerate{
#'   \item \code{mean_years_of_schooling}: evolution of mean years of
#'   schooling from the initial to the last year.
#'   \item \code{countries}: countries used to compute mean years of schooling.
#'   \item If \code{plot = TRUE}, graphical representation of the numerical results.
#'   }
#' @seealso \code{\link{data_country}}. Visit \url{http://www.educationdata.unican.es}
#'   for more information on the constructoin of the dataset and the available
#'   \href{http://www.educationdata.unican.es/countries}{countries}.
#' @references Jorda, V. and Alonso, J.M. (2017). New estimates on educational
#'  attainment using a continuous approach (1970-2010), World Development,
#'  90, 281 - 293.
#'  \url{http://www.sciencedirect.com/science/article/pii/S0305750X16305010}
#' @export
#' @examples
#' emean(countries = "Advanced Economies", init.y = 1980, final.y = 2000,
#'       database = "male25")
#' emean(countries = c("DNK", "FIN", "ISL", "NOR", "SWE"), init.y = 1980,
#'       final.y = 2000, database = "male25")
emean <- function(countries, init.y, final.y, database, plot = TRUE) {
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
  mys <- matrix(NA, 1, length(time))
  for(k in 1:length(time)){
    x<-ok.data$mys[ok.data$year==time[k]]
    w <- ok.data$pop[ok.data$year==time[k]]/sum(ok.data$pop[ok.data$year==time[k]])
    mys[k] <- sum(w * x)
  }

  if (plot==TRUE){
    plot(time, mys, xlab = "Year", ylab = "Mean years of schooling",panel.first = grid(col="gray78"),
      xlim = c(min(time), max(time)), ylim = c(0,max(mys)), type = "o", pch = 20,col ="Blue")
    box(lwd = 2)
  }
  colnames(mys) <- time
  list(mean_years_of_schooling = mys, countries = unique(ok.data$country))
}
