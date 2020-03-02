#' Read RAFI XLSX file
#'
#' This function reads a set of returns and correlations from XLS file
#' downloaded from the RAFI website.  No processing is done. This function
#' allows one to see the data in an unprocessed form.
#'
#' @param fn name of XLSX file including path
#' @param rafi_xlranges list with 4 elements: return, corr, cov, and date are the Excel ranges
#' which contain the respective data.
#'
#' @keywords asset allocation efficient frontier
#' @export
#' @return A list with 4 items: ret is the return data, corr is the
#'   correlation data, cov is the covariance matrix and as_of_date is the date of the file
#'
readRAFIFile <- function(fn, rafi_xlranges = NULL){
  if(is.null(rafi_xlranges)) rafi_xlranges <- rafi_xlranges_default
  out<-list()
  out$as_of_date <- suppressMessages(as.character(readxl::read_xlsx(fn, sheet = "Expected.Returns",
                                                                    range = rafi_xlranges$date,
                                                                    col_names = FALSE, .name_repair = "unique")))
  out$ret <- suppressMessages(readxl::read_excel(fn, sheet = "Expected.Returns", range = rafi_xlranges$return))
  out$corr <- suppressMessages(readxl::read_excel(fn, sheet = "Expected.Risk.Matrix", range = rafi_xlranges$corr))
  out$cov <- suppressMessages(readxl::read_excel(fn, sheet = "Expected.Risk.Matrix", range = rafi_xlranges$cov))
  colnames(out$ret) <- make.names(colnames(out$ret))
  out$ret$Asset.Class <- make.names(out$ret$Asset.Class)
  colnames(out$corr) <- make.names(colnames(out$corr))
  colnames(out$cov) <- make.names(colnames(out$cov))
  out$corr <- cbind(Asset.Class = colnames(out$corr), out$corr)
  out$cov <- cbind(Asset.Class = colnames(out$cov), out$cov)
  if(!(all(out$ret$Asset.Class == out$corr$Asset.Class))) warning("Returns and correlations are not in same order")
  if(!(all(out$ret$Asset.Class == out$cov$Asset.Class))) warning("Returns and Covariances are not in same order")
  if(!(all(out$cov$Asset.Class == out$corr$Asset.Class))) warning("Covariances and correlations are not in same order")
  return(out)
}

#' Read Asset Class Name File
#'
#' This file contains both a mapping of RT asset class names to RAFI and characteristics of the asset classes
#'
#' @param fn name of file
#' @param sheet which sheet to read, defaults to first
#'
#' @return
#' @export
#'
readACNameFile <- function(fn, sheet = 1){
  out <- suppressMessages(readxl::read_excel(acNamesFN, sheet = sheet))
  out$rafi_ret_class_names <- make.names(out$rafi_ret_class_names)
  out$rafi_corr_class_names <- make.names(out$rafi_corr_class_names)
  return(out)
}

#' Reads and Excel file
#'
#' Makes sure the names are legit in R
#'
#' @param fileName name of Excel file
#' @param sheet sheet to read, default, NULL, leads to first
#'
#' @return
#' @export
#'
readExcelFile <- function(fileName, sheet = NULL){
  out <- readxl::read_excel(fileName, sheet, .name_repair = "unique")  # "data/Rebalancer - Portfolio Targets.xlsx"
  colnames(out) <- make.names(colnames(out))
  return(out)
}

#' Read Portfolio Holdings from File
#'
#'# Read in a file exported from Black Diamond / Data Mining / Rebalance Template extract a portfolio
#'  Munge a bit
#'
#' @param portfolioName name of portfolio for which to filter holdings. If NULL all holdings for all portfolios are returned.
#' @param portfolioHoldingsFN name of Excel file with the holdings
#'
#' @return
#' @export
#'
readPortfolioHoldings <- function(portfolioName = NULL, portfolioHoldingsFN){
  out <- suppressWarnings(readxl::read_excel(portfolioHoldingsFN, .name_repair = "unique",
                                                 col_types = c(rep("guess",9), rep("logical", 3), rep("guess", 12))))
  colnames(out) <- make.names(colnames(out))
  if(!is.null(portfolioName)){
    out <- out %>%
      filter(Portfolio.Name == portfolioName)
  }
  return(out)
}

#' Read External Data Files
#'
#' @param portfolioName name of portfolio in BD
#' @param portfolioHoldingsFN name of file with portfolio holdings exported from BD
#' @param scenariosFN name of Excel file with scenario returns.
#' @param scenarios_sheet name of worksheet in the scenariosFN file containing the data.
#' @param benchmarkDefinitionFN name of file containing the definitions of benchmarks.
#' @param rafiFN name of file containing RAFI expected return, correlation, covariance data.
#' @param rafi_xlranges default (null) uses rafi_xlranges_default or specify names ranges. See readRAFIFile
#'
#' @return list with external data
#' @export
#'
loadExternalData <- function(portfolioName, portfolioHoldingsFN, scenariosFN, scenarios_sheet, benchmarkDefinitionFN,
                             rafiFN, rafi_xlranges){
  if(missing(rafi_xlranges)) rafi_xlranges <- NULL
  out <- addItem(name = "portfolioHoldings", item = readPortfolioHoldings(portfolioName, portfolioHoldingsFN)) %>%
    addItem(name = "scenarios", item = readExcelFile(scenariosFN, sheet=scenarios_sheet)) %>%
    addItem(name = "benchmarkDefinitions", item = readExcelFile(benchmarkDefinitionFN))  %>%
    addItem(name = "cashBuffers", item = readExcelFile(cashBuffersFN)) %>%
    addItem(name = "rafiData", item = readRAFIFile(rafiFN, rafi_xlranges)) %>%
    addItem(name = "acNames", item = readACNameFile(acNamesFN))
  return(out)

}
