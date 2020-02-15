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

readACNameFile <- function(fn, sheet = 1){
  out <- suppressMessages(readxl::read_excel(acNamesFN, sheet = sheet))
  out$rafi_ret_class_names <- make.names(out$rafi_ret_class_names)
  out$rafi_corr_class_names <- make.names(out$rafi_corr_class_names)
  return(out)
}
