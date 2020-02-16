#' Create Benchmark Weights
#'
#' Given a benchmark definition in a string assignes the weights to each class.
#' It creates a vector for the base classes (n asset classes) and a vector with the tax status (3n).
#' If the benchmark definition has a weight to an asset class called Bonds, it will assign a percentage of that weight
#' to the taxable account in Munis and a percentage into USCoreBonds for the deferred and exempt accounts.
#'
#' @param benchmark.txt string of benchmark definition
#' @param investor investor class object containing initialValues$PreTaxPct to allocate Bonds appropriately.
#'
#' @return a list with the two vectors and a the string defining the benchmark.
#' @export
#'
createBenchmarkWts <- function(benchmark.txt, investor){
  pctTaxable <- investor$initialValues$PreTaxPct[1]
  short.benchmark<-eval(parse(text=paste0("c(",benchmark.txt,")")))
  out <- list()
  out$txt <- benchmark.txt
  out$baseClasses <- rep(0, cma$nClasses)
  names(out$baseClasses)<-cma$classes

  for (i in 1:length(short.benchmark)){
    if(toupper(names(short.benchmark)[i]) == "BONDS"){
      out$baseClasses["Munis"] <- short.benchmark[i] * pctTaxable
      out$baseClasses["USCoreBonds"] <- short.benchmark[i] * (1 - pctTaxable)
    } else {
      out$baseClasses[names(short.benchmark[i])]<-short.benchmark[i]
    }
  }

  out$taxClasses<-c(out$baseClasses * investor$initialValues$PreTaxPct[1],
           out$baseClasses * investor$initialValues$PreTaxPct[2],
           out$baseClasses * investor$initialValues$PreTaxPct[3])
  names(out$taxClasses) <- c(paste0(cma$classes, "-Taxable"), paste0(cma$classes, "-Deferred"),paste0(cma$classes, "-Exempt"))
  out$taxClasses["Munis-Taxable"] <- out$taxClasses["Munis-Taxable"] + out$taxClasses["USCoreBonds-Taxable"]
  out$taxClasses["USCoreBonds-Taxable"] <- 0
  out$taxClasses["USCoreBonds-Deferred"] <- out$taxClasses["Munis-Deferred"] + out$taxClasses["USCoreBonds-Deferred"]
  out$taxClasses["Munis-Deferred"] <- 0
  out$taxClasses["USCoreBonds-Exempt"] <- out$taxClasses["Munis-Exempt"] + out$taxClasses["USCoreBonds-Exempt"]
  out$taxClasses["Munis-Exempt"] <- 0
  return(out)
}



