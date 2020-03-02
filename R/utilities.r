# Miscellaneous utilities

#' Add Item to a List. Used to make reading, editing of lists easier.
#'
#' @param .lst optional list to which to add item. If omitted, a list with only the item is returned.
#' @param name name of item
#' @param item item to be added to the list
#'
#' @return list
#' @export
#'
addItem <- function(.lst, name = "", item){
  out <- list(item)
  names(out) <- name
  if(missing(.lst)) {
    return(out)
  } else {
    return(c(.lst, out))
  }
}

#' Current Asset Allocation by Segment
#'
#' @param lots tibble or data frame with rtSegement, Market.Value (and Tax.Status if type = taxstatus)
#' @param type Overall (default) for overall allocation, taxstatus for allocation by segment within account type
#'
#' @return Tibble with results
#' @export
#'
calcAllocationBySegment <- function(lots, type = "overall"){
  if("rtSegment" %in% names(lots)) lots <- lots %>% rename(segment = rtSegment)
  out <- switch(type,
                "overall" = lots %>% group_by(segment) %>% summarise(Value = sum(Market.Value)) %>% mutate(Wt = Value / sum(Value)) %>%
                  rename(Segment = segment),
                "taxstatus" = lots %>% group_by(segment, Tax.Status) %>% summarise(Value = sum(Market.Value)) %>% mutate(Wt = Value / sum(Value)) %>%
                  rename(Segment = segment))
  return(out)
}

#' Find RT Segment Name Given a BD Segment Name
#'
#' Given something like 'Cash and Equi' which is used in BD, this will return something like 'USCash' as used by our asset allocation routines.
#'
#' @param segment BD style segment name.
#'
#' @return character string
#' @export
#'
convertBDSegmentToRT <- function(segment){
  # acMap below is now part of the package data.
  # acMap <- data.frame(stringsAsFactors = FALSE,
  #                     rt = c("Munis", "STUSTsy", "LTUSTsy", "IntUSTsy", "USTIPs", "USCash", "BankLoans", "USTIPs",
  #                            "BankLoans", "Munis", "HiYld", "EMLocalDebt", "EMNonLclDebt", "GlobalCore", "GlobalCore",
  #                            "HiYld", "EMCurrency", NA, NA, NA, "REIT", "Commodities", "USCash", NA, NA, "USLarge",
  #                            "USSmall", "EMEquity", "EAFEEquity", "GlblEquity", "USCash", "LTUSTsy", "Munis", "USTIPs",
  #                            "USCoreBonds","USCoreBonds"),
  #                     bd = toupper(c("Tax-Exempt", "U.S. Short Taxable", "U.S. Long Term", "U.S. Int. Taxable", "Inflation Protected",
  #                                    "Cash & Equiv", "Floating Rate Note", "Global ILBs", "Bank Loans", "High Yield Tax-Exemp",
  #                                    "High Yield Taxable", "EM (Local) Debt", "EM (Non-Local) Debt", "Foreign Bonds", "Global Bonds",
  #                                    "Preferred", "Emerging Currency", "Conservative", "Moderate", "Aggressive", "REITs",
  #                                    "Commodity", "Alternatives", "Other", "To Be Classified", "U.S. Large", "U.S. Small",
  #                                    "Emerging", "International", "Global", "Cash & Equivalents", "U.S. Long Term Taxable",
  #                                    "High Yield Tax-Exempt", "U.S. TIPS", "U.S. Short/Intermediate Taxable",
  #                                    "U.S. Core Taxable")))
  return(acMap %>% filter(bd == toupper(segment)) %>% pull(rt))
}

#' Lookup the Return of an Asset Class
#'
#' @param segment Name of segment in BD style
#' @param taxStatus Taxable or something else
#' @param afterTaxReturns Data frame with rows for segments and columsn for tax status
#'
#' @return numeric value of after tax return
#' @export
#'
lookupAssetClassReturn <- function(segment, taxStatus, afterTaxReturns){
  # mapping schema in Black Diamond to Red Tortoise / RAFI classes.
  # if an asset exists in BD, we need an expected return for it
  taxStatus <- toupper(taxStatus)

  rtClass <- convertBDSegmentToRT(segment) # lookup RT name for segment given BD
  if(is_empty(rtClass)){
    warning(paste("Unrecognized segment:", segment, "Using 0 expected return"))
    out <- 0
    return(out)
  }
  # handle simple cases where we've got a match
  if(!is.na(rtClass)){
    if(toupper(taxStatus) == "TAXABLE") {
      out <- afterTaxReturns %>% filter(classes == rtClass) %>% pull(Taxable)
    } else {
      out <- afterTaxReturns %>% filter(classes == rtClass) %>% pull(Exempt)
    }
    return(out)
  }
  # handle more complex cases (the NAs)
  taxCol <- ifelse(taxStatus == "TAXABLE", "Taxable", "Exempt")
  if(segment == "Conservative"){
    out <- afterTaxReturns[afterTaxReturns$classes=="USLarge", taxCol] * .35 * .6 +
      afterTaxReturns[afterTaxReturns$classes=="EAFEEquity", taxCol] * .35 * .4 +
      afterTaxReturns[afterTaxReturns$classes=="USCoreBonds", taxCol] * .65
    return(out)
  }
  if(segment == "Moderate"){
    out <- afterTaxReturns[afterTaxReturns$classes=="USLarge", taxCol] * .50 * .6 +
      afterTaxReturns[afterTaxReturns$classes=="EAFEEquity", taxCol] * .50 * .4 +
      afterTaxReturns[afterTaxReturns$classes=="USCoreBonds", taxCol] * .50
    return(out)
  }
  if(segment == "Aggressive"){
    out <- afterTaxReturns[afterTaxReturns$classes=="USLarge", taxCol] * .65 * .6 +
      afterTaxReturns[afterTaxReturns$classes=="EAFEEquity", taxCol] * .65 * .4 +
      afterTaxReturns[afterTaxReturns$classes=="USCoreBonds", taxCol] * .35
    return(out)
  }
  if(segment == "Other"){
    warning("Security with segment = 'Other'. Using 0 expected return")
    out <- 0
    return(out)
  }
  if(segment == "To Be Classified"){
    warning("Security with segment = 'To Be Classified'. Using 0 expected return")
    out <- 0
    return(out)
  }
  warning(paste("Unhandled segment:", segment, "Using 0 expected return"))
  out <- 0
  return(out) # shouldn't have gotten here
}

#' Calculate the Returns in the Scenarios
#'
#' @param scenarios data frame with rows segments, columns for scenarios
#' @param portwts each column is the weights in a portfolio
#' @param digits results are rounded to this many digits
#'
#' @return data frame with rows as scenarios and columns as portfolio returns
#' @export
#'
calc_scenario_returns <- function(scenarios, portwts, digits=NULL){
  out <- data.frame(matrix(NA, nrow = ncol(scenarios)-2, ncol = ncol(portwts)))
  for(s in 3:ncol(scenarios)){
    for(p in 1:ncol(portwts)){
      out[s-2,p] <- sum(scenarios[,s] * portwts[,p])
    }
  }
  if(!is.null(digits)) out <- round(out,digits = digits)
  out <- cbind(colnames(scenarios[,3:ncol(scenarios)]), out)
  colnames(out) <- c("Scenario", colnames(portwts))
  return(out)
}

