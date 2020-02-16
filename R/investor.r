#' Create Investor
#'
#' This function creates an investor object.
#'
#' @param portfolioHoldings tibble with portfolio holdings
#' @param horizon Time horizon in years. Default is 10.
#' @param taxrate.state State income tax rate. Default is 0.
#' @param taxrate.ordinc Federal tax rate on ordinary income. Default is 0.
#' @param taxrate.LTCG Federal tax rate on long-term capital gains. Default is
#'   0.
#' @param taxrate.STCG Federal tax rate on short-term capital gains. Default is
#'   0.
#' @param taxrate.qualdiv Federal tax rate qualified dividends. Default is 0.
#' @param taxrate.surcharge Net investment income tax rate (section 1411 of
#'   IRC). Default is 0.038.
#' @param taxrate.retirement.ordinc Federal tax rate on ordinary income in retirment. If NULL (default) uses the taxrate.ordinc value.
#' @param benchmarkType either "US" (default) for a U.S. centric investor, or "Global"
#' @keywords asset allocation efficient frontier
#' @export
#' @return An investor class object which is a list containing items related to
#'   the investor. This package internally calculate income and capital gain
#'   rates that combine federal, state and surcharge taxes.
#'
investor.create<-function(portfolioHoldings,
                          horizon = 10,
                          taxrate.state = 0,
                          taxrate.ordinc = 0,
                          taxrate.LTCG = 0,
                          taxrate.STCG = 0,
                          taxrate.qualdiv = 0,
                          taxrate.surcharge = 0.038,
                          taxrate.retirement.ordinc = NULL,
                          benchmarkType = "US"){

    taxrates<-list(OrdInc = taxrate.ordinc + taxrate.state + taxrate.surcharge,
                   LTCG = taxrate.LTCG + taxrate.state + taxrate.surcharge,
                   STCG = taxrate.STCG + taxrate.state + taxrate.surcharge,
                   QualDiv = taxrate.qualdiv + taxrate.state + taxrate.surcharge,
                   RetirementOrdInc = ifelse(is.null(taxrate.retirement.ordinc),
                                             taxrate.ordinc + taxrate.state + taxrate.surcharge,
                                             taxrate.retirement.ordinc + taxrate.state + taxrate.surcharge),
                   raw = list(fedOrdInc = taxrate.ordinc,
                              stateOrdInc = taxrate.state,
                              fedLTCG = taxrate.LTCG,
                              fedSTCG = taxrate.STCG,
                              fedQualDiv = taxrate.qualdiv,
                              surcharge = taxrate.surcharge,
                              fedRetirementOrdInc = taxrate.retirement.ordinc))
    unrealizedGL <- portfolioHoldings %>% filter(Tax.Status == "Taxable") %>%
        mutate(Total.Gain.Loss = Short.Term.Gain.Loss + Long.Term.Gain.Loss) %>%
        summarize(Amount = sum(Total.Gain.Loss)) %>% pull(Amount)

    initialValues <- data.frame(Account = c("Taxable", "Deferred", "Exempt", "Total"),
                                PreTaxValue = numeric(4),
                                PreTaxPct = numeric(4),
                                AfterTaxValue = numeric(4),
                                AfterTaxPct = numeric(4))
    initialValues$PreTaxValue <- c(taxed = as.numeric(portfolioHoldings %>% filter(Tax.Status == "Taxable") %>% summarize(value = sum(Market.Value))),
                       deferred = as.numeric(portfolioHoldings %>% filter(Tax.Status == "Tax Deferred") %>% summarize(value = sum(Market.Value))),
                          exempt = as.numeric(portfolioHoldings %>% filter(Tax.Status == "Non Taxable") %>% summarize(value = sum(Market.Value))),
                          totalassets = sum(portfolioHoldings$Market.Value))

    initialValues$PreTaxPct <- initialValues$PreTaxValue / initialValues$PreTaxValue[4]
    # intentionally taxing all unrealized gains at LTCG rate
    initialValues$AfterTaxValue[1] <- initialValues$PreTaxValue[1] -
        unrealizedGL * taxrates$LTCG
    initialValues$AfterTaxValue[2] <- initialValues$PreTaxValue[2]*(1-taxrates$OrdInc)
    initialValues$AfterTaxValue[3] <- initialValues$PreTaxValue[3]
    initialValues$AfterTaxValue[4] <- sum(initialValues$AfterTaxValue[1:3])
    initialValues$AfterTaxPct <- initialValues$AfterTaxValue / initialValues$AfterTaxValue[4]

    investor <- list(initialValues = initialValues,
                  taxRates = taxrates,
                  unrealizedGL= unrealizedGL,
                  horizon = horizon,
                  accountNumbers = unique(portfolioHoldings$Account.Number),
                  nAccounts = length(unique(portfolioHoldings$Account.Number)),
                  holdings = portfolioHoldings,
                  benchmarkType = benchmarkType)
    class(investor)<-"investor"
    return(investor)
}

#' Print investor
#'
#' @param x Investor
#' @param ... Additional print parameters
#'
#' @return Prints
#' @export
#'
#'
print.investor<-function(x, ...){
    av<-x$initialValues
    av$PreTaxPct<-round(100*av$PreTaxPct,2)
    av$AfterTaxPct<-round(100*av$AfterTaxPct,2)
    tax<-data.frame(Tax=unlist(strsplit("Ordinary Income, LT Cap Gain, ST Cap Gain, Qual Div, State",split = ",")),
                    Rate=c(x$taxRates$OrdInc, x$taxRates$LTCG, x$taxRates$STCG,
                           x$taxRates$QualDiv, x$taxRates$raw$stateOrdInc))
    rownames(tax)<-NULL
    av$Account<-format(av$Account,justify="left")
    av$PreTaxValue<-prettyNum(format(round(av$PreTaxValue,0), scientific = FALSE),big.mark = ",")
    av$AfterTaxValue<-prettyNum(format(round(av$AfterTaxValue,0), scientific = FALSE),big.mark = ",")
    tax$Tax<-format(tax$Tax,justify="left")
    tax$Rate<-prettyNum(tax$Rate*100,format="f",digits=4,nsmall=2)
    print(av,row.names=FALSE)
    cat("\n")
    if(x$initialValues$PreTaxValue[1] > 0){ # value in taxable account so print this
        cat("Net unrealized gains in taxable accounts ",
            prettyNum(format(round(x$unrealizedGL, 0), scientific = FALSE),big.mark = ","),
            "\n\n")
    }
    print(tax,row.names=FALSE)
    cat("\n")
    cat(paste("Time horizon is ",x["horizon"],"years."))
    cat("\n")
}

# Older Version using named vector
# investor.create<-function(portfolioHoldings,
#                           horizon = 10,
#                           taxrate.state = 0,
#                           taxrate.ordinc = 0,
#                           taxrate.LTCG = 0,
#                           taxrate.STCG = 0,
#                           taxrate.qualdiv = 0,
#                           taxrate.surcharge = 0.038,
#                           taxrate.retirement.ordinc = NULL){
#
#     investor.values<-c(taxed = as.numeric(portfolioHoldings %>% filter(Tax.Status == "Taxable") %>% summarize(value = sum(Market.Value))),
#                        deferred = as.numeric(portfolioHoldings %>% filter(Tax.Status == "Tax Deferred") %>% summarize(value = sum(Market.Value))),
#                        exempt = as.numeric(portfolioHoldings %>% filter(Tax.Status == "Non Taxable") %>% summarize(value = sum(Market.Value))),
#                        totalassets = sum(portfolioHoldings$Market.Value))
#
#     pretax.pctwts <- investor.values / investor.values["totalassets"]
#     names(pretax.pctwts) <- c("taxed.pct", "deferred.pct", "exempt.pct", "totalassets.pct")
#
#     taxrates<-c(OrdInc = taxrate.ordinc + taxrate.state + taxrate.surcharge,
#                 LTCG = taxrate.LTCG + taxrate.state + taxrate.surcharge,
#                 STCG = taxrate.STCG + taxrate.state + taxrate.surcharge,
#                 QualDiv = taxrate.qualdiv + taxrate.state + taxrate.surcharge,
#                 taxRState = taxrate.state,
#                 RetirementOrdInc = ifelse(is.null(taxrate.retirement.ordinc),
#                                           taxrate.ordinc + taxrate.state + taxrate.surcharge,
#                                           taxrate.retirement.ordinc + taxrate.state + taxrate.surcharge))
#
#
#     investor.values.at <- c(taxed.at = 0,
#                             deferred.at = as.numeric(investor.values["deferred"]*(1-taxrates["OrdInc"])),
#                             exempt.at = as.numeric(investor.values["exempt"]),
#                             totalassets.at = 0)
#     investor.values.at["taxed.at"] <- investor.values["taxed"] -
#         (sum(portfolioHoldings$Long.Term.Gain.Loss) + sum(portfolioHoldings$Short.Term.Gain.Loss)) *
#         taxrates["LTCG"]
#
#     # intentionally taxing all unrealized gains at LTCG rate
#     investor.values.at["totalassets.at"] <- investor.values.at["taxed.at"] +
#         investor.values.at["deferred.at"] +
#         investor.values.at["exempt.at"]
#     investor <- c(investor.values, investor.values.at, pretax.pctwts,
#                   taxrates, horizon = horizon)
#     class(investor)<-"investor"
#     return(investor)
# }
# print.investor<-function(x, ...){
#     av<-data.frame(Account=c("Taxable", "Deferred", "Exempt", "Total"),
#                    "PreTaxValue"=c(x["taxed"], x["deferred"], x["exempt"], x["totalassets"]),
#                    "AfterTaxValue"=c(x["taxed.at"], x["deferred.at"], x["exempt.at"], x["totalassets.at"]),
#                    "PreTaxPct"=c(x["taxed.pct"], x["deferred.pct"], x["exempt.pct"], x["totalassets.pct"]))
#     av$PreTaxPct<-round(100*av$PreTaxPct,2)
#     tax<-data.frame(Tax=unlist(strsplit("Ordinary Income, LT Cap Gain, ST Cap Gain, Qual Div, State",split = ",")),
#                     Rate=c(x["OrdInc"],x["LTCG"],x["STCG"],x["QualDiv"],x["taxRState"]))
#     rownames(tax)<-NULL
#     av$Account<-format(av$Account,justify="left")
#     av$PreTaxValue<-prettyNum(format(round(av$PreTaxValue,0), scientific = FALSE),big.mark = ",")
#     av$AfterTaxValue<-prettyNum(format(round(av$AfterTaxValue,0), scientific = FALSE),big.mark = ",")
#     tax$Tax<-format(tax$Tax,justify="left")
#     tax$Rate<-prettyNum(tax$Rate*100,format="f",digits=4,nsmall=2)
#     print(av,row.names=FALSE)
#     cat("\n")
#     print(tax,row.names=FALSE)
#     cat("\n")
#     cat(paste("Time horizon is ",x["horizon"],"years."))
#     cat("\n")
# }
