#' Create Investor
#'
#' This function creates an investor object.
#'
#' @param portfolioHoldings tibble with portfolio holdings
#' @param portfolioName name of portfolio from BD
#' @param cashBuffers tibble of portoflio cash buffers read in from BD
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
                          portfolioName = "",
                          cashBuffers = NULL,
                          horizon = 10,
                          taxrate.state = 0,
                          taxrate.ordinc = 0,
                          taxrate.LTCG = 0,
                          taxrate.STCG = 0,
                          taxrate.qualdiv = 0,
                          taxrate.surcharge = 0.038,
                          taxrate.retirement.ordinc = NULL,
                          benchmarkType = "US"){
    out <- addItem(name = "portfolioName", item = portfolioName) %>%
        addItem(name = "holdings", item = portfolioHoldings) %>%
        addItem(name = "lots", item = createLots(portfolioHoldings)) %>%
        addItem(name = "cashBuffers", item = cashBuffers) %>%
        addItem(name = "benchmarkType", item = benchmarkType) %>%
        addItem(name = "taxRates", item = combineTaxRates(taxrate.state= 0.06,
                                                          taxrate.ordinc = 0.35,
                                                          taxrate.LTCG = 0.15,
                                                          taxrate.STCG = 0.35,
                                                          taxrate.qualdiv = 0.15,
                                                          taxrate.surcharge = 0.038)) %>%
        addItem(name = "unrealizedGL", item = calcUnrealizedGL(portfolioHoldings)) %>%
        addItem(name = "initialValues", item = calcValuesByTaxStatus(.)) %>%
        addItem(name = "horizon", item = horizon) %>%
        addItem(name = 'accountNumbers', item = unique(portfolioHoldings$Account.Number)) %>%
        addItem(name = "nAccounts", item =  length(unique(portfolioHoldings$Account.Number))) %>%
        addItem(name = "minCashWt", item = ifelse(is.null(cashBuffers), 0, calcMinCashWt(.)))
    return(out)
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
    tax<-data.frame(Tax=unlist(strsplit("Ordinary Income,LT Cap Gain,ST Cap Gain,Qual Div,State,Retirement Ordinary Income",split = ",")),
                    Rate=c(x$taxRates$OrdInc, x$taxRates$LTCG, x$taxRates$STCG,
                           x$taxRates$QualDiv, x$taxRates$raw$stateOrdInc, x$taxRates$RetirementOrdInc))
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

#' Calculate Minimum Portfolio Weight
#'
#' calculates the minimum wt (percent) the entire portfolio must have in cash by looking at minimums across all accounts
#'
#' @param investor A list with holdings and cash buffers
#'
#' @return numeric value decimal weight of portfolio required to be in cash
#' @export
#'
calcMinCashWt <- function(investor){
    # calculates the minimum wt (percent) the entire portfolio must have in cash by looking at minimums across all accounts
    out <-
        left_join(
            investor$holdings %>% group_by(Account.Number) %>% summarize(MarketValue = sum(Market.Value)) %>%
                mutate(Account.Number = as.character(Account.Number)),
            cashBuffers,
            by = "Account.Number"
        ) %>%
        mutate(minCashP = MarketValue * Percentage.Cash.Buffers..Blue.Sky. / 100) %>%
        mutate(minCash = pmax(minCashP, Dollar.Value.Cash.Buffers..Blue.Sky.)) %>%
        summarize(minCash = sum(minCash))  %>% pull(minCash)
    out <-
        out / investor$initialValues %>% filter(Account == "Total") %>% pull(PreTaxValue)
    return(out)
}

#' Combine Tax Rates
#'
#' Takes federal and state rates for an individual and combines them.
#'
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
#' @export
#' @return An list of combined and raw tax rates
#'
combineTaxRates <- function(taxrate.state = 0,
                          taxrate.ordinc = 0,
                          taxrate.LTCG = 0,
                          taxrate.STCG = 0,
                          taxrate.qualdiv = 0,
                          taxrate.surcharge = 0.038,
                          taxrate.retirement.ordinc = NULL){
    return(list(OrdInc = taxrate.ordinc + taxrate.state + taxrate.surcharge,
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
                              fedRetirementOrdInc = taxrate.retirement.ordinc)))
}

#' Calculate Unrealized Gains and Losses
#'
#' @param portfolioHoldings holdings for a portfolio
#'
#' @return scalar value
#' @export
#'
calcUnrealizedGL <- function(portfolioHoldings){
    return(portfolioHoldings %>% filter(Tax.Status == "Taxable") %>%
        mutate(Total.Gain.Loss = Short.Term.Gain.Loss + Long.Term.Gain.Loss) %>%
        summarize(Amount = sum(Total.Gain.Loss)) %>% pull(Amount))
}

#' Create Table of Portfolio Values by Tax Status
#'
#' The table has rows for Taxable, Deferred, Exempt and Total and columns for Pretax and AfterTax values and percent.
#'
#' @param investor list with holdings item.
#'
#' @return data frame with values
#' @export
#'
calcValuesByTaxStatus <- function(investor){
    portfolioHoldings <- investor$holdings
    taxrates <- investor$taxRates
    unrealizedGL <- investor$unrealizedGL
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
    # intentionally taxing all unrealized gains at LTCG rate, use retirement rate for deferred account
    initialValues$AfterTaxValue[1] <- initialValues$PreTaxValue[1] -
        unrealizedGL * taxrates$LTCG
    initialValues$AfterTaxValue[2] <- initialValues$PreTaxValue[2]*(1-taxrates$RetirementOrdInc)
    initialValues$AfterTaxValue[3] <- initialValues$PreTaxValue[3]
    initialValues$AfterTaxValue[4] <- sum(initialValues$AfterTaxValue[1:3])
    initialValues$AfterTaxPct <- initialValues$AfterTaxValue / initialValues$AfterTaxValue[4]
    return(initialValues)
}

#' Create Tibble of Lots
#'
#' Useful for working with tax lots
#'
#' @param holdings
#'
#' @return Tibble of lots
#' @export
#'
createLots <- function(holdings){
    out <- holdings %>%
        mutate(Open.Date = as.Date(Open.Date, "%m/%d/%Y")) %>%
        rename(DoNotSell = Do.Not.Sell..Blue.Sky.) %>%
        mutate(Account.Number = factor(Account.Number),
               Segment = factor(Segment),
               Tax.Status = factor(Tax.Status),
               Unit.GL = (Market.Value - Cost.Basis) / Units,
               Price = Market.Value / Units) %>%
        mutate(isLT = ifelse(Unit.GL == 0, TRUE, (Sys.Date() - Open.Date) > 365)) %>%
        mutate(isLT = ifelse(is.na(isLT), TRUE, isLT)) %>%
        mutate(rtSegment = sapply(Segment, convertBDSegmentToRT)) %>%
        select(Ticker, Tax.Status, Units, Price, Unit.GL, isLT, Segment, rtSegment, Account.Number, Open.Date,
               DoNotSell, Class, Security.Type, CUSIP, Market.Value, Cost.Basis, Unit.Cost) %>% arrange(Account.Number, Segment)
    return(out)
}
