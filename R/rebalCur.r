# These are utilities related to estimating the future value of the portfolio as currently allocated

simulateCurrentStrategy <- function(investor, cma){
  # for each year in horizon
  #   calculate value at end of year keeping cash to side in taxable accounts (ATV.taxable1 function)
  #   pay taxes reduce taxable account value (part of ATV.taxable1 function)
  #   rebalance to initial segment wts. No movement between portfolios
  # end for
  # Adjust values for taxes on  final sales
  ac <- cma$acData %>% rename(intOrd = IntOrd, intTE = IntTE, divQual = DivQual,
                              divOrd = DivOrd, turnover = Turnover, expense = Expense, segment = rt_class_names) %>%
    select(yld, growth, valChg, intOrd, intTE, divQual, divOrd, turnover, LTCG, STCG, expense, segment)

  lots <- investor$lots %>%
    select(Ticker, Price, Units, Cost.Basis, Tax.Status, rtSegment, Unit.GL, Account.Number, Security.Type, Market.Value, DoNotSell) %>%
    left_join(ac, by = c("rtSegment" = "segment")) %>% mutate(endCash = 0)

  # taxRates <- investor$taxRates

  targetAllocation <- calcAllocationBySegment(lots)
  for(y in 1:investor$horizon){
    # calc after tax values for each lot after one year of investing
    nLots <- nrow(lots)
    for(i in 1:nLots){
      lots[i,] <- ATV.year1(lots[i,], investor$taxRates, FALSE)
    }
    if(y < investor$horizon){
      # rebalance to targetAllocation except after last year since it's implied everything is sold to realize taxes.
      rebal <- rebalCur(lots, targetAllocation, investor$taxRates$LTCG)
      # apply results of rebal
      # subtract sales from units, recalc cost and market, adj end cash for tax on realized gains
      lots <- lots %>% mutate(Cost.Basis = ifelse(Units == 0, 0, Cost.Basis * (1 - rebal$lpSolve$solution[1:nLots] / Units)),
                            Units = Units - rebal$lpSolve$solution[1:nrow(lots)],
                            Market.Value = Units * Price,
                            endCash = -rebal$lpSolve$solution[1:nrow(lots)]*(lots$Tax.Status=="Taxable")*lots$Unit.GL*investor$taxRates$LTCG)
      # add lots for each buy
      temp <- lots[0,]
      for(i in (nLots+1):rebal$lpSolve$x.count){
        if(rebal$lpSolve$solution[i] > 0){
          ticker = substr(rebal$lpMatrix$colLabels[i], 1, regexpr("_", rebal$lpMatrix$colLabels[i])-1)
          accountNumber <- substr(rebal$lpMatrix$colLabels[i], regexpr("_", rebal$lpMatrix$colLabels[i])+1, nchar(rebal$lpMatrix$colLabels[i]))
          taxStatus <- investor$holdings %>% filter(Account.Number == accountNumber) %>% top_n(1, wt = Market.Value) %>% pull(Tax.Status)
          temp <- temp %>% add_row(Ticker = ticker,
                                   Price = 1,
                                   Units = rebal$lpSolve$solution[i],
                                   Cost.Basis = rebal$lpSolve$solution[i],
                                   Tax.Status = taxStatus,
                                   rtSegment = Ticker,
                                   Unit.GL = 0,
                                   Account.Number = accountNumber,
                                   Market.Value = rebal$lpSolve$solution[i],
                                   DoNotSell = FALSE)
        }
      }
      temp <- temp %>%
        select(Ticker, Price, Units, Cost.Basis, Tax.Status, rtSegment, Unit.GL, Account.Number, Security.Type, Market.Value, DoNotSell) %>%
        left_join(ac, by = c("rtSegment" = "segment")) %>% mutate(endCash = 0)
      lots <- rbind(lots, temp)
    }
  }

  # add lots for each ending cash position
  temp <- lots[0,]
  outWithEndCash <- lots %>% filter(endCash != 0)
  for(i in nrow(outWithEndCash)){
    temp <- temp %>% add_row(Ticker = "USCash",
                             Price = 1,
                             Units = outWithEndCash$endCash,
                             Cost.Basis = outWithEndCash$endCash,
                             Tax.Status = outWithEndCash$Tax.Status,
                             rtSegment = "USCash",
                             Unit.GL = 0,
                             Account.Number = outWithEndCash$Account.Number,
                             Market.Value = outWithEndCash$endCash,
                             DoNotSell = FALSE)
  }
  temp <- temp %>%
    select(Ticker, Price, Units, Cost.Basis, Tax.Status, rtSegment, Unit.GL, Account.Number, Security.Type, Market.Value, DoNotSell) %>%
    left_join(ac, by = c("rtSegment" = "segment")) %>% mutate(endCash = 0)
  lots <- rbind(lots, temp)

  lots$Short.Term.Gain.Loss <- 0
  lots$Long.Term.Gain.Loss <- lots$Market.Value - lots$Cost.Basis
  out<-investor.create(lots,
                             horizon=10,
                             taxrate.state= 0.06,
                             taxrate.ordinc = 0.35,
                             taxrate.LTCG = 0.15,
                             taxrate.STCG = 0.35,
                             taxrate.qualdiv = 0.15,
                             taxrate.surcharge = 0.038,
                             benchmarkType = "US")

  # end of modeling current allocation through time.
  return(out)
}

rebalCur <- function(lots, targetAllocation, LTCG=0){
  out <- list()
  out$lpMatrix <- rebalCurBuildConstraintMatrix(lots, targetAllocation, LTCG)
  if(sum(is.na(out$lpMatrix$mat))>0){
    inds = which(is.na(lpMatrix$mat), arr.ind=TRUE)
    warning(paste(sum(is.na(lpMatrix$mat))), "NAs found in constraint matrix.")
    for(i in 1:nrow(inds)){
      cat(lpMatrix$rowLabels[inds[i,1]], ":", out$lpMatrix$colLabels[inds[i,2]], "\n")
    }
  }
  out$lpObjective <- rebalCurBuildObjectiveCoefficients(lots, targetAllocation)
  out$lpSolve <- lp(direction = "min", objective.in = out$lpObjective, const.mat = out$lpMatrix$mat,
                    const.dir = out$lpMatrix$dir, const.rhs = out$lpMatrix$rhs)
  if(out$lpSolve$status != 0){
    stop("No feasible solution")
  }
  return(out)
}
#' Build constraints related to rebalancing to the current allocation
#'
#' Used to help estimate the future after tax value of the portfolio as currently allocated
#'
#' @param lots current holdings by lot
#' @param targetAllocation target allocation which should be initial allocation
#'
#' @return list with constraint matrix and RHS and dir
#' @export
#'
rebalCurBuildConstraintMatrix <- function(lots, targetAllocation, LTCG){
  rowLabels <- character()
  out <- list()

  temp <- rebalCurBuildSegmentMinMaxConstraints(lots, targetAllocation, LTCG)
  out$mat <- temp$mat
  out$dir <- temp$dir
  out$rhs <- temp$rhs
  rowLabels <-c(rowLabels, paste("Target Allocation:", targetAllocation$Segment))
  # rowLabels <-c(rowLabels, paste("Min:", targetAllocation$Segment))
  # rowLabels <-c(rowLabels, paste("Max:", targetAllocation$Segment))
  out$rowLabels <- rowLabels

  temp <- rebalCurBuildPerAccountConstraints(lots, targetAllocation, LTCG)
  out$mat <- rbind(out$mat, temp$mat)
  out$dir <- c(out$dir, temp$dir)
  out$rhs <- c(out$rhs, temp$rhs)
  rowLabels <- c(rowLabels, paste("AccountCash (=0):", unique(lots$Account.Number)))

  temp <- rebalCurBuildMaxQuantityConstraints(lots, targetAllocation)
  out$mat <- rbind(out$mat, temp$mat)
  out$dir <- c(out$dir, temp$dir)
  out$rhs <- c(out$rhs, temp$rhs)
  rowLabels <- c(rowLabels, paste("Max Quantity Lot", 1:nrow(lots)))

  out$rowLabels <- rowLabels
  out$colLabels <- labelMatrixCol(lots, targetAllocation)
  return(out)
}

#' Build constraints related to targeting segment weights
#'
#' Used to help estimate the future after tax value of the portfolio as currently allocated
#'
#' @param lots current holdings by lot
#' @param targetAllocation target allocation which should be initial allocation
#' @param minTol value allowed below target wt
#' @param maxTol value allowed above target wt
#'
#' @return list with constraint matrix, rhs and dir entries
#' @export
#'
rebalCurBuildSegmentMinMaxConstraints <- function(lots, targetAllocation, LTCG = 0){
  # net sales may not exceed difference between current allocation and minimum allocation
  # net buys may not exceed difference between maximum allocation and current allocation
  # first nSegments are the minimum; next nSegments are the maximums

  # if minCashWt <= strategyCashWt then there's no adjustment.  StrategyCash is sufficient.
  # if minCashWt > strategyCashWt then we need to adjust strategy weights.

  currentAllocation <- calcAllocationBySegment(lots)
  nAccounts <- length(unique(lots$Account.Number))
  nLots <- nrow(lots)
  nSegments <- nrow(targetAllocation)
  portfolioValue <- sum(lots$Market.Value) + sum(lots$endCash)

  out <- list()
  out$mat <- matrix(0, nrow = nSegments,
                    ncol = nrow(lots) + nSegments * nAccounts)
  out$rhs <- numeric(nSegments)
  for(i in 1:nrow(targetAllocation)){
    segmentName <- targetAllocation$Segment[i]
    segmentValue <- currentAllocation %>% filter(Segment == segmentName) %>% pull(Value)
    targetValue <- (targetAllocation %>% filter(Segment == segmentName) %>% pull(Wt)) * portfolioValue
    out$mat[i, ] <- c(-1 * (lots$rtSegment == segmentName) * lots$Price +
                        lots$Unit.GL * LTCG * (lots$Tax.Status == "Taxable") * (lots$rtSegment == segmentName),
                      rep(1 * (targetAllocation$Segment == segmentName), nAccounts))
    out$rhs[i] <- targetValue - segmentValue
  }
  out$dir <- rep("==", nSegments)
  return(out)
}

CopyOfrebalCurBuildSegmentMinMaxConstraints <- function(lots, targetAllocation, minTol = 0.004, maxTol = 0.004){
  # net sales may not exceed difference between current allocation and minimum allocation
  # net buys may not exceed difference between maximum allocation and current allocation
  # first nSegments are the minimum; next nSegments are the maximums

  # if minCashWt <= strategyCashWt then there's no adjustment.  StrategyCash is sufficient.
  # if minCashWt > strategyCashWt then we need to adjust strategy weights.

  currentAllocation <- calcAllocationBySegment(lots)
  nAccounts <- length(unique(lots$Account.Number))
  nLots <- nrow(lots)
  nSegments <- nrow(targetAllocation)
  portfolioValue <- sum(lots$Market.Value)

  out <- list()
  out$mat <- matrix(0, nrow = 2 * nSegments,
                    ncol = nrow(lots) + nSegments * nAccounts)
  out$rhs <- numeric(2 * nSegments)
  for(i in 1:nrow(targetAllocation)){
    segmentName <- targetAllocation$Segment[i]
    segmentValue <- currentAllocation %>% filter(Segment == segmentName) %>% pull(Value)
    targetValue <- targetAllocation %>% filter(Segment == segmentName) %>% pull(Value)
    out$mat[i, ]             <- c(-1 * (lots$rtSegment == segmentName) * lots$Price,
                                  rep(1 * (targetAllocation$Segment == segmentName), nAccounts))

    out$mat[i + nSegments, ] <- c(-1 * (lots$rtSegment == segmentName) * lots$Price,
                                  rep(1 * (targetAllocation$Segment == segmentName), nAccounts))

    out$rhs[i] <- targetValue * (1 - minTol) - segmentValue
    out$rhs[i + nSegments] <- targetValue * (1 + maxTol) - segmentValue
  }
  out$dir <- c(rep(">=", nSegments), rep("<=", nSegments))
  return(out)
}


rebalCurBuildPerAccountConstraints <- function(lots, targetAllocation, LTCG = 0){
  nAccounts <- length(unique(lots$Account.Number))
  nLots <- nrow(lots)
  nSegments <- nrow(targetAllocation)
  out <- list()
  out$mat <- matrix(0, nrow = nAccounts,
                    ncol = nLots + nSegments * nAccounts)
  out$rhs <- rep(0, nAccounts)
  for(i in 1:nAccounts){
    accountNumber <- unique(lots$Account.Number)[i]
    endingCash <- sum(lots %>% filter(Account.Number == accountNumber) %>% pull(endCash))
    out$mat[i, 1:nLots] <- -1 * (lots$Account.Number == accountNumber) * lots$Price +
      lots$Unit.GL * LTCG * (lots$Tax.Status == "Taxable") * (lots$Account.Number == accountNumber)
    out$mat[i, (nLots + (i-1)*nSegments + 1):(nLots + i*nSegments)] <- 1.0
    out$rhs[i] <- endingCash
  }
  out$dir <- rep("==", nAccounts)
  return(out)
}

#' Build objective coefficients
#'
#' Used to help estimate the future after tax value of the portfolio as currently allocated
#' The coefficients aim to first minimize realized gains and second to minimize turnover.
#' To reduce turnover, the coefficient is set to the price of the security held or 1 for purchases.
#' If a security is held in a taxable account the coefficient is increased by 10 x the unrealized gain / loss per unit.
#'
#' @param lots current holdings by lot
#' @param targetAllocation target allocation which should be initial allocation
#'
#' @return vector of objective coefficients
#' @export
#'
rebalCurBuildObjectiveCoefficients <- function(lots, targetAllocation){
  nAccounts <- length(unique(lots$Account.Number))
  nLots <- nrow(lots)
  nSegments <- nrow(targetAllocation)
  out <- rep(1, nLots + nSegments * nAccounts)
  out[1:nLots] <- (lots$Tax.Status == "Taxable") * lots$Unit.GL * 10 + lots$Price
  return(out)
}

rebalCurBuildMaxQuantityConstraints <- function(lots, targetAllocation){
  nAccounts <- length(unique(lots$Account.Number))
  nLots <- nrow(lots)
  nSegments <- nrow(targetAllocation)
  out <- list()
  out$mat <- matrix(0, nrow = nLots,
                    ncol = nLots + nSegments * nAccounts)
  out$mat[1:nLots, 1:nLots] <- diag(nLots)
  out$dir <- rep("<=", nLots)
  out$rhs <- lots$Units * as.numeric(!lots$DoNotSell) # account for Do not sell
  return(out)
}

#' Calculate after-tax portfolio values over a one year period for assets in a taxable account
#'
#' @param x a tibble with price, units, Cost.Basis, taxStatus, segment, yld, growth, valChg, intOrd, intTE, divQual, divOrd, turnover, LTCG, STCG, expense,  endCash
#' @param taxRates list with following OrdInc, LTCG, STCG, QualDiv, RetirementOrdInc
#' @param lastYear boolean. If TRUE, the investment is sold and taxes are paid on unrealized gains
#'
#' @return a tibble just like x. price, units, yld, endCash are updated. Other values remain.
#' @export
#'
ATV.taxable1 <- function(x, taxRates, lastYear = FALSE){
  out <- x
  v <- x$Price * x$Units # value
  b <- x$Cost.Basis
  div <- x$Price * x$yld
  yield <- x$yld
  # similar to AT return calc, this is one year in loop ----
  div <- div * (1 + x$growth) # Dividends per share
  price.prev <- x$Price
  if (x$yld == 0 | div == 0) {
    out$Price <- x$Price * (x$valChg + 1)
  } else{
    out$Price <- x$Price* (x$valChg + 1) * (1 + x$growth) # (1+growth)^t * (Pbeg/Divbeg)*Divend
  }
  income <- v * x$yld # income for the year pretax
  unrealgl <-
    x$Units * (out$Price - x$Price) # previous shares * current price - previous value
  taxIncome <-
    income * (x$intOrd * taxRates$OrdInc + x$divQual * taxRates$QualDiv + x$divOrd * taxRates$OrdInc)
  valuesold <- (v + unrealgl) * x$turnover
  basissold <- b * x$turnover
  realgl <- valuesold - basissold
  taxCG <- realgl * (x$LTCG * taxRates$LTCG + x$STCG * taxRates$STCG)
  out$yld <- div /out$Price
  out$endCash <- x$endCash + valuesold + income - taxIncome - taxCG
  out$Cost.Basis <- x$Cost.Basis - basissold
  out$Units <- x$Units - valuesold /out$Price
  if(lastYear){ # sell all
    taxCG <- (out$Price * out$Units - out$Cost.Basis) * (x$LTCG * taxRates$LTCG + x$STCG * taxRates$STCG)
    out$endCash <- out$endCash + out$Price * out$Units - taxCG
    out$Units <- 0
    out$Cost.Basis <- 0
  }
  out$Market.Value <- out$Units * out$Price
  return(out)
}

#' Calculate after-tax portfolio values over a one year period for assets in a non-taxable account
#'
#' @param x a tibble with price, units, Cost.Basis, taxStatus, segment, yld, growth, valChg, intOrd, intTE, divQual, divOrd, turnover, LTCG, STCG, expense,  endCash
#' @param taxRates list with following OrdInc, LTCG, STCG, QualDiv, RetirementOrdInc
#' @param lastYear boolean. If TRUE, the investment is sold and taxes are paid on unrealized gains
#'
#' @return a tibble just like x. price, units, yld, endCash are updated. Other values remain.
#' @export
#'
#' @examples
ATV.nonTaxable1 <- function(x, taxRates, lastYear = FALSE){
  out <- x
  v <- x$Price * x$Units # value
  div <- x$Price * x$yld
  yield <- x$yld
  # similar to AT return calc, this is one year in loop ----
  div <- div * (1 + x$growth) # Dividends per share
  price.prev <- x$Price
  if (x$yld == 0 | div == 0) {
    out$Price <- x$Price * (x$valChg + 1)
  } else{
    out$Price <- x$Price* (x$valChg + 1) * (1 + x$growth) # (1+growth)^t * (Pbeg/Divbeg)*Divend
  }
  income <- v * x$yld # income for the year pretax
  taxIncome <- 0
  out$yld <- div /out$Price
  out$endCash <- x$endCash + income
  out$Units <- x$Units
  if(lastYear){
    out$endCash <- out$endCash + out$Price * out$Units
    out$Units <- 0
    out$Cost.Basis <- 0
    if(x$Tax.Status == "Tax Deferred"){
      tax <- (out$Price * out$Units) * taxRates$RetirementOrdInc
      out$endCash <- out$endCash - tax
    }
  }
  out$Market.Value <- out$Units * out$Price
  return(out)
}

#' Calculate after-tax portfolio values over a one year period
#'
#' @param x a tibble with price, units, Cost.Basis, taxStatus, segment, yld, growth, valChg, intOrd, intTE, divQual, divOrd, turnover, LTCG, STCG, expense,  endCash
#' @param taxRates list with following OrdInc, LTCG, STCG, QualDiv, RetirementOrdInc
#' @param lastYear boolean. If TRUE, the investment is sold and taxes are paid on unrealized gains
#'
#' @return a tibble just like x. price, units, yld, endCash are updated. Other values remain.
#' @export
#'
#' @examples
ATV.year1 <- function(x, taxRates, lastYear = FALSE){
  if(x$Tax.Status == "Taxable"){
    out <- ATV.taxable1(x, taxRates, lastYear)
  } else {
    out <- ATV.nonTaxable1(x, taxRates, lastYear)
  }
  return(out)
}

#' Create labels for optimization matrix
#'
#' @param lots current holdings by lot
#' @param targetAllocation target allocation which should be initial allocation
#'
#' @return character vector with labels for variables
#' @export
#'
labelMatrixCol <- function(lots, targetAllocation){
  nAccounts <- length(unique(lots$Account.Number))
  nLots <- nrow(lots)
  nSegments <- nrow(targetAllocation)
  out <- paste0("lot_", seq(1, nLots))
  for(i in 1:nAccounts){
    accountNumber <- unique(lots$Account.Number)[i]
    for(j in 1:nSegments){
      out <- c(out, paste0(targetAllocation$Segment[j], "_", accountNumber))
    }
  }
  return(out)
}
