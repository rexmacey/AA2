# Read RAFI file
library(devtools)
library(roxygen2)
library(tidyverse)
setwd("~/AA2")
devtools::load_all()
source("sandbox/setInputs.r")

portfolioHoldings <- readPortfolioHoldings(portfolioName, portfolioHoldingsFN)
portfolioHoldingsALL <- readPortfolioHoldings(portfolioName = NULL, portfolioHoldingsFN)
scenarios <- readExcelFile(scenariosFN, sheet=scenarios_sheet)
benchmarkDefinitions <- readExcelFile(benchmarkDefinitionFN)

cma <- createCMA(rafiFN, acNamesFN)


investor<-investor.create(portfolioHoldings,
                          horizon=10,
                          taxrate.state= 0.06,
                          taxrate.ordinc = 0.35,
                          taxrate.LTCG = 0.15,
                          taxrate.STCG = 0.35,
                          taxrate.qualdiv = 0.15,
                          taxrate.surcharge = 0.038,
                          benchmarkType = "US")

print(investor)

afterTaxReturns <- ATReturn.calc(cma, investor)
benchmark.txt <- as.character(benchmarkDefinitions[3, "Benchmark"])
benchmarkWts <- createBenchmarkWts(benchmark.txt, investor)
benchmarkWtsList <- lapply(benchmarkDefinitions %>% filter(Group == investor$benchmarkType) %>% pull(Benchmark),
                       createBenchmarkWts, investor)

# lookup RT class from BD segment  how is it done in the rebalancer?
# todo how much will current portfolio value be worth in 10 years
# what are scenario risks of current portfolio?

# The following is a "buy and hold" of current holdings.  No rebalancing
holdingsByTaxStatusAndSegment <- investor$holdings %>% group_by(Tax.Status, Segment) %>% summarize(value = sum(Market.Value))
holdingsByTaxStatusAndSegment <- cbind(holdingsByTaxStatusAndSegment,
                                       geomRet = apply(holdingsByTaxStatusAndSegment, 1, function(x, y) {
                                                 lookupAssetClassReturn(x["Segment"], x["Tax.Status"], y)
}, y=afterTaxReturns$geomTable)) %>% mutate(growthFactor = (1+geomRet)^investor$horizon * ifelse(Tax.Status == "Tax Deferred", 1 - investor$taxRates$RetirementOrdInc, 1),
                 valueAT = value * growthFactor) %>% ungroup()
head(holdingsByTaxStatusAndSegment)

investor$lots <- investor$holdings %>%
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



# for each year in horizon
#   calculate value at end of year keeping cash to side in taxable accounts (ATV.taxable1 function)
#   pay taxes reduce taxable account value (part of ATV.taxable1 function)
#   rebalance to initial segment wts. No movement between portfolios
# end for
# Adjust values for taxes on  final sales
ac <- cma$acData %>% rename(intOrd = IntOrd, intTE = IntTE, divQual = DivQual,
                             divOrd = DivOrd, turnover = Turnover, expense = Expense, segment = rt_class_names) %>%
  select(yld, growth, valChg, intOrd, intTE, divQual, divOrd, turnover, LTCG, STCG, expense, segment)

test <- investor$lots %>%
  select(Ticker, Price, Units, Cost.Basis, Tax.Status, rtSegment, Unit.GL, Account.Number, Security.Type, Market.Value) %>%
  left_join(ac, by = c("rtSegment" = "segment")) %>% mutate(endCash = 0)


taxRates <- investor$taxRates

test1 <- test[1,] %>% mutate(price = 1, units = 0, costBasis=0)

out <- test
targetAllocation <- calcAllocationBySegment(investor$lots)
for(y in 1:investor$horizon){
  for(i in 1:nrow(out)){
    out[i,] <- ATV.year1(out[i,], investor$taxRates, y == investor$horizon)
  }
  # rebalance here
}

rebal <- rebalCur(test, targetAllocation)
# Current Portfolio under various scenarios
portwts <- calcAllocationBySegment(investor$lots)[,c(1,3)]
temp <- scenarios %>% select(Segment =rt_class_names) %>% left_join(portwts, by = "Segment") %>%
  replace_na(list(Pct = 0)) %>% select(Pct)

calc_scenario_returns(scenarios, temp, digits = 1)
