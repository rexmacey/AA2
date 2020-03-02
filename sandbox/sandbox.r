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
cashBuffers <- readExcelFile(cashBuffersFN)
cma <- createCMA(rafiFN, acNamesFN)

investor<-investor.create(portfolioHoldings,
                          portfolioName,
                          cashBuffers,
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




i2 <- simulateCurrentStrategy(investor, cma)
# Current Portfolio under various scenarios
portwts <- calcAllocationBySegment(investor$lots)[,c(1,3)]
temp <- scenarios %>% select(Segment =rt_class_names) %>% left_join(portwts, by = "Segment") %>%
  replace_na(list(Pct = 0)) %>% select(Pct)

calc_scenario_returns(scenarios, temp, digits = 1)




temp <- c(list(), name="GF")
