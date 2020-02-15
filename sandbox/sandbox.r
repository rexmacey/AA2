# Read RAFI file

rafiFN <- paste0("tests/testthat/RAFI 201912 w Muni.xlsx")
acNamesFN <- paste0("tests/testthat/acname_US_TaxSens.xlsx")

createCMA <- function(rafiFN, acNamesFN){}
  data("rafi_xlranges")
  out <- list()
  rafiData <- readRAFIFile(rafiFN, rafi_xlranges)
  out$inflationRate <- as.numeric(rafiData$ret[1, "Expected.Return..Nominal."] -
    rafiData$ret[1, "Expected.Return..Real."])
  acNames <- readACNameFile(acNamesFN)
  # make sure acNames and rafiData$ret are in same order
  acNames <- acNames[match(rafiData$ret$Asset.Class, acNames$rafi_ret_class_names),]

  firstConstraintCol<-match("Max",colnames(acNames))+1 # Constraints begin after Max Col
  # todo you are here
  # line 290 : rafi.cma.v2 : geom.ret in rafi.r in TaxAwareAA
  # consider rewrite using dplyr style
  # geomRet <- rafiData$ret$Expected.Return..Nominal. -
  #            acNames$Expense
  # arithRet <- geomRet + (rafiData$ret$Volatility^2 / 2)
  # yield <- rafiData$ret$Average.Net.Yield
  # idx <- rafiData$ret$Average.Net.Yield <= 0
  # yield[idx] <- yield[idx] + out$inflationRate
  # growth <- rafiData$ret$Capital.Growth
  # growth[!idx] <- growth[!idx] + out$inflationRate
  # valChange <- geomRet - yield - growth
  # acData <- data.frame(ret = arithRet,
  #                      geomRet = geomRet,
  #                      yld = yield,
  #                      growth=growth,
  #                      valChg = valChange,
  #                      risk = rafiData$ret$Volatility)
  idx <- rafiData$ret$Average.Net.Yield <= 0
  out$acData <- rafiData$ret %>%
    mutate(expense = acNames$Expense) %>%
    mutate(geomRet = Expected.Return..Nominal. - expense,
            ret = geomRet + Volatility^2/2,
           yld = Average.Net.Yield,
           growth = Capital.Growth) %>%
    mutate(yld = ifelse(idx, yld + out$inflationRate, yld),
           growth = ifelse(!idx, growth + out$inflationRate, growth),
           valChg = geomRet - yld - growth,
           risk = Volatility) %>%
    select(Asset.Class, ret, geomRet, yld, growth, valChg, risk)

