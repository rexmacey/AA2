#' Create CMA (Capital Market Assumptions)
#'
#' This function creates a cma object.
#'
#' @param rafiFN name including path of file with RAFI asset allocation assumptions
#' @param acNamesFN name including path of file with asset class characteristics
#' @param rafi_xlranges list of four ranges: return, corr, cov, and date
#'
#' @return a cma object
#' @export
#'
createCMA <- function(rafiFN, acNamesFN, rafi_xlranges = NULL){
  out <- list()
  rafiData <- readRAFIFile(rafiFN, rafi_xlranges)
  out$inflationRate <- as.numeric(rafiData$ret[1, "Expected.Return..Nominal."] -
                                    rafiData$ret[1, "Expected.Return..Real."])
  acNames <- readACNameFile(acNamesFN)
  # make sure acNames and rafiData$ret are in same order
  acNames <- acNames[match(rafiData$ret$Asset.Class, acNames$rafi_ret_class_names),]
  firstConstraintCol<-match("Max",colnames(acNames))+1 # Constraints begin after Max Col
  out$nconstraints <- ncol(acNames) - firstConstraintCol + 1

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
    select(Asset.Class, ret, geomRet, yld, growth, valChg, risk) %>%
    left_join(acNames, by = c("Asset.Class" = "rafi_ret_class_names")) %>%
    filter(Max != 0)
  out$asOfDate <- rafiData$as_of_date
  out$classes <- out$acData$rt_class_names
  out$nClasses <- length(out$acData$rt_class_names)
  out$ret <- out$acData$ret
  idx <- match(out$acData$rafi_corr_class_names, rafiData$corr$Asset.Class)
  out$corr <- as.matrix(rafiData$corr[idx, 1+idx]     )
  rownames(out$corr) <- colnames(rafiData$corr[1+idx])
  out$cov <- as.matrix(rafiData$cov[idx, 1+idx]     )
  rownames(out$cov) <- colnames(rafiData$cov[1+idx])
  if (!matrixcalc::is.positive.semi.definite(out$cov)) {
    out$cov <- Matrix::nearPD(out$cov, corr = FALSE, keepDiag = TRUE,
                              maxit = 1000)$mat
  }
  class(out) <- "cma"
  return(out)
}

#' Print cma object
#'
#' @param cma cma object
#'
#' @return NULL
#' @export
#'
print.cma<-function(cma, ...){
  cat("As of",cma$asOfDate,"\n" )
  cat("Number of asset classes",cma$nClasses,"\n\n")
  #temp<-cma$ac.data[,c("LongName","AssetClass","geom.ret","risk")]
  #temp<-cbind(cma$acData[,c("LongName","Asset.Class","geomRet","risk")],row.names(cma$acData))
  temp<-cma$acData[,c("LongName","Asset.Class","geomRet","risk", "rt_class_names")]
  temp[,3:4]<-round(100*temp[,3:4],1)
  colnames(temp)<-c("Asset Class","Broad Class","Return%","Risk%","Abbrev")
  temp<-temp[order(temp$'Risk%',temp$'Asset Class'),]
  print(temp, right=FALSE, row.names=FALSE)
  cat("\nInflation ",round(100*cma$inflationRate,1),"% \n",sep="" )
  plot((cma$acData %>% pull(risk))*100, (cma$acData %>% pull(geomRet))*100,
       main="Asset Class Assumptions", xlab="Std Dev %", ylab="Return %",col="blue",pch=16)
  loc<-100*cma$acData[,c("risk","geomRet")]
  loc[,2]<-loc[,2]-0.25 #move down a bit
  text(loc,cma$classes)
  abline(h=100*cma$inflationRate,col="red")
  invisible(NULL)
}

