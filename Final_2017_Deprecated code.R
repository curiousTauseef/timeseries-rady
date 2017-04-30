install.packages(c("dplyr", "xts", "rugarch", "forecast", "car"),
                  repos = 'http://cran.us.r-project.org')

require(dplyr)
require(xts)
require(rugarch)
require(forecast)
require(car)

SOSAFIC <- function(realized.measure, model.forecasts,
                    bench.forecast, model.names){
  
  # "LR" - linear regression coefficients,
  # "MZ" - Minzer-Zarnowiz test,
  # "LF" - MSE, QLIKE loss functions,
  # "DM" - Debold-Mariano test
  
  
  n.row = nrow(model.forecasts)
  n.col = ncol(model.forecasts)
  
  mse.model = matrix(ncol = n.col+1, nrow = n.row)
  qlike.model = matrix(ncol = n.col+1, nrow = n.row)
  loss = matrix(nrow = 2, ncol = n.col+1)
  
  
  colnames(loss) <- c(model.names, "bench")
  rownames(loss) <- c("MSE", "QLIKE")
  
  tmp.forecasts = cbind(model.forecasts, bench.forecast)
  
  ## LF for models
  for(i in 1:(n.col+1)){
    mse.model[,i] = (realized.measure - tmp.forecasts[,i])^2
    qlike.model[,i] = log(tmp.forecasts[,i]) +
      realized.measure/tmp.forecasts[,i]
    
    loss["MSE", i] = mean(mse.model[,i])
    loss["QLIKE", i] = mean(qlike.model[,i])
  }
  
  ## DM-test
  dm = matrix(nrow = 2, ncol = n.col)
  colnames(dm) <- model.names
  rownames(dm) <- c("MSE", "QLIKE")
  for(i in 1:n.col){
    dm["MSE",i] = dm.test(e1 = mse.model[,i],
                          e2 = mse.model[,n.col+1],
                          #e2 = mse.bench,
                          alternative = "two.sided",
                          h = 1,
                          power = 1)["statistic"]%>%as.numeric()
    dm["QLIKE",i] = dm.test(e1 = qlike.model[,i],
                            e2 = qlike.model[,n.col+1],
                            #e2 = qlike.bench,
                            alternative = "two.sided",
                            h = 1,
                            power = 1)["statistic"]%>%as.numeric()
  }
  
  ## Mincer-Zarnowitz Linear Regression
  lr = matrix(nrow = 2, ncol = n.col+1)
  mz = matrix(nrow = 2, ncol = n.col+1)
  colnames(lr) <- c(model.names, "bench")
  rownames(lr) <- c("a0", "a1")
  colnames(mz) <- c(model.names, "bench")
  rownames(mz) <- c("ub", "mz") #ub - unbiasness test
  for(i in 1:(n.col+1)){
    lm.tmp = lm(realized.measure ~ tmp.forecasts[,i])
    names.tmp = names(lm.tmp$coefficients)
    
    lr[1, i] = lm.tmp$coefficients[1]
    lr[2, i] = lm.tmp$coefficients[2]
    
    mz[1, i] = linearHypothesis(
      model = lm.tmp,
      c(paste(names.tmp[1],'= 0')))$F[2]
    mz[2, i] = linearHypothesis(
      model = lm.tmp,
      c(paste(names.tmp[1],'= 0'),
        paste(names.tmp[2],'= 1')))$F[2]
  }
  
  return(list("LR" = lr, "MZ" = mz, "LF" = loss, "DM" = dm))
}



comparison.sp <-
  SOSAFIC(realized.measure = datas$Inflation.rate[,"infl"],
          model.forecasts = matrix(c(
            datas$SPF.t.t.1.,
            datas$SPF.t.t.2.,
            datas$Greenbook.t.t.1.)),
          bench.forecast = datas$Greenbook.t.t.2.,
          model.names = c("spf1", "spf2", "green"))