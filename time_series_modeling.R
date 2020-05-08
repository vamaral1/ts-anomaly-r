library(ggplot2)
library(ggfortify)
library(forecast)

## x is a dataframe with a column called "values" with the univariate time series values
## freq is the frequency of the data ex) 12 for monthly data
## num_preds is the number of periods to be forecasted
## name is the name of the data - used in writing to files
model_prediction <- function(x, freq, num_preds, name) {
  
  #convert data to time series
  x_ts <- ts(x$values,freq=freq)
  
  #uses Hyndman-Khandakar algorithm for automatic ARIMA modelling - https://www.otexts.org/fpp/8/7
  #minimize AICc, AIC, BIC
  if(nrow(x) <= freq) {
    result <- forecast(auto.arima(x_ts, seasonal=TRUE, stepwise = FALSE, parallel = TRUE), h = num_preds)
  } else {
    result <- forecast(stl(x_ts, s.window = "periodic", robust=TRUE), method = 'arima', h = num_preds)
    
  }
  model <- result$model
  
  #create an output directory
  dir.create(file.path('out'), showWarnings = FALSE)
  
  #save plot
  ggsave(plot=autoplot(result),file=paste(c('out/', name, '_plot.png'), collapse = ''))
  
  #get measurements that evaluation goodness of model
  model_eval <- c(model$loglik, model$aic, model$aicc, model$bic)
  names(model_eval) <- c('log lik','aic','aicc','bic')
  model_eval <- as.data.frame(model_eval)
  
  #get mse,rmse, mase, etc
  pred_eval <- accuracy(result)
  tmp <- as.data.frame(t(pred_eval))
  colnames(tmp) <- colnames(model_eval)
  eval <- rbind(tmp,model_eval)
  
  #next num_preds values forecasted with levels
  forecast_vals <- as.data.frame(summary(result))
  write.csv(forecast_vals, paste(c('out/', name, '_forecast_vals.csv'), collapse = ''))
  write.csv(eval, paste(c('out/', name, '_eval.csv'), collapse = ''))
}