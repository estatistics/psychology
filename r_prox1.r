
# ts_aapl[is.nan_dt(ts_aapl)] <- 0







#### 
# Data to be forecasted
fc_dt <- ts_aapl[200:650]

# How Auto Arima works
# : Select current model (with smallest AIC) from:
# ARIMA(2, d, 2)(1, D, 1)m
# ARIMA(0, d, 0)(0, D, 0)m
# ARIMA(1, d, 0)(1, D, 0)m if seasonal
# ARIMA(0, d, 1)(0, D, 1)m

# Automated forecasting using an exponential model
exp_fit   <- ets( fc_dt )
arima_fit <- auto.arima( fc_dt )



# ME: Mean Error
# -- The mean error is an informal term that usually refers to the average of
# -- all the errors in a set. An “error” in this context is an uncertainty in a measurement, 
# -- or the difference between the measured value and true/correct value

# RMSE: Root Mean Squared Error
# MAE: Mean Absolute Error
# -- The MAE measures the average magnitude of the errors in a set of forecasts,
# -- without considering their direction. It measures accuracy for continuous variables. 
# -- The RMSE will always be larger or equal to the MAE; 
# -- the greater difference between them, the greater the variance in the individual errors 
# -- in the sample. If the RMSE=MAE, then all the errors are of the same magnitude
# -- Both the MAE and RMSE can range from 0 to ∞. 
# -- They are negatively-oriented scores: Lower values are better.

# MPE: Mean Percentage Error
# -- the mean percentage error (MPE) is the computed average of 
# -- percentage errors by which forecasts of a model differ from actual values of the 
# -- quantity being forecast.

# MAPE: Mean Absolute Percentage Error
# -- The MAPE, as a percentage, only makes sense for values where divisions and
# -- ratios make sense. It doesn't make sense to calculate percentages of temperatures
# -- MAPEs greater than 100% can occur.
# -- then this may lead to negative accuracy, which people may have a hard time understanding
# -- Error close to 0% => Increasing forecast accuracy
# -- Around 2.2% MAPE implies the model is about 97.8% accurate in predicting the next 15 observations.


# MASE: Mean Absolute Scaled Error
# -- Scale invariance: The mean absolute scaled error is independent of the scale of the data,
# -- so can be used to compare forecasts across data sets with different scales.
# -- ok for scales that do not have a meaningful 0,
# -- penalizes positive and negative forecast errors equally
# -- Values greater than one indicate that in-sample one-step forecasts
#    from the naïve method perform better than the forecast values under consideration.
# -- When comparing forecasting methods, the method with the lowest MASE is the preferred method.

# ACF1: Autocorrelation of errors at lag 1.'
# -- it is a measure of how much is the current value influenced by the previous values in a time series.
# -- Specifically, the autocorrelation function tells you the correlation between points separated by various time lags
# -- the ACF tells you how correlated points are with each other, 
# -- based on how many time steps they are separated by. That is the gist of autocorrelation,
# -- it is how correlated past data points are to future data points, for different values of the time separation.
# -- Typically, you'd expect the autocorrelation function 
# -- to fall towards 0 as points become more separated (i.e. n becomes large in the above notation) 
# -- because its generally harder to forecast further into the future from a given set of data. 
# -- This is not a rule, but is typical.
# -- ACF(0)=1 (all data are perfectly correlated with themselves), 
# -- ACF(1)=.9 (the correlation between a point and the next point is 0.9), ACF(2)=.4
# -- (the correlation between a point and a point two time steps ahead is 0.4)...etc.

# MAPE (???), Correlation and Min-Max Error can be used
# an RMSE of 100 for a series whose mean is in 1000’s is better than an RMSE of 5 for series in 10’s. 
# So, you can’t really use them to compare the forecasts of two different scaled time series.

# All your indicators (ME, RMSE, MAE, MPE, MAPE, MASE, ACF1,...) are aggregations of two types of errors : 
# a bias (you have the wrong model but an accurate fit) + a variance (you have the right model but a inaccurate fit). 
# And there is no statistical method to know if you have a high bias and low variance or a high variance and low bias.
# So I suggest, you make a plot and make an eye-stimate to select the "best" one,
# best meaning with the least business consequences if you are wrong.


# Generally, all of these values to be as small as possible 

error_list <-list(
  accuracy( exp_fit ), 
  accuracy( arima_fit ),
  accuracy( meanf( fc_dt, 50), ts_aapl[651:700] ),
  accuracy( naive( fc_dt, 50), ts_aapl[651:700] ),                        
  accuracy( rwf( fc_dt, drift = T, h=50), ts_aapl[651:700] ),      
  accuracy( ses( fc_dt,  h=50), ts_aapl[651:700] )
)

nm_dt <- c( 
  "exp_fit - train", 
  "arima_fit - train", 
  "meanf - train", 
  "meanf - test",
  "naive - train",
  "naive - test",
  "rwf drift - train",
  "rwf drift - test",
  "ses - train",
  "ses - test"
)



errors <- round( rbind.fill.matrix( error_list  ), 4 )
rownames( errors ) <- nm_dt



# Exponential fit
plot( forecast( exp_fit, 50), xlim = c( 400, 500 ) )
lines( ts_aapl[200:700], col="green" )

# Arima Fit
plot( forecast( arima_fit, 50), xlim = c( 400, 500 ) )
lines( ts_aapl[200:700], col="green" )

# Mean method: Forecast of all future values is equal to mean of historical data Mean: meanf(x, h=10)
plot( meanf( fc_dt, h=50), xlim = c( 400, 500 ) )
lines( ts_aapl[200:700], col="green" )

# Naive method: Forecasts equal to last observed value Optimal for efficient stock markets
# Same - rwf stands for random walk function
plot( naive( fc_dt, h=50), xlim = c( 400, 500 ) )
lines( ts_aapl[200:700], col="green" )

# Drift method: Forecasts equal to last value plus average change Equivalent to extrapolating 
# the line between the first and last observations 
plot( rwf( fc_dt, drift = T, h=50), xlim = c( 400, 500 ) )
lines( ts_aapl[200:700], col="green" )


# Simple exponential smoothing 
plot( ses( fc_dt,  h=50), xlim = c( 400, 500 ) )
lines( ts_aapl[200:700], col="green" )


# theta
# splinef

# Plot residual errors
# residuals = pd.DataFrame(model_fit.resid)
# fig, ax = plt.subplots(1,2)
# residuals.plot(title="Residuals", ax=ax[0])
# residuals.plot(kind='kde', title='Density', ax=ax[1])
# plt.show()


# 
# # Holt-Winters Filter
# # Forecasting (non) seasonals and trends by exponentially weighted moving averages
# # beta = False then exponential smoothing
# # gamma =FALSE, then non-seasonal model is fitted.
# ts_aapl[is.nan_dt(ts_aapl)] <- 0
# HoltWinters( ts_aapl, beta = FALSE, gamma = FALSE )
# 
# 
# ## Non-Seasonal Holt-Winters
# x <- ts_aapl[1:100] + rnorm(ts_aapl[1:100], sd = 5)
# m <- HoltWinters(x, gamma = FALSE)
# plot(m)
# 
# ## Exponential Smoothing
# m2 <- HoltWinters(x, gamma = FALSE, beta = FALSE)
# lines(fitted(m2)[,1], col = 3)

