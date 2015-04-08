## Scrape historical stock prices from GGoogle Finances as a csv

format_url = function(market, ticker){
    ## Insert stock market and ticker name into url to scrape

    market = toupper(market)
    ticker = toupper(ticker)
    baseurl = "http://www.google.com/finance/historical?q=MARKET%3ATICKER&ei=CCGrVLnnD4-PigK38K38YD4AQ&output=csv"
    url = sub("MARKET", market, sub("TICKER", ticker, baseurl))
    return(url)
}

get_data = function(url){
    ## Returns financial data as a data frame with cols Date and Close (price)

    classes = c("character", "NULL", "NULL", "NULL", "numeric")
    read.csv(url, header = TRUE, stringsAsFactors = FALSE, colClasses = classes)
}


## Process data

format_data = function(x, dateformat){
    ## Sets data from earliest to latest and coerces to correct data types.

    force(dateformat)

    function(x){
        x = x[, c("Date", "Close")]
        x = as.data.frame(apply(x, 2, rev))
        x["Close"] = as.numeric(as.character(x[["Close"]]))
        x[x["Close"] < 0, "Close"] = NA
        x["Date"] = as.Date(x[["Date"]], format = dateformat)
        return(x)
    }
}

format_yahoo_data = format_data(x, dateformat = "%Y-%m-%d")
format_goog_data = format_data(x, dateformat = "%d-%b-%y")

daily_change = function(x){
    ## Price change from day t-1 to day t
    ## day t - day t-1 / day t-1

    i = 1:(length(x) - 1)
    dailychange = diff(x) / x[i]
    return(c(NA, dailychange))
}


## Add predictions to data frame

get_weekends  = function(start, p){
     ## Create dates for predicted values.
     ## Skip over weekends.
     ## start is the starting date. p is the number of days to go ahead.

     extradays = p * ceiling((7/5))
     dates = start + 1:(p + extradays)
     weekend = c("Saturday", "Sunday")
     dates = dates[!(weekdays(dates) %in% weekend)]
     dates = dates[1:p]
     return(dates)
}

set_predictions = function(x, pred, type = "Change"){
    ## Append predicted values to the data frame for plotting.

    p = length(pred)
    na_vector = rep(NA, p)
    n = nrow(x)

    # Add NA values to the data frame and populate them where appropriate
    prediction_df = data.frame(replicate(ncol(x), na_vector))
    colnames(prediction_df) = colnames(x)
    x = rbind(x, prediction_df)
    prediction_indices = (n + 1):(n + p)

    predicted_dates = get_weekends(start = x$Date[n], p = p)
    x[[type]][prediction_indices] = pred
    x[["Date"]][prediction_indices] = predicted_dates
    return(x)
}

rm_na_rows = function(x) x[!rowSums(is.na(x)), ]

## Fit model and forecast

make_folds = function(n, minimum = 50L, foldsize = 10L){
    ## Create an index of equally sized cross validation folds.

    if (n <= minimum)
        stop()

    excess = (n - minimum) %% foldsize
    minimum = minimum + excess # ensures that fold sizes are equal
    cutoffs = c(0, seq(minimum, n, by = foldsize))
    fold_index = (1:n)[cut(1:n, cutoffs)]
    return(fold_index)
}

arima_cv = function(x, parameters, folds, fold = 1L, acc = 0){
    ## Calculate predicted MSE using rolling origin cross-validation.

    if (fold == max(folds))
        return(acc)

    training = x[folds <= fold]
    test = x[folds == (fold+1)]

    model = arima(training, order = parameters)
    forecasted = forecast(model, h = length(test), level = 0.95)
    forecast_error = test - forecasted$mean
    mse = sum(forecast_error ** 2) / length(forecast_error)
    acc = acc + mse
    arima_cv(x, parameters, folds, fold = fold + 1L, acc = acc)
}

choose_model = function(x, parameters, folds){
    ## Fit the ARIMA model that minimizes predicted MSE

    predicted_error = apply(parameters, 1, function(a)
                            tryCatch({
                                arima_cv(x, parameters = unlist(a),
                                         folds = folds)
                            }, error = function(e) { NA }
                            ))
    model_params = parameters[which.min(predicted_error), ]
    return(unlist(model_params))
}

