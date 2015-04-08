## Plot return rates using historical stock prices with predictions
## Run from command line:
## Rscript main.R TICKER MARKET FORECAST

require(ggplot2)
require(forecast, quietly = TRUE)
source("functions.R")

## Take command line arguments
argv = commandArgs(trailingOnly = TRUE)

ticker = try(argv[1], silent = TRUE)
market = try(argv[2], silent = TRUE)
h = tryCatch({
    as.integer(argv[3])
}, warning = function(w) {
    NA
}, error = function(e) {
    NA
})

if (any(is.na(c(ticker, market, h)))) {
    cat("You must provide valid arguments\n")
    quit(save = 'no')
}

ticker = toupper(ticker)
market = toupper(market)

## Get stock data
url = format_url(market = market, ticker = ticker)
dat = get_data(url = url)
dat = format_goog_data(dat)
returnrate = daily_change(dat$Close) * 100 # percent rate of return
dat = data.frame(dat, Change = returnrate)
dat = rm_na_rows(dat)

## Forecast
if (h > 0) {
    model_parameters = expand.grid(0:4L, 0L, 0:4L)
    colnames(model_parameters) = c("p", "d", "q")
    folds = make_folds(n = nrow(dat))

    best_model_parameters = choose_model(dat$Change, model_parameters, folds)
    model = arima(dat$Change, unlist(best_model_parameters))
    forecasts = forecast(model, h = h, level = 0.95)
    predicted_rate = forecasts$mean

    predictions = set_predictions(dat, predicted_rate)
    # add index for ggplot
    predictions = cbind(predictions, t = 1:nrow(predictions))
}

## Plot
dat_breaks = seq(nrow(dat), 1, by = -50)
prediction_breaks = seq(nrow(predictions), nrow(dat), by = -5)
x_axis_breaks = sort(unique(c(dat_breaks, prediction_breaks)))

financialplot <-
	ggplot(predictions, aes(x = t, y = Change)) +
	geom_line() +
	geom_smooth() +
	scale_x_discrete(breaks = x_axis_breaks, labels = predictions$Date[x_axis_breaks]) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	ylab("Daily change (%)") +
	xlab("") +
	ggtitle(ticker)

if (h > 0) {
    financialplot <- financialplot +
            geom_point(data = predictions[(nrow(dat)+1):nrow(predictions),],
                   aes(x = t, y = Change),
                   colour = "red",
                   size = 0.8)
}

png(paste0(ticker, '.png'))
print(financialplot)
junk = dev.off()

