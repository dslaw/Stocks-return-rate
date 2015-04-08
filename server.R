## Server script for Shiny app.

require(shiny)
require(ggplot2)
require(forecast, quietly = TRUE)
source("functions.R")

shinyServer(function(input, output) {
    # inputs: input$ticker, input$market, input$p

    p  <- reactive({ input$p })

    stockdata <- reactive({

        url = format_url(market = input$market, ticker = input$ticker)
        dat = try(get_data(url = url), silent = TRUE)

        if (class(dat) == 'try-error')
            # suppress Shiny output if market and ticker fields are empty
            return(NULL)

        dat = format_goog_data(dat)
        returnrate = daily_change(dat$Close) * 100 # percent rate of return
        dat = data.frame(dat, Change = returnrate)
        dat = rm_na_rows(dat)
        return(dat)
    })

    predictions <- reactive({

        dat = stockdata()

        if (!length(dat))
            return(NULL)

        if (p() > 0) {
            model_parameters = expand.grid(0:4, 0, 0:4)
            colnames(model_parameters) = c('p', 'd', 'q')
            folds = make_folds(n = nrow(dat))

            best_model_parameters = choose_model(dat$Change,
                                                 model_parameters,
                                                 folds)
            model = arima(dat$Change, best_model_parameters)
            forecasts = forecast(model, h = p(), level = 0.95)
            predicted_rate = forecasts$mean

            predictions = set_predictions(dat, predicted_rate)
            predictions = cbind(predictions, t = 1:nrow(predictions)) # add index for plots
            return_dat = predictions
        } else {
            return_dat = dat
        }

        return(list(dat = return_dat, n = nrow(dat)))
    })

    output$table <- renderTable({

        p = p()
        dat = predictions()

        if (is.null(dat)) {
            return(NULL)
        } else if (p > 0 & !is.null(dat)) {
            dat = dat$dat
            n = nrow(dat)
            pred = dat[(n - p + 1):n, c("Date", "Change")]
            pred = t(pred)
            colnames(pred) = NULL
            pred
        } else {
            NULL
        }
    })

    output$plot <- renderPlot({

        pred = predictions()
        dat = stockdata()

        if (!length(pred) & !length(dat))
            return(NULL)

        dat_breaks = seq(nrow(dat), 1, by = -50)
        prediction_breaks = seq(nrow(pred$dat), nrow(dat), by = -5)
        x_axis_breaks = sort(unique(c(dat_breaks, prediction_breaks)))
        predicted = pred$dat
        n = pred$n

        financialplot  <-
            ggplot(predicted, aes(x = t, y = Change)) +
            geom_line() +
            geom_smooth() +
            scale_x_discrete(breaks = x_axis_breaks,
                             labels = predicted$Date[x_axis_breaks]) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            ylab("Daily change (%)") +
            xlab("") +
            ggtitle(input$ticker)

        if (p() > 0) {
            financialplot <- financialplot +
                    geom_point(data = predicted[(n+1):nrow(predicted),],
                           aes(x = t, y = Change),
                           colour = "red",
                           size = 0.8)
        }

        financialplot
    })
})

