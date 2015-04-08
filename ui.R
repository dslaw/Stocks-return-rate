## ui for Shiny App

shinyUI(fluidPage(
    titlePanel("Stock Return Rates"),

    sidebarLayout(
        sidebarPanel(
            helpText("Plot daily percent change in stock price."),

            textInput(inputId = "ticker",
                      label = "Stock name (ticker):",
                      value = ""),

            textInput(inputId = "market",
                      label = "Market that the stock is on:",
                      value = ""),

            sliderInput(inputId = "p",
                        label = "Number of days to forecast:",
                        min = 0, max = 10, value = 10)
        ),

        mainPanel(
            plotOutput("plot"),
            tableOutput("table")
        )
    )
))

