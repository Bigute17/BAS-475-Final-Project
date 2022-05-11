library(shiny)
library(shinythemes)
library(shinyWidgets)
library(fpp3)
library(seasonal)
data("aus_production")


ui <- fluidPage(navlistPanel(
  tabPanel("Introduction", 
           h1("Hi! Welcome to my Shiny App!"), 
           p("Each tab presents a different feature of the app. 
            We are analyzing Australian production data from 1956 to 2010.
            This data came from the fpp3 package as well as many of the functions that are utilized in the app.
            The data is also quarterly which is good for us to analyze seasonalty.
            Thank you for taking the time to try my app, and please see below for a quick tutorial."),
            p("The Full Time Series Plot tab displays a time series plot for Australian beer production using autoplot.
            Additionally, I provided my interpretation of the data including comments on trend, seasonal and cyclical behavior, and potential transformations."),
           p("The Seasonality, Autocorrelation, and Decomposition tab will allow you to further investigate the time series.
             First, you must choose what you want to analyze.
             The Seasonality choice uses gg_season and gg_subseries to analyze the seasonality of Australian beer production.
             Look for the differences in production by quarter and see if there any patterns.
             The seasonality plots generally make it easier to confirm or deny your initial thoughts on the seasonal patterns in the data.
             The Autocorrelation choice will produce a plot of the ACF of Australian beer production.
             If the lines are outside of the bounds of the plot, then there is no white noise and autocorrelation is not close to zero.
             If not, then the past values are not correlated with current values which is an issue.
             The Decomposition choice will produce an x13 decomposition plot.
             This allows you to seperate each aspect of the full time series plot including seasonality, trend, and randomness.
             You can use this to further analyze seasonality and trend.
             Additionally, you can more specifically see how the production changes by season compared to the average."),
           p("Lastly, the Experiment tab can be used to test out what you've learned from the previous pages of the app.
             You can choose what series of Australian production that you'd like to plot, and it will generate a full time series plot.
             Try your hand at interpreting the plot based on what you've learned.
             Then, you can generate seasonality, autocorrelation, and decomposition plots.
             See if you can find any interesting patterns yourself!"),
           p("Thank you again for taking the time to try out my app. 
             I hope you were able to learn something new!
             Credit: Blake Curtsinger 03/26/2022")),
  
  tabPanel("Full Time Series Plot", 
           h2("Australian Beer Production"), 
           plotOutput("full1"), 
           textOutput("fullinterp")),
  
  tabPanel("Seasonality, Autocorrelation, and Decomposition", 
           h2("Seasonality, Autocorrelation, and Decomposition"),
           radioGroupButtons(
             inputId = "SADChoice",
             label = h5("What are you analyzing?"),
             choices = c("Seasonality", 
                         "Autocorrelation", "Decomposition")), 
           textOutput("SADInterp"), plotOutput("plot1"), plotOutput("plot2") 
),

  tabPanel("Experiment!", h2("Test Your Own Knowledge"), textOutput("exptxt"),
           pickerInput(
             inputId = "selected_col",
             label = h5("Pick a series to plot."),
             selected = "Tobacco",
             choices = c("Tobacco", "Bricks", "Cement", "Gas", "Electricity"),
             options = list(
               `live-search` = TRUE
             )),
           plotOutput("plot3"),
           radioGroupButtons(
             inputId = "SADChoice2",
             label = h5("What are you analyzing?"),
             choices = c("Seasonality", 
                         "Autocorrelation", "Decomposition")),
           plotOutput("plot4"),
           plotOutput("plot5")
           ),
  tabPanel("Simple Forecasting Models",
         h2("Simple Forecasting Methods"),
         radioGroupButtons(
           inputId = "SimpleChoice",
           label = h5("Choose a simple forecasting method."),
           choices = c("Naive", 
                       "Seasonal Naive", "Mean", "Drift")),
        plotOutput("SimplePlot"),
        verbatimTextOutput("SimpleReport")
        ),
  tabPanel("Exponential Smoothing",
         h2("Let's Try the Holts and Holt/Winters' models!"),
         radioGroupButtons(
           inputId = "ETSChoice",
           label = h5("Holt or Holt/Winters?"),
           choices = c("Holt", "Holt/Winters")),
         plotOutput("ETSPlot"),
         verbatimTextOutput("ETSReport")
         ),
  tabPanel("ARIMA",
         h2("Auto ARIMA and ARIMA With Parameter Selection!"),
         radioGroupButtons(
           inputId = "ARIMAChoice",
           label = h5("Auto ARIMA or Input Parameters"),
           choices = c("Auto", "Input Parameters")),
           numericInput("p", "p", value = 1, min = 0, max = 2),
           numericInput("d", "d", value = 0, min = 0, max = 2),
           numericInput("q", "q", value = 1, min = 0, max = 2),
         plotOutput("ARIMAPlot"))
),
theme = shinytheme("superhero")
)

server <- function(input, output, session) {

  output$full1 <- renderPlot({
    aus_production %>%
      autoplot(Beer) 
  })
  output$fullinterp <- renderText("Check out this time series plot for Australian beer production from 1956 to 2010. We can see that beer production was trending upwards from 1960 to about 1974. However, after that there is no clear trend. It may be trending downards slightly from 1990 to 2010, but it is has otherwise remained fairly stagnant. We can also see that there appears to be strong seasonal patterns in the data where production is at a low near the beginning of the year but then peaks towards the end of the year before dropping off again. We can confirm or deny this later with seasonality plots! It also appears that the seasonality does vary a bit over time which could call for a box-cox transformation.")
  
 output$plot1 <- renderPlot({
   if (input$SADChoice == "Seasonality") {
       aus_production %>%
         gg_season(Beer)
   } else if (input$SADChoice == "Autocorrelation") {
       aus_production %>% 
         ACF(Beer) %>% 
         autoplot()
   } else if (input$SADChoice == "Decomposition") {
       aus_production %>% 
         model(X_13ARIMA_SEATS(Beer~seats())) %>% 
         components() %>% 
         autoplot()
   }
 })
 
 output$plot2 <- renderPlot({
   if (input$SADChoice == "Seasonality") {
     aus_production %>%
       gg_subseries(Beer)
   } else if (input$SADChoice == "Autocorrelation") {
     NULL
   } else if (input$SADChoice == "Decomposition") {
     NULL
   }
 })
 
 output$SADInterp <- renderText(
   if (input$SADChoice == "Seasonality") {
   "The first plot created by gg_season is a bit tough to interpret given the amount of years that we have.
   However, it does appear that generally Australian beer production is higher in Q1 and Q4 compared to Q2 and Q3.
   We can confirm this more clearly in the second plot created by gg_subseries.
   We can see that Australian beer production is higher in Q1 before dropping off during Q2 and Q3 and then reaching its peak in Q4.
   One thing to consider is Australia's seasons. Summer is typically from December to February, and Spring is typically from September to November.
   It seems like there could be a relationship between the average temperature and Australian beer production."
   } else if (input$SADChoice == "Autocorrelation") {
     "Looking at the plot of the ACF, we can see that autocorrelation is not close to zero.
     In other words, the changes in Australian beer production is not due to white noise.
     We know this because there are many spikes outside of the bounds of the plot.
     Generally, past values of Australian beer production are correlated with current values.
     If we wanted to use lag to predict, we would be most interested in lags 4 and 8."
   } else if (input$SADChoice == "Decomposition") {
     "Looking at the x13 decomposition output, we can see that all the elements of the full time series plot have been seperated out.
     This can help confirm some of the thoughts we had earlier.
     It looks like it does trend upward from  1960 to 1980 but may be slightly trending downwards from there on.
     We can also clearly see the strong seasonality over time.
     There appears to be some variation in the seasonality over time, but not much.
     This is known as heteroscedasticity.
     The Box Cox Transformation is good for data that does have heteroscedasticity so it could be used in this case, but it may not have as significant of an effect as we would hope considering that the heteroscedasticity is not very obvious.
     Looking at the y-axis of the seasonal aspect of the decomposition output, we can see that during the low points of the seasonal periods, the Australian production of beer was down as low as about 40% compared to the average."
   }
 )
 
 output$exptxt <- renderText("Now, you can test out what you've learned! 
                             Choose a series to plot and analyze the Australian production.
                             Try and interpret the trend and seasonal or cyclical behaviors of the time series. 
                             Then, you can choose to create seasonality plots, an autocorrelation plot, and a decomposition plot.
                             See if you can interpret each one like I did earlier!")
 
 output$plot3 <- renderPlot({
   aus_production[, c("Quarter", input$selected_col)] %>%
     autoplot() +
     labs(title = input$selected_col)
 })
 
 output$plot4 <- renderPlot({
   if (input$SADChoice2 == "Seasonality") {
     aus_production[, c("Quarter", input$selected_col)] %>%
       gg_season()
   } else if (input$SADChoice2 == "Autocorrelation") {
     aus_production[, c("Quarter", input$selected_col)] %>% 
       ACF() %>% 
       autoplot()
   } else if (input$SADChoice2 == "Decomposition") {
     aus_production[, c("Quarter", input$selected_col)] %>% 
       model(X_13ARIMA_SEATS()) %>% 
       components() %>% 
       autoplot()
   }
 })
 
 output$plot5 <- renderPlot({
   if (input$SADChoice2 == "Seasonality") {
     aus_production[, c("Quarter", input$selected_col)] %>%
       gg_subseries()
   } else if (input$SADChoice2 == "Autocorrelation") {
     NULL
   } else if (input$SADChoice2 == "Decomposition") {
     NULL
   }
 })
 output$SimplePlot <- renderPlot({
   if (input$SimpleChoice == "Naive") {
     fit <- aus_production %>%
              model(NAIVE(Beer))
     fit %>%
       forecast() %>%
       autoplot(aus_production) +
       labs(title = "Naïve model")
   } else if (input$SimpleChoice == "Seasonal Naive") {
     fit <- aus_production %>% 
       model(SNAIVE(Beer))
     fit %>%
       forecast() %>%
       autoplot(aus_production) +
       labs(title = "Seasonal Naïve model")
   } else if (input$SimpleChoice == "Mean") {
     fit <- aus_production %>% 
       model(MEAN(Beer))
     fit %>%
       forecast() %>%
       autoplot(aus_production) +
       labs(title = "Mean Model")
   } else if (input$SimpleChoice == "Drift") {
     fit <- aus_production %>% 
       model(RW(Beer~drift()))
     fit %>%
       forecast() %>%
       autoplot(aus_production) +
       labs(title = "Drift Model")
   }
 })
 output$SimpleReport <- renderPrint({
   if (input$SimpleChoice == "Naive") {
     fit <- aus_production %>%
       model(NAIVE(Beer))
     report(fit)
   } else if (input$SimpleChoice == "Seasonal Naive") {
     fit <- aus_production %>% 
       model(SNAIVE(Beer))
     report(fit)
   } else if (input$SimpleChoice == "Mean") {
     fit <- aus_production %>% 
       model(MEAN(Beer))
     report(fit)
   } else if (input$SimpleChoice == "Drift") {
     fit <- aus_production %>% 
       model(RW(Beer~drift()))
     report(fit)
   }
 })
 output$ETSPlot <- renderPlot({
   if (input$ETSChoice == "Holt" ) {
     fit <- aus_production %>% 
       model(ETS(Beer ~ error("A") + trend("A") + season("N")))
     fit %>%
       forecast() %>%
       autoplot(aus_production) +
       labs(title = "Holt's Model")
   } else if (input$ETSChoice == "Holt/Winters") {
     fit <- aus_production %>% 
       model(
         additive = ETS(Beer ~ error("A") + trend("A") +
                          season("A")),
         multiplicative = ETS(Beer ~ error("M") + trend("A") +
                                season("M"))
             )
     fit %>%
       forecast() %>%
       autoplot(aus_production) +
       labs(title = "Holt/Winters' Model")
   }
 })
 output$ETSReport <- renderPrint({
   if (input$ETSChoice == "Holt" ) {
     fit <- aus_production %>% 
       model(ETS(Beer ~ error("A") + trend("A") + season("N")))
     report(fit)
   } else if (input$ETSChoice == "Holt/Winters") {
     fit <- aus_production %>% 
       model(
         additive = ETS(Beer ~ error("A") + trend("A") +
                          season("A")),
         multiplicative = ETS(Beer ~ error("M") + trend("A") +
                                season("M"))
       )
     report(fit)
     
 }
})
  output$ARIMAPlot <- renderPlot({
    if (input$ARIMAChoice == "Auto") {
      fit <- aus_production %>% 
        model(ARIMA(Beer))
      fit %>%
        forecast() %>%
        autoplot(aus_production) +
        labs(title = "Auto ARIMA Model")
    } else if (input$ARIMAChoice == "Input Parameters") {
      fit <- aus_production %>% 
        model(ARIMA(Beer ~ 1 + pdq(input$p, input$d, input$q)))
      fit %>% 
        forecast() %>%
        autoplot(aus_production) +
        labs(title = "ARIMA With Selected Parameters")
    }
  })
  
}
shinyApp(ui, server)
