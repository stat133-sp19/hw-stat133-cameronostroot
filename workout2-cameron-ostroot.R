#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Growth of Investments from Differing Modalities"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(3, 
         sliderInput("amount",
                     "Initial Amount",
                     min = 1,
                     max = 100000,
                     value = 1000),
         sliderInput("contrib",
                     "Annual Contribution",
                     min = 0,
                     max = 50000,
                     value = 2000)),
     column(4, offset = 1,
            sliderInput("rate",
                        "Return Rate (in %)",
                        min = 0,
                        max = 20,
                        value = 5),
         sliderInput("growth",
                     "Growth Rate (in %)",
                     min = 0,
                     max = 20,
                     value = 2)),
     column(4,
            sliderInput("years",
                        "Years",
                        min = 0,
                        max = 50,
                        value = 10),
         selectInput("facet",
                     "Facet?",
                     choices = c("No", "Yes"),
                     selected = "No"))
         
         
      ),
      
      # Show a plot of the generated distribution
    mainPanel( h4("Timelines"), width = 12, plotOutput("distPlot")),
    mainPanel(h4("Balances"), width = 12, verbatimTextOutput("disttable"))
        
   )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     #Functions
     future_value <- function(amount = 0 , rate = 0 , years = 0) {
       return(amount*((1+rate)^years))
     }
     annuity <- function(contrib = 0 , rate = 0 , years = 0) {
       return(contrib*(((1+rate)^years)-1)/rate)
     }
     growing_annuity <- function(contrib = 0 , rate = 0 , growth = 0, years = 0) {
       return(contrib*(((1+rate)^years)-((1+growth)^years))/(rate-growth))
     }
     
     #combining into data frame
     #modalities <- data.frame("year" = 0:input$years)
     x<- list()
     y<- list()
     z <- list()
     for (i in 0:input$years) {
       x <- rbind(x, future_value(amount = input$amount, rate = ((input$rate)/100), years = i))
       y <- rbind(y, future_value(amount = input$amount, rate = ((input$rate)/100), years = i) + annuity(contrib = input$contrib, rate = ((input$rate)/100), years = i))
       z <- rbind(z, future_value(amount = input$amount, rate = ((input$rate)/100), years = i) + growing_annuity(contrib = input$contrib, rate = ((input$rate)/100), growth = ((input$growth)/100), years = i))
     }
     #modalities <- cbind(modalities, "no_contrib" = as.numeric(x), "fixed_contrib" = as.numeric(y), "growing_contrib" = as.numeric(z))
     
     modalities <- data.frame(
       year = 0:input$years,
       no_contrib = as.numeric(x),
       fixed_contrib = as.numeric(y), 
       growing_contrib = as.numeric(z)
     )

     a<- list()
     b<- list()
     c <- list()
     d <- list()
     e <- list()
     f <- list()
     for (i in 0:input$years) {
       a <- rbind(a, future_value(amount = input$amount, rate = (input$rate)/100, years = i))
       b <- rbind(b, future_value(amount = input$amount, rate = (input$rate)/100, years = i) + annuity(contrib = input$contrib, rate = (input$rate)/100, years = i))
       c <- rbind(c, future_value(amount = input$amount, rate = (input$rate)/100, years = i) + growing_annuity(contrib = input$contrib, rate = (input$rate)/100, growth = (input$growth)/100, years = i))
       d <- rbind(d, "no_contrib")
       e <- rbind(e, "fixed_contrib")
       f <- rbind(f, "growing_contrib")
     }
     
     w <- rbind(a,b,c)
     h<- rbind(d,e,f)
     h <- factor(h, levels = h)
     
     app_frame <- data.frame("year" = rep(0:input$years, 3), "data" = as.numeric(w), "type" = h)
     
     if (input$facet == "No"){
       return(ggplot(data = modalities) +
         geom_line(aes(x = year, y = no_contrib, color = "no_contrib")) + geom_point(aes(x = year, y = no_contrib, color = "no_contrib")) +
         geom_line(aes(x = year, y = fixed_contrib, color = "fixed_contrib")) + geom_point(aes(x = year, y = fixed_contrib, color = "fixed_contrib")) +
         geom_line(aes(x = year, y = growing_contrib, color = "growing_contrib")) + geom_point(aes(x = year, y = growing_contrib, color = "growing_contrib")) +
         labs(x = "year", y = "value", title = "Three modes of investing") +
         scale_color_manual(name = "variable", values = c("green", "blue", "red"), breaks = c("no_contrib", "fixed_contrib", "growing_contrib"))
       )
     } 
     if (input$facet == "Yes") {
       return(ggplot(app_frame) + 
          geom_line(aes(x = year, y = data, color = type)) + 
          geom_point(aes(x = year, y = data, color = type)) +
          geom_ribbon(aes(x = year, ymin = 0, ymax = data, fill = type), linetype = 0, alpha = 0.3)+
          labs(title = "Three modes of investing", x = "year", y = "value") + 
          scale_color_manual(name = "variable", values = c("red", "green", "blue")) +
          scale_fill_manual(name = "variable", values = c("red", "green", "blue"))+
          facet_grid(~type) +
          theme_bw()
       )
     }
     

   })
   output$disttable <- renderPrint({
     #Functions
     future_value <- function(amount = 0 , rate = 0 , years = 0) {
       return(amount*((1+rate)^years))
     }
     annuity <- function(contrib = 0 , rate = 0 , years = 0) {
       return(contrib*(((1+rate)^years)-1)/rate)
     }
     growing_annuity <- function(contrib = 0 , rate = 0 , growth = 0, years = 0) {
       return(contrib*(((1+rate)^years)-((1+growth)^years))/(rate-growth))
     }
     
     #combining into data frame
     #modalities <- data.frame("year" = 0:input$years)
     x<- list()
     y<- list()
     z <- list()
     for (i in 0:input$years) {
       x <- rbind(x, future_value(amount = input$amount, rate = ((input$rate)/100), years = i))
       y <- rbind(y, future_value(amount = input$amount, rate = ((input$rate)/100), years = i) + annuity(contrib = input$contrib, rate = ((input$rate)/100), years = i))
       z <- rbind(z, future_value(amount = input$amount, rate = ((input$rate)/100), years = i) + growing_annuity(contrib = input$contrib, rate = ((input$rate)/100), growth = ((input$growth)/100), years = i))
     }
     #modalities <- cbind(modalities, "no_contrib" = as.numeric(x), "fixed_contrib" = as.numeric(y), "growing_contrib" = as.numeric(z))
     
     modalities <- data.frame(
       year = 0:input$years,
       no_contrib = as.numeric(x),
       fixed_contrib = as.numeric(y), 
       growing_contrib = as.numeric(z)
     )
     modalities
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

