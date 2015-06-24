library(shiny)

# Define UI
shinyUI <- fluidPage(
  # Mass dependence
  sliderInput(inputId = "mass",
              label = "Choose a gas mass",
              value = 132, min = 1, max = 132),
 
  # Temperature dependence
  sliderInput(inputId = "temp",
              label = "Choose a temp",
              value = 298, min = 198, max = 398),
 
   plotOutput("pdf")
)

# Define server logic
server <- function(input, output){
  # Define PDF
  R <- 8.3145
  c <- seq(0, 2500)
  
  mbd <- function(){
    sqrt(2/pi)*(c^2*exp(-c^2/(2*(as.numeric(input$temp))/
                                (as.numeric(input$mass)/1000))))/
      (2*(as.numeric(input$temp))/(as.numeric(input$mass)/1000))^(3/2)
  }
  
  # Display plot
  output$pdf <- renderPlot({
    title <- "Maxwell-Boltzmann Distribution"
    plot(mbd(), type="l", main=title, ylim=c(0,0.0045))
  })
}

# Execute app
shinyApp(ui = shinyUI, server = server)

