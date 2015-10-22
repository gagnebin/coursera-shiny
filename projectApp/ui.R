fluidPage(
  # Application title
  titlePanel("Plateform 2015 Explorer"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Choose a party:",
                  choices = parties),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Min Frequency of Words:",
                  min = 5,  max = 50, value = 20),
      sliderInput("max",
                  "Max Number of Words displayed (histogram):",
                  min = 1,  max = 40,  value = 10)
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot"),
      plotOutput("hist")
    )
  )
)

