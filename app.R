library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Base rate fallacy"),
  
  tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
            });
            </script >
            ")),
  
  verticalLayout(
    withMathJax(),
    p("Suppose that we have a population of 100,000 people, in which some have a disease, $D$. Using the sliders, you can set the baseline prevalence of the disease, $P(\\text{D})$, as well as the specificity and sensitivity of the test."),
    
    p("Recall that the $specificity$ of a test represents how often the test correctly identifies those without the disease, $P(- | \\neg D)$ and the $sensitivity$ of a test represents how often the test correctly identifies those with the disease, $P(+ | D)$."),
    
    p("Note that false positives are depicted in this Euler diagram by the $\\color{steelblue}{\\text{blue area}}$."),
    
    h5("What happens to the size of the blue area as the prevalence — or 'base rate' — changes?"),
  
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "prev",
                  label = "Prevalence",
                  min = 0,
                  max = 1,
                  value = 0.1),
      
      sliderInput(inputId = "spec",
                  label = "Specificity",
                  min = 0,
                  max = 1,
                  value = 0.9),
      
      sliderInput(inputId = "sens",
                  label = "Sensitivity",
                  min = 0,
                  max = 1,
                  value = 0.6),
      radioButtons("radio", "Display quantites as:",
                   choices = list("Counts, out of 100,000" = "num", "Percent" = "pc"), selected = "num")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      conditionalPanel(
        condition = "input.radio == 'num'", plotOutput("plot.num")),
      conditionalPanel(
        condition = "input.radio == 'pc'", plotOutput("plot.pc"))
      
       )   
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  library(tidyverse)
  library(ggplot2)
  
  
  # prevalence = 0.9%
  # prev <- 0.009 
  
  # sensitivity = 75%
  # sens <- 0.75
  
  # specificity = 90%
  # spec <- 0.90
  
  n <- 100000
  
  
  df1 <- reactive({
  
  df <- data.frame(rep(NA, n))
  df$id <- seq(1:n)
  df$disease <- rbinom(n, 1, input$prev)
  df$test.pos[df$disease==1] <- rbinom(sum(df$disease==1), 1, input$sens)
  df$test.pos[df$disease==0] <- rbinom(sum(df$disease==0), 1, 1-input$spec)
  return(df)
  }
)
  
  venn.values <- reactive({
    temp <- df1()
   a <- sum(temp$disease==1)
   b <- sum(temp$test.pos==1)
   c <- sum(temp$disease==1 & temp$test.pos==1)
   cbind(a, b, c)
  })
  

  
# select colors
  colorblind.values=c("#000000","#004949","#009292","#ff6db6","#ffb6db", "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff", "#920000","#924900","#db6d00","#24ff24","#ffff6d")
  col2 <- colorblind.values[c(6, 13)]
  col3 <- colorblind.values[c(6, 13, 5)]
  col4 <- colorblind.values[c(3,6,11,13)]
  
 fit1 <- reactive({
    library(eulerr)
    temp <- venn.values()
    disease <- as.numeric(temp[1])
    test.pos <- as.numeric(temp[2])
    overlap <- as.numeric(temp[3])
    total <- as.numeric(n)
    # Input in the form of a named numeric vector
    fit1 <- euler(c("A" = n-disease-test.pos+overlap, "B" = 0, "C" = 0,
                    "A&B" = disease-overlap, "A&C" = test.pos-overlap, "B&C" = 0,
                    "A&B&C" = overlap),
                  shape="ellipse")
 })
 
 output$plot.num = renderPlot({
   labs <- c("All individuals", "Has the disease", "Tests Positive")
   plot(fit1(), quantities = list(type = c("counts")),
        legend = list(side = "right", labels = labs),
        fills = c("white", "red", "steelblue"))
 })
 
 output$plot.pc = renderPlot({
   labs <- c("All individuals", "Has the disease", "Tests Positive")
   plot(fit1(), quantities = list(type = c("percent")),
        legend = list(side = "right", labels = labs),
        fills = c("white", "red", "steelblue"))
 })

}

# Run the application 
shinyApp(ui = ui, server = server)