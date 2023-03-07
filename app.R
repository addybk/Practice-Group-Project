library(shiny)
library(tidyverse)
covid19 <- read_delim("COVID19_state.csv")

ui<- fluidPage(
  titlePanel("COVID-19 Implications"),
  mainPanel(
    tabsetPanel(
      #INTRODUCTION
      tabPanel("About",
               
      ),
      
      #SECTION 1
      tabPanel("Section 1", 
               
      ),
      
      #SECTION 2
      tabPanel("Section 2",
               
      ),
      
      #SECTION 3
      tabPanel("Section 3",
               #Relationship between population density and rate of infections:
               sidebarLayout(
                 sidebarPanel(
                   p("In this section, we can explore the relationship between population
                    density for each state and the infection rate for that state."),
                   p("We can also look to see if states with different", strong("ranges of income per capita"), 
                     "have obviously different infection rates."),
                   sliderInput("income_range",
                               "Income Per Capita Range:",
                               min = 37994,
                               max = 74561,
                               value = c(40000,60000))
                 ),
                 mainPanel(plotOutput("section3plot"))
               )
      ),
      
      #CONCLUSION
      tabPanel("Conclusion",
               
      )
    )
  )
)


server <- function(input, output){
  #INTRODUCTION
  
  #SECTION 1
  
  #SECTION 2
  
  #SECTION 3
  #plotting pop density vs infection rate:
  filteredcovid19_3 <- reactive({
    covid19 %>% 
      filter(Income %in% input$income_range)
  })
  
  output$section3plot <- renderPlot({
    filteredcovid19_3() %>%
      mutate(InfectionRate = Infected/Tested) %>% 
      ggplot( aes( `Pop Density`, y= InfectionRate))+
      geom_point(col = "black")+
      geom_smooth(method = lm, se = FALSE, col = "red")
    labs(title = "Infection Rate for Different Population Densities",
         x = "Population Density",
         y= "Infection Rate")
  })
  
  #CONCLUSION
  
}


#Run the App
shinyApp(ui = ui, server = server)
