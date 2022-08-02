library(shiny)
library(tidyverse)
library(bslib)

suicide = read.csv("Data/who_suicide_statistics.csv")
#suicide = read.csv(here::here("Documents", "Intro to R", "Data", "who_suicide_statistics.csv"))


as.data.frame(suicide)

suicide <- suicide %>% 
  mutate(
    per_100000 = round((suicides_no / population) * 100000, 3)
  )

ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  
  titlePanel("WHO Suicide Statistics 1979 - 2016"),
  
  tabsetPanel(
    tabPanel("Data",
             fluidRow(
               column(4,
                      selectInput(inputId = "country",
                                  label = "Country:",
                                  c("All", unique(as.character(suicide$country))))
               ),
               column(4,
                      selectInput(inputId = "year",
                                  label = "Year:",
                                  c("All", unique(sort(as.character(suicide$year)))))
               ),
               column(4,
                      selectInput(inputId = "age",
                                  label = "Age:",
                                  c("All", unique(as.character(suicide$age))))
               ),
               
               
               DT::dataTableOutput("table"),
               
               h4("Overall Summary Statistics"),
               br(),
               br(),
               br(),
               tableOutput("summary"),
               
               h5("Based on the results above, we can see that Japan has had the most suicides at 937,614 since 1979, 
                     which is more than double the amount of the next country: France (395,500), which is then followed by Germany,
                     Korea, and Brazil.")
               
             )),
    
    # Plot tab
    tabPanel("Plot",
             fluidRow(
               column(4,
                      selectInput(inputId = "x",
                                  label = "X",
                                  names(suicide[sapply(suicide, is.integer)]))
               ),
               column(4,
                      selectInput(inputId = "y",
                                  label = "Y",
                                  names(suicide[sapply(suicide, is.integer)]))
               ),
               column(4,
                      selectInput(inputId = "block",
                                  label = "Block",
                                  c("None", names(suicide[sapply(suicide, is.character)])))
               ),
               
               plotOutput("plot1", hover = "plot_hover"),
               verbatimTextOutput("info")
               
             )
    )))




server <- function(input, output, session) {
  
  # Showing the updated data table based on user input
  output$table <- 
    DT::renderDataTable(DT::datatable({
      data <- suicide
      
      if (input$country != "All") {
        data <- data[data$country == input$country,]
      }
      if (input$year != "All") {
        data <- data[data$year == input$year,]
      }
      if (input$age != "All") {
        data <- data[data$age == input$age,]
      }
      data
    }))
  
  plot <- suicide %>% 
    group_by(country) %>% 
    summarise(sum = sum(suicides_no)) %>% 
    arrange(desc(sum)) %>% 
    top_n(5)
  
  output$summary <- renderTable({
    plot
  })
  
  # Showing the plot
  output$plot1 <- renderPlot({
    
    if(input$block != "None")
      p <- ggplot(suicide, aes_string(x = input$x, y = input$y, col = input$block, group = input$block)) +
        geom_point() 
    if(input$block == "None")
      p <- ggplot(suicide, aes_string(x = input$x, y = input$y)) +
        geom_point()
    print(p)
  })
  
  br()
  br()
  br()
  br()
  
  # hover
  output$info <- renderText({
    paste0("x = ", input$plot_hover$x, "\ny = ", input$plot_hover$y)
  })
  
}

shinyApp(ui, server)