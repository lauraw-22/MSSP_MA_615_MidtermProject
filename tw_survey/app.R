#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(shiny)
library(shinydashboard)
library(plotly)

ui <- dashboardPage(
    
    
    dashboardHeader(title = "World Values Survey"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home Page", tabName = "Home", icon = icon("dashboard")),
            menuItem("Questions-V4-V5", tabName = "V4-V5", icon = icon("th"))
            #menuItem("Player Analysis", tabName = "PA", icon = icon("th")),
            #menuItem("Raw Data", tabName = "RD", icon = icon("th")),
            #menuItem("Help", tabName = "Help", icon = icon("th"))
        )## sidebarMenu
        
    ),## dashboardSidebar
    dashboardBody(
        tabItems(
            tabItem(tabName = "Home", 
                    #Greetings
                    h2("Questionnaire_Taiwan_2012")
                    
                    #Heading image
                    #fluidRow(h3(""),h3(""),h3(""),column(12,mainPanel(imageOutput("fieldhockey")))),
                    
                   # h4("This Application is developed by Boston University MSSP program. The objective of this app is to help BU Field Hockey Coaches to review athletes' performance both from real games and exerives and to have a better decision making on future coaching.")
                    
            )
            
            ,
            # First tab content for Performance Overview
            # tabItem(tabName = "V4-V5",
            #         fluidRow(
            #             column(width = 5,
            #                    wellPanel( title = "Game",
            #                               selectInput("Games", "Select Game:", 
            #                                           c("Game#1","Game#2","Game#3")),width = NULL), ## box 1
            #                    wellPanel( title = "Training",
            #                               selectInput("Training", "Select Training:", 
            #                                           c("Training#1","Training#2","Training#3")),width = NULL) ## box 2
            #             ))), ## tabItem 1
            
            # Second tab content for Player Analysis
            tabItem(tabName = "V4-V5",
                    fluidRow(
    
                        
                        ###########################mainPanel###########################
                        column(9,wellPanel(style = "background: floralwhite",h4("Your Output:"),
                                           plotlyOutput("performance")
                                           
                        ))
                        
                    )) ## tabItem 2
        ))) ## tabItems



# server = function(input, output) {
#     output$plot <- renderPlot({
#         input$newplot
#         # Add a little noise to the cars data
#         cars2 <- cars + rnorm(nrow(cars))
#         plot(cars2)
#     })
# }
# )


server <- function(input, output) {
    tw_select_all$V240_Sex <- factor(tw_select_all$V240_Sex,levels = c(1,2),labels = c("male","female")) 
    testdf <- tw_select_all%>%select(1:4,V240_Sex)%>%
        pivot_longer(cols = 1:4,names_to = "Question",values_to = "answer") %>%
        filter(answer>=0)
    p <-ggplot(testdf)+
        aes(x = answer,fill = V240_Sex)+
        geom_bar(position = "dodge")+facet_wrap(as.factor(testdf$Question),ncol = 2)
    
    p2 <- ggplotly(p)
    output$performance <- renderPlotly({p2})
}


shinyApp(ui,server)

