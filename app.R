#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library("readxl")

tw_text<- read_excel("./data/F00007687_WV6_Data_Taiwan_2012_Excel_v20180912_text.xlsx")

tw_num<- read_excel("./data/F00007625_WV6_Data_Taiwan_2012_Excel_v20180912_num.xlsx")

tw_select_all <- select(tw_num,5:12,24,62,161:163,306:308,309)

tw_select <- select(tw_num,5:12,24,62,161:163,306:308,309)
# column name
tw_select_col <- colnames(tw_select)
tw_select_col_copy <- tw_select_col
# split comlumn name 
tw_select_col_code <- str_split_fixed(tw_select_col_copy , ": ", 2)[,1]

# replace column name make it easy to use in filter function
tw_select_col <- str_replace_all(tw_select_col,": ","_")
tw_select_col <- str_replace_all(tw_select_col," ","_")
colnames(tw_select) <- tw_select_col_code


colnames(tw_select_all) <- tw_select_col

set.seed(2019)
tw_select_all$V240_Sex <- factor(tw_select_all$V240_Sex,levels = c(1,2),labels = c("male","female"))
testdf <- tw_select_all%>%select(1:4,V240_Sex)%>%
    pivot_longer(cols = 1:4,names_to = "Question",values_to = "answer") %>%
    filter(answer>=0)


ui <- dashboardPage(
    
    
    dashboardHeader(title = "World Values Survey"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home Page", tabName = "Home", icon = icon("dashboard")),
            menuItem("Questions-V4-V5", tabName = "V4-V5", icon = icon("th"))
        )## sidebarMenu
        
    ),## dashboardSidebar
    dashboardBody(
        tabItems(
            tabItem(tabName = "Home", 
                    #Greetings
                    h2("Questionnaire_Taiwan_2012")
            )
            
            ,
            tabItem(tabName = "V4-V5",
                    fluidRow(
                        column(9,wellPanel(h4("Your Output:"),
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
    
    output$performance <- renderPlotly({
        p <-ggplot(testdf)+
            aes(x = answer)+
            geom_bar(position = "dodge" )+
            facet_wrap(as.factor(testdf$Question),ncol = 2)
        plotly_build(p)
    })
}


shinyApp(ui,server) 

