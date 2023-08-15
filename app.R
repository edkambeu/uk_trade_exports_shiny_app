#Loading relevant libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(scales)
#Loading data
exports_clean <- read.csv("exports_data_clean.csv")

# Defining user interface logic
ui <- fluidPage(
div(titlePanel("UK Exports Third Quarter of 2022"), style = "background:blue;color:white;padding-left:10px; margin-bottom:-10px;padding-top:2px;margin-left:-14px; margin-right:-14px;"),
fluidRow(
 column(3, div(valueBoxOutput("value_box1"), style ="color:white;font-weight:bold;font-size:20px"), style = "background:blue;"),
 column(3, div(valueBoxOutput("value_box2"), style="color:white;font-weight:bold; font-size:20px"), style = "background:blue;"),
 column(3, div(valueBoxOutput("value_box3"), style="color:white;font-weight:bold;font-size:20px"), style = "background:blue;"),
 column(3, div(valueBoxOutput("value_box4"), style="color:white; font-weight:bold; font-size:20px"), style = "background:blue;")
),
fluidRow(
  column(4, 
         div(
           h2("Monthly Exports", style="background:blue;color:white; padding-top:1px; padding-bottom:1px;padding-left:2px;"),
           plotOutput("plot1")
           )
         ),
  column(4, 
         div(
           h2("Top 5 Export Commodities", style="background:blue;color:white; padding-top:1px; padding-bottom:1px;padding-left:2px;"),
           plotOutput("plot2")
         )
  ),
  column(4, 
         div(
           h2("Top 5 Export Countries", style="background:blue;color:white; padding-top:1px; padding-bottom:1px;padding-left:2px;"),
           plotOutput("plot3")
         )
  )

)
)

# Server logic 
server <- function(input, output) {
  #Value box 1
  output$value_box1 <- renderValueBox(
    {
      exports_total_by_month_july  <- sum(exports_clean$Jul, na.rm = TRUE)
      valueBox(
        value = paste(prettyNum(exports_total_by_month_july, big.mark = ",", preserve.width = "none"), "£",  sep = " "), subtitle = "July", color = "red"
      )})
    
    
    #Value box 2
    output$value_box2 <- renderValueBox(
      {
        exports_total_by_month_aug  <- sum(exports_clean$Aug, na.rm = TRUE)
        valueBox(
          value = paste(prettyNum(exports_total_by_month_aug, big.mark = ",", preserve.width = "none"), "£", sep = " "), subtitle = "August"
        )
      }
  )
  
  #Value box 3
    output$value_box3 <- renderValueBox(
      {
        exports_total_by_month_sep  <- sum(exports_clean$Sep, na.rm = TRUE)
        valueBox(
          value = paste(prettyNum(exports_total_by_month_sep, big.mark = ",", preserve.width = "none" ), "£", sep = " "), subtitle = "September", color = "red"
        )
      }
    )
    #Value box 4
    output$value_box4 <- renderValueBox(
      {
        exports_total_by_month  <- as.character(sum(exports_clean$Jul,
                                       exports_clean$Aug,
                                       exports_clean$Sep,
                                       na.rm = TRUE))
        valueBox(
          value= paste(prettyNum(exports_total_by_month, big.mark = ",", preserve.width = "none"), "£", sep = " "),subtitle = "Total", color = "orange"
        )
      }
    )  
   
   #Plot 1-summary plot 
    output$plot1 <- renderPlot(
      {
      exports_by_month_values <- as.numeric(c(sum(exports_clean$Jul, na.rm=TRUE),
                                              sum(exports_clean$Aug, na.rm = TRUE), 
                                              sum(exports_clean$Sep, na.rm =TRUE))
                                            )
      month = c("Jul", "Aug", "Sep")
      exports_by_month_df <- as.data.frame(cbind(month, exports_by_month_values))
      ggplot(data = exports_by_month_df) +
        geom_bar(aes(x = month, y = exports_by_month_values ), fill = 'orange',  stat = "identity")+
        geom_line(aes(x = month, y = exports_by_month_values), stat = "identity", color = "purple", linewidth = 1, group =1)+
        labs(x= "Month", y = "Exports(Pounds)") +
        theme(
        axis.text.x = element_text(size = 12, color = "blue", face="bold"), 
        axis.text.y = element_text(size = 12, color = "blue", face="bold"),
        axis.title.x = element_text(size =12, color = "blue", face = "bold"),
        axis.title.y = element_text(size =12, color = "blue", face = "bold"),
        panel.background = element_rect(fill = "white"),  
        panel.grid = element_blank()
        )
  
      
    }
      
    )
    
    #Plot 2-top five export products
    output$plot2 <- renderPlot({
      
      total_exports_by_commodity <- exports_clean %>% 
        group_by(commodity) %>%
        mutate(commodity_total_exports = Jul + Aug + Sep) %>%
        summarise(commodity_exports_all= sum(commodity_total_exports, na.rm = TRUE)) %>% 
        arrange(desc(commodity_exports_all)) %>%
        slice(1:5)
      
      ggplot(data= total_exports_by_commodity) +
        geom_bar(aes(x = reorder(commodity, +commodity_exports_all), y = commodity_exports_all ), stat = "identity", fill = "orange") +
        coord_flip()+
        labs(y = "Exports(Pounds)") +
        theme(
          axis.text.x = element_text(size = 12, color = "blue", face="bold"), 
          axis.title.x = element_text(size =12, color = "blue", face = "bold"),
          axis.text.y = element_text(size =12, color = "blue", face = "bold"),
          axis.title.y = element_blank(),
          panel.background = element_rect(fill = "white"),  
          panel.grid = element_blank()
        )
    })
    
    
    #Plot 3- top five export countries 
    output$plot3 <- renderPlot({
      total_exports_by_country <- exports_clean %>% 
        group_by(country) %>%
        filter(!country %in% c("Whole world", "Extra EU 28 (Rest of World)", "Total EU(28)" ) ) %>%
        mutate(country_total_exports = Jul + Aug + Sep) %>%
        summarise(country_exports_all= sum(country_total_exports, na.rm = TRUE)) %>% 
        arrange(desc(country_exports_all)) %>%
        slice(1:5)
      
      ggplot(data= total_exports_by_country) +
        geom_bar(aes(x = reorder(country, +country_exports_all), y = country_exports_all ), stat = "identity", fill = "orange") +
        coord_flip()+
        labs(y = "Exports(Pounds)") +
        theme(
          axis.text.x = element_text(size = 12, color = "blue", face="bold"), 
          axis.title.x = element_text(size =12, color = "blue", face = "bold"),
          axis.text.y = element_text(size =12, color = "blue", face = "bold"),
          axis.title.y = element_blank(),
          panel.background = element_rect(fill = "white"),  
          panel.grid = element_blank()
          )
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
