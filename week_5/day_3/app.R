library(shiny)
library(dplyr)
library(ggplot2)
library(CodeClanData)
install.packages("shinythemes")
library(shinythemes)

all_seasons <- unique(olympics_overall_medals$season)
all_medals <- unique(olympics_overall_medals$medal)

ui <- fluidPage(
    theme = shinytheme("superhero"),
    tabsetPanel(
        tabPanel("First Tab",
          titlePanel(HTML("<h1>Five countries medal comparison</h1>")),
           sidebarLayout(
            mainPanel(
            plotOutput("medal_season_plot")),
            sidebarPanel(
            
            fluidRow(
                column(width = 6,
                    radioButtons("medal",
                                 tags$h3(tags$b("Medal")),
                                 choices = all_medals
                    )
                ),
                column(width = 6,
                    radioButtons("season",
                                 tags$h3(tags$b("Season")),
                                 choices = all_seasons
                    )
                )
            ),
            tags$br(),
            tags$a(tags$i("Olympics Website"), href = "https://www.Olympic.org/"))
        )), 
        tabPanel("Second Tab",
                 titlePanel(HTML("<h1>Training</h1>")),
                 sidebarLayout(
                   sidebarPanel(
                            
                            radioButtons("hours_trained",
                                         tags$h3(tags$b("Hours of training per day")),
                                         choices = c("4", "5", "6", "7", "8")
                            ), 
                            tags$br(),
                            tags$a(tags$i("Olympics Website"), href = "https://www.Olympic.org/")
                     ), 
                   mainPanel()
                 )
        )
    )
)

server <- function(input, output){
    output$medal_season_plot <- renderPlot({
        olympics_overall_medals %>%
            filter(team %in% c("United States",
                               "Soviet Union",
                               "Germany",
                               "Italy",
                               "Great Britain")) %>%
            filter(medal == input$medal) %>%
            filter(season == input$season) %>%
            ggplot() +
            aes(x = team, y = count, fill = medal
            ) +
            geom_col()+
            scale_fill_manual(values = c(
                "Gold" = "#DAA520",
                "Silver" = "#C0C0C0", 
                "Bronze" = "#CD7F32"
            ))
    })
}
shinyApp(ui = ui, server = server)
