library(shiny)
library(tidyverse)
library(shinythemes)
library(CodeClanData)
library(ggthemes)
library(viridis)

selected_game_sales <- game_sales %>% 
  select(-c("name")) %>% 
  mutate(
    sales_width = cut_width(sales, 20)
  )
selected_game_sales_ui <- selected_game_sales %>% 
  select(-sales_width)

ui <- fluidPage(
  tabsetPanel(
      tabPanel("Basic analysis",
      fluidRow(
          column(3,
                 varSelectInput("variable", 
                                HTML("<br/><br/><h3>Variable</h3>"), 
                                selected_game_sales_ui), 
                 radioButtons("colour",
                              "Do you want to change the colour for year_of_release?",
                              choices = c(dark_blue = "#000839", red = "red"))
          ),
          
          column(9,
                 plotOutput("bar_chart"),
                 textOutput("text_output")
          )
      )
    ), 
    tabPanel("Deeper analysis",
             fluidRow(
               column(3,
                      selectInput("variable_deeper_analysis", 
                                     HTML("<br/><br/><h3>Variable</h3>"), 
                                     choices = c(
                                                 "ratio_critic_score", 
                                                 "ratio_user_score",
                                                 "critic_score_vs_user_score",
                                                 "count_yr_genre", 
                                                 "sum_yr_genre", 
                                                 "sum_yr_publisher",
                                                 "sum_yr_rating",
                                                 "sum_yr_platform"
                                                 )
                                     )
               ),
               
               column(9,
                      plotOutput("bar_chart_2"),
                      textOutput("text_output_2")
               )
             )
             )
    )
             
)

server <- function(input, output) {
    output$bar_chart <- renderPlot({
        if(!!input$variable == "genre"){
          genre_reordered <- selected_game_sales %>% 
            mutate(genre = reorder(genre, genre, FUN=length))
          ggplot(genre_reordered) +
            aes(!!input$variable, fill = !!input$variable) + 
            geom_bar() +
            coord_flip() +
            scale_fill_viridis_d()  +
            scale_y_continuous(breaks = seq (0, 600, by = 100)) +
            labs(title = "\n Distribution of video games per genre \n") + 
            theme(title = element_text(size = 22, face = "bold", colour = "black"), 
                  legend.position = "none", 
                  axis.text = element_text(size = 12))
        } else if(!!input$variable == "year_of_release"){
          ggplot(selected_game_sales) +
            aes(x = year_of_release) + 
            geom_bar(fill = input$colour) +
            scale_x_continuous(breaks = seq (1980, 2020, by = 2)) +
            labs(title = "\n Distribution of video games per year of release \n") + 
            theme(title = element_text(size = 22, face = "bold", colour = "black"), 
                  axis.text = element_text(size = 12))
         } else if(!!input$variable == "publisher"){
           publisher_reordered <- selected_game_sales %>% 
             mutate(publisher = reorder(publisher, publisher, FUN=length))
            ggplot(publisher_reordered) +
              aes(!!input$variable, fill = !!input$variable) + 
              geom_bar() +
              coord_flip() +
              scale_fill_viridis_d()  +
              scale_y_continuous(breaks = seq (0, 700, by = 100)) +
              labs(title = "\n Distribution of video games per publisher \n") + 
              theme(title = element_text(size = 22, face = "bold", colour = "black"), 
                    legend.position = "none", 
                    axis.text = element_text(size = 12))
        } else if(!!input$variable == "sales"){
          ggplot(selected_game_sales) +
            aes(x = sales) + 
            geom_histogram(col = "white", fill = "#000839") +
            facet_wrap(~sales_width, scales = "free_y") +
            scale_x_continuous(breaks = seq(0, 100, 5), labels = scales::dollar_format(prefix = "", suffix = "M")) +
            labs(title = "\n Distribution of video games per number of copies sold by each one \n") + 
            theme(title = element_text(size = 22, face = "bold", colour = "black"))
        } else if (!!input$variable == "critic_score"){
          ggplot(selected_game_sales) +
            aes(x = critic_score) + 
            geom_histogram(col = "white", fill = "#000839") +
            scale_x_continuous(breaks = seq (0, 100, by = 10)) +
            labs(title = "\n Distribution of video games per critic score \n") + 
            theme(title = element_text(size = 22, face = "bold", colour = "black"), 
                  axis.text = element_text(size = 12))
        } else if (!!input$variable == "user_score"){
            ggplot(selected_game_sales) +
            aes(x = user_score) + 
            geom_histogram(col = "white", fill = "#000839") + 
            scale_x_continuous(breaks = seq (0, 10, by = 1)) +
            labs(title = "\n Distribution of video games per user score \n") + 
            theme(title = element_text(size = 22, face = "bold", colour = "black"), 
                  axis.text = element_text(size = 12))
        } else if (!!input$variable == "developer"){
          developer_reordered <- selected_game_sales %>% 
            mutate(developer = reorder(developer, developer, FUN=length))
            ggplot(developer_reordered) +
            aes(!!input$variable, fill = !!input$variable) + 
            geom_bar() +
            coord_flip() +
            scale_fill_viridis_d()  +
            scale_y_continuous(breaks = seq (0, 700, by = 100)) +
            labs(title = "\n Distribution of video games per developer \n") + 
            theme(title = element_text(size = 22, face = "bold", colour = "black"), 
                  legend.position = "none", 
                  axis.text = element_text(size = 12))
        } else if (!!input$variable == "rating") {
          rating_reordered <- selected_game_sales %>% 
            mutate(rating = fct_relevel(rating, c("E", "E10+", "T", "M")))
          ggplot(rating_reordered) +
            aes(!!input$variable, fill = !!input$variable) + 
            geom_bar() +
            scale_fill_viridis_d()  +
            labs(title = "\n Distribution of video games per rating \n") + 
            theme(title = element_text(size = 22, face = "bold", colour = "black"), 
                  legend.position = "none", 
                  axis.text = element_text(size = 12))
        } else if (!!input$variable == "platform") {
          platform_reordered <- selected_game_sales %>% 
            mutate(platform = reorder(platform, platform, FUN=length))
            ggplot(platform_reordered) +
            aes(!!input$variable, fill = !!input$variable) + 
            geom_bar() +
            coord_flip() +
            scale_fill_viridis_d() + 
            scale_y_continuous(breaks = seq (0, 300, by = 50)) +
            labs(title = "\n Distribution of video games per platform \n") + 
            theme(title = element_text(size = 22, face = "bold", colour = "black"), 
                  legend.position = "none", 
                  axis.text = element_text(size = 12))
    }
  })
    
    output$text_output <- renderText({
      if(!!input$variable == "genre"){
        HTML("The genres with more video games produced are Sports and Action.")
      } else if(!!input$variable == "year_of_release"){
        HTML("The peak of the number of new video games released occurred from 2002 to 2009.")
      } else if(!!input$variable == "publisher"){
        HTML("The main publisher in the History of video games is Electronic Arts.")
      } else if(!!input$variable == "sales"){
        HTML("Most of the video games sold less than 10 million copies, with only a few outliers.")
      } else if(!!input$variable == "critic_score"){
        HTML("The critic score has an average value of 74 out of 100.")
      } else if(!!input$variable == "user_score"){
        HTML("The user score has an average value of 7.2 out of 10, being really similar to the average critic score value.")
      } else if(!!input$variable == "developer"){
        HTML("The main developer in the History of video games is Electronic Arts.")
      } else if(!!input$variable == "rating"){
        HTML('The first rating in numbers of video games sold is "Everyone". The second rating most populated is 
             "Teenagers". Probably because teenagers dedicate many hours to play video games')
      } else if(!!input$variable == "platform"){
        HTML("The first platform in numbers of video games sold was the PS2, followed by the X360 and the PS3.")
      }
    })
    
    output$bar_chart_2 <- renderPlot({
      if(!!input$variable_deeper_analysis == "ratio_critic_score"){
        ratio_critic_score <- game_sales %>% 
          group_by(critic_score) %>% 
          mutate(ratio = sum(sales)/n())
        ggplot(ratio_critic_score) +
          aes(x = critic_score, y = ratio) +
          geom_line()
      } else if(!!input$variable_deeper_analysis == "ratio_user_score"){
        ratio_user_score <- game_sales %>% 
          group_by(user_score) %>% 
          mutate(ratio = sum(sales)/n())
        ggplot(ratio_user_score) +
          aes(x = user_score, y = ratio) +
          geom_line()
      } else if(!!input$variable_deeper_analysis == "critic_score_vs_user_score"){
        ggplot(game_sales) +
          aes(x = critic_score, y = user_score) +
          geom_line()
      }
      else if(!!input$variable_deeper_analysis == "count_yr_genre"){
        count_yr_genre <- game_sales %>% 
          group_by(year_of_release, genre) %>% 
          mutate(count = n())
        ggplot(count_yr_genre) +
          aes(x = year_of_release, y = count, colour = genre) +
          geom_line()
      } else if(!!input$variable_deeper_analysis == "sum_yr_genre"){
        sum_yr_genre <- game_sales %>% 
          group_by(year_of_release, genre) %>% 
          mutate(sum_sales = sum(sales))
        ggplot(sum_yr_genre) +
          aes(x = year_of_release, y = sum_sales, colour = genre) +
          geom_line()
      }  else if(!!input$variable_deeper_analysis == "sum_yr_publisher"){
        sum_yr_publisher <- game_sales %>% 
          group_by(year_of_release, publisher) %>% 
          mutate(sum_sales = sum(sales))
        ggplot(sum_yr_publisher) +
          aes(x = year_of_release, y = sum_sales, colour = publisher) +
          geom_line()
      } else if(!!input$variable_deeper_analysis == "sum_yr_rating"){
        sum_yr_rating <- game_sales %>% 
          group_by(rating, year_of_release) %>% 
          mutate(sum_sales = sum(sales))
        ggplot(sum_yr_rating) +
          aes(x = year_of_release, y = sum_sales, colour = rating) +
          geom_line()
      } else if(!!input$variable_deeper_analysis == "sum_yr_platform"){
        sum_yr_platform <- game_sales %>% 
          group_by(platform, year_of_release) %>% 
          mutate(sum_sales = sum(sales))
        ggplot(sum_yr_platform) +
          aes(x = year_of_release, y = sum_sales, colour = platform) +
          geom_line()
      }
      })
    
    output$text_output_2 <- renderText({
      if(!!input$variable_deeper_analysis == "ratio_critic_score"){
        HTML("The number of video games sold increases along with the critic score")
      } else if(!!input$variable_deeper_analysis == "ratio_user_score"){
        HTML("The number of video games sold increases along with the user score")
      } else if(!!input$variable_deeper_analysis == "critic_score_vs_user_score"){
        HTML("The user score increases with the critic score")
      } else if(!!input$variable_deeper_analysis == "count_yr_genre"){
        HTML("The largest number of new video games released occurred from 2002 to 2009, with sports
             and action genres especially peaking during that period.")
      } else if(!!input$variable_deeper_analysis == "sum_yr_genre"){
        HTML("The biggest amount of sales in video games occurred from 2005 to 2010, with the sports
             genre especially peaking during that period.")
      } else if(!!input$variable_deeper_analysis == "sum_yr_publisher"){
        HTML("In 2016, Electronic Arts was the main publisher. From 2004 to 2010 Nintendo peaked, being by far the main publisher (probably because of the Wii)")
      } else if(!!input$variable_deeper_analysis == "sum_yr_rating"){
        HTML('From 2002 to 2010, the "Everyone" rating peaked being by far the rating with most video games sold. In 2016 it 
             was still ahead of the other ratings, but the difference was much smaller')
      } else if(!!input$variable_deeper_analysis == "sum_yr_rating"){
        HTML('The first rating in numbers of video games sold is "Everyone". The second rating most populated is 
             "Teenagers". Probably because teenagers dedicate many hours to play video games')
      } else if(!!input$variable_deeper_analysis == "sum_yr_platform"){
        HTML("The first platform in numbers of video games sold in 2016 was the PS4. The sales of games for the Wii console peaked just after it apeared in 2006, being by 
             far the first in ranking by platform, and remaining like that until almost 2010.")
      }
    })
}

shinyApp(ui = ui, server = server)