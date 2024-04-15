# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white',
#              xlab = 'Waiting time to next eruption (in mins)',
#              main = 'Histogram of waiting times')
#     })
# }

library(shiny)
library(scrollytell)
library(shinyjs)
library(ggvis)
library(plotly)

library(tidyverse)
library(tidycensus)
library(tigris)
library(sf) 
library(tmap)
library(dplyr)
library(ggthemes)
library(RColorBrewer)
library(plotly)
library(gapminder)
library(readxl)
library(huxtable)
library(table1)
library(kableExtra)
library(sjmisc)
library(mapboxapi)
library(tmap)
library(ggplot2)

theme_set(theme_minimal())

# clean data
gradtrends = read_excel("statetrends.xlsx")

gradtrends$`2021 4 yr` = as.character(gradtrends$`2021 4 yr`)
gradtrends$`2020 4 yr` = as.character(gradtrends$`2020 4 yr`)
gradtrends$`2019 4 yr` = as.character(gradtrends$`2019 4 yr`)
gradtrends$`2018 4yr` = as.character(gradtrends$`2018 4yr`)
gradtrends$`2017 4yr` = as.character(gradtrends$`2017 4yr`)
gradtrends$`2016 4yr` = as.character(gradtrends$`2016 4yr`)
gradtrends$`2015 4yr` = as.character(gradtrends$`2015 4yr`)
gradtrends$`2014 4yr` = as.character(gradtrends$`2014 4yr`)
gradtrends$`2013 4yr` = as.character(gradtrends$`2013 4yr`)
gradtrends$`2012 4yr` = as.character(gradtrends$`2012 4yr`)
gradtrends$`2011 4yr` = as.character(gradtrends$`2011 4yr`)


gradtrends = gradtrends %>% add_row(Group = 'Year',
                                    '2021 4 yr' = '2021',
                                    '2020 4 yr' = '2020',
                                    '2019 4 yr' = '2019',
                                    '2018 4yr' = '2018',
                                    '2017 4yr' = '2017',
                                    '2016 4yr' = '2016',
                                    '2015 4yr' = '2015',
                                    '2014 4yr' = '2014',
                                    '2013 4yr' = '2013',
                                    '2012 4yr' = '2012',
                                    '2011 4yr' = '2011',
                                    '2010 4yr' = '2010',
                                    '2009 4 yr' = '2009',
                                    '2008 4yr' = '2008',
                                    '2007 4yr' = '2007',
                                    '2006 4yr' = '2006',
                                    .before=1)


gradtrends_long = gradtrends %>% 
  rotate_df()

colnames(gradtrends_long) = as.character(gradtrends_long[1,])

gradtrends_long = gradtrends_long[-1,]

gradtrends_long = gradtrends_long %>%
  rename("Black" = "African-American")

gradtrends_long = gradtrends_long %>%
  pivot_longer(c('Black', 'Asian', 'Hispanic', 'White'),
               names_to = 'race',
               values_to = 'grad_rate')

gradtrends_long$grad_rate = as.numeric(gradtrends_long$grad_rate)

gradtrends_long$Year = as.character(gradtrends_long$Year)

gradtrends_long = na.omit(gradtrends_long)

gradtrends_long <- gradtrends_long %>%
  mutate(reveal = case_when(
    race == "Asian" ~ 1,
    race == "White" ~ 2,
    race == "Black" ~ 3,
    race == "Hispanic" ~ 4
  ))
  

# plot

# gradplot <- gt %>% filter(if (add != 5) add >= reveal else reveal %in% c(1:5)) %>%
#   ggplot(mapping = aes(x = Year, y = grad_rate, color = race, na.rm = TRUE)) +
#   geom_line(aes(group = race, fill = race, size=TOT_EMP,
#                 alpha=ifelse(add == reveal, 1/5, 1/10), 
#                 text = glue::glue('<b>Graduation Rate</b>: {grad_rate}%
#                                           <b>Race</b>: {race}'))) +
#   labs(title = "MA High School Graduation Rate, 2006 - 2020", 
#        caption = "Source: MA DESE",
#        x = "Date",
#        y = "Graduation Rate",
#        color = "4-year Graduation Rates, by race"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
#         axis.text.y = element_text(vjust = 0.2, hjust= 0.5),
#         plot.title = element_text(hjust = 0.5), 
#         plot.caption = element_text(hjust = 0, face = "italic")) + 
#   scale_color_brewer(palette = "Set2") + 
#   scale_y_continuous(labels = function(y) paste0(y, '%'))

server <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    
    add <- input$scr
    
      # static ggplot goes here
      
    accumulate_by = function(dat, var) {
      var = lazyeval::f_eval(var, dat)
      lvls = plotly:::getLevels(var)
      dats = lapply(seq_along(lvls), function(x) {
        cbind(dat[var %in% lvls[seq(0, x)], ], frame = lvls[[x]])
      })
      dplyr::bind_rows(dats)
    }
    
    gt = gradtrends_long 
    gt$Year = as.numeric(gt$Year)
    gt_fig = gt 
    gt_fig = gt_fig %>% accumulate_by(~Year)
    
    
    gt_fig = gt_fig %>%
      plot_ly(
        x = ~Year, 
        y = ~grad_rate,
        color = ~race,
        frame = ~frame, 
        colors = "Set2",
        type = 'scatter',
        mode = 'lines', 
        line = list(simplyfy = F)) 
    
    gt_fig = gt_fig %>% layout(
      title = "MA High School Graduation Trends, 2006-2021",
      margin = list(
        b = 65,
        t = 50
      ),
      xaxis = list(
        title = "Year",
        zeroline = F),
      yaxis = list(
        title = "Percent Graduated",
        zeroline = F)) 
    
    gt_fig = gt_fig %>% animation_opts(
      frame = 100, 
      transition = 0, 
      # redraw = TRUE) 
      redraw = FALSE) 
    gt_fig = gt_fig %>% animation_slider(
      hide = T) 
    
    gt_fig = gt_fig %>% animation_button(
      x = 1, xanchor = "right", y = 0, yanchor = "bottom")
      })
  
  # render the plot here
  output$scr <- renderScrollytell({scrollytell()})
  renderText(paste0("Section: ", input$scr))
  observe({cat("section:", input$scr, "\n")})
  
}


# Run the application 
shinyApp(ui = ui, server = server)
