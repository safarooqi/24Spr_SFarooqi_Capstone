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


server <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    
    add <- input$scr
    
    gradplot <-
      
      ggplotly(gradplot)})
  
  # render the plot here
  output$scr <- renderScrollytell({scrollytell()})
  renderText(paste0("Section: ", input$scr))
  observe({cat("section:", input$scr, "\n")})
  
}


# plot

gradplot <- gradtrends_long %>% 
  filter(if (add != 5) add >= reveal else reveal %in% c(1:5)) %>%
  ggplot(mapping = aes(x = Year, y = grad_rate, color = race, na.rm = TRUE)) +
  geom_line(aes(group = race, fill = race, size=TOT_EMP,
                alpha=ifelse(add == reveal, 1/5, 1/10),
                text = glue::glue('<b>Graduation Rate</b>: {grad_rate}%
                                          <b>Race</b>: {race}'))) +
  labs(title = "MA High School Graduation Rate, 2006 - 2020",
       caption = "Source: MA DESE",
       x = "Date",
       y = "Graduation Rate",
       color = "4-year Graduation Rates, by race"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        axis.text.y = element_text(vjust = 0.2, hjust= 0.5),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic")) +
  scale_color_brewer(palette = "Set2") +
  scale_y_continuous(labels = function(y) paste0(y, '%'))



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


#######

##UI
ui <- fluidPage(
  theme = 'style.css', 
  HTML("<style>@import url('https://fonts.googleapis.com/css?family=Roboto+Slab&display=swap');</style>"),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  tags$head(
    tags$style(HTML("
      body {
      font-family: 'Helvetica'; }
      .topimg { width: 120px; display: block; margin: 0px auto 40px auto; }
      .title { text-align: center; }
      "))
  ),
  
  
  fluidRow(HTML("<center>
                <h1>Inequity Visualized:<br>Segregation in the MA Public Education System</h1>
                <p style='size:25px';> by Samiha Farooqi</a></p>
                </center>")
  ),
  br(),
  
  
  mainPanel(
    tabPanel("Graduation Trends", 
             div(plotlyOutput("grad_trends_plot", height = "100%"), align = "center"),
             style = "padding: 20px; background-color: #f8f9fa; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);"
    ),
    tabPanel("MA Counties by Race", 
             div(plotlyOutput("race_plot", height = "100%"), align = "center"),
             style = "padding: 20px; background-color: #f8f9fa; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);"
    )
  ))



##SERVER  
server <- function(input, output) {
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(0, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  output$grad_trends_plot <- renderPlotly({
    gt <- gradtrends_long 
    gt$Year <- as.numeric(gt$Year)
    gt_fig <- gt 
    gt_fig <- gt_fig %>% accumulate_by(~Year)
    
    gt_fig <- gt_fig %>%
      plot_ly(
        x = ~Year, 
        y = ~grad_rate,
        color = ~race,
        frame = ~frame, 
        colors = "Set2",
        type = 'scatter',
        mode = 'lines', 
        line = list(simplyfy = F)) 
    
    gt_fig <- gt_fig %>% layout(
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
    
    gt_fig <- gt_fig %>% animation_opts(
      frame = 100, 
      transition = 0, 
      redraw = FALSE) 
    gt_fig <- gt_fig %>% animation_slider(
      hide = T) 
    
    gt_fig <- gt_fig %>% animation_button(
      x = 1, xanchor = "right", y = 0, yanchor = "bottom")
    
    return(gt_fig)
  })
  
  output$race_plot <- renderPlotly({
    ma_race_data <- get_acs(
      geography = "county",
      variables = c(
        "White" = "B03002_003",
        "Black" = "B03002_004",
        "Native" = "B03002_005",
        "Asian" = "B03002_006",
        "Hispanic" = "B03002_012",
        "Pacific_Islander" = "B03002_007",
        "Other_Race" = "B03002_008",
        "Two_or_More_Races" = "B03002_009"
      ),
      state = "MA",
      geometry = FALSE,
      output = "wide"
    )
    
    ma_race_data <- ma_race_data %>%
      rename("Black" = "BlackE",
             "White" = "WhiteE",
             "Asian" = "AsianE",
             "Hispanic" = "HispanicE") %>%
      pivot_longer(c('Black', 'White', 'Asian', 'Hispanic'),
                   names_to = 'Race',
                   values_to = 'Population') %>%
      mutate(NAME = str_remove_all(NAME, " County, Massachusetts"))
    
    race_plot <- ma_race_data %>%
      ggplot(aes(x = reorder(NAME, -Population), y = Population, fill = Race, 
                 text = paste("County: ", NAME, "<br>", 
                              "Race: ", Race, "<br>", 
                              "Population: ", Population))) +
      geom_col(position = "stack", width = 0.7) +
      scale_fill_brewer(palette = "Set2") +  
      theme_minimal() +
      labs(
        title = "Population by Race in MA Counties",
        x = "County",
        y = "Population",
        fill = "Race") +
      # theme(
      #   axis.text.x = element_text(angle = 45, hjust = 1, size = 2),
      #   axis.text = element_text(size = 2)
      # ) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()
    
    ggplotly(race_plot, tooltip = "text") %>% 
      layout( xaxis = list(tickfont = list(size = 10)))
  })
}

####


output$map <- renderLeaflet({
  variable <- input$variable
  leaflet(data = d_acs) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~colorNumeric(palette = "Greens", domain = d_acs$estimate)(estimate),
      fillOpacity = 0.7,
      weight = 1,
      highlight = highlightOptions(
        weight = 5,
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~paste(NAME,"\n", "Average Income:", estimate, selected_variable()),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      position = "topright",
      pal = colorQuantile("Greens", selected_variable()),
      values = d_acs$estimate,
      title = "ACS Estimate, 2022",
      labFormat = labelFormat(suffix = ""),
      opacity = 1
    ) 
})
}

shinyApp(ui = ui, server = server)




