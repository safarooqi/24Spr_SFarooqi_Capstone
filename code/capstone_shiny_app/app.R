library(shiny)
# devtools::install_github("statistiekcbs/scrollytell")
library(shinyWidgets)
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
library(leaflet)
library(gapminder)
library(readxl)
library(DT)
library(huxtable)
library(table1)
library(kableExtra)
library(sjmisc)
library(mapboxapi)
library(tmap)
library(ggplot2)

## GRADTRENDS cleaned data
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

## ENROLLMENT cleaned data
d_enrollment = read_excel("enrollmentbyracegender.xlsx")

colnames(d_enrollment) = as.character(d_enrollment[1,])

d_enrollment = d_enrollment[-1,]

d_enrollment = d_enrollment %>%
  rename("district_name" = "District Name",
         "district_code" = "District Code", 
         "Black" = "African American",
         "Native_American" = "Native American",
         "Pacific_Islander" = "Native Hawaiian, Pacific Islander",
         "Multi_Race" = "Multi-Race, Non-Hispanic",
         "Non_Binary" = "Non-Binary")

d_enrollment = mutate(d_enrollment, Black = as.numeric(Black),
                      Asian = as.numeric(Asian),
                      Hispanic = as.numeric(Hispanic),
                      White = as.numeric(White),
                      Native_American = as.numeric(Native_American),
                      Pacific_Islander = as.numeric(Pacific_Islander),
                      Multi_Race = as.numeric(Multi_Race), 
                      Males = as.numeric(Males),
                      Females = as.numeric(Females),
                      Non_Binary = as.numeric(Non_Binary))

d_enrollment = d_enrollment %>% mutate(d_enrollment, district_name = str_remove_all(district_name, "((District))"))

d_enrollment = d_enrollment %>% mutate(d_enrollment, district_name = str_remove_all(district_name, "[()]"))

mapping_table <- data.frame(
  district = c("Lexington", "Wellesley", "Dover-Sherborn", "Needham", "Northboro-Southboro", "Hopkinton", "Winchester", "Groton-Dunstable", "Acton-Boxborough", "Westborough"),
  county = c("Middlesex", "Norfolk", "Norfolk, Middlesex", "Norfolk", "Worcester", "Middlesex", "Middlesex", "Middlesex", "Middlesex", "Worcester")
)

d_enrollment$county <- mapping_table$county[match(d_enrollment$district_name, mapping_table$district)]

# --enrollment table--
enrollment_columns <- c("county", "district_name", "White", "Black", "Asian", "Hispanic")

specific_districts <- c("Lexington", "Wellesley", "Dover-Sherborn", "Needham", "Northboro-Southboro", "Hopkinton", "Winchester", "Groton-Dunstable", "Acton-Boxborough", "Westborough")
filtered_district <- d_enrollment %>%
  filter(district_name %in% specific_districts)

enrollment_table <- filtered_district %>%
  select(all_of(enrollment_columns))

enrollment_table$district_name <- factor(enrollment_table$district_name, levels = specific_districts)

enrollment_table <- enrollment_table[order(enrollment_table$district_name), ]

enrollment_table$Asian <- paste0(enrollment_table$Asian, "%")
enrollment_table$Black <- paste0(enrollment_table$Black, "%")
enrollment_table$Hispanic <- paste0(enrollment_table$Hispanic, "%")
enrollment_table$White <- paste0(enrollment_table$White, "%")

# -- racial breakdown table --
table1::label(d_enrollment$White) = "Avg Number of White Students in MA School Districts"
table1::label(d_enrollment$Black) = "Avg # of Black Students in MA School Districts"
table1::label(d_enrollment$Asian) = "Avg # of Asian Students in MA School Districts"
table1::label(d_enrollment$Hispanic) = "Avg # of Hispanic Students in MA School Districts"

dwt <- table1::table1(~ White, data = d_enrollment)
dbt <- table1::table1(~ Black, data = d_enrollment)
dat <- table1::table1(~ Asian, data = d_enrollment)
dht <- table1::table1(~ Hispanic, data = d_enrollment)

## EDUCATION LEVEL cleaned data
h_acs_wide <- get_acs(
  geography = "county",
  variables = c(college = "B17018_024", 
                hs = "B17018_022"), 
  state = "MA",
  year = 2022,
  survey = "acs1",
  geometry = FALSE,
  output = "wide"
)


h_acs_wide <- h_acs_wide %>% 
  mutate(County = str_remove(NAME, " County, Massachusetts"))  


## RACE PLOT cleaned data
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

ma_race_plot <- ma_race_data %>%
  ggplot(aes(x = reorder(NAME, Population), y = Population, fill = Race)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_brewer(palette = "Set2") +  
  theme_minimal() +
  labs(
    title = "Population by Race in MA Counties",
    x = "County",
    y = "Population",
    fill = "Race"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 6)
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

ma_race_plot <- ggplotly(ma_race_plot)

## INCOME MAP cleaned data

my_variables_income <- c(
  Black = "B19001B_001", 
  Hispanic = "B19001I_001",
  Asian = "B19001D_001",
  White = "B19001A_001"
)

d_acs <- get_acs(
  geography = "county",
  variables = my_variables_income, 
  state = "MA",
  year = 2022,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
)

d_acs$NAME <- gsub(" County, Massachusetts", "", d_acs$NAME)

## UI

ui <- fluidPage(
  theme = 'style.css', 
  HTML("<style>@import url('https://fonts.googleapis.com/css?family=Roboto+Slab&display=swap');</style>"),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  tags$head(
    tags$style(HTML("
      .container-fluid { background-color: #fff; width: 1200px; padding: 60px; }
      .topimg { width: 800px; display: block; margin: 0px auto 40px auto; }
      .title { text-align: center; }
      .filters { margin: 0px auto; }
      .shiny-input-container { width:100% !important; }
      .table { padding: 30px; margin-top: 30px; }
      .leaflet-top { z-index:999 !important; }
      "))
  ),
  
  
  fluidRow(
    # Parallax reference: https://www.w3schools.com/howto/howto_css_parallax.asp
    HTML(
      '<style>
        .parallax {
          /* The image used */
          background-image: url("https://ichef.bbci.co.uk/images/ic/1200x675/p01hyv22.jpg");
        
          /* Set a specific height */
          height: 500px;
          
          /* Create the parallax scrolling effect */
          background-attachment: fixed;
          background-position: center;
          background-repeat: no-repeat;
          background-size: cover;
          filter: grayscale(15%) blur(0px) sharpen(2px);
          }
        </style>
        <!-- Container element -->
        <div class="parallax"></div>'
    )
  ),
  
  
  fluidRow(HTML("<center>
                <h1>Inequity Visualized:<br>Segregation in MA Public Schools</h1>
                <p style='size:40px';> by Samiha Farooqi</a></p>
                </center>")
  ),
  br(),
  
  fluidRow(
    column(1),
    column(10,
           # Introduction
           fluidRow(id='text',
                    column(2),
                    column(8, 
                           br(),
                           HTML("<p>For decades,  Massachusetts public schools system have remained highly divided—chiefly by class and race. As <a href='https://www.bostonglobe.com/2020/12/11/opinion/massachusetts-public-schools-are-highly-segregated-its-time-we-treated-that-like-crisis-it-is/' target='_blank'>The Boston Globe</a> 
                           has reported on for years, the state has not worked to adequately integrate its schools, nor has it addressed a myriad of housing reform issues: from the legacy of redlining in, which has racialized neighborhoods and zipcodes, to affordable housing, to whether or not Black or Hispanic families will even reecieve a loan from local banks.
                           With 2024 being the 50th anniversary of the Boston Busing Crisis, in which a federal court mandted that  certain schools in the city of Boston integrate through busing, sparking racial protests, violence, and white flight into the suburbs, my project aims to examine how segregation continues to manifest itself in the state's public education sytem.
                           <br><br>
                           By visualizing the disparities in state exam achievement scores, graduation trends, and geographical location, as well as comparing the enrollment of public schools by race, and showcasing underlying socioeconomic inequality (e.g. average income by race), the enduring effect of segregation in Massachusetts can be seen. 
           <br><br>
           "),
                           br()
                    ),
                    column(2)
           ))),
           
  
  navbarPage(
    " ",
    tabPanel("Education Trends",
             fluidRow(
               column(12, 
                      plotlyOutput("grad_trends_plot", height = "600px"),
                      style = "padding: 20px; background-color: #f8f9fa; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);"
               ),
               column(12, 
                      style = "height: 50px;" 
               ),
               column(12,
                      HTML("<h3 style='font-size: 20px;'>This table displays the districts with the top 10 best achievment scores from the state math exam (MCAS).</h3>
                            <p style='font-size: 17px;'>The breakdown in racial makeup in these districts highlight the geographic divide in race, income, and achievement.</p>"),                     
                      DTOutput("enrollment_table"),
                      style = "padding: 20px; background-color: #f8f9fa; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);"
               ),
               column(12, 
                      style = "height: 50px;"  
               ),
               column(12,
                      plotlyOutput("college_bar_plot", height = "600px"),
                      style = "padding: 20px; background-color: #f8f9fa; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);"
               )
             )
    ),
    tabPanel("Population Breakdown",
             fluidRow(
               column(12, 
                       plotlyOutput("race_plot", height = "600px"),
                       style = "padding: 20px; background-color: #f8f9fa; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);"
               ),
               column(12, 
                      style = "height: 50px;"
                      )
             ),
              fluidRow(
                column(6, #white
                      plotlyOutput("enroll_plot1", height = "550px"),
                      style = "padding: 20px; background-color: #f8f9fa; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);"
               ),
               column(6, #black
                      plotlyOutput("enroll_plot2", height = "550px"),
                      style = "padding: 20px; background-color: #f8f9fa; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);"
               ),
               column(6, #asian
                      plotlyOutput("enroll_plot3", height = "550px"),
                      style = "padding: 20px; background-color: #f8f9fa; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);"
               ),
               column(6, #hispanic
                      plotlyOutput("enroll_plot4", height = "550px"),
                      style = "padding: 20px; background-color: #f8f9fa; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);"
               )
               )
    ),
    tabPanel("Mapping",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable", "Select Race", choices = c("Black", "Hispanic", "Asian", "White"))
               ),
               mainPanel(
                 leafletOutput("map")
               )
             )
    )
  )
)

  


## SERVER  
server <- function(input, output) {
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(0, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  selected_variable <- reactive({
      variable <- input$variable
      d_acs[[variable]]
    })
  
  
#--GRAD TRENDS PLOT--  
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
  
#--ENROLLMENT TABLE--  
  output$enrollment_table <- renderDT({
    datatable(enrollment_table,              
              colnames = c("County", "District Name", "White", "Black", "Asian", "Hispanic"),
              options = list(
                dom = 't',
                paging = FALSE,
                ordering = FALSE
                )
              )
    })

#--COLLEGE PLOT--  
  output$college_bar_plot <- renderPlotly({
    college_bar_plot <- h_acs_wide %>% 
      ggplot(aes(x = reorder(County, collegeE), y = collegeE)) +
      geom_col(alpha = 0.8, width = 0.7, fill = "#9ecae1") +  
      theme_few() + 
      labs(title = "Income of MA Families with a Bachelor's Degree or higher", 
           subtitle = "Bachelor's Degree or higher", 
           y = "Average Income",  
           x = "County",
           caption = "Source: 2022 ACS")   
    
    ggplotly(college_bar_plot, tooltip = "text")
  })
  
#--POPULATION BY RACE PLOT--  
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
      layout( xaxis = list(tickfont = list(size = 9)))
  })
  
#--RACE ENROLLMENT PLOT: WHITE--
   output$enroll_plot1 <- renderPlotly({
      enroll_fig1 <- plot_ly(data = d_enrollment, 
                             x = ~district_name, 
                             y = ~White, 
                             type = 'scatter',
                             text = paste("District:", d_enrollment$district_name,
                                          "<br>Enrollment:", d_enrollment$White,"%"),
                             hoverinfo = 'text',
                             mode = 'markers',
                             marker = list(color = '#70569C')
      ) %>%
        layout(title = 'Enrollment of White Students by School District',
               plot_bgcolor='#F4EEFE',
               margin = list(
                 b = 50,
                 t = 50),
               xaxis = list(
                 title = 'Districts',
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 showticklabels=FALSE),
               yaxis = list(
                 title = 'Percentage Enrollment',
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'))
      })
  
#--RACE ENROLLMENT PLOT: BLACK--
   output$enroll_plot2 <- renderPlotly({
     enroll_fig1 <- plot_ly(data = d_enrollment, 
                            x = ~district_name, 
                            y = ~Black, 
                            type = 'scatter',
                            text = paste("District:", d_enrollment$district_name,
                                         "<br>Enrollment:", d_enrollment$Black,"%"),
                            hoverinfo = 'text',
                            mode = 'markers',
                            marker = list(color = '#70569C')
     ) %>%
       layout(title = 'Enrollment of Black Students by School District',
              plot_bgcolor='#F4EEFE',
              margin = list(
                b = 50,
                t = 50),
              xaxis = list(
                title = 'Districts',
                zerolinecolor = '#ffff',
                zerolinewidth = 2,
                gridcolor = 'ffff',
                showticklabels=FALSE),
              yaxis = list(
                title = 'Percentage Enrollment',
                zerolinecolor = '#ffff',
                zerolinewidth = 2,
                gridcolor = 'ffff'))
   })
  
#--RACE ENROLLMENT PLOT: ASIAN--
   output$enroll_plot3 <- renderPlotly({
     enroll_fig1 <- plot_ly(data = d_enrollment, 
                            x = ~district_name, 
                            y = ~Asian, 
                            type = 'scatter',
                            text = paste("District:", d_enrollment$district_name,
                                         "<br>Enrollment:", d_enrollment$Asian,"%"),
                            hoverinfo = 'text',
                            mode = 'markers',
                            marker = list(color = '#70569C')
     ) %>%
       layout(title = 'Enrollment of Asian Students by School District',
              plot_bgcolor='#F4EEFE',
              margin = list(
                b = 50,
                t = 50),
              xaxis = list(
                title = 'Districts',
                zerolinecolor = '#ffff',
                zerolinewidth = 2,
                gridcolor = 'ffff',
                showticklabels=FALSE),
              yaxis = list(
                title = 'Percentage Enrollment',
                zerolinecolor = '#ffff',
                zerolinewidth = 2,
                gridcolor = 'ffff'))
   })  
  
#--RACE ENROLLMENT PLOT: HISPANIC--
   output$enroll_plot4 <- renderPlotly({
     enroll_fig1 <- plot_ly(data = d_enrollment, 
                            x = ~district_name, 
                            y = ~Hispanic, 
                            type = 'scatter',
                            text = paste("District:", d_enrollment$district_name,
                                         "<br>Enrollment:", d_enrollment$Hispanic,"%"),
                            hoverinfo = 'text',
                            mode = 'markers',
                            marker = list(color = '#70569C')
     ) %>%
       layout(title = 'Enrollment of Hispanic Students by School District',
              plot_bgcolor='#F4EEFE',
              margin = list(
                b = 50,
                t = 50),
              xaxis = list(
                title = 'Districts',
                zerolinecolor = '#ffff',
                zerolinewidth = 2,
                gridcolor = 'ffff',
                showticklabels=FALSE),
              yaxis = list(
                title = 'Percentage Enrollment',
                zerolinecolor = '#ffff',
                zerolinewidth = 2,
                gridcolor = 'ffff'))
   })
  
#--INCOME MAP--  
  output$map <- renderLeaflet({
    race_variable <- input$variable  
    
    avg_income <- d_acs %>%
      filter(variable == race_variable) %>%
      group_by(NAME) %>%
      summarize(average_income = mean(estimate, na.rm = TRUE))
    
    overall_min <- min(avg_income$average_income, na.rm = TRUE)
    overall_max <- max(avg_income$average_income, na.rm = TRUE)
    
    leaflet(data = avg_income) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorNumeric(palette = "Greens", domain = c(overall_min, overall_max))(average_income),
        fillOpacity = 0.7,
        weight = 1,
        highlight = highlightOptions(
          weight = 5,
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste(NAME,"\n", "Average Income:", round(average_income, 2)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        position = "topright",
        pal = colorNumeric("Greens", domain = c(overall_min, overall_max)),
        values = avg_income$average_income,
        title = "ACS Estimate, 2022",
        labFormat = labelFormat(suffix = ""),
        opacity = 1
      ) 
  })
  
}

shinyApp(ui = ui, server = server)

  
  
  
  
  
  
  
