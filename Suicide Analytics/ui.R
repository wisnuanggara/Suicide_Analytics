# Load necessary packages. 
library(flexdashboard) # Dashboard package
library(highcharter) # Interactive data visualizations
library(viridis) # Color gradients
library(tidyverse) # Metapackge
library(countrycode) # Converting country names/codes
library(DT) # Displaying data tables
library(crosstalk) # Provides interactivity for HTML widgets
library(plotly) # Interactive data visualizations
library(shinydashboard)

data <- read.csv('master.csv') %>%
  filter(year != 2016, # filter out 2016 and countries with 0 data. 
         country != 'Dominica',
         country != 'Saint Kitts and Nevis')
# Fix the names of some of the countries in our data to match the country names 
# used by our map later on so that they'll be interpreted and displayed. 
data <- data %>%
  mutate(country = fct_recode(country, "The Bahamas" = "Bahamas"),
         country = fct_recode(country, "Cape Verde" = "Cabo Verde"),
         country = fct_recode(country, "South Korea" = "Republic of Korea"),
         country = fct_recode(country, "Russia" = "Russian Federation"),
         country = fct_recode(country, "Republic of Serbia" = "Serbia"),
         country = fct_recode(country, "United States of America" = "United States"))
# Reorder levels of age to be in chronological order. 
data$age <- factor(data$age, levels = c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years"))

country_year_tibble <- data %>%
  select(country, year, suicides_no, population) %>%
  group_by(country, year) %>%
  summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) 

shared_data <- SharedData$new(country_year_tibble, group = 'hello')

set.seed(80085)


shinyUI(
  dashboardPage(
    dashboardHeader(
      title = "Suicides Analytics"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("WordWide", tabName = "dashboard", icon = icon("globe")),
        menuItem("Continents", tabName = "continents", icon = icon("globe-europe")),
        menuItem("Countries", tabName = "countries", icon = icon("flag-usa")),
        menuItem("Search", tabName = "search", icon = icon("search"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(
                  tabBox(
                    height = "250px", width = 12,
                    tabPanel("Worldwide suicides", highchartOutput("WorldwideSuicides")),
                    tabPanel("Worldwide suicides by Gender", highchartOutput("WorldwideSuicidesByGender")),
                    tabPanel("Worldwide suicides by Age", highchartOutput("WorldwideSuicidesByAge"))
                  )
          )
        ),
        tabItem(tabName = "continents",
                fluidRow(
                  box(width = 12,
                      highchartOutput("continent")
                  ),
                  box(
                    highchartOutput("continentGender")
                  ),
                  box(
                    highchartOutput("continentAge")
                  )
                )
        ),
        tabItem(tabName = "countries",
                fluidRow(
                  box(width = 12,
                      highchartOutput("country")),
                  tabBox(
                    height = "250px", width = 12,
                    tabPanel("By County", highchartOutput("bycountry")),
                    tabPanel("By Gender", highchartOutput("bygender")),
                    tabPanel("By Age", highchartOutput("byage"))
                  )
                )
        ),
        tabItem(tabName = "search",
                fluidRow(
                    box(width = 12,
                        plot_ly(shared_data, x = ~year, y = ~suicide_capita, 
                                color = ~country, colors = sample(colours(), 120),
                                type = 'scatter', mode = 'lines',
                                hoverinfo = 'text', text = ~paste("Country: ", country, '<br>Year: ', year, "<br>Suicides: ", suicide_capita)) %>%
                          layout(showlegend = FALSE,
                                 title = "Suicide by Country",
                                 xaxis = list(title = "Year"),
                                 yaxis = list(title = "Suicides per 100K people")) %>%
                          layout(plot_bgcolor = 'transparent') %>% 
                          layout(paper_bgcolor = 'transparent') %>% 
                          add_markers() %>% 
                          highlight("plotly_click")
                    ),
                    box(width = 12,
                        filter_slider("year", "Year", shared_data, ~year, step = 1),
                        filter_select("country", "Country", shared_data, ~country, allLevels = TRUE, multiple = TRUE),
                        datatable(shared_data,
                                  rownames = FALSE,
                                  colnames = c('Country', 'Year', 'Suicides /100K'),
                                  class = 'cell-border stripe',
                                  width = '100%',
                                  extensions = "Scroller",
                                  options=list(deferRender = FALSE, 
                                               scrollY = 370, 
                                               scrollCollapse = TRUE,
                                               scroller = TRUE,
                                               dom = 't'))
                    )
                  
                )
        )
        
      )
    )
  )
)
