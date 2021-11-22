# Load necessary packages. 
library(flexdashboard) # Dashboard package
library(highcharter) # Interactive data visualizations
library(viridis) # Color gradients
library(tidyverse) # Metapackge
library(countrycode) # Converting country names/codes
library(DT) # Displaying data tables
library(crosstalk) # Provides interactivity for HTML widgets
library(plotly) # Interactive data visualizations


data <- read.csv('master.csv') %>%
  filter(year != 2016, # filter out 2016 and countries with 0 data. 
         country != 'Dominica',
         country != 'Saint Kitts and Nevis')

data <- data %>%
  mutate(country = fct_recode(country, "The Bahamas" = "Bahamas"),
         country = fct_recode(country, "Cape Verde" = "Cabo Verde"),
         country = fct_recode(country, "South Korea" = "Republic of Korea"),
         country = fct_recode(country, "Russia" = "Russian Federation"),
         country = fct_recode(country, "Republic of Serbia" = "Serbia"),
         country = fct_recode(country, "United States of America" = "United States"))

data$age <- factor(data$age, levels = c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years"))


custom_theme <- hc_theme(
  colors = c('#5CACEE', 'green', 'red'),
  chart = list(
    backgroundColor = '#FAFAFA', 
    plotBorderColor = "black"),
  xAxis = list(
    gridLineColor = "C9C9C9", 
    labels = list(style = list(color = "#333333")), 
    lineColor = "#C9C9C9", 
    minorGridLineColor = "#C9C9C9", 
    tickColor = "#C9C9C9", 
    title = list(style = list(color = "#333333"))), 
  yAxis = list(
    gridLineColor = "#C9C9C9", 
    labels = list(style = list(color = "#333333")), 
    lineColor = "#C9C9C9", 
    minorGridLineColor = "#C9C9C9", 
    tickColor = "#C9C9C9", 
    tickWidth = 1, 
    title = list(style = list(color = "#333333"))),   
  title = list(style = list(color = '#333333', fontFamily = "Lato")),
  subtitle = list(style = list(color = '#666666', fontFamily = "Lato")),
  legend = list(
    itemStyle = list(color = "#333333"), 
    itemHoverStyle = list(color = "#FFF"), 
    itemHiddenStyle = list(color = "#606063")), 
  credits = list(style = list(color = "#666")),
  itemHoverStyle = list(color = 'gray'))



shinyServer(function(input, output) {
  
  #worldwide
  ### Worldwide suicides {.no-padding}
  overall_tibble <- data %>%
    select(year, suicides_no, population) %>%
    group_by(year) %>%
    summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) 
  
  sex_color <- c("#EE6AA7", "#87CEEB") # baby blue & pink
  
  age_color <- rev(plasma(6))
  
    output$WorldwideSuicides <- renderHighchart({
      # Create line plot.
      highchart() %>% 
        hc_add_series(overall_tibble, hcaes(x = year, y = suicide_capita, color = suicide_capita), type = "line") %>%
        hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", pointFormat = paste("Year: <b>{point.x}</b> <br> Suicides: <b>{point.y}</b>")) %>%
        hc_title(text = "Worldwide suicides by year") %>% 
        hc_subtitle(text = "1985-2015") %>%
        hc_xAxis(title = list(text = "Year")) %>%
        hc_yAxis(title = list(text = "Suicides per 100K people"),
                 allowDecimals = FALSE,
                 plotLines = list(list(
                   color = "black", width = 1, dashStyle = "Dash", 
                   value = mean(overall_tibble$suicide_capita),
                   label = list(text = "Mean = 13.12", 
                                style = list(color = "black"))))) %>%
        hc_legend(enabled = FALSE) %>% 
        hc_add_theme(custom_theme)
    })
    
    ### Worldwide suicides by Gender {.no-padding} 
    output$WorldwideSuicidesByGender <- renderHighchart({
      sex_tibble <- data %>%
        select(year, sex, suicides_no, population) %>%
        group_by(year, sex) %>%
        summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))
      sex_color <- c("#EE6AA7", "#87CEEB") # baby blue & pink
      
      highchart() %>% 
        hc_add_series(sex_tibble, hcaes(x = year, y = suicide_capita, group = sex), type = "line", color = sex_color) %>%
        hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", pointFormat = paste("Year: <b>{point.x}</b> <br>","Gender: <b>{point.sex}</b><br>", "Suicides: <b>{point.y}</b>")) %>%
        hc_title(text = "Worldwide suicides by gender") %>% 
        hc_subtitle(text = "1985-2015") %>%
        hc_xAxis(title = list(text = "Year")) %>%
        hc_yAxis(title = list(text = "Suicides per 100K people"),
                 allowDecimals = FALSE,
                 plotLines = list(list(
                   color = "black", width = 1, dashStyle = "Dash",
                   value = mean(overall_tibble$suicide_capita),
                   label = list(text = "Mean = 13.12", 
                                style = list(color = 'black'))))) %>% 
        hc_add_theme(custom_theme)
    })
    
    ### Worldwide suicides by Age {.no-padding}
    output$WorldwideSuicidesByAge <- renderHighchart({
      age_tibble <- data %>%
        select(year, age, suicides_no, population) %>%
        group_by(year, age) %>%
        summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))
      
      highchart() %>% 
        hc_add_series(age_tibble, hcaes(x = year, y = suicide_capita, group = age), type = "line", color = age_color) %>%
        hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", pointFormat = paste("Year: <b>{point.x}</b> <br>","Age: <b>{point.age}</b><br>", "Suicides: <b>{point.y}</b>")) %>%
        hc_title(text = "Worldwide suicides by age") %>% 
        hc_subtitle(text = "1985-2015") %>%
        hc_xAxis(title = list(text = "Year")) %>%
        hc_yAxis(title = list(text = "Suicides per 100K people"),
                 allowDecimals = FALSE,
                 plotLines = list(list(
                   color = "black", width = 1, dashStyle = "Dash",
                   value = mean(overall_tibble$suicide_capita),
                   label = list(text = "Mean = 13.12", 
                                style = list(color = 'black'))))) %>% 
        hc_add_theme(custom_theme)
    })
    
    #continent
    
    data$continent <- countrycode(sourcevar = data$country,
                                  origin = "country.name",
                                  destination = "continent")
    # Reclassify countries that have been coded as 'Americas', by countrycode(), into 'North America' and 'South America'. 
    south_america <- c('Argentina', 'Brazil', 'Chile', 'Colombia', 'Ecuador', 'Guyana', 'Paraguay', 'Suriname', 'Uruguay')
    data$continent[data$country %in% south_america] <- 'South America'
    data$continent[data$continent=='Americas'] <- 'North America'
    
    ### Suicides by continent and Gender {.no-title .no-padding}
    output$continentGender <- renderHighchart({
      continent_sex_tibble <- data %>%
        select(continent, sex, suicides_no, population) %>%
        group_by(continent, sex) %>%
        summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))
      
      highchart() %>%
        hc_add_series(continent_sex_tibble, hcaes(x = continent, y = suicide_capita, group = sex), type = "column")  %>% 
        hc_colors(colors = sex_color) %>%
        hc_tooltip(borderWidth = 1.5, pointFormat = paste("Gender: <b> {point.sex} </b> <br> Suicides: <b>{point.y}</b>")) %>%
        hc_title(text = "Suicides by continent and <b>Gender<b>") %>% 
        hc_subtitle(text = "1985-2015") %>%
        hc_xAxis(categories = c("Africa", "Asia", "Europe", "North <br> America", "Oceania", "South <br> America"), labels = list(style = list(fontSize = 10))) %>%
        hc_yAxis(labels = list(style = list(fontSize = 11)),
                 title = list(text = "Suicides per 100K people", 
                              style = list(fontSize = 12)),
                 plotLines = list(
                   list(color = "black", width = 1, dashStyle = "Dash", 
                        value = mean(overall_tibble$suicide_capita),
                        label = list(text = "Mean = 13.12", style = list(color = "black", fontSize = 10))))) %>%       
        hc_legend(verticalAlign = 'bottom', enabled = TRUE) %>% 
        hc_add_theme(custom_theme)
    })
    
    ### Suicides by continent and Age {.no-title .no-padding}
    output$continentAge <- renderHighchart({
      continent_age_tibble <- data %>%
        select(continent, age, suicides_no, population) %>%
        group_by(continent, age) %>%
        summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) 
      
      highchart() %>%
        hc_add_series(continent_age_tibble, hcaes(x = continent, y = suicide_capita, group = age), type = "column")  %>% 
        hc_colors(colors = age_color) %>%
        hc_tooltip(borderWidth = 1.5, pointFormat = paste("Age: <b> {point.age} </b> <br> Suicides: <b>{point.y}</b>")) %>%
        hc_title(text = "Suicides by continent and <b>Age<b>") %>%
        hc_subtitle(text = "1985-2015") %>%
        hc_xAxis(categories = c("Africa", "Asia", "Europe", "North <br> America", "Oceania", "South <br> America"), labels = list(style = list(fontSize = 10))) %>%
        hc_yAxis(labels = list(style = list(fontSize = 11)),
                 title = list(text = "Suicides per 100K people", 
                              style = (list(fontSize = 12))),
                 plotLines = list(
                   list(color = "black", width = 1, dashStyle = "Dash", 
                        value = mean(overall_tibble$suicide_capita),
                        label = list(text = "Mean = 13.12", style = list(color = "black", fontSize = 10))))) %>%
        hc_legend(verticalAlign = 'bottom', enabled = TRUE, 
                  itemStyle = list(fontSize = 10)) %>% 
        hc_add_theme(custom_theme)
    })
    
    ### Suicides by continent {.no-title .no-padding}
    output$continent <- renderHighchart({
      map_data <- download_map_data("custom/world-continents")
      continent_tibble <- data %>%
        select(continent, suicides_no, population) %>%
        group_by(continent) %>%
        summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) %>%
        arrange(suicide_capita)
      
      highchart() %>%
        hc_add_series_map(map_data, continent_tibble, value = "suicide_capita", joinBy = c('name','continent'), name = "Suicides (per 100K people)")  %>% 
        hc_add_series(continent_tibble, hcaes(x = continent, y = suicide_capita, color = suicide_capita), type = "pie", name = 'Suicides (per 100K people)')  %>% 
        hc_colorAxis(stops = color_stops()) %>% 
        hc_title(text = "Suicides by Continent") %>% 
        hc_subtitle(text = "1985-2015") %>%
        hc_tooltip(borderWidth = 1.5, valueSuffix = '') %>%
        hc_plotOptions(
          pie = list(center = c('10%', '80%'), size = 130, dataLabels = list(enabled = FALSE))) %>% 
        hc_add_theme(custom_theme)
    })
    
    #country
    #bycountry
    output$bycountry <- renderHighchart({
      country_bar <- data %>%
        select(country, suicides_no, population) %>%
        group_by(country) %>%
        summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) %>%
        arrange(desc(suicide_capita))
      highchart() %>%
        hc_add_series(country_bar, hcaes(x = country, y = suicide_capita, color = suicide_capita), type = "bar")  %>% 
        hc_tooltip(borderWidth = 1.5, 
                   pointFormat = paste("Suicides: <b>{point.y}</b>")) %>%
        hc_legend(enabled = FALSE) %>%
        hc_title(text = "Suicides by country") %>% 
        hc_subtitle(text = "1985-2015") %>%
        hc_xAxis(categories = country_bar$country, 
                 labels = list(step = 1),
                 min = 0, max = 30,
                 scrollbar = list(enabled = TRUE)) %>%
        hc_yAxis(title = list(text = "Suicides per 100K people")) %>%
        hc_plotOptions(bar = list(stacking = "normal", 
                                  pointPadding = 0, groupPadding = 0, borderWidth = 0.5)) %>% 
        hc_add_theme(custom_theme)
    })
    
    country_tibble <- data %>%
      select(country, suicides_no, population) %>%
      group_by(country) %>%
      summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) 
    
    #bygender
    output$bygender <- renderHighchart({
      country_bar_sex <- data %>%
        select(country, sex, suicides_no, population) %>%
        group_by(country, sex) %>%
        summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))
      
      highchart() %>%
        hc_add_series(country_bar_sex, hcaes(x = country, y = suicide_capita, group = sex), type = "bar", color = sex_color)  %>% 
        hc_tooltip(borderWidth = 1.5, pointFormat = paste("Gender: <b>{point.sex} ({point.percentage:.1f}%)</b> <br> Suicides per 100K: <b>{point.y}</b>")) %>%
        hc_legend(enabled = TRUE, colorByPoint = TRUE) %>%
        hc_title(text = "Suicides by country and gender") %>%
        hc_subtitle(text = "1985-2015") %>%
        hc_xAxis(categories = country_tibble$country,
                 labels = list(step = 1),
                 min = 0, max = 30,
                 scrollbar = list(enabled = TRUE)) %>%
        hc_yAxis(title = list(text = "Percent of total suicides")) %>%
        hc_plotOptions(bar = list(stacking = "percent", 
                                  pointPadding = 0, groupPadding = 0, borderWidth = 0.4)) %>% 
        hc_add_theme(custom_theme)
    })
    
    #byage
    output$byage <- renderHighchart({
      country_bar_age <- data %>%
        select(country, age, suicides_no, population) %>%
        group_by(country, age) %>%
        summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))
      
      highchart() %>%
        hc_add_series(country_bar_age, hcaes(x = country, y = suicide_capita, group = age), type = "bar", color = age_color)  %>% 
        hc_tooltip(borderWidth = 1.5, pointFormat = paste("Age: <b>{point.age} ({point.percentage:.1f}%)</b> <br> Suicides per 100K: <b>{point.y}</b>")) %>%
        hc_title(text = "Suicides by country and age") %>% 
        hc_subtitle(text = "1985-2015") %>%
        hc_xAxis(categories = country_tibble$country,
                 labels = list(step = 1),
                 min = 0, max = 30,
                 scrollbar = list(enabled = TRUE)) %>%
        hc_yAxis(title = list(text = "Percent of total suicides")) %>%
        hc_plotOptions(bar = list(stacking = "percent", 
                                  pointPadding = 0, groupPadding = 0, borderWidth = 0.5)) %>% 
        hc_add_theme(custom_theme)
    })
    
    #suicedie country
    output$country <- renderHighchart({
      country_tibble <- data %>%
        select(country, suicides_no, population) %>%
        group_by(country) %>%
        summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))
      
      highchart() %>%
        hc_add_series_map(worldgeojson, country_tibble, value = "suicide_capita", joinBy = c('name','country'))  %>% 
        #  hc_colorAxis(dataClasses = color_classes(c(seq(0, 30, by = 10), 50))) %>% 
        #  hc_colorAxis(minColor = "#FF0000", maxColor = "#F5F5F5") %>%
        hc_colorAxis(stops = color_stops()) %>% 
        hc_title(text = "Suicides by Country") %>% 
        hc_subtitle(text = "1985-2015") %>%
        hc_tooltip(borderWidth = 1.5, headerFormat = "", valueSuffix = " suicides (per 100K people)") %>% 
        hc_add_theme(custom_theme)
    })
})
