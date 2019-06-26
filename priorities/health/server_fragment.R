# Health and wellbeing #

# Alcohol related admissions --------------------------------------------------

alcohol_related_admissions <- read_csv("data/health/alcohol_related_admissions.csv") %>% 
  filter(!is.na(value))

output$alcohol_related_admissions_plot <- renderggiraph({
  
  if (input$alcohol_related_admissions_selection == "Boxplot") {
    
    gg <- ggplot(data = filter(alcohol_related_admissions, area_name != "England"),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(alcohol_related_admissions, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", round(value, 1), "</strong>", " per 100,000", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(alcohol_related_admissions, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", round(filter(alcohol_related_admissions, area_name == "England")$value, 1), "</strong>", " per 100,000", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(alcohol_related_admissions, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Admission episodes for alcohol-related conditions",
           subtitle = NULL,
           caption = "Source: NHS Digital, Hospital Episode Statistics",
           x = NULL, y = "per 100,000",
           fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE),
                   opts_toolbar(saveaspng = FALSE))
    
  }
  else {
    
    gg <-
      ggplot(
        filter(alcohol_related_admissions, area_name %in% c("Trafford", "Greater Manchester", "England")),
        aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip = 
                                   paste0("<strong>", round(value, 1), "</strong>", " per 100,000", "<br/>",
                                          "<em>", area_name, "</em><br/>",
                                          period)), 
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Admission episodes for alcohol-related conditions",
        subtitle = NULL,
        caption = "Source: NHS Digital, Hospital Episode Statistics",
        x = NULL,
        y = "per 100,000",
        colour = NULL
      ) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$alcohol_related_admissions_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Alcohol related admissions",
      withSpinner(
        ggiraphOutput("alcohol_related_admissions_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "alcohol_related_admissions_selection",
            label = tags$h4("Show as:"),
            choiceNames = c("Trend", "Boxplot"),
            choiceValues = c("Trend", "Boxplot"), 
            selected = "Trend", 
            direction = "vertical"
          ),
          icon = icon("filter"),
          size = "xs",
          style = "jelly",
          width = "200px",
          up = TRUE
        )
      ),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em; ",
        dropdown(
          includeMarkdown("data/health/metadata/alcohol_related_admissions.md"),
          icon = icon("question"),
          size = "xs",
          style = "jelly",
          width = "300px",
          up = TRUE
        ),
        tags$style(
          HTML(
            '.fa {color: #212121;}
              .bttn-jelly.bttn-default{color:#f0f0f0;}
              .bttn-jelly:hover:before{opacity:1};'
          )
        )
      )
  )
  
})

# Alcohol related mortality --------------------------------------------------

alcohol_related_mortality <- read_csv("data/health/alcohol_related_mortality.csv") %>% 
  mutate(period = as_factor(period)) %>% 
  filter(!is.na(value))

output$alcohol_related_mortality_plot <- renderggiraph({
  
  if (input$alcohol_related_mortality_selection == "Boxplot") {
    
    gg <- ggplot(data = filter(alcohol_related_mortality, area_name != "England"),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value), 
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(alcohol_related_mortality, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", round(value, 1), "</strong>", " per 100,000", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(alcohol_related_mortality, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", round(filter(alcohol_related_mortality, area_name == "England")$value, 1), "</strong>", " per 100,000", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(alcohol_related_mortality, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Deaths from alcohol-related conditions",
           subtitle = NULL,
           caption = "Source: Office for National Statistics",
           x = NULL, y = "per 100,000",
           fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE),
                   opts_toolbar(saveaspng = FALSE))
    
  }
  else {
    
    gg <-
      ggplot(
        filter(alcohol_related_mortality, area_name %in% c("Trafford", "Greater Manchester", "England")),
        aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip = 
                                   paste0("<strong>", round(value, 1), "</strong>", " per 100,000", "<br/>",
                                          "<em>", area_name, "</em><br/>",
                                          period)), 
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Deaths from alcohol-related conditions",
        subtitle = NULL,
        caption = "Source: Office for National Statistics",
        x = NULL,
        y = "per 100,000",
        colour = NULL
      ) +
      guides(fill = FALSE) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$alcohol_related_mortality_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Alcohol related mortality",
      withSpinner(
        ggiraphOutput("alcohol_related_mortality_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "alcohol_related_mortality_selection",
            label = tags$h4("Show as:"),
            choiceNames = c("Trend", "Boxplot"),
            choiceValues = c("Trend", "Boxplot"), 
            selected = "Trend", 
            direction = "vertical"
          ),
          icon = icon("filter"),
          size = "xs",
          style = "jelly",
          width = "200px",
          up = TRUE
        )
      ),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em; ",
        dropdown(
          includeMarkdown("data/health/metadata/alcohol_related_mortality.md"),
          icon = icon("question"),
          size = "xs",
          style = "jelly",
          width = "300px",
          up = TRUE
        ),
        tags$style(
          HTML(
            '.fa {color: #212121;}
              .bttn-jelly.bttn-default{color:#f0f0f0;}
              .bttn-jelly:hover:before{opacity:1};'
          )
        )
      )
  )
  
})

# Healthy life expectancy --------------------------------------------------

healthy_life_expectancy_at_birth <- read_csv("data/health/healthy_life_expectancy_at_birth.csv") %>%
  mutate(area_name = as_factor(area_name),
         period = as_factor(period),
         group = as_factor(group)) %>% 
  filter(!is.na(value))

output$healthy_life_expectancy_at_birth_plot <- renderggiraph({
  
  if (input$healthy_life_expectancy_at_birth_selection == "Boxplot") {
    
    gg <- ggplot(data = filter(healthy_life_expectancy_at_birth, area_name != "England"),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL)  +
      geom_point_interactive(data = filter(healthy_life_expectancy_at_birth, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", round(value, 1), "</strong>", " years", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(healthy_life_expectancy_at_birth, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", round(filter(healthy_life_expectancy_at_birth, area_name == "England")$value, 1), "</strong>", " years", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(healthy_life_expectancy_at_birth, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(50,70), breaks = seq(50,70,5)) +
      facet_wrap(~group) +
      labs(title = "Healthy life expectancy at birth",
           subtitle = NULL,
           caption = "Source: Office for National Statistics",
           x = NULL, y = "Years",
           fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE),
                   opts_toolbar(saveaspng = FALSE))
    
  }
  else {
    
    gg <-
      ggplot(
        filter(healthy_life_expectancy_at_birth, area_name %in% c("Trafford", "Greater Manchester", "England")),
        aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip = 
                                   paste0("<strong>", round(value, 1), "</strong>", " years", "<br/>",
                                          "<em>", area_name, "</em><br/>",
                                          period)), 
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_y_continuous(limits = c(55,70), breaks = seq(55,70,5)) +
      facet_wrap(~group) +
      labs(
        title = "Healthy life expectancy at birth",
        subtitle = NULL,
        caption = "Source: Office for National Statistics",
        x = NULL,
        y = "Years",
        colour = NULL
      ) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$healthy_life_expectancy_at_birth_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Healthy life expectancy at birth",
      withSpinner(
        ggiraphOutput("healthy_life_expectancy_at_birth_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "healthy_life_expectancy_at_birth_selection",
            label = tags$h4("Show as:"),
            choiceNames = c("Trend", "Boxplot"),
            choiceValues = c("Trend", "Boxplot"), 
            selected = "Trend", 
            direction = "vertical"
          ),
          icon = icon("filter"),
          size = "xs",
          style = "jelly",
          width = "200px",
          up = TRUE
        )
      ),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em; ",
        dropdown(
          includeMarkdown("data/health/metadata/healthy_life_expectancy_at_birth.md"),
          icon = icon("question"),
          size = "xs",
          style = "jelly",
          width = "300px",
          up = TRUE
        ),
        tags$style(
          HTML(
            '.fa {color: #212121;}
              .bttn-jelly.bttn-default{color:#f0f0f0;}
              .bttn-jelly:hover:before{opacity:1};'
          )
        )
      )
  )
  
})


# Slope index of inequality --------------------------------------------------

slope_index_of_inequality <- read_csv("data/health/slope_index_of_inequality.csv") %>% 
  mutate(area_name = as_factor(area_name),
         period = as_factor(period),
         group = as_factor(group)) %>% 
  filter(!is.na(value))

output$slope_index_of_inequality_plot <- renderggiraph({
    
    if (input$slope_index_of_inequality_selection == "Boxplot") {
        
        gg <- ggplot(data = filter(slope_index_of_inequality, area_name != "England"),
                     aes(x = period, y = value)) +
          stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
          geom_boxplot_interactive(colour = "#C9C9C9",
                                   outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                                   fatten = NULL)  +
          geom_point_interactive(data = filter(slope_index_of_inequality, area_name == "Trafford"), 
                                 aes(x = period, y = value,
                                     tooltip =  paste0(
                                       "<strong>", round(value, 1), "</strong>", " years", "<br/>",
                                       "<em>", area_name, "</em><br/>", period)),
                                 shape = 21, fill = "#C9C9C9", colour = "#000000", size = 3, alpha = 0.5) +
          geom_boxplot_interactive(data = filter(slope_index_of_inequality, area_name == "England"),
                                   aes(x = factor(period), y = value,
                                       tooltip =  paste0(
                                         "<strong>", round(filter(slope_index_of_inequality, area_name == "England")$value, 1), "</strong>", " years", "<br/>",
                                         "<em>", "England", "</em><br/>",
                                         filter(slope_index_of_inequality, area_name == "England")$period)),
                                   fill = "#C9C9C9", size = 0.5) +
          scale_y_continuous(limits = c(0, NA), breaks = pretty_breaks()) +
          facet_wrap(~group) +
    
          labs(title = "Inequality in life expectancy at birth",
               subtitle = NULL,
               caption = "Source: Public Health England",
               x = NULL, y = "Years") +
          theme_x()
        
        gg <- girafe(ggobj = gg)
        girafe_options(gg, opts_tooltip(use_fill = TRUE),
                       opts_toolbar(saveaspng = FALSE))
        
      }
      else {
        
        gg <-
          ggplot(
            filter(slope_index_of_inequality, area_name %in% c("Trafford", "Greater Manchester", "England")),
            aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
          geom_line(size = 1) +
          geom_point_interactive(aes(tooltip = 
                                       paste0("<strong>", round(value, 1), "</strong>", " years", "<br/>",
                                              "<em>", area_name, "</em><br/>",
                                              period)), 
                                 shape = 21, size = 2.5, colour = "white") +
          scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
          scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
          scale_y_continuous(limits = c(0, NA), breaks = pretty_breaks()) +
          facet_wrap(~group) +
          labs(
            title = "Inequality in life expectancy at birth",
            subtitle = NULL,
            caption = "Source: Public Health England",
            x = NULL,
            y = "Years",
            colour = NULL
          ) +
          theme_x()
        
        gg <- girafe(ggobj = gg)
        girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
        
      }
      
    })

output$slope_index_of_inequality_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Inequalities in life expectancy",
      withSpinner(
        ggiraphOutput("slope_index_of_inequality_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "slope_index_of_inequality_selection",
            label = tags$h4("Show as:"),
            choiceNames = c("Trend", "Boxplot"),
            choiceValues = c("Trend", "Boxplot"), 
            selected = "Trend", 
            direction = "vertical"
          ),
          icon = icon("filter"),
          size = "xs",
          style = "jelly",
          width = "200px",
          up = TRUE
        )
      ),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em; ",
        dropdown(
          includeMarkdown("data/health/metadata/slope_index_of_inequality.md"),
          icon = icon("question"),
          size = "xs",
          style = "jelly",
          width = "300px",
          up = TRUE
        ),
        tags$style(
          HTML(
            '.fa {color: #212121;}
              .bttn-jelly.bttn-default{color:#f0f0f0;}
              .bttn-jelly:hover:before{opacity:1};'
          )
        )
      )
  )
  
})

# Smoking prevalence in adults --------------------------------------------------

smoking_adults <- read_csv("data/health/smoking_adults.csv") %>% 
  mutate(area_name = as_factor(area_name),
         period = as_factor(period)) %>% 
  filter(!is.na(value))

output$smoking_adults_plot <- renderggiraph({
  
  if (input$smoking_adults_selection == "Boxplot") {
    
    gg <- ggplot(data = filter(smoking_adults, area_name != "England"),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(smoking_adults, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", round(value, 1), "</strong>", "%", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(smoking_adults, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", round(filter(smoking_adults, area_name == "England")$value, 1), "</strong>", "%", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(smoking_adults, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Smoking prevalence in adults (18+)",
           subtitle = NULL,
           caption = "Source: Annual Population Survey",
           x = NULL, y = "Percentage",
           fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE),
                   opts_toolbar(saveaspng = FALSE))
    
  }
  else {
    
    gg <-
      ggplot(
        filter(smoking_adults, area_name %in% c("Trafford", "Greater Manchester", "England")),
        aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip = 
                                   paste0("<strong>", round(value, 1), "</strong>", "%", "<br/>",
                                          "<em>", area_name, "</em><br/>",
                                          period)), 
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Smoking prevalence in adults (18+)",
        subtitle = NULL,
        caption = "Source: Annual Population Survey",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$smoking_adults_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Adult smoking prevalence",
      withSpinner(
        ggiraphOutput("smoking_adults_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "smoking_adults_selection",
            label = tags$h4("Show as:"),
            choiceNames = c("Trend", "Boxplot"),
            choiceValues = c("Trend", "Boxplot"), 
            selected = "Trend", 
            direction = "vertical"
          ),
          icon = icon("filter"),
          size = "xs",
          style = "jelly",
          width = "200px",
          up = TRUE
        )
      ),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em; ",
        dropdown(
          includeMarkdown("data/health/metadata/smoking_adults.md"),
          icon = icon("question"),
          size = "xs",
          style = "jelly",
          width = "300px",
          up = TRUE
        ),
        tags$style(
          HTML(
            '.fa {color: #212121;}
              .bttn-jelly.bttn-default{color:#f0f0f0;}
              .bttn-jelly:hover:before{opacity:1};'
          )
        )
      )
  )
  
})

# Smoking prevalence in adults in routine and manual occupations --------------------------------------------------

smoking_adults_manual <- read_csv("data/health/smoking_adults_manual.csv") %>% 
  mutate(area_name = as_factor(area_name),
         period = as_factor(period)) %>% 
  filter(!is.na(value))

output$smoking_adults_manual_plot <- renderggiraph({
  
  if (input$smoking_adults_manual_selection == "Boxplot") {
    
    gg <- ggplot(data = filter(smoking_adults_manual, area_name != "England"),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(smoking_adults_manual, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", round(value, 1), "</strong>", "%", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(smoking_adults_manual, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", round(filter(smoking_adults_manual, area_name == "England")$value, 1), "</strong>", "%", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(smoking_adults_manual, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Smoking prevalence in adults in routine and manual occupations",
           subtitle = NULL,
           caption = "Source: Annual Population Survey",
           x = NULL, y = "Percentage",
           fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE),
                   opts_toolbar(saveaspng = FALSE))
    
  }
  else {
    
    gg <-
      ggplot(
        filter(smoking_adults_manual, area_name %in% c("Trafford", "Greater Manchester", "England")),
        aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip = 
                                   paste0("<strong>", round(value, 1), "</strong>", "%", "<br/>",
                                          "<em>", area_name, "</em><br/>",
                                          period)), 
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Smoking prevalence in adults in routine and manual occupations",
        subtitle = NULL,
        caption = "Source: Annual Population Survey",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$smoking_adults_manual_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Smoking amongst Routine and Manual workers",
      withSpinner(
        ggiraphOutput("smoking_adults_manual_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "smoking_adults_manual_selection",
            label = tags$h4("Show as:"),
            choiceNames = c("Trend", "Boxplot"),
            choiceValues = c("Trend", "Boxplot"), 
            selected = "Trend", 
            direction = "vertical"
          ),
          icon = icon("filter"),
          size = "xs",
          style = "jelly",
          width = "200px",
          up = TRUE
        )
      ),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em; ",
        dropdown(
          includeMarkdown("data/health/metadata/smoking_adults_manual.md"),
          icon = icon("question"),
          size = "xs",
          style = "jelly",
          width = "300px",
          up = TRUE
        ),
        tags$style(
          HTML(
            '.fa {color: #212121;}
              .bttn-jelly.bttn-default{color:#f0f0f0;}
              .bttn-jelly:hover:before{opacity:1};'
          )
        )
      )
  )
  
})

# Emergency admissions due to falls in people aged 65 and over --------------------------------------------------

admissions_falls <- read_csv("data/health/admissions_falls.csv") %>% 
  mutate(area_name = as_factor(area_name),
         period = as_factor(period)) %>% 
  filter(!is.na(value))

output$admissions_falls_plot <- renderggiraph({
  
  if (input$admissions_falls_selection == "Boxplot") {
    
    gg <- ggplot(data = filter(admissions_falls, !area_name %in% c("Greater Manchester", "England")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(admissions_falls, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", comma(value), "</strong>", " per 100,000", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(admissions_falls, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", comma(filter(admissions_falls, area_name == "England")$value), "</strong>", " per 100,000", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(admissions_falls, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Emergency hospital admissions due to falls in people aged 65 and over",
           subtitle = NULL,
           caption = "Source: NHS Digital, Hospital Episode Statistics",
           x = NULL, y = "per 100,000",
           fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE),
                   opts_toolbar(saveaspng = FALSE))
    
  }
  else {
    
    gg <-
      ggplot(
        filter(admissions_falls, area_name %in% c("Trafford", "Greater Manchester", "England")),
        aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip = 
                                   paste0("<strong>", comma(value), "</strong>", " per 100,000", "<br/>",
                                          "<em>", area_name, "</em><br/>",
                                          period)), 
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Emergency hospital admissions due to falls in people aged 65 and over",
        subtitle = NULL,
        caption = "Source: NHS Digital, Hospital Episode Statistics",
        x = NULL,
        y = "per 100,000",
        colour = NULL
      ) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$admissions_falls_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Emergency admissions for falls",
      withSpinner(
        ggiraphOutput("admissions_falls_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "admissions_falls_selection",
            label = tags$h4("Show as:"),
            choiceNames = c("Trend", "Boxplot"),
            choiceValues = c("Trend", "Boxplot"), 
            selected = "Trend", 
            direction = "vertical"
          ),
          icon = icon("filter"),
          size = "xs",
          style = "jelly",
          width = "200px",
          up = TRUE
        )
      ),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em; ",
        dropdown(
          includeMarkdown("data/health/metadata/admissions_falls.md"),
          icon = icon("question"),
          size = "xs",
          style = "jelly",
          width = "300px",
          up = TRUE
        ),
        tags$style(
          HTML(
            '.fa {color: #212121;}
              .bttn-jelly.bttn-default{color:#f0f0f0;}
              .bttn-jelly:hover:before{opacity:1};'
          )
        )
      )
  )
  
})

# Percentage of deaths in usual place of residence --------------------------------------------------

deaths_at_home <- read_csv("data/health/deaths_at_home.csv") %>% 
  mutate(area_name = as_factor(area_name),
         period = as_factor(period)) %>% 
  filter(!is.na(value))

output$deaths_at_home_plot <- renderggiraph({
  
  if (input$deaths_at_home_selection == "Boxplot") {
    
    gg <- ggplot(data = filter(deaths_at_home, area_name != "England"),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(deaths_at_home, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", "%", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(deaths_at_home, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(deaths_at_home, area_name == "England")$value, "</strong>", "%", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(deaths_at_home, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Percentage of deaths in usual place of residence",
           subtitle = NULL,
           caption = "Source: Office for National Statistics",
           x = NULL, y = "Percentage",
           fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE),
                   opts_toolbar(saveaspng = FALSE))
    
  }
  else {
    
    gg <-
      ggplot(
        filter(deaths_at_home, area_name %in% c("Trafford", "Greater Manchester", "England")),
        aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip = 
                                   paste0("<strong>", value, "</strong>", "%", "<br/>",
                                          "<em>", area_name, "</em><br/>",
                                          period)), 
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Percentage of deaths in usual place of residence",
        subtitle = NULL,
        caption = "Source: Office for National Statistics",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$deaths_at_home_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Deaths in usual place of residence",
      withSpinner(
        ggiraphOutput("deaths_at_home_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "deaths_at_home_selection",
            label = tags$h4("Show as:"),
            choiceNames = c("Trend", "Boxplot"),
            choiceValues = c("Trend", "Boxplot"), 
            selected = "Trend", 
            direction = "vertical"
          ),
          icon = icon("filter"),
          size = "xs",
          style = "jelly",
          width = "200px",
          up = TRUE
        )
      ),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em; ",
        dropdown(
          includeMarkdown("data/health/metadata/deaths_at_home.md"),
          icon = icon("question"),
          size = "xs",
          style = "jelly",
          width = "300px",
          up = TRUE
        ),
        tags$style(
          HTML(
            '.fa {color: #212121;}
              .bttn-jelly.bttn-default{color:#f0f0f0;}
              .bttn-jelly:hover:before{opacity:1};'
          )
        )
      )
  )
  
})

# Preventable mortality from cancer --------------------------------------------------

preventable_mortality_from_cancer <- read_csv("data/health/preventable_mortality_from_cancer.csv") %>% 
  mutate(area_name = as_factor(area_name),
         period = as_factor(period)) %>% 
  filter(!is.na(value))

output$preventable_mortality_from_cancer_plot <- renderggiraph({
  
  if (input$preventable_mortality_from_cancer_selection == "Boxplot") {
    
    gg <- ggplot(data = filter(preventable_mortality_from_cancer, area_name != "England"),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(preventable_mortality_from_cancer, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", "  per 100,000", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(preventable_mortality_from_cancer, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(preventable_mortality_from_cancer, area_name == "England")$value, "</strong>", "  per 100,000", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(preventable_mortality_from_cancer, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Under 75 mortality rate from cancer considered preventable",
           subtitle = NULL,
           caption = "Source: Public Health England",
           x = NULL, y = " per 100,000",
           fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE),
                   opts_toolbar(saveaspng = FALSE))
    
  }
  else {
    
    gg <-
      ggplot(
        filter(preventable_mortality_from_cancer, area_name %in% c("Trafford", "Greater Manchester", "England")),
        aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip = 
                                   paste0("<strong>", value, "</strong>", "  per 100,000", "<br/>",
                                          "<em>", area_name, "</em><br/>",
                                          period)), 
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Under 75 mortality rate from cancer considered preventable",
        subtitle = NULL,
        caption = "Source: Public Health England",
        x = NULL,
        y = " per 100,000",
        colour = NULL
      ) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$preventable_mortality_from_cancer_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Under 75 mortality rate from cancer considered preventable",
      withSpinner(
        ggiraphOutput("preventable_mortality_from_cancer_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "preventable_mortality_from_cancer_selection",
            label = tags$h4("Show as:"),
            choiceNames = c("Trend", "Boxplot"),
            choiceValues = c("Trend", "Boxplot"), 
            selected = "Trend", 
            direction = "vertical"
          ),
          icon = icon("filter"),
          size = "xs",
          style = "jelly",
          width = "200px",
          up = TRUE
        )
      ),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em; ",
        dropdown(
          includeMarkdown("data/health/metadata/preventable_mortality_from_cancer.md"),
          icon = icon("question"),
          size = "xs",
          style = "jelly",
          width = "300px",
          up = TRUE
        ),
        tags$style(
          HTML(
            '.fa {color: #212121;}
              .bttn-jelly.bttn-default{color:#f0f0f0;}
              .bttn-jelly:hover:before{opacity:1};'
          )
        )
      )
  )
  
})

# Percentage of physically inactive adults --------------------------------------------------

physically_inactive <- read_csv("data/health/physically_inactive.csv") %>% 
  mutate(area_name = as_factor(area_name),
         period = as_factor(period)) %>% 
  filter(!is.na(value))

output$physically_inactive_plot <- renderggiraph({
  
  if (input$physically_inactive_selection == "Boxplot") {
    
    gg <- ggplot(data = filter(physically_inactive, area_name != "England"),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(physically_inactive, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", "%", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(physically_inactive, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(physically_inactive, area_name == "England")$value, "</strong>", "%", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(physically_inactive, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Percentage of physically inactive adults",
           subtitle = NULL,
           caption = "Source: Active Lives, Sport England",
           x = NULL, y = "Percentage",
           fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE),
                   opts_toolbar(saveaspng = FALSE))
    
  }
  else {
    
    gg <-
      ggplot(
        filter(physically_inactive, area_name %in% c("Trafford", "Greater Manchester", "England")),
        aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip = 
                                   paste0("<strong>", value, "</strong>", "%", "<br/>",
                                          "<em>", area_name, "</em><br/>",
                                          period)), 
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Percentage of physically inactive adults",
        subtitle = NULL,
        caption = "Source: Active Lives, Sport England",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$physically_inactive_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Physical inactivity",
      withSpinner(
        ggiraphOutput("physically_inactive_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "physically_inactive_selection",
            label = tags$h4("Show as:"),
            choiceNames = c("Trend", "Boxplot"),
            choiceValues = c("Trend", "Boxplot"), 
            selected = "Trend", 
            direction = "vertical"
          ),
          icon = icon("filter"),
          size = "xs",
          style = "jelly",
          width = "200px",
          up = TRUE
        )
      ),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em; ",
        dropdown(
          includeMarkdown("data/health/metadata/physically_inactive.md"),
          icon = icon("question"),
          size = "xs",
          style = "jelly",
          width = "300px",
          up = TRUE
        ),
        tags$style(
          HTML(
            '.fa {color: #212121;}
              .bttn-jelly.bttn-default{color:#f0f0f0;}
              .bttn-jelly:hover:before{opacity:1};'
          )
        )
      )
  )
  
})

# Estimated dementia diagnosis rate (aged 65+) --------------------------------------------------

dementia_diagnosis <- read_csv("data/health/dementia_diagnosis.csv") %>% 
  mutate(area_name = as_factor(area_name),
         period = as_factor(period)) %>% 
  filter(!is.na(value))

output$dementia_diagnosis_plot <- renderggiraph({
  
  if (input$dementia_diagnosis_selection == "Boxplot") {
    
    gg <- ggplot(data = filter(dementia_diagnosis, area_name != "England"),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(dementia_diagnosis, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", "%", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(dementia_diagnosis, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(dementia_diagnosis, area_name == "England")$value, "</strong>", "%", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(dementia_diagnosis, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Estimated dementia diagnosis rate (aged 65 and over)",
           subtitle = NULL,
           caption = "Source: NHS Digital",
           x = NULL, y = "Percentage",
           fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE),
                   opts_toolbar(saveaspng = FALSE))
    
  }
  else {
    
    gg <-
      ggplot(
        filter(dementia_diagnosis, area_name %in% c("Trafford", "Greater Manchester", "England")),
        aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip = 
                                   paste0("<strong>", value, "</strong>", "%", "<br/>",
                                          "<em>", area_name, "</em><br/>",
                                          period)), 
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Estimated dementia diagnosis rate (aged 65 and over)",
        subtitle = NULL,
        caption = "Source: NHS Digital",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$dementia_diagnosis_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Dementia diagnosis rate",
      withSpinner(
        ggiraphOutput("dementia_diagnosis_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "dementia_diagnosis_selection",
            label = tags$h4("Show as:"),
            choiceNames = c("Trend", "Boxplot"),
            choiceValues = c("Trend", "Boxplot"), 
            selected = "Trend", 
            direction = "vertical"
          ),
          icon = icon("filter"),
          size = "xs",
          style = "jelly",
          width = "200px",
          up = TRUE
        )
      ),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em; ",
        dropdown(
          includeMarkdown("data/health/metadata/dementia_diagnosis.md"),
          icon = icon("question"),
          size = "xs",
          style = "jelly",
          width = "300px",
          up = TRUE
        ),
        tags$style(
          HTML(
            '.fa {color: #212121;}
              .bttn-jelly.bttn-default{color:#f0f0f0;}
              .bttn-jelly:hover:before{opacity:1};'
          )
        )
      )
  )
  
})

# Excess under 75 mortality rate in adults with serious mental illness --------------------------------------------------

mortality_serious_mental_illness <- read_csv("data/health/mortality_serious_mental_illness.csv") %>% 
  mutate(area_name = as_factor(area_name),
         period = as_factor(period)) %>% 
  filter(!is.na(value))

output$mortality_serious_mental_illness_plot <- renderggiraph({
  
  if (input$mortality_serious_mental_illness_selection == "Boxplot") {
    
    gg <- ggplot(data = filter(mortality_serious_mental_illness, area_name != "England"),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(mortality_serious_mental_illness, area_name == "Trafford"), 
                             aes(x = period, y = value,
                                 tooltip =  paste0(
                                   "<strong>", round(value, 1), "</strong>", "%", "<br/>",
                                   "<em>", area_name, "</em><br/>", period)),
                             shape = 21, fill = "#C9C9C9", colour = "#000000", size = 3, alpha = 0.5) +
      geom_boxplot_interactive(data = filter(mortality_serious_mental_illness, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(mortality_serious_mental_illness, area_name == "England")$value, "</strong>", "%", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(mortality_serious_mental_illness, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Excess under 75 mortality rate in adults\nwith serious mental illness",
           subtitle = NULL,
           caption = "Source: NHS Digital",
           x = NULL, y = "Indirectly standardised ratio",
           fill = "Compared with England:") +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE),
                   opts_toolbar(saveaspng = FALSE))
    
  }
  else {
    
    gg <-
      ggplot(
        filter(mortality_serious_mental_illness, area_name %in% c("Trafford", "Greater Manchester", "England")),
        aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip = 
                                   paste0("<strong>", value, "</strong>", "%", "<br/>",
                                          "<em>", area_name, "</em><br/>",
                                          period)), 
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Excess under 75 mortality rate in adults\nwith serious mental illness",
        subtitle = NULL,
        caption = "Source: NHS Digital",
        x = NULL,
        y = "Indirectly standardised ratio",
        colour = NULL
      ) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$mortality_serious_mental_illness_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Excess under 75 mortality rate in adults with serious mental illness",
      withSpinner(
        ggiraphOutput("mortality_serious_mental_illness_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "mortality_serious_mental_illness_selection",
            label = tags$h4("Show as:"),
            choiceNames = c("Trend", "Boxplot"),
            choiceValues = c("Trend", "Boxplot"), 
            selected = "Trend", 
            direction = "vertical"
          ),
          icon = icon("filter"),
          size = "xs",
          style = "jelly",
          width = "200px",
          up = TRUE
        )
      ),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em; ",
        dropdown(
          includeMarkdown("data/health/metadata/mortality_serious_mental_illness.md"),
          icon = icon("question"),
          size = "xs",
          style = "jelly",
          width = "300px",
          up = TRUE
        ),
        tags$style(
          HTML(
            '.fa {color: #212121;}
              .bttn-jelly.bttn-default{color:#f0f0f0;}
              .bttn-jelly:hover:before{opacity:1};'
          )
        )
      )
  )
  
})

# Emergency hospital admissions for intentional self-harm --------------------------------------------------

admissions_self_harm <- read_csv("data/health/admissions_self_harm.csv") %>% 
  mutate(area_name = as_factor(area_name),
         period = as_factor(period)) %>% 
  filter(!is.na(value))

output$admissions_self_harm_plot <- renderggiraph({
  
  if (input$admissions_self_harm_selection == "Boxplot") {
    
    gg <- ggplot(data = filter(admissions_self_harm, area_name != "England"),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(admissions_self_harm, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", " per 100,000", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(admissions_self_harm, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(admissions_self_harm, area_name == "England")$value, "</strong>", " per 100,000", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(admissions_self_harm, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +

      labs(title = "Emergency hospital admissions for intentional self-harm",
           subtitle = NULL,
           caption = "Source: NHS Digital, Hospital Episode Statistics",
           x = NULL, y = "per 100,000",
           fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE),
                   opts_toolbar(saveaspng = FALSE))
    
  }
  else {
    
    gg <-
      ggplot(
        filter(admissions_self_harm, area_name %in% c("Trafford", "Greater Manchester", "England")),
        aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip = 
                                   paste0("<strong>", value, "</strong>", " per 100,000", "<br/>",
                                          "<em>", area_name, "</em><br/>",
                                          period)), 
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Emergency hospital admissions for intentional self-harm",
        subtitle = NULL,
        caption = "Source: NHS Digital, Hospital Episode Statistics",
        x = NULL,
        y = "per 100,000",
        colour = NULL
      ) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$admissions_self_harm_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Emergency hospital admissions for intentional self-harm",
      withSpinner(
        ggiraphOutput("admissions_self_harm_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "admissions_self_harm_selection",
            label = tags$h4("Show as:"),
            choiceNames = c("Trend", "Boxplot"),
            choiceValues = c("Trend", "Boxplot"), 
            selected = "Trend", 
            direction = "vertical"
          ),
          icon = icon("filter"),
          size = "xs",
          style = "jelly",
          width = "200px",
          up = TRUE
        )
      ),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em; ",
        dropdown(
          includeMarkdown("data/health/metadata/admissions_self_harm.md"),
          icon = icon("question"),
          size = "xs",
          style = "jelly",
          width = "300px",
          up = TRUE
        ),
        tags$style(
          HTML(
            '.fa {color: #212121;}
              .bttn-jelly.bttn-default{color:#f0f0f0;}
              .bttn-jelly:hover:before{opacity:1};'
          )
        )
      )
  )
  
})
