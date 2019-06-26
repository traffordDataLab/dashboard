# Children and Young People #

# Child excess weight in 4-5 year olds --------------------------------------------------

excess_weight_reception <- read_csv("data/children/excess_weight_reception.csv") %>% 
  mutate(area_name = as_factor(area_name),
         period = as_factor(period)) %>% 
  filter(!is.na(value))

output$excess_weight_reception_plot <- renderggiraph({
  
  if (input$excess_weight_reception_selection == "Boxplot") {
    
    gg <- ggplot(data = filter(excess_weight_reception, area_name != "England"),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(excess_weight_reception, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", "%", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(excess_weight_reception, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(excess_weight_reception, area_name == "England")$value, "</strong>", "%", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(excess_weight_reception, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Children aged 4-5 years who have excess weight",
           subtitle = NULL,
           caption = "Source: NHS Digital, National Child Measurement Programme",
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
        filter(excess_weight_reception, area_name %in% c("Trafford", "Greater Manchester", "England")),
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
        title = "Children aged 4-5 years who have excess weight",
        subtitle = NULL,
        caption = "Source: NHS Digital, National Child Measurement Programme",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$excess_weight_reception_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "4-5 year olds with excess weight",
      withSpinner(
        ggiraphOutput("excess_weight_reception_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "excess_weight_reception_selection",
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
          includeMarkdown("data/children/metadata/excess_weight_reception.md"),
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

# Child excess weight in 10-11 year olds --------------------------------------------------

excess_weight_year6 <- read_csv("data/children/excess_weight_year6.csv") %>% 
  mutate(area_name = as_factor(area_name),
         period = as_factor(period)) %>% 
  filter(!is.na(value))

output$excess_weight_year6_plot <- renderggiraph({
  
  if (input$excess_weight_year6_selection == "Boxplot") {
    
    gg <- ggplot(data = filter(excess_weight_year6, area_name != "England"),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(excess_weight_year6, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", "%", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(excess_weight_year6, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(excess_weight_year6, area_name == "England")$value, "</strong>", "%", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(excess_weight_year6, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Children aged 10-11 years who have excess weight",
           subtitle = NULL,
           caption = "Source: NHS Digital, National Child Measurement Programme",
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
        filter(excess_weight_year6, area_name %in% c("Trafford", "Greater Manchester", "England")),
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
        title = "Children aged 10-11 years who have excess weight",
        subtitle = NULL,
        caption = "Source: NHS Digital, National Child Measurement Programme",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$excess_weight_year6_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "10-11 year olds with excess weight",
      withSpinner(
        ggiraphOutput("excess_weight_year6_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "excess_weight_year6_selection",
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
          includeMarkdown("data/children/metadata/excess_weight_year6.md"),
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

# Children in low income families (under 16s) --------------------------------------------------

low_income_families <- read_csv("data/children/low_income_families.csv") %>% 
  mutate(area_name = as_factor(area_name),
         period = as_factor(period)) %>% 
  filter(!is.na(value))

output$low_income_families_plot <- renderggiraph({
  
  if (input$low_income_families_selection == "Boxplot") {
    
    gg <- ggplot(data = filter(low_income_families, area_name != "England"),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(low_income_families, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", "%", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(low_income_families, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(low_income_families, area_name == "England")$value, "</strong>", "%", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(low_income_families, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Children in low income families (under 16s)",
           subtitle = NULL,
           caption = "Source: HM Revenue and Customs",
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
        filter(low_income_families, area_name %in% c("Trafford", "Greater Manchester", "England")),
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
        title = "Children in low income families (under 16s)",
        subtitle = NULL,
        caption = "Source: HM Revenue and Customs",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$low_income_families_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Children living in poverty",
      withSpinner(
        ggiraphOutput("low_income_families_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "low_income_families_selection",
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
          includeMarkdown("data/children/metadata/low_income_families.md"),
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

# Hospital admissions as a result of self-harm (10-24 years) --------------------------------------------------

admissions_self_harm_young_people <- read_csv("data/children/admissions_self_harm_young_people.csv") %>% 
  mutate(area_name = as_factor(area_name),
         period = as_factor(period)) %>% 
  filter(!is.na(value))

output$admissions_self_harm_young_people_plot <- renderggiraph({
  
  if (input$admissions_self_harm_young_people_selection == "Boxplot") {
    
    gg <- ggplot(data = filter(admissions_self_harm_young_people, area_name != "England"),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(admissions_self_harm_young_people, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", " per 100,000", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(admissions_self_harm_young_people, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(admissions_self_harm_young_people, area_name == "England")$value, "</strong>", " per 100,000", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(admissions_self_harm_young_people, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Hospital admissions as a result of self-harm (10-24 years)",
           subtitle = NULL,
           caption = "Source: Hospital Episode Statistics",
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
        filter(admissions_self_harm_young_people, area_name %in% c("Trafford", "Greater Manchester", "England")),
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
        title = "Hospital admissions as a result of self-harm (10-24 years)",
        subtitle = NULL,
        caption = "Source: Hospital Episode Statistics",
        x = NULL,
        y = "per 100,000",
        colour = NULL
      ) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$admissions_self_harm_young_people_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Young people admitted to hospital as a result of self-harm",
      withSpinner(
        ggiraphOutput("admissions_self_harm_young_people_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "admissions_self_harm_young_people_selection",
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
          includeMarkdown("data/children/metadata/admissions_self_harm_young_people.md"),
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

# Proportion of five year old children free from dental decay --------------------------------------------------

dental_decay <- read_csv("data/children/dental_decay.csv") %>% 
  mutate(area_name = as_factor(area_name),
         period = as_factor(period)) %>% 
  filter(!is.na(value))

output$dental_decay_plot <- renderggiraph({
  
  if (input$dental_decay_selection == "Boxplot") {
    
    gg <- ggplot(data = filter(dental_decay, area_name != "England"),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(dental_decay, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", "%", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(dental_decay, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(dental_decay, area_name == "England")$value, "</strong>", "%", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(dental_decay, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Proportion of five year old children free from dental decay",
           subtitle = NULL,
           caption = "Source: Dental Public Health Epidemiology Programme for England",
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
        filter(dental_decay, area_name %in% c("Trafford", "Greater Manchester", "England")),
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
        title = "Proportion of five year old children free from dental decay",
        subtitle = NULL,
        caption = "Source: Dental Public Health Epidemiology Programme for England",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$dental_decay_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Children free from tooth decay",
      withSpinner(
        ggiraphOutput("dental_decay_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "dental_decay_selection",
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
          includeMarkdown("data/children/metadata/dental_decay.md"),
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

# School readiness amongst children with free school meals --------------------------------------------------

school_readiness_fsm <- read_csv("data/children/school_readiness_fsm.csv") %>% 
  mutate(area_name = as_factor(area_name),
         period = as_factor(period)) %>% 
  filter(!is.na(value))

output$school_readiness_fsm_plot <- renderggiraph({
  
  if (input$school_readiness_fsm_selection == "Boxplot") {
    
    gg <- ggplot(data = filter(school_readiness_fsm, area_name != "England"),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(school_readiness_fsm, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", "%", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(school_readiness_fsm, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(school_readiness_fsm, area_name == "England")$value, "</strong>", "%", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(school_readiness_fsm, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Children with free school meal status achieving\na good level of development",
           subtitle = NULL,
           caption = "Source: Department for Education, Early Years Foundation Stage Profile",
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
        filter(school_readiness_fsm, area_name %in% c("Trafford", "Greater Manchester", "England")),
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
        title = "Children with free school meal status achieving\na good level of development",
        subtitle = NULL,
        caption = "Source: Department for Education, Early Years Foundation Stage Profile",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$school_readiness_fsm_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "School readiness amongst children with free school meals",
      withSpinner(
        ggiraphOutput("school_readiness_fsm_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "school_readiness_fsm_selection",
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
          includeMarkdown("data/children/metadata/school_readiness_fsm.md"),
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

# Children on child protection plans --------------------------------------------------

child_protection_plans <- read_csv("data/children/child_protection_plans.csv") %>% 
  mutate(area_name = as_factor(area_name),
         period = as_factor(period)) %>% 
  filter(!is.na(value))

output$child_protection_plans_plot <- renderggiraph({
  
  if (input$child_protection_plans_selection == "Boxplot") {
    
    gg <- ggplot(data = filter(child_protection_plans, area_name != "England"),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(child_protection_plans, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", " per 10,000", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(child_protection_plans, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(child_protection_plans, area_name == "England")$value, "</strong>", " per 10,000", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(child_protection_plans, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Children on child protection plans",
           subtitle = NULL,
           caption = "Source: Children in need statistics",
           x = NULL, y = "per 10,000",
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
        filter(child_protection_plans, area_name %in% c("Trafford", "Greater Manchester", "England")),
        aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip = 
                                   paste0("<strong>", value, "</strong>", " per 10,000", "<br/>",
                                          "<em>", area_name, "</em><br/>",
                                          period)), 
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Children on child protection plans",
        subtitle = NULL,
        caption = "Source: Children in need statistics",
        x = NULL,
        y = "per 10,000",
        colour = NULL
      ) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$child_protection_plans_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Children on child protection plans",
      withSpinner(
        ggiraphOutput("child_protection_plans_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "child_protection_plans_selection",
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
          includeMarkdown("data/children/metadata/child_protection_plans.md"),
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

# NEETs --------------------------------------------------

neets <- read_csv("data/children/neets.csv") %>% 
  mutate(area_name = as_factor(area_name),
         period = as_factor(period)) %>% 
  filter(!is.na(value))

output$neets_plot <- renderggiraph({
  
  if (input$neets_selection == "Boxplot") {
    
    gg <- ggplot(data = filter(neets, area_name != "England"),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(neets, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", "%", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(neets, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(neets, area_name == "England")$value, "</strong>", "%", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(neets, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "16-17 year olds not in education, employment or training",
           subtitle = NULL,
           caption = "Source: Department for Education",
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
        filter(neets, area_name %in% c("Trafford", "Greater Manchester", "England")),
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
        title = "16-17 year olds not in education, employment or training",
        subtitle = NULL,
        caption = "Source:	Department for Education",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$neets_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "NEETS",
      withSpinner(
        ggiraphOutput("neets_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "neets_selection",
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
          includeMarkdown("data/children/metadata/neets.md"),
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

# Children in care --------------------------------------------------

children_in_care <- read_csv("data/children/children_in_care.csv") %>% 
  mutate(area_name = as_factor(area_name),
         period = as_factor(period)) %>% 
  filter(!is.na(value))

output$children_in_care_plot <- renderggiraph({
  
  if (input$children_in_care_selection == "Boxplot") {
    
    gg <- ggplot(data = filter(children_in_care, area_name != "England"),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(children_in_care, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", " per 10,000", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(children_in_care, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(children_in_care, area_name == "England")$value, "</strong>", " per 10,000", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(children_in_care, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Children in care",
           subtitle = NULL,
           caption = "Source: Children looked after in England, Department for Education",
           x = NULL, y = "per 10,000",
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
        filter(children_in_care, area_name %in% c("Trafford", "Greater Manchester", "England")),
        aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip = 
                                   paste0("<strong>", value, "</strong>", " per 10,000", "<br/>",
                                          "<em>", area_name, "</em><br/>",
                                          period)), 
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Children in care",
        subtitle = NULL,
        caption = "Source: Children looked after in England, Department for Education",
        x = NULL,
        y = "per 10,000",
        colour = NULL
      ) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$children_in_care_box <- renderUI({
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Children in care",
      withSpinner(
        ggiraphOutput("children_in_care_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "children_in_care_selection",
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
          includeMarkdown("data/children/metadata/children_in_care.md"),
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

# Childcare provider Ofsted ratings --------------------------------------------------

childcare_providers <- read_csv("data/children/childcare_providers.csv") %>% 
  mutate(rating = fct_relevel(as_factor(rating),
                              level = c("Outstanding", "Good", "Satisfactory / Requires improvement", "Inadequate")))

output$childcare_providers_map = renderLeaflet({
  
  sf <- filter(childcare_providers, rating == input$childcare_providers_selection)
  
  leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron,
                     options = tileOptions(minZoom = 11, maxZoom = 17)) %>% 
    addPolygons(data = boundary,
                fillOpacity = 0, color = "#212121", weight = 2, opacity = 1) %>% 
    addCircleMarkers(data = sf,
                     lng = ~lon, lat = ~lat,
                     stroke = TRUE, color = "#212121", weight = 2, 
                     fillColor = "#00AFBB", fillOpacity = 0.5, radius = 4,
                     popup = paste("<strong>", sf$name, "</strong><br />",
                                   "<em>", sf$rating, "</em><br />",
                                   "Last inspection date:",  sf$inspection_date, "<br />",
                                   "<a href='", sf$url, "'target='_blank'>View latest Ofsted report</a>")) %>% 
    addControl("<strong>Childcare providers</strong>", position = 'topright')
  
})

output$childcare_providers_box <- renderUI({
  
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Childcare providers",
      withSpinner(
        leafletOutput("childcare_providers_map"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioButtons(inputId = "childcare_providers_selection", 
                       tags$h4("Inspection rating:"),
                       choices = unique(levels(childcare_providers$rating)), 
                       selected = "Outstanding"
          ),
          icon = icon("filter"),
          size = "xs",
          style = "jelly",
          width = "200px",
          up = TRUE
        )
      ),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em;",
        dropdown(
          includeMarkdown("data/children/metadata/childcare_providers.md"),
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

# Primary school Ofsted ratings --------------------------------------------------
primary_schools <- read_csv("data/children/primary_schools.csv") %>% 
  mutate(rating = fct_relevel(as_factor(rating),
                              level = c("Outstanding", "Good", "Satisfactory / Requires improvement", "Inadequate")))

output$primary_schools_map = renderLeaflet({
  
  sf <- filter(primary_schools, rating == input$primary_schools_selection)
  
  leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron,
                     options = tileOptions(minZoom = 11, maxZoom = 17)) %>% 
    addPolygons(data = boundary,
                fillOpacity = 0, color = "#212121", weight = 2, opacity = 1) %>% 
    addCircleMarkers(data = sf,
                     lng = ~lon, lat = ~lat,
                     stroke = TRUE, color = "#212121", weight = 2, 
                     fillColor = "#00AFBB", fillOpacity = 0.5, radius = 4,
                     popup = paste("<strong>", sf$name, "</strong><br />",
                                   "<em>", sf$rating, "</em><br />",
                                   "Last inspection date:",  sf$inspection_date, "<br />",
                                   "<a href='", sf$url, "'target='_blank'>View latest Ofsted report</a>")) %>% 
    addControl("<strong>Primary schools</strong>", position = 'topright')
  
})

output$primary_schools_box <- renderUI({
  
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Primary schools",
      withSpinner(
        leafletOutput("primary_schools_map"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioButtons(inputId = "primary_schools_selection", 
                       tags$h4("Inspection rating:"),
                       choices = unique(levels(primary_schools$rating)), 
                       selected = "Outstanding"
          ),
          icon = icon("filter"),
          size = "xs",
          style = "jelly",
          width = "200px",
          up = TRUE
        )
      ),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em;",
        dropdown(
          includeMarkdown("data/children/metadata/primary_schools.md"),
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

# Secondary school Ofsted ratings --------------------------------------------------
secondary_schools <- read_csv("data/children/secondary_schools.csv") %>% 
  mutate(rating = fct_relevel(as_factor(rating),
                              level = c("Outstanding", "Good", "Satisfactory / Requires improvement", "Inadequate")))

output$secondary_schools_map = renderLeaflet({
  
  sf <- filter(secondary_schools, rating == input$secondary_schools_selection)
  
  leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron,
                     options = tileOptions(minZoom = 11, maxZoom = 17)) %>% 
    addPolygons(data = boundary,
                fillOpacity = 0, color = "#212121", weight = 2, opacity = 1) %>% 
    addCircleMarkers(data = sf,
                     lng = ~lon, lat = ~lat,
                     stroke = TRUE, color = "#212121", weight = 2, 
                     fillColor = "#00AFBB", fillOpacity = 0.5, radius = 4,
                     popup = paste("<strong>", sf$name, "</strong><br />",
                                   "<em>", sf$rating, "</em><br />",
                                   "Last inspection date:",  sf$inspection_date, "<br />",
                                   "<a href='", sf$url, "'target='_blank'>View latest Ofsted report</a>")) %>% 
    addControl("<strong>Secondary schools</strong>", position = 'topright')
  
})

output$secondary_schools_box <- renderUI({
  
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Secondary schools",
      withSpinner(
        leafletOutput("secondary_schools_map"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioButtons(inputId = "secondary_schools_selection", 
                       tags$h4("Inspection rating:"),
                       choices = unique(levels(secondary_schools$rating)), 
                       selected = "Outstanding"
          ),
          icon = icon("filter"),
          size = "xs",
          style = "jelly",
          width = "200px",
          up = TRUE
        )
      ),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em;",
        dropdown(
          includeMarkdown("data/children/metadata/secondary_schools.md"),
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

# Special school Ofsted ratings --------------------------------------------------
special_schools <- read_csv("data/children/special_schools.csv") %>% 
  mutate(rating = fct_relevel(as_factor(rating),
                              level = c("Outstanding", "Good", "Satisfactory / Requires improvement", "Inadequate")))

output$special_schools_map = renderLeaflet({
  
  sf <- filter(special_schools, rating == input$special_schools_selection)
  
  leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron,
                     options = tileOptions(minZoom = 11, maxZoom = 17)) %>% 
    addPolygons(data = boundary,
                fillOpacity = 0, color = "#212121", weight = 2, opacity = 1) %>% 
    addCircleMarkers(data = sf,
                     lng = ~lon, lat = ~lat,
                     stroke = TRUE, color = "#212121", weight = 2, 
                     fillColor = "#00AFBB", fillOpacity = 0.5, radius = 4,
                     popup = paste("<strong>", sf$name, "</strong><br />",
                                   "<em>", sf$rating, "</em><br />",
                                   "Last inspection date:",  sf$inspection_date, "<br />",
                                   "<a href='", sf$url, "'target='_blank'>View latest Ofsted report</a>")) %>% 
    addControl("<strong>Special schools</strong>", position = 'topright')
  
})

output$special_schools_box <- renderUI({
  
  box(width = 4, 
      hr(style = "border-top: 1px solid #757575;"),
      title = "Special schools",
      withSpinner(
        leafletOutput("special_schools_map"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioButtons(inputId = "special_schools_selection", 
                       tags$h4("Inspection rating:"),
                       choices = unique(levels(special_schools$rating)), 
                       selected = "Outstanding"
          ),
          icon = icon("filter"),
          size = "xs",
          style = "jelly",
          width = "200px",
          up = TRUE
        )
      ),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em;",
        dropdown(
          includeMarkdown("data/children/metadata/special_schools.md"),
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