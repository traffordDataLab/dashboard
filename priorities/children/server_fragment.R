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
                               fill = "#FFFFFF", colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(excess_weight_reception, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", "%", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 5) +
      geom_boxplot_interactive(data = filter(excess_weight_reception, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(excess_weight_reception, area_name == "England")$value, "</strong>", "%", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(excess_weight_reception, area_name == "England")$period)),
                               colour = "red", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Children aged 4-5 years who have excess weight",
           subtitle = NULL,
           caption = "Source: NHS Digital, National Child Measurement Programme",
           x = NULL, y = "Percentage",
           fill = "Compared with England:") +
      theme_minimal(base_family = "Open Sans") +
      theme(
        panel.grid.major = element_blank(),
        axis.title.y = element_text(size = 7, hjust = 1),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.caption = element_text(margin = margin(t = 15)),
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(css = "background-color:#8B8B8B;font-family:'Open Sans',sans-serif;color:white;padding:10px;border-radius:5px;"),
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
        x = "",
        y = "Percentage",
        colour = NULL
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 7, hjust = 1),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none"
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$excess_weight_reception_box <- renderUI({
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","excess weight in Reception year children","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
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
                               fill = "#FFFFFF", colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(excess_weight_year6, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", "%", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 5) +
      geom_boxplot_interactive(data = filter(excess_weight_year6, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(excess_weight_year6, area_name == "England")$value, "</strong>", "%", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(excess_weight_year6, area_name == "England")$period)),
                               colour = "red", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Children aged 10-11 years who have excess weight",
           subtitle = NULL,
           caption = "Source: NHS Digital, National Child Measurement Programme",
           x = NULL, y = "Percentage",
           fill = "Compared with England:") +
      theme_minimal(base_family = "Open Sans") +
      theme(
        panel.grid.major = element_blank(),
        axis.title.y = element_text(size = 7, hjust = 1),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.caption = element_text(margin = margin(t = 15)),
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(css = "background-color:#8B8B8B;font-family:'Open Sans',sans-serif;color:white;padding:10px;border-radius:5px;"),
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
        x = "",
        y = "Percentage",
        colour = NULL
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 7, hjust = 1),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none"
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$excess_weight_year6_box <- renderUI({
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","excess weight in Year 6 children","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
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
                               fill = "#FFFFFF", colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(low_income_families, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", "%", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 5) +
      geom_boxplot_interactive(data = filter(low_income_families, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(low_income_families, area_name == "England")$value, "</strong>", "%", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(low_income_families, area_name == "England")$period)),
                               colour = "red", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Children in low income families (under 16s)",
           subtitle = NULL,
           caption = "Source: HM Revenue and Customs",
           x = NULL, y = "Percentage",
           fill = "Compared with England:") +
      theme_minimal(base_family = "Open Sans") +
      theme(
        panel.grid.major = element_blank(),
        axis.title.y = element_text(size = 7, hjust = 1),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.caption = element_text(margin = margin(t = 15)),
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(css = "background-color:#8B8B8B;font-family:'Open Sans',sans-serif;color:white;padding:10px;border-radius:5px;"),
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
        x = "",
        y = "Percentage",
        colour = NULL
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 7, hjust = 1),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none"
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$low_income_families_box <- renderUI({
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","children living in poverty aged under 16 years","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
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
                               fill = "#FFFFFF", colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(admissions_self_harm_young_people, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", " per 100,000", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 5) +
      geom_boxplot_interactive(data = filter(admissions_self_harm_young_people, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(admissions_self_harm_young_people, area_name == "England")$value, "</strong>", " per 100,000", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(admissions_self_harm_young_people, area_name == "England")$period)),
                               colour = "red", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Hospital admissions as a result of self-harm (10-24 years)",
           subtitle = NULL,
           caption = "Source: Hospital Episode Statistics",
           x = NULL, y = "per 100,000",
           fill = "Compared with England:") +
      theme_minimal(base_family = "Open Sans") +
      theme(
        panel.grid.major = element_blank(),
        axis.title.y = element_text(size = 7, hjust = 1),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.caption = element_text(margin = margin(t = 15)),
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(css = "background-color:#8B8B8B;font-family:'Open Sans',sans-serif;color:white;padding:10px;border-radius:5px;"),
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
        x = "",
        y = "per 100,000",
        colour = NULL
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 7, hjust = 1),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none"
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$admissions_self_harm_young_people_box <- renderUI({
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","levels of self-harm among young people","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
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
                               fill = "#FFFFFF", colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(dental_decay, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", "%", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 5) +
      geom_boxplot_interactive(data = filter(dental_decay, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(dental_decay, area_name == "England")$value, "</strong>", "%", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(dental_decay, area_name == "England")$period)),
                               colour = "red", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Proportion of five year old children free from dental decay",
           subtitle = NULL,
           caption = "Source: Dental Public Health Epidemiology Programme for England",
           x = NULL, y = "Percentage",
           fill = "Compared with England:") +
      theme_minimal(base_family = "Open Sans") +
      theme(
        panel.grid.major = element_blank(),
        axis.title.y = element_text(size = 7, hjust = 1),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.caption = element_text(margin = margin(t = 15)),
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(css = "background-color:#8B8B8B;font-family:'Open Sans',sans-serif;color:white;padding:10px;border-radius:5px;"),
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
        x = "",
        y = "Percentage",
        colour = NULL
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 7, hjust = 1),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none"
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$dental_decay_box <- renderUI({
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","children free from tooth decay","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
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
                               fill = "#FFFFFF", colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(school_readiness_fsm, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", "%", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 5) +
      geom_boxplot_interactive(data = filter(school_readiness_fsm, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(school_readiness_fsm, area_name == "England")$value, "</strong>", "%", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(school_readiness_fsm, area_name == "England")$period)),
                               colour = "red", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Children with free school meal status achieving\na good level of development",
           subtitle = NULL,
           caption = "Source: Department for Education, Early Years Foundation Stage Profile",
           x = NULL, y = "Percentage",
           fill = "Compared with England:") +
      theme_minimal(base_family = "Open Sans") +
      theme(
        panel.grid.major = element_blank(),
        axis.title.y = element_text(size = 7, hjust = 1),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.caption = element_text(margin = margin(t = 15)),
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(css = "background-color:#8B8B8B;font-family:'Open Sans',sans-serif;color:white;padding:10px;border-radius:5px;"),
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
        x = "",
        y = "Percentage",
        colour = NULL
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 7, hjust = 1),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none"
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$school_readiness_fsm_box <- renderUI({
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","school readiness amongst children with free school meal","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
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
                               fill = "#FFFFFF", colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(child_protection_plans, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", " per 10,000", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 5) +
      geom_boxplot_interactive(data = filter(child_protection_plans, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(child_protection_plans, area_name == "England")$value, "</strong>", " per 10,000", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(child_protection_plans, area_name == "England")$period)),
                               colour = "red", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Children on child protection plans",
           subtitle = NULL,
           caption = "Source: Children in need statistics",
           x = NULL, y = "per 10,000",
           fill = "Compared with England:") +
      theme_minimal(base_family = "Open Sans") +
      theme(
        panel.grid.major = element_blank(),
        axis.title.y = element_text(size = 7, hjust = 1),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.caption = element_text(margin = margin(t = 15)),
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(css = "background-color:#8B8B8B;font-family:'Open Sans',sans-serif;color:white;padding:10px;border-radius:5px;"),
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
        x = "",
        y = "per 10,000",
        colour = NULL
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 7, hjust = 1),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none"
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$child_protection_plans_box <- renderUI({
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","children on child protection plans","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
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
                               fill = "#FFFFFF", colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(neets, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", "%", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 5) +
      geom_boxplot_interactive(data = filter(neets, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(neets, area_name == "England")$value, "</strong>", "%", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(neets, area_name == "England")$period)),
                               colour = "red", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "16-17 year olds not in education, employment or training (NEET)",
           subtitle = NULL,
           caption = "Source: Department for Education",
           x = NULL, y = "Percentage",
           fill = "Compared with England:") +
      theme_minimal(base_family = "Open Sans") +
      theme(
        panel.grid.major = element_blank(),
        axis.title.y = element_text(size = 7, hjust = 1),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.caption = element_text(margin = margin(t = 15)),
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(css = "background-color:#8B8B8B;font-family:'Open Sans',sans-serif;color:white;padding:10px;border-radius:5px;"),
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
        title = "16-17 year olds not in education, employment or training (NEET)",
        subtitle = NULL,
        caption = "Source:	Department for Education",
        x = "",
        y = "Percentage",
        colour = NULL
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 7, hjust = 1),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none"
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$neets_box <- renderUI({
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","16-17 year olds not in education, employment or training (NEET)","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
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
                               fill = "#FFFFFF", colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(children_in_care, area_name == "Trafford"), 
                             aes(x = period, y = value, fill = significance, 
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", " per 10,000", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)), 
                             shape = 21, colour = "#000000", size = 5) +
      geom_boxplot_interactive(data = filter(children_in_care, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(children_in_care, area_name == "England")$value, "</strong>", " per 10,000", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(children_in_care, area_name == "England")$period)),
                               colour = "red", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Children in care",
           subtitle = NULL,
           caption = "Source: Children looked after in England, Department for Education",
           x = NULL, y = "per 10,000",
           fill = "Compared with England:") +
      theme_minimal(base_family = "Open Sans") +
      theme(
        panel.grid.major = element_blank(),
        axis.title.y = element_text(size = 7, hjust = 1),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.caption = element_text(margin = margin(t = 15)),
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(css = "background-color:#8B8B8B;font-family:'Open Sans',sans-serif;color:white;padding:10px;border-radius:5px;"),
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
        x = "",
        y = "per 10,000",
        colour = NULL
      ) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 7, hjust = 1),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none"
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$children_in_care_box <- renderUI({
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","children in care","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
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

# Early years settings rated 'Good' or 'Outstanding' --------------------------------------------------

early_years_settings <- read_csv("data/children/early_years_settings.csv") %>% 
  mutate(area_name = as_factor(area_name),
         group = as_factor(group, levels = c("Oustanding", "Good", "Requires improvement", "Inadequate"), ordered = TRUE),
         tooltip = 
           paste0("<strong>", paste(round(value*100, 1)), "</strong>", "%", "<br/>",
                  "<em>", area_name, "</em><br/>",
                  group))

output$early_years_settings_plot <- renderggiraph({
  
  gg <-
    ggplot(
      filter(early_years_settings, area_name %in% input$early_years_settings_selection),
      aes(x = factor(group), y = value, fill = area_name)) +
    geom_bar_interactive(aes(tooltip = tooltip), stat = "identity", position = "dodge", colour = "#FFFFFF", size = 1) +
    scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_y_continuous(limits = c(0, NA), labels = percent_format(accuracy = 1)) +
    labs(
      title = "Early years settings rated 'Good' or 'Outstanding'",
      subtitle = NULL,
      caption = "Source: Ofsted",
      x = "",
      y = "Percentage",
      colour = NULL
    ) +
    guides(fill = FALSE) +
    theme_minimal(base_family = "Open Sans") +
    theme(
      panel.grid.major.x = element_blank(),
      axis.title.y = element_text(size = 7, hjust = 1),
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "bottom"
    )
  
  gg <- girafe(ggobj = gg)
  girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
  
})

output$early_years_settings_box <- renderUI({
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","Early years settings rated 'Good' or 'Outstanding'","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
      title = "Early years settings rated 'Good' or 'Outstanding'",
      withSpinner(
        ggiraphOutput("early_years_settings_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          checkboxGroupInput(
            inputId = "early_years_settings_selection",
            label = tags$h4("Select area:"),
            choices = unique(levels(early_years_settings$area_name)),
            selected = "Trafford"
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
          includeMarkdown("data/children/metadata/early_years_settings.md"),
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

# Indicator name --------------------------------------------------

