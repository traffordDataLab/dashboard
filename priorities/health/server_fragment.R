# Health and wellbeing #

# Alcohol related admissions --------------------------------------------------

alcohol_related_admissions <- read_csv("data/health/alcohol_related_admissions.csv") %>%
  mutate(area_name = as_factor(area_name),
         tooltip = paste0(
           "Area: ", area_name, "<br/>",
           "Period: ", period, "<br/>",
           "Value: ", round(value, 1), " per 100,000"))

output$alcohol_related_admissions_plot <- renderggiraph({
  
  if (input$alcohol_related_admissions_area_name == "Statistical neighbours") {
    
    gg <-
      ggplot(
        alcohol_related_admissions, aes(x = period, y = value, 
                                        colour = area_name, fill = area_name, 
                                        group = fct_relevel(area_name, "Trafford", after = Inf)
        )) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip = tooltip, data_id = area_name), 
                             shape = 21, size = 2.5, colour = "white", alpha = 0.01) +
      scale_colour_manual(values = ifelse(alcohol_related_admissions$area_name == "Trafford", "#00AFBB", "#d9d9d9")) +
      scale_fill_manual(values = ifelse(alcohol_related_admissions$area_name == "Trafford", "#00AFBB", "#d9d9d9")) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Admission episodes for alcohol-related conditions (narrow)",
        subtitle = NULL,
        caption = "Source: PHE Fingertips (Local Alcohol Profiles for England)",
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
    girafe_options(gg, opts_tooltip(use_fill = TRUE, opacity = 1),
                   opts_hover(css = "fill-opacity:1;stroke:white;stroke-opacity:1;r:2.5pt"),
                   opts_selection(type = "single"),
                   opts_toolbar(saveaspng = FALSE))
    
  }
  else {
    
    gg <-
      ggplot(
        filter(alcohol_related_admissions, area_name == "Trafford"),
        aes(x = period, y = value, group = area_name)) +
      geom_line(colour = "#00AFBB", size = 1) +
      geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, fill = "#00AFBB", colour = "white") +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Admission episodes for alcohol-related conditions (narrow)",
        subtitle = NULL,
        caption = "Source: PHE Fingertips (Local Alcohol Profiles for England)",
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
    
    x <- girafe(ggobj = gg)
    x <- girafe_options(x, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    x
    
  }
  
})

output$alcohol_related_admissions_box <- renderUI({
  box(div(HTML(paste0("<h5>", "By x, the prevalence of ", "<b>","alcohol related addmissions","</b>", "  will be x.", "</h5>")),
          style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
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
            inputId = "alcohol_related_admissions_area_name",
            label = tags$h4("Group by:"),
            choiceNames = c("Trafford", "Statistical neighbours"),
            choiceValues = c("Trafford", "Statistical neighbours"), 
            selected = "Trafford", 
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
  mutate(area_name = as_factor(area_name),
         period = as.Date(paste(period, 1, 1, sep = "-")),
         tooltip = paste0(
           "Area: ", area_name, "<br/>",
           "Period: ", year(period), "<br/>",
           "Value: ", round(value, 1), " per 100,000"))

output$alcohol_related_mortality_plot <- renderggiraph({
  
  if (input$alcohol_related_mortality_area_name == "Statistical neighbours") {
    
    gg <-
      ggplot(
        alcohol_related_mortality, aes(x = period, y = value, 
                                       colour = area_name, fill = area_name, 
                                       group = fct_relevel(area_name, "Trafford", after = Inf)
        )) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip = tooltip, data_id = area_name), 
                             shape = 21, size = 2.5, colour = "white", alpha = 0.01) +
      scale_colour_manual(values = ifelse(alcohol_related_mortality$area_name == "Trafford", "#00AFBB", "#d9d9d9")) +
      scale_fill_manual(values = ifelse(alcohol_related_mortality$area_name == "Trafford", "#00AFBB", "#d9d9d9")) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Alcohol related mortality",
        subtitle = NULL,
        caption = "Source: PHE Fingertips (Local Alcohol Profiles for England)",
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
    girafe_options(gg, opts_tooltip(use_fill = TRUE, opacity = 1),
                   opts_hover(css = "fill-opacity:1;stroke:white;stroke-opacity:1;r:2.5pt"),
                   opts_selection(type = "single"),
                   opts_toolbar(saveaspng = FALSE))
    
  }
  else {
    
    gg <-
      ggplot(
        filter(alcohol_related_mortality, area_name == "Trafford"),
        aes(x = period, y = value, group = area_name)) +
      geom_line(colour = "#00AFBB", size = 1) +
      geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, fill = "#00AFBB", colour = "white") +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Alcohol related mortality",
        subtitle = NULL,
        caption = "Source: PHE Fingertips (Local Alcohol Profiles for England)",
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
    
    x <- girafe(ggobj = gg)
    x <- girafe_options(x, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    x
    
  }
  
})

output$alcohol_related_mortality_box <- renderUI({
  box(div(HTML(paste0("<h5>", "By x, the prevalence of ", "<b>","alcohol related mortality","</b>", "  will be x.", "</h5>")),
          style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
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
            inputId = "alcohol_related_mortality_area_name",
            label = tags$h4("Group by:"),
            choiceNames = c("Trafford", "Statistical neighbours"),
            choiceValues = c("Trafford", "Statistical neighbours"), 
            selected = "Trafford", 
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
         group = as_factor(group),
         tooltip = paste0(
           "Area: ", area_name, "<br/>",
           "Period: ", period, "<br/>",
           "Sex: ", group, "<br/>",
           "Value: ", round(value, 1), " years"))

output$healthy_life_expectancy_at_birth_plot <- renderggiraph({
  
  if (input$healthy_life_expectancy_at_birth_area_name == "Statistical neighbours") {
    
    gg <-
      ggplot(
        healthy_life_expectancy_at_birth, aes(x = period, y = value, 
                                              colour = area_name, fill = area_name, 
                                              group = fct_relevel(area_name, "Trafford", after = Inf)
        )) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip = tooltip, data_id = area_name), 
                             shape = 21, size = 2.5, colour = "white", alpha = 0.01) +
      scale_colour_manual(values = ifelse(healthy_life_expectancy_at_birth$area_name == "Trafford", "#00AFBB", "#d9d9d9")) +
      scale_fill_manual(values = ifelse(healthy_life_expectancy_at_birth$area_name == "Trafford", "#00AFBB", "#d9d9d9")) +
      scale_y_continuous(limits = c(55,70), breaks = seq(55,70,5)) +
      facet_wrap(~group) +
      labs(
        title = "Healthy life expectancy at birth",
        subtitle = NULL,
        caption = "Source: PHE Fingertips (PHOF 0.1i)",
        x = "",
        y = "Years",
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
    girafe_options(gg, opts_tooltip(use_fill = TRUE, opacity = 1),
                   opts_hover(css = "fill-opacity:1;stroke:white;stroke-opacity:1;r:2.5pt"),
                   opts_selection(type = "single"),
                   opts_toolbar(saveaspng = FALSE))
    
  }
  else {
    
    gg <-
      ggplot(
        filter(healthy_life_expectancy_at_birth, area_name == "Trafford"),
        aes(x = period, y = value, group = area_name)) +
      geom_line(colour = "#00AFBB", size = 1) +
      geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, fill = "#00AFBB", colour = "white") +
      scale_y_continuous(limits = c(60,70), breaks = seq(60,70,5)) +
      facet_wrap(~group) +
      labs(
        title = "Healthy life expectancy at birth",
        subtitle = NULL,
        caption = "Source: PHE Fingertips (PHOF 0.1i)",
        x = "",
        y = "Years",
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
    
    x <- girafe(ggobj = gg)
    x <- girafe_options(x, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    x
    
  }
  
})

output$healthy_life_expectancy_at_birth_box <- renderUI({
  box(div(HTML(paste0("<h5>", "By x, ", "<b>","healthy life expectancy at birth","</b>", "  will exceed x years.", "</h5>")),
          style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
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
            inputId = "healthy_life_expectancy_at_birth_area_name",
            label = tags$h4("Group by:"),
            choiceNames = c("Trafford", "Statistical neighbours"),
            choiceValues = c("Trafford", "Statistical neighbours"), 
            selected = "Trafford", 
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
         group = as_factor(group),
         tooltip = paste0(
           "Area: ", area_name, "<br/>",
           "Period: ", period, "<br/>",
           "Sex: ", group, "<br/>",
           "Value: ", round(value, 1), " years"))

output$slope_index_of_inequality_plot <- renderggiraph({
  
  if (input$slope_index_of_inequality_area_name == "Statistical neighbours") {
    
    gg <-
      ggplot(
        slope_index_of_inequality, aes(x = period, y = value, 
                                       colour = area_name, fill = area_name, 
                                       group = fct_relevel(area_name, "Trafford", after = Inf)
        )) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip = tooltip, data_id = area_name), 
                             shape = 21, size = 2.5, colour = "white", alpha = 0.01) +
      scale_colour_manual(values = ifelse(slope_index_of_inequality$area_name == "Trafford", "#00AFBB", "#d9d9d9")) +
      scale_fill_manual(values = ifelse(slope_index_of_inequality$area_name == "Trafford", "#00AFBB", "#d9d9d9")) +
      scale_y_continuous(limits = c(0, NA), breaks = pretty_breaks()) +
      facet_wrap(~group) +
      labs(
        title = "Inequality in life expectancy at birth",
        subtitle = NULL,
        caption = "Source: PHE Fingertips (PHOF 0.2vi)",
        x = "",
        y = "Years",
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
    girafe_options(gg, opts_tooltip(use_fill = TRUE, opacity = 1),
                   opts_hover(css = "fill-opacity:1;stroke:white;stroke-opacity:1;r:2.5pt"),
                   opts_selection(type = "single"),
                   opts_toolbar(saveaspng = FALSE))
    
  }
  else {
    
    gg <-
      ggplot(
        filter(slope_index_of_inequality, area_name == "Trafford"),
        aes(x = period, y = value, group = area_name)) +
      geom_line(colour = "#00AFBB", size = 1) +
      geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, fill = "#00AFBB", colour = "white") +
      scale_y_continuous(limits = c(0, NA), breaks = pretty_breaks()) +
      facet_wrap(~group) +
      labs(
        title = "Inequality in life expectancy at birth",
        subtitle = NULL,
        caption = "Source: PHE Fingertips (PHOF 0.2vi)",
        x = "",
        y = "Years",
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
    
    x <- girafe(ggobj = gg)
    x <- girafe_options(x, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    x
    
  }
  
})

output$slope_index_of_inequality_box <- renderUI({
  box(div(HTML(paste0("<h5>", "By x, the ", "<b>","inequality in life expectancy at birth","</b>", "  will be below x years.", "</h5>")),
          style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
      title = "Slope Index of Inequality",
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
            inputId = "slope_index_of_inequality_area_name",
            label = tags$h4("Group by:"),
            choiceNames = c("Trafford", "Statistical neighbours"),
            choiceValues = c("Trafford", "Statistical neighbours"), 
            selected = "Trafford", 
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

# Indicator name --------------------------------------------------
