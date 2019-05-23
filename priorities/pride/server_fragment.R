# Pride in our area #

# Crime rate  --------------------------------------------------

crime_rate <- read_csv("data/pride/crime_rate.csv") %>% 
  mutate(area_name = fct_relevel(as_factor(area_name),
                                 "Trafford", 
                                 "Greater Manchester",
                                 "England"),
         group = fct_relevel(as_factor(group), 
                             "Criminal damage and arson", 
                             "Other crimes against society",
                             "Sexual offences",
                             "Robbery",
                             "Theft offences",
                             "Violence against the person",
                             "Total recorded crime"),
         tooltip = 
           paste0("<strong>", paste(round(value, 1)), "</strong>", " per 1,000", "<br/>",
                  "<strong><em>", group, "</strong></em><br/>",
                  "<em>", area_name, "</em><br/>",
                  period))

output$crime_rate_plot <- renderggiraph({
  
  gg <-
    ggplot(
      filter(crime_rate, group %in% input$crime_rate_selection),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(size = 1) +
    geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, colour = "white") +
    scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(
      title = "Offence rate per 1,000 population",
      subtitle = input$crime_rate_selection,
      caption = "Source: Home Office",
      x = NULL,
      y = "per 1,000",
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

output$crime_rate_box <- renderUI({
  
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","crime rate per 1,000 population","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
      title = "Crime rates",
      withSpinner(
        ggiraphOutput("crime_rate_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioButtons(
            inputId = "crime_rate_selection",
            label = tags$h4("Select area:"),
            choices = unique(levels(crime_rate$group)),
            selected = "Total recorded crime"
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
          includeMarkdown("data/pride/metadata/crime_rate.md"),
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


# Crime severity  --------------------------------------------------

crime_severity <- read_csv("data/pride/crime_severity.csv") %>% 
  mutate(area_name = fct_relevel(as_factor(area_name),
                                 "Trafford", 
                                 "Greater Manchester",
                                 "England"),
         group = fct_relevel(as_factor(group), 
                             "Criminal damage and arson", 
                             "Other crimes against society",
                             "Sexual offences",
                             "Robbery",
                             "Theft offences",
                             "Violence against the person",
                             "Total recorded crime"),
         tooltip = 
           paste0("<strong>", paste(round(value, 1)), "</strong><br/>",
                  "<strong><em>", group, "</strong></em><br/>",
                  "<em>", area_name, "</em><br/>",
                  period))

output$crime_severity_plot <- renderggiraph({
  
  gg <-
    ggplot(
      filter(crime_severity, group %in% input$crime_severity_selection),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(size = 1) +
    geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, colour = "white") +
    scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(
      title = "Trends in Crime Severity Score",
      subtitle = input$crime_severity_selection,
      caption = "Source: Home Office",
      x = NULL,
      y = "Crime Severity Score",
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

output$crime_severity_box <- renderUI({
  
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","crime severity score","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
      title = "Crime Severity Score",
      withSpinner(
        ggiraphOutput("crime_severity_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioButtons(
            inputId = "crime_severity_selection",
            label = tags$h4("Select area:"),
            choices = unique(levels(crime_severity$group)),
            selected = "Total recorded crime"
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
          includeMarkdown("data/pride/metadata/crime_severity.md"),
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