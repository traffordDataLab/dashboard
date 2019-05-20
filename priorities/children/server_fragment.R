# Children and Young People #

# Child excess weight in Reception year children --------------------------------------------------

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
      coord_flip() +
      labs(title = "Child excess weight in Reception year children",
           subtitle = NULL,
           caption = "Source: PHE Fingertips (PHOF 2.10ii)",
           x = NULL, y = "Percentage",
           fill = "Compared with England:") +
      theme_minimal(base_family = "Open Sans") +
      theme(
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 7, hjust = 1),
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
        filter(excess_weight_reception, area_name %in% c("Trafford", "England")),
        aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip = 
                                   paste0("<strong>", value, "</strong>", "%", "<br/>",
                                          "<em>", area_name, "</em><br/>",
                                          period)), 
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = "#00AFBB", "England" = "#757575")) +
      scale_fill_manual(values = c("Trafford" = "#00AFBB", "England" = "#757575")) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Child excess weight in Reception year children",
        subtitle = NULL,
        caption = "Source: PHE Fingertips (PHOF 2.10ii)",
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
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","emergency hospital admissions for intentional self-harm","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
      title = "Child excess weight in Reception year children",
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

# Indicator name --------------------------------------------------
