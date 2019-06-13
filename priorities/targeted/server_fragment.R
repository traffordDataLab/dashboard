# Targeted support #

# Rough sleeping --------------------------------------------------
rough_sleeping <- read_csv("data/housing/rough_sleeping.csv") %>%
  mutate(area_name = factor(area_name),
         tooltip = 
           paste0("<strong>", round(value, 2), "</strong><br/>",
                  "<em>", area_name, "</em><br/>",
                  period))

output$rough_sleeping_plot <- renderggiraph({
  
  if (input$rough_sleeping_selection == "GM boroughs") {
    
    gg <-
      ggplot(
        rough_sleeping, aes(x = period, y = value, fill = area_name)) +
      geom_bar_interactive(aes(tooltip = tooltip), stat = "identity") +
      scale_fill_manual(values = c("Bolton" = "#E7B800", "Bury" = "#E7B800", "Manchester" = "#E7B800", 
                                   "Oldham" = "#E7B800", "Rochdale" = "#E7B800", "Salford" = "#E7B800", 
                                   "Tameside" = "#E7B800", "Stockport" = "#E7B800", "Trafford" = "#00AFBB", 
                                   "Wigan" = "#E7B800")) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Rough sleeping estimates",
        subtitle = NULL,
        caption = "Source: MHCLG",
        x = NULL,
        y = "Count",
        colour = NULL
      ) +
      facet_wrap(~area_name, nrow = 2) +
      theme_x() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, margin = margin(t = 5))
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  else {
    
    gg <-
      ggplot(
        filter(rough_sleeping, area_name == "Trafford"),
        aes(x = period, y = value)) +
      geom_bar_interactive(aes(tooltip = tooltip), stat = "identity", fill = "#00AFBB") +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Rough sleeping estimates",
        subtitle = NULL,
        caption = "Source: MHCLG",
        x = NULL,
        y = "Count",
        colour = NULL
      ) +
      theme_x()
    
    x <- girafe(ggobj = gg)
    x <- girafe_options(x, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    x
    
  }
  
})

output$rough_sleeping_box <- renderUI({
  
  box(width = 4, 
      hr(style = "border-top: 1px dashed #757575;"),
      title = "Rough sleeping",
      withSpinner(
        ggiraphOutput("rough_sleeping_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "rough_sleeping_selection",
            label = tags$h4("Group by:"),
            choiceNames = c("Trafford", "GM boroughs"),
            choiceValues = c("Trafford", "GM boroughs"), 
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
          includeMarkdown("data/housing/metadata/rough_sleeping.md"),
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