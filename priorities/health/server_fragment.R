# Health and wellbeing #

# Alcohol related admissions --------------------------------------------------

alcohol_related_admissions <- read_csv("data/health/alcohol_related_admissions.csv") %>%
  mutate(area_name = factor(area_name),
         tooltip = paste0(
           "Area: ", area_name, "<br/>",
           "Period: ", period, "<br/>",
           "Value: ", round(value,0), " per 100,000"))

output$alcohol_related_admissions_plot <- renderggiraph({
  
  if (input$alcohol_related_admissions_area_name == "Statistical neighbours") {
    
    gg <-
      ggplot(
        alcohol_related_admissions, aes(x = period, y = value, 
                                        colour = area_name, fill = area_name, 
                                        group = fct_relevel(area_name, "Trafford", after = Inf)
        )) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = ifelse(alcohol_related_admissions$area_name == "Trafford", "#00AFBB", "#d9d9d9")) +
      scale_fill_manual(values = ifelse(alcohol_related_admissions$area_name == "Trafford", "#00AFBB", "#d9d9d9")) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Alcohol related admissions",
        subtitle = NULL,
        caption = "Source: Public Health England",
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
        title = "Alcohol related admissions",
        subtitle = NULL,
        caption = "Source: Public Health England",
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
