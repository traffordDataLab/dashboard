# Successful and thriving places #

# employment rate  --------------------------------------------------
employment_rate <- read_csv("data/places/employment_rate.csv") %>% 
  mutate(area_name = factor(area_name, levels = c("Trafford", "Greater Manchester", "England"), ordered = TRUE),
         tooltip = 
           paste0("<strong>", paste(round(value, 1)), "</strong>", "%", "<br/>",
                  "<em>", area_name, "</em><br/>",
                  paste0(year(period), "-", month(period))))

output$employment_rate_plot <- renderggiraph({
  
  gg <-
    ggplot(
      filter(employment_rate, area_name %in% input$employment_rate_selection),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(size = 1) +
    geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, colour = "white") +
    scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_x_date(breaks = seq(as.Date("2009-12-01"), as.Date("2018-12-01"),  by = "1 year"), labels = date_format("%b-%Y")) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(
      title = "Employment rate - aged 16-64",
      subtitle = NULL,
      caption = "Source: Annual Population Survey",
      x = "12 months ending",
      y = "Percentage",
      colour = NULL
    ) +
    guides(fill = FALSE) +
    theme_x()
  
  gg <- girafe(ggobj = gg)
  girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
  
})

output$employment_rate_box <- renderUI({
  
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","employment_rate","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
      title = "Employment rate",
      withSpinner(
        ggiraphOutput("employment_rate_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          checkboxGroupInput(
            inputId = "employment_rate_selection",
            label = tags$h4("Select area:"),
            choices = unique(levels(employment_rate$area_name)),
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
          includeMarkdown("data/places/metadata/employment_rate.md"),
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