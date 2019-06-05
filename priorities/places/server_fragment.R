# Successful and thriving places #

# Employment rate  --------------------------------------------------
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
  
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","employment rate","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em; padding-right:1em;"),
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

# Median resident earnings  --------------------------------------------------
median_resident_earnings <- read_csv("data/places/median_resident_earnings.csv") %>% 
  mutate(area_name = factor(area_name, levels = c("Trafford", "Greater Manchester", "England"), ordered = TRUE),
         tooltip = 
           paste0("<strong>", paste0("£", formatC(value, format="f", big.mark = ",", digits=0)), "</strong>", " per annum", "<br/>",
                  "<em>", area_name, "</em><br/>",
                  period))

output$median_resident_earnings_plot <- renderggiraph({
  
  gg <-
    ggplot(
      filter(median_resident_earnings, area_name %in% input$median_resident_earnings_selection),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(size = 1) +
    geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, colour = "white") +
    scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_y_continuous(limits = c(0, NA), labels = comma) +
    labs(
      title = "Median resident earnings, all employees",
      subtitle = NULL,
      caption = "Source: Annual Survey of Hours and Earnings",
      x = NULL,
      y = "£",
      colour = NULL
    ) +
    guides(fill = FALSE) +
    theme_x()
  
  gg <- girafe(ggobj = gg)
  girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
  
})

output$median_resident_earnings_box <- renderUI({
  
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","median resident earnings","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em; padding-right:1em;"),
      br(),
      title = "Median pay",
      withSpinner(
        ggiraphOutput("median_resident_earnings_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          checkboxGroupInput(
            inputId = "median_resident_earnings_selection",
            label = tags$h4("Select area:"),
            choices = unique(levels(median_resident_earnings$area_name)),
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
          includeMarkdown("data/places/metadata/median_resident_earnings.md"),
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

# Claimant count  --------------------------------------------------

claimant_count <- read_csv("data/places/claimant_count.csv") %>% 
  filter(area_name == "Trafford") %>% 
  mutate(tooltip = 
           paste0("<strong>", comma(value), "</strong><br/>",
                  "<strong><em>", group, "</strong></em><br/>",
                  "<em>", area_name, "</em><br/>",
                  period))

output$claimant_count_plot <- renderggiraph({
  
  gg <-
    ggplot(claimant_count, aes(x = period, y = value, group = group)) +
    geom_vline_interactive(xintercept = as.Date("2016-05-01"), tooltip = "Universal Credit full roll-out") +
    geom_line_interactive(aes(linetype = group, tooltip = tooltip), size = 1, colour = "#00AFBB") +
    scale_y_continuous(limits = c(0, NA), labels = comma)+
    labs(
      title = "Claimant counts",
      subtitle = input$claimant_count_selection,
      caption = "Source: DWP and ONS",
      x = NULL,
      y = "Count",
      linetype = NULL
    ) +
    theme_x() +
    theme(legend.position = "top")
  
  gg <- girafe(ggobj = gg)
  tooltip_css <- "background-color:#00AFBB;color:#FFFFFF;padding:0.3em;"
  girafe_options(gg, opts_tooltip(css = tooltip_css), opts_toolbar(saveaspng = FALSE))
  
})

output$claimant_count_box <- renderUI({
  
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","claimant count","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em; padding-right:1em;"),
      br(),
      title = "Claimant count",
      withSpinner(
        ggiraphOutput("claimant_count_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          includeMarkdown("data/places/metadata/claimant_count.md"),
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
