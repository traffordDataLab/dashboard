# Pride in our area #

# Crime rate  --------------------------------------------------

crime_rate <- read_csv("data/pride/crime_rate.csv") %>% 
  mutate(area_name = as_factor(area_name),
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
    theme_x()
  
  gg <- girafe(ggobj = gg)
  girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
  
})

output$crime_rate_box <- renderUI({
  
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","crime rate per 1,000 population","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em; padding-right:1em;"),
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
  mutate(area_name = as_factor(area_name),
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
    theme_x()
  
  gg <- girafe(ggobj = gg)
  girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
  
})

output$crime_severity_box <- renderUI({
  
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","crime severity score","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em; padding-right:1em;"),
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

# Potholes --------------------------------------------------

potholes <- read_csv("data/pride/potholes.csv") %>% 
  mutate(area_name = factor(area_name),
         period = as.Date(paste(period, 1, 1, sep = "-")),
         tooltip = 
           paste0("<strong>", paste(round(value, 1)), "</strong><br/>",
                  "<em>", area_name, "</em><br/>",
                  year(period)))

output$potholes_plot <- renderggiraph({
  
  if (input$potholes_selection == "GM boroughs") {
    
    gg <-
      ggplot(
        potholes, aes(x = period, y = value, fill = area_name)) +
      geom_bar_interactive(aes(tooltip = tooltip), stat = "identity") +
      scale_fill_manual(values = c("Bolton" = "#E7B800", "Bury" = "#E7B800", "Manchester" = "#E7B800", 
                                   "Oldham" = "#E7B800", "Rochdale" = "#E7B800", "Salford" = "#E7B800", 
                                   "Tameside" = "#E7B800", "Stockport" = "#E7B800", "Trafford" = "#00AFBB", 
                                   "Wigan" = "#E7B800")) +
      scale_x_date(date_labels = "%Y", date_breaks = "2 year", expand = c(0,0)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Reports of potholes",
        subtitle = NULL,
        caption = "Source: FixMyStreet.com",
        x = NULL,
        y = "Count",
        colour = NULL
      ) +
      facet_wrap(~area_name, nrow = 2) +
      theme_x() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, margin = margin(t = 0))
      )
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  else {
    
    gg <-
      ggplot(
        filter(potholes, area_name == "Trafford"),
        aes(x = period, y = value)) +
      geom_bar_interactive(aes(tooltip = tooltip), stat = "identity", fill = "#00AFBB") +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year", expand = c(0,0)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Reports of potholes",
        subtitle = NULL,
        caption = "Source: FixMyStreet.com",
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

output$potholes_box <- renderUI({
  
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","reports of potholes","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em; padding-right:1em;"),
      br(),
      title = "Potholes",
      withSpinner(
        ggiraphOutput("potholes_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "potholes_selection",
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
          includeMarkdown("data/pride/metadata/potholes.md"),
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


# Flytipping --------------------------------------------------

flytipping <- read_csv("data/pride/flytipping.csv") %>% 
  mutate(area_name = factor(area_name, levels = c("Trafford", "Greater Manchester", "England"), ordered = TRUE),
         tooltip = 
           paste0("<strong>", formatC(value, format = "f", big.mark = ",", digits = 0), "</strong><br/>",
                  "<em>", area_name, "</em><br/>",
                  period))

output$flytipping_plot <- renderggiraph({
  
  gg <-
    ggplot(
      filter(flytipping, area_name %in% input$flytipping_selection),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(size = 1) +
    geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, colour = "white") +
    scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_y_continuous(limits = c(0, NA), labels = comma) +
    labs(
      title = "Flytipping incidents",
      subtitle = input$flytipping_selection,
      caption = "Source: DEFRA",
      x = NULL,
      y = "Count",
      colour = NULL
    ) +
    guides(fill = FALSE) +
    theme_x()
  
  gg <- girafe(ggobj = gg)
  girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
  
})

output$flytipping_box <- renderUI({
  
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","flytipping indcients","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em; padding-right:1em;"),
      br(),
      title = "Flytipping incidents",
      withSpinner(
        ggiraphOutput("flytipping_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioButtons(
            inputId = "flytipping_selection",
            label = tags$h4("Select area:"),
            choices = unique(levels(flytipping$area_name)),
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
          includeMarkdown("data/pride/metadata/flytipping.md"),
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

# Green Flag Awards --------------------------------------------------

green_flags <- read_csv("data/pride/green_flags.csv") %>% 
  mutate(area_name = as_factor(area_name))

output$green_flags_map = renderLeaflet({
  
  if(input$green_flags_selection == "Greater Manchester"){
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(data = gm_boundary,
                  fillOpacity = 0, color = "#212121", weight = 2, opacity = 1) %>% 
      addCircleMarkers(data = green_flags,
                       lng = ~lon, lat = ~lat,
                       stroke = TRUE, color = "#212121", weight = 2, 
                       fillColor = ifelse(green_flags$area_name == "Trafford", "#00AFBB", "#E7B800"),
                       fillOpacity = 0.5, radius = 4,
                       popup = paste("<strong>", green_flags$name, "</strong><br />",
                                     "<em>", green_flags$area_name, "</em><br/>",
                                     "<a href='", green_flags$url, "' target='_blank'>Further info</a>")) %>% 
      addControl("<strong>Green Flag Awards</strong>", position = 'topright')
  }
  else {
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(data = boundary,
                  fillOpacity = 0, color = "#212121", weight = 2, opacity = 1) %>% 
      addCircleMarkers(data = filter(green_flags, area_name == "Trafford"),
                       lng = ~lon, lat = ~lat,
                       stroke = TRUE, color = "#212121", weight = 2, 
                       fillColor = "#00AFBB", fillOpacity = 0.5, radius = 4,
                       popup = paste("<strong>", filter(green_flags, area_name == "Trafford")$name, "</strong><br />",
                                     "<em>", filter(green_flags, area_name == "Trafford")$area_name, "</em><br/>",
                                     "<a href='", filter(green_flags, area_name == "Trafford")$url, "' target='_blank'>Further info</a>")) %>% 
      addControl("<strong>Green Flag Awards</strong>", position = 'topright')
  }
})

output$green_flags_box <- renderUI({
  
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","Green Flag Awards","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em; padding-right:1em;"),
      br(),
      title = "Green Flag Awards",
      withSpinner(
        leafletOutput("green_flags_map"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioButtons(inputId = "green_flags_selection", 
                       tags$h4("Show:"),
                       choices = c("Trafford", "Greater Manchester"), 
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
        style = "position: absolute; left: 4em; bottom: 0.5em;",
        dropdown(
          includeMarkdown("data/pride/metadata/green_flags.md"),
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


# Recycling --------------------------------------------------

recycling <- read_csv("data/pride/recycling.csv") %>% 
  mutate(area_name = factor(area_name, levels = c("Trafford", "Greater Manchester", "England"), ordered = TRUE),
         tooltip = 
           paste0("<strong>", percent(value), "</strong><br/>",
                  "<em>", area_name, "</em><br/>",
                  period))  

output$recycling_plot <- renderggiraph({
  
  gg <-
    ggplot(recycling, aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(size = 1) +
    geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, colour = "white") +
    scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_y_continuous(limits = c(0, NA), labels = percent_format(accuracy = 1)) +
    labs(
      title = "Household waste sent for recycling",
      subtitle = input$recycling_selection,
      caption = "Source: DEFRA",
      x = NULL,
      y = "Percentage",
      colour = NULL
    ) +
    guides(fill = FALSE) +
    theme_x()
  
  gg <- girafe(ggobj = gg)
  girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
  
})

output$recycling_box <- renderUI({
  
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","recycling household waste","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em; padding-right:1em;"),
      br(),
      title = "Recycled household waste",
      withSpinner(
        ggiraphOutput("recycling_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          includeMarkdown("data/pride/metadata/recycling.md"),
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






