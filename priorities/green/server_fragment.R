# Green and connected #

# Nitrogen dioxide concentrations --------------------------------------------------

stations <- tibble(
  site = c("Trafford", "Trafford A56"),
  site_id = c("TRAF", "TRF2")
)

nitrogen_dioxide <- read_csv("data/green/nitrogen_dioxide.csv") %>% 
  mutate(tooltip = 
           paste0("<strong>", value, " μg/m", "<sup>", 3, "</sup>", "</strong><br/>",
                  "<em>", site, "</em><br/>",
                  period))

no2_readings <- reactive({
  
  site_id <- input$nitrogen_dioxide_selection
  start_date <- as.Date(Sys.time()) %m-% months(12)
  end_date <- Sys.Date()
  
  url <- paste0("http://www.airqualityengland.co.uk/local-authority/data.php?site_id=", site_id, "&parameter_id%5B%5D=NO2&f_query_id=920788&data=%3C%3Fphp+print+htmlentities%28%24data%29%3B+%3F%3E&f_date_started=", start_date, "&f_date_ended=", end_date, "&la_id=368&action=download&submit=Download+Data")
  readings <- read_html(url) %>% 
    html_node("a.b_xls.valignt") %>% 
    html_attr('href') %>% 
    read_csv(skip = 5) %>% 
    mutate(`End Date` = as.Date(`End Date`, format = "%d/%m/%Y"),
           date_hour = as.POSIXct(paste(`End Date`, `End Time`), format = "%Y-%m-%d %H:%M:%S"),
           value = as.double(NO2)) %>% 
    select(date_hour:value) %>%
    arrange(date_hour) %>% 
    mutate(tooltip = 
             paste0("<strong>", round(value, 1), " μg/m", "<sup>", 3, "</sup>", "</strong><br/>",
                    "<em>", filter(stations, site_id == input$nitrogen_dioxide_selection)$site, "</em><br/>",
                    date_hour))
  
})

output$nitrogen_dioxide_plot <- renderggiraph({
  
  if (input$nitrogen_dioxide_measure == "Annual mean") {
    
    gg <-
      ggplot(filter(nitrogen_dioxide, site_id == input$nitrogen_dioxide_selection), 
             aes(x = period, y = value)) +
      geom_hline_interactive(aes(yintercept = 40, tooltip = paste0("NO", "<sub>", 2, "</sub>", " annual mean objective")), linetype = "dotted", color = "#000000", size = 1.5) +
      geom_line_interactive(colour = "#00AFBB", size = 1) +
      geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, fill = "#00AFBB", colour = "white") +
      scale_y_continuous(limits = c(0, NA)) +
      labs(title = expression(paste("Annual mean ", NO[2], " concentrations")),
           subtitle = filter(stations, site_id == input$nitrogen_dioxide_selection)$site,
           caption = "Source: Trafford Council / Ricardo EE",
           x = "",
           y = expression(paste("μg/m"^3))) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  }
  else {
    
    gg <-
      ggplot(no2_readings(), aes(x = date_hour, y = value)) +
      geom_hline_interactive(aes(yintercept = 200, tooltip = paste0("NO", "<sub>", 2, "</sub>", " 1-hour mean objective")), linetype = "dotted", color = "#000000", size = 1.5) +
      geom_line(colour = "#00AFBB", size = 1) +
      scale_x_datetime(breaks = no2_readings()$date_hour, date_labels = "%b-%y", date_breaks = "1 month") +
      scale_y_continuous(limits = c(0, max(no2_readings()$value))) +
      labs(title = expression(paste("1-hour mean ", NO[2], " concentrations")),
           subtitle = paste0(sum(no2_readings()$value >= 200, na.rm = TRUE), " exceedances at ", filter(stations, site_id == input$nitrogen_dioxide_selection)$site, " over last 12 months"),
           caption = "Source: Trafford Council / Ricardo EE",
           x = "",
           y = expression(paste("μg/m"^3))) +
      theme_x()
    
    gg <- girafe(ggobj = gg)
    tooltip_css <- "background-color:#00AFBB;color:#FFFFFF;padding:0.3em;"
    girafe_options(gg, opts_tooltip(css = tooltip_css), opts_toolbar(saveaspng = FALSE))
    
  }
  
})

output$nitrogen_dioxide_box <- renderUI({
  
  box(width = 4,
      hr(style = "border-top: 1px solid #757575;"),
      title = "Nitrogen dioxide concentrations",
      withSpinner(
        ggiraphOutput("nitrogen_dioxide_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioButtons(
            inputId = "nitrogen_dioxide_selection",
            label = tags$h4("Select monitoring station:"),
            choices = c("Trafford" = "TRAF", "Trafford A56" = "TRF2"),
            selected = "TRAF"
          ),
          radioButtons(
            inputId = "nitrogen_dioxide_measure",
            label = tags$h4("Select measure:"),
            choices = c("Annual mean", "1-hour mean"),
            selected = "Annual mean"
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
          includeMarkdown("data/green/metadata/nitrogen_dioxide.md"),
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

# Particulate Matter concentrations --------------------------------------------------
  
  particulate_matter <- read_csv("data/green/particulate_matter.csv") %>% 
    mutate(tooltip = 
             paste0("<strong>", value, " μg/m", "<sup>", 3, "</sup>", "</strong><br/>",
                    "<em>", site, "</em><br/>",
                    period))
  
  pm10_readings <- reactive({
    
    site_id <- input$particulate_matter_selection
    start_date <- as.Date(Sys.time()) %m-% months(12)
    end_date <- Sys.Date()
    
    url <- paste0("http://www.airqualityengland.co.uk/local-authority/data.php?site_id=", site_id, "&parameter_id%5B%5D=GE10&f_query_id=920788&data=%3C%3Fphp+print+htmlentities%28%24data%29%3B+%3F%3E&f_date_started=", start_date, "&f_date_ended=", end_date, "&la_id=368&action=download&submit=Download+Data")
    readings <- read_html(url) %>% 
      html_node("a.b_xls.valignt") %>% 
      html_attr('href') %>% 
      read_csv(skip = 5) %>% 
      mutate(date = as.Date(`End Date`, format = "%d/%m/%Y"),
             hour = hour(strptime(`End Time`, format = "%H:%M:%S")),
             value = as.double(PM10)) %>% 
      select(date:value) %>% 
      group_by(date) %>% 
      summarise(mean = mean(value, na.rm = TRUE)) %>% 
      arrange(date) %>% 
      mutate(tooltip = 
               paste0("<strong>", round(mean, 1), " μg/m", "<sup>", 3, "</sup>", "</strong><br/>",
                      "<em>", filter(stations, site_id == input$particulate_matter_selection)$site, "</em><br/>",
                      paste0(day(date), "-", month(date), "-", year(date))))
    
  })
  
  output$particulate_matter_plot <- renderggiraph({
    
    if (input$particulate_matter_measure == "Annual mean") {
      
      gg <-
        ggplot(filter(particulate_matter, site_id == input$particulate_matter_selection), 
               aes(x = period, y = value)) +
        geom_hline_interactive(aes(yintercept = 40, tooltip = paste0("PM", "<sub>", 10, "</sub>", " annual mean objective")), linetype = "dotted", color = "#000000", size = 1.5) +
        geom_line(colour = "#00AFBB", size = 1) +
        geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, fill = "#00AFBB", colour = "white") +
        scale_y_continuous(limits = c(0, NA)) +
        labs(title = expression(paste("Annual mean ", PM[10], " concentrations")),
             subtitle = filter(stations, site_id == input$particulate_matter_selection)$site,
             caption = "Source: Trafford Council / Ricardo EE",
             x = "",
             y = expression(paste("μg/m"^3))) +
        theme_x()
      
      gg <- girafe(ggobj = gg)
      girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
      
    }
    else {
      
      gg <-
        ggplot(pm10_readings(), aes(x = date, y = mean)) +
        geom_hline_interactive(aes(yintercept = 50, tooltip = paste0("PM", "<sub>", 10, "</sub>", " annual mean objective")), linetype = "dotted", color = "#000000", size = 1.5) +
        geom_line(colour = "#00AFBB", size = 1) +
        #geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, fill = "#00AFBB", colour = "white") +
        scale_x_date(breaks = pm10_readings()$date, date_labels = "%b-%y", date_breaks = "1 month") +
        scale_y_continuous(limits = c(0, max(pm10_readings()$mean))) +
        labs(title = expression(paste("24-hour mean ", PM[10], " concentrations")),
             subtitle = paste0(sum(pm10_readings()$mean >= 50, na.rm = TRUE), " exceedances at ", filter(stations, site_id == input$particulate_matter_selection)$site, " over last 12 months"),
             caption = "Source: Trafford Council / Ricardo EE",
             x = "",
             y = expression(paste("μg/m"^3))) +
        theme_x()
      
      gg <- girafe(ggobj = gg)
      girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
      
    }
    
  })
  
  output$particulate_matter_box <- renderUI({
    
    box(width = 4, 
        hr(style = "border-top: 1px solid #757575;"),
        title = "Particulate Matter concentrations",
        withSpinner(
          ggiraphOutput("particulate_matter_plot"),
          type = 4,
          color = "#bdbdbd",
          size = 1
        ),
        div(
          style = "position: absolute; left: 1.5em; bottom: 0.5em;",
          dropdown(
            radioButtons(
              inputId = "particulate_matter_selection",
              label = tags$h4("Select monitoring station:"),
              choices = c("Trafford" = "TRAF", "Trafford A56" = "TRF2"),
              selected = "TRAF"
            ),
            radioButtons(
              inputId = "particulate_matter_measure",
              label = tags$h4("Select measure:"),
              choices = c("Annual mean", "24-hour mean"),
              selected = "Annual mean"
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
            includeMarkdown("data/green/metadata/particulate_matter.md"),
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
  
# CO2 emissions --------------------------------------------------
  
co2_emissions <- read_csv("data/green/co2_emissions.csv") %>% 
    mutate(group = fct_relevel(group, "Total"),
           tooltip = 
             paste0("<strong>", round(value,1), " (kt CO", "<sub>", 2, ")", "</sub>", "</strong><br/>",
                    "<strong><em>", group, "</strong></em><br/>",
                    "<em>", area_name, "</em><br/>",
                    period))
  
  output$co2_emissions_plot <- renderggiraph({
    
    gg <- ggplot(filter(co2_emissions, group %in% input$co2_emissions_selection),
                 aes(x = period, y = value, fill = group)) +
      geom_line(size = 1, colour = "#00AFBB") +
      geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, colour = "white", fill = "#00AFBB") +
      scale_y_continuous(limits = c(0, NA), labels = comma) +
      labs(title = expression(paste(CO[2], " emissions")),
           subtitle = input$co2_emissions_selection,
           caption = "Source: Department for Business, Energy & Industrial Strategy",
           x = NULL,
           y = expression(paste("kt ", CO[2])),
           fill = NULL) +
      theme_x() 
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    
  })
  
  output$co2_emissions_box <- renderUI({
    
    box(width = 4, 
        hr(style = "border-top: 1px solid #757575;"),
        title = HTML(paste0("CO", "<sub>", 2, "</sub>", " emissions")),
        withSpinner(
          ggiraphOutput("co2_emissions_plot"),
          type = 4,
          color = "#bdbdbd",
          size = 1
        ),
        div(
          style = "position: absolute; left: 1.5em; bottom: 0.5em;",
          dropdown(
            radioButtons(
              inputId = "co2_emissions_selection",
              label = tags$h4("Select sector:"),
              choices = unique(levels(co2_emissions$group)),
              selected = "Total"
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
            includeMarkdown("data/green/metadata/co2_emissions.md"),
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
  
# Licensed vehicles  -------------------------------------------------- 
  
licensed_vehicles <- read_csv("data/green/licensed_vehicles.csv") %>% 
    mutate(group = fct_relevel(group, "Diesel vehicles"),
           tooltip = 
             paste0("<strong>", comma(value), "</strong><br/>",
                    "<strong><em>", group, "</strong></em><br/>",
                    "<em>", area_name, "</em><br/>",
                    period))
  
  output$licensed_vehicles_plot <- renderggiraph({
    
    gg <- ggplot(filter(licensed_vehicles, group %in% input$licensed_vehicles_selection),
                 aes(x = period, y = value, colour = area_name, fill = area_name,
                     group = fct_relevel(area_name, "Trafford", after = Inf))) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip = tooltip, data_id = area_name), 
                             shape = 21, size = 2.5, colour = "white", alpha = 0.01) +
      scale_colour_manual(values = ifelse(licensed_vehicles$area_name == "Trafford", "#00AFBB", "#d9d9d9")) +
      scale_fill_manual(values = ifelse(licensed_vehicles$area_name == "Trafford", "#00AFBB", "#d9d9d9")) +
      scale_y_continuous(limits = c(0, NA), labels = comma) +
      labs(title = "Number of licensed vehicles",
           subtitle = input$licensed_vehicles_selection,
           caption = "Source: DfT and DVLA",
           x = NULL,
           y = "Count",
           fill = NULL) +
      theme_x() 
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg, opts_tooltip(use_fill = TRUE, opacity = 1),
                   opts_hover(css = "fill-opacity:1;stroke:white;stroke-opacity:1;r:2.5pt"),
                   opts_selection(type = "single"),
                   opts_toolbar(saveaspng = FALSE))
    
  })
  
  output$licensed_vehicles_box <- renderUI({
    
    box(width = 4, 
        hr(style = "border-top: 1px solid #757575;"),
        title = "Licensed vehicles",
        withSpinner(
          ggiraphOutput("licensed_vehicles_plot"),
          type = 4,
          color = "#bdbdbd",
          size = 1
        ),
        div(
          style = "position: absolute; left: 1.5em; bottom: 0.5em;",
          dropdown(
            radioButtons(
              inputId = "licensed_vehicles_selection",
              label = tags$h4("Select type:"),
              choices = unique(levels(licensed_vehicles$group)),
              selected = "Diesel vehicles"
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
            includeMarkdown("data/green/metadata/licensed_vehicles.md"),
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

# Greater Manchester Accessibility Levels --------------------------------------------------
  
gmal <- st_read("data/green/gmal.geojson")
  
  output$gmal_map = renderLeaflet({
    
    pal <- colorFactor(c("#084594","#4292c6","#9ecae1","#deebf7","#ffffb3",
                         "#fdb462","#e41a1c","#a65628"), domain = 1:8, ordered = TRUE)
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = tileOptions(minZoom = 11, maxZoom = 17)) %>%
      addPolygons(data = gmal,
                  fillColor = ~pal(GMALLevel), fillOpacity = 0.5,
                  stroke = FALSE) %>%
      addPolygons(data = boundary,
                  fillOpacity = 0, color = "#212121", weight = 2, opacity = 1) %>%
      addLegend(position = "bottomright",
                colors = c("#084594","#4292c6","#9ecae1","#deebf7","#ffffb3","#fdb462","#e41a1c","#a65628"),
                title = NULL,
                labels = c("1 Low accessibility",
                           "2","3","4","5","6","7",
                           "8 High accessibility"), opacity = 0.5) %>%
      addControl("<strong>GMAL</strong>", position = 'topright')
  })
  
  output$gmal_box <- renderUI({
    
    box(width = 4,
        hr(style = "border-top: 1px solid #757575;"),
        title = "Public transport accessibility",
        withSpinner(
          leafletOutput("gmal_map"),
          type = 4,
          color = "#bdbdbd",
          size = 1
        ),
        div(
          style = "position: absolute; left: 1.5em; bottom: 0.5em;",
          dropdown(
            includeMarkdown("data/green/metadata/gmal.md"),
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
  
  