# Green and connected #

# Nitrogen dioxide concentrations --------------------------------------------------

stations <- tibble(
  site = c("Trafford", "Trafford A56", "Trafford Wellacre Academy"),
  site_id = c("TRAF", "TRF2", "TRF3")
)

nitrogen_dioxide <- read_csv("data/green/nitrogen_dioxide.csv") %>% 
  mutate(tooltip = 
           paste0("<strong>", value, " μg/m", "<sup>", 3, "</sup>", "</strong><br/>",
                  "<em>", site, "</em><br/>",
                  period))
  
  readings <- reactive({
    
    site_id <- input$nitrogen_dioxide_selection
    start_date <- as.Date(Sys.time()) %m-% months(6)
    end_date <- Sys.Date()
    
    url <- paste0("http://www.airqualityengland.co.uk/local-authority/data.php?site_id=", site_id, "&parameter_id%5B%5D=NO2&f_query_id=920788&data=%3C%3Fphp+print+htmlentities%28%24data%29%3B+%3F%3E&f_date_started=", start_date, "&f_date_ended=", end_date, "&la_id=368&action=download&submit=Download+Data")
    readings <- url %>% 
      read_html() %>% 
      html_nodes(xpath = "//*[@id='pageArea']/table") %>% 
      html_table(header = FALSE, trim = TRUE, fill = TRUE) %>% 
      as.data.frame() 
    colnames(readings) <- readings[2,]
    readings <- readings[c(-1,-2),]
    readings <- readings %>% 
      mutate(date = as.Date(`End Date`, format = "%d/%m/%Y"),
             hour = hour(strptime(Time, format = "%H:%M:%S")),
             value = as.double(NO2),
             units = `Status/units`) %>% 
      select(date:units)
    
  })
  
  output$nitrogen_dioxide_plot <- renderggiraph({
    
    if (input$nitrogen_dioxide_measure == "Annual mean") {
      
      gg <-
        ggplot(filter(nitrogen_dioxide, site_id == input$nitrogen_dioxide_selection), 
               aes(x = period, y = value)) +
        geom_hline_interactive(aes(yintercept = 40, tooltip = paste0("NO", "<sub>", 2, "</sub>", " annual mean objective")), color = "#000000", linetype = "dotted", size = 1.5) +
        geom_line(colour = "#00AFBB", size = 1) +
        geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, fill = "#00AFBB", colour = "white") +
        scale_y_continuous(limits = c(0, NA)) +
        labs(title = expression(paste("Annual mean ", NO[2], " concentrations")),
             subtitle = filter(stations, site_id == input$nitrogen_dioxide_selection)$site,
             caption = "Source: Trafford Council / Air Quality England",
             x = "",
             y = expression(paste("μg/m"^3))) +
        theme_x()
      
      gg <- girafe(ggobj = gg)
      girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
      
    }
    else {
      
      daily_means <- readings() %>% 
        group_by(date) %>% 
        summarise(mean = mean(value, na.rm = TRUE)) %>% 
        arrange(date) %>% 
        mutate(tooltip = 
                 paste0("<strong>", round(mean, 1), " μg/m", "<sup>", 3, "</sup>", "</strong><br/>",
                        "<em>", filter(stations, site_id == input$nitrogen_dioxide_selection)$site, "</em><br/>",
                        paste0(day(date), "-", month(date), "-", year(date))))
      
      gg <-
        ggplot(daily_means, aes(x = date, y = mean)) +
        geom_line(colour = "#00AFBB", size = 1) +
        geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, fill = "#00AFBB", colour = "white") +
        scale_x_date(breaks = daily_means$date, date_labels = "%b", date_breaks = "1 month") +
        scale_y_continuous(limits = c(0, max(daily_means$mean))) +
        labs(title = expression(paste("24-hour mean ", NO[2], " concentrations")),
             subtitle = filter(stations, site_id == input$nitrogen_dioxide_selection)$site,
             caption = "Source: Trafford Council / Air Quality England",
             x = "",
             y = expression(paste("μg/m"^3))) +
        theme_x()
      
      gg <- girafe(ggobj = gg)
      girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
      
    }
    
  })
  
  output$nitrogen_dioxide_box <- renderUI({
    
    box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","NO", "<sub>", 2, "</sub>", " concentrations","</b>", "  not set.", "</h5>")),
                       style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
        br(),
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
              choices = c("Trafford" = "TRAF", "Trafford A56" = "TRF2", "Trafford Wellacre Academy" = "TRF3"),
              selected = "TRAF"
            ),
            radioButtons(
              inputId = "nitrogen_dioxide_measure",
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



# Particulate matter concentrations --------------------------------------------------