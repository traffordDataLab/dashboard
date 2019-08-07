# Targeted support #

# Rough sleeping --------------------------------------------------
rough_sleeping <- read_csv("data/targeted/rough_sleeping.csv") %>%
  mutate(
    area_name = factor(area_name),
    tooltip =
      paste0(
        "<strong>",
        round(value, 2),
        "</strong><br/>",
        "<em>",
        area_name,
        "</em><br/>",
        period
      )
  )

output$rough_sleeping_plot <- renderggiraph({
  if (input$rough_sleeping_selection == "GM boroughs") {
    gg <-
      ggplot(rough_sleeping, aes(x = period, y = value, fill = area_name)) +
      geom_bar_interactive(aes(tooltip = tooltip), stat = "identity") +
      scale_fill_manual(
        values = c(
          "Bolton" = "#E7B800",
          "Bury" = "#E7B800",
          "Manchester" = "#E7B800",
          "Oldham" = "#E7B800",
          "Rochdale" = "#E7B800",
          "Salford" = "#E7B800",
          "Tameside" = "#E7B800",
          "Stockport" = "#E7B800",
          "Trafford" = "#00AFBB",
          "Wigan" = "#E7B800"
        )
      ) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Rough sleeping estimates",
        subtitle = NULL,
        caption = "Source: MHCLG",
        x = NULL,
        y = "Count",
        colour = NULL
      ) +
      facet_wrap( ~ area_name, nrow = 2) +
      theme_x() +
      theme(axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        margin = margin(t = 5)
      ))
    
    gg <- girafe(ggobj = gg)
    girafe_options(gg,
                   opts_tooltip(use_fill = TRUE),
                   opts_toolbar(saveaspng = FALSE))
    
  }
  else {
    gg <-
      ggplot(filter(rough_sleeping, area_name == "Trafford"),
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
    x <-
      girafe_options(x,
                     opts_tooltip(use_fill = TRUE),
                     opts_toolbar(saveaspng = FALSE))
    x
    
  }
  
})

output$rough_sleeping_box <- renderUI({
  div(
    class = "col-sm-12 col-md-6 col-lg-4",
    box(
      width = '100%',
      hr(style = "border-top: 1px solid #757575;"),
      title = "Rough sleeping",
      withSpinner(
        ggiraphOutput("rough_sleeping_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      )
    ),
    br(),
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
        includeMarkdown("data/targeted/metadata/rough_sleeping.md"),
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

# Delayed Transfers of Care --------------------------------------------------
dtoc <- read_csv("data/targeted/dtoc.csv") %>%
  mutate(
    area_name = factor(area_name),
    period = as.Date(period, format = "%Y-%b-%d"),
    tooltip =
      paste0(
        "<strong>",
        round(value, 1),
        "</strong><br/>",
        "<em>",
        area_name,
        "</em><br/>",
        period
      )
  )

  output$dtoc_plot <- renderggiraph({
    if (input$dtoc_selection == TRUE) {
      gg <-
        ggplot(dtoc, aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
        geom_line(size = 1) +
        geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, colour = "white") +
        scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800")) +
        scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800")) +
        scale_y_continuous(limits = c(0, NA), label = comma) +
        labs(
          title = "DTOC beds attributable to social care",
          subtitle = NULL,
          caption = "Source: NHS England",
          x = NULL,
          y = "Daily DTOC beds",
          colour = NULL
        ) +
        theme_x()
      
      gg <- girafe(ggobj = gg)
      girafe_options(gg,
                     opts_tooltip(use_fill = TRUE),
                     opts_toolbar(saveaspng = FALSE))
      
    }
    else {
      gg <-
        ggplot(filter(dtoc, area_name == "Trafford"),
               aes(x = period, y = value, group = area_name)) +
        geom_line(size = 1, colour = "#00AFBB") +
        geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, colour = "white", fill = "#00AFBB") +
        scale_y_continuous(limits = c(0, NA), label = comma) +
        labs(
          title = "DTOC beds attributable to social care",
          subtitle = NULL,
          caption = "Source: NHS England",
          x = NULL,
          y = "Daily DTOC beds",
          colour = NULL
        ) +
        theme_x()
      
      gg <- girafe(ggobj = gg)
      girafe_options(gg,
                     opts_tooltip(use_fill = TRUE),
                     opts_toolbar(saveaspng = FALSE))
      
    }
    
  })
  
  output$dtoc_box <- renderUI({
    
    div(
      class = "col-sm-12 col-md-6 col-lg-4",
      box(
        width = '100%',
        hr(style = "border-top: 1px solid #757575;"),
        title = "Delayed transfers of care",
        withSpinner(
          ggiraphOutput("dtoc_plot"),
          type = 4,
          color = "#bdbdbd",
          size = 1
        )
      ),
      br(),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          checkboxInput("dtoc_selection", label = "Average for local authorities in Greater Manchester", value = FALSE),
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
          includeMarkdown("data/targeted/metadata/dtoc.md"),
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


# Residential home admissions --------------------------------------------------
  
residential_home_admissions <- read_csv("data/targeted/residential_home_admissions.csv") %>%
    mutate(
      area_name = factor(area_name),
      period = as.factor(period),
      tooltip =
        paste0(
          "<strong>",
          round(value, 1),
          "</strong><br/>",
          "<em>",
          area_name,
          "</em><br/>",
          period
        )
    )
  
output$residential_home_admissions_plot <- renderggiraph({
      if (input$residential_home_admissions_selection == "GM boroughs") {
        gg <-
          ggplot(residential_home_admissions, aes(x = period, y = value, fill = area_name)) +
          geom_bar_interactive(aes(tooltip = tooltip), stat = "identity") +
          scale_fill_manual(
            values = c(
              "Bolton" = "#E7B800",
              "Bury" = "#E7B800",
              "Manchester" = "#E7B800",
              "Oldham" = "#E7B800",
              "Rochdale" = "#E7B800",
              "Salford" = "#E7B800",
              "Tameside" = "#E7B800",
              "Stockport" = "#E7B800",
              "Trafford" = "#00AFBB",
              "Wigan" = "#E7B800"
            )
          ) +
          scale_y_continuous(limits = c(0, NA)) +
          labs(
            title = "Admissions to residential and nursing care",
            subtitle = NULL,
            caption = "Source: NHS Digital",
            x = NULL,
            y = "Per 100,000 population",
            colour = NULL
          ) +
          facet_wrap( ~ area_name, nrow = 2) +
          theme_x() +
          theme(axis.text.x = element_text(
            angle = 90,
            hjust = 1,
            margin = margin(t = 0)
          ))
        
        gg <- girafe(ggobj = gg)
        girafe_options(gg,
                       opts_tooltip(use_fill = TRUE),
                       opts_toolbar(saveaspng = FALSE))
        
      }
      else {
        gg <-
          ggplot(filter(residential_home_admissions, area_name == "Trafford"),
                 aes(x = period, y = value)) +
          geom_bar_interactive(aes(tooltip = tooltip), stat = "identity", fill = "#00AFBB") +
          scale_y_continuous(limits = c(0, NA)) +
          labs(
            title = "Admissions to residential and nursing care",
            subtitle = NULL,
            caption = "Source: NHS Digital",
            x = NULL,
            y = "Per 100,000 population",
            colour = NULL
          ) +
          theme_x()
        
        x <- girafe(ggobj = gg)
        x <-
          girafe_options(x,
                         opts_tooltip(use_fill = TRUE),
                         opts_toolbar(saveaspng = FALSE))
        x
        
      }
      
    })
    
    output$residential_home_admissions_box <- renderUI({
      div(
        class = "col-sm-12 col-md-6 col-lg-4",
        box(
          width = '100%',
          hr(style = "border-top: 1px solid #757575;"),
          title = "Residential home admissions",
          withSpinner(
            ggiraphOutput("residential_home_admissions_plot"),
            type = 4,
            color = "#bdbdbd",
            size = 1
          )
        ),
        br(),
        div(
          style = "position: absolute; left: 1.5em; bottom: 0.5em;",
          dropdown(
            radioGroupButtons(
              inputId = "residential_home_admissions_selection",
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
            includeMarkdown("data/targeted/metadata/residential_home_admissions.md"),
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

# Older people still at home 91 days after discharge --------------------------------------------------
    
    home_after_discharge <- read_csv("data/targeted/home_after_discharge.csv") %>%
      mutate(
        area_name = factor(area_name),
        period = as.factor(period),
        tooltip =
          paste0(
            "<strong>",
            paste0(value, "%"),
            "</strong><br/>",
            "<em>",
            area_name,
            "</em><br/>",
            period
          )
      )
  
    output$home_after_discharge_plot <- renderggiraph({
      if (input$home_after_discharge_selection == "GM boroughs") {
        gg <-
          ggplot(home_after_discharge, aes(x = period, y = value, fill = area_name)) +
          geom_bar_interactive(aes(tooltip = tooltip), stat = "identity") +
          scale_fill_manual(
            values = c(
              "Bolton" = "#E7B800",
              "Bury" = "#E7B800",
              "Manchester" = "#E7B800",
              "Oldham" = "#E7B800",
              "Rochdale" = "#E7B800",
              "Salford" = "#E7B800",
              "Tameside" = "#E7B800",
              "Stockport" = "#E7B800",
              "Trafford" = "#00AFBB",
              "Wigan" = "#E7B800"
            )
          ) +
          scale_y_continuous(limits = c(0, NA)) +
          labs(
            title = "Older people still at home 91 days after discharge",
            subtitle = NULL,
            caption = "Source: NHS Digital",
            x = NULL,
            y = "Percent",
            colour = NULL
          ) +
          facet_wrap( ~ area_name, nrow = 2) +
          theme_x() +
          theme(axis.text.x = element_text(
            angle = 90,
            hjust = 1,
            margin = margin(t = 0)
          ))
        
        gg <- girafe(ggobj = gg)
        girafe_options(gg,
                       opts_tooltip(use_fill = TRUE),
                       opts_toolbar(saveaspng = FALSE))
        
      }
      else {
        gg <-
          ggplot(filter(home_after_discharge, area_name == "Trafford"),
                 aes(x = period, y = value)) +
          geom_bar_interactive(aes(tooltip = tooltip), stat = "identity", fill = "#00AFBB") +
          scale_y_continuous(limits = c(0, NA)) +
          labs(
            title = "Older people still at home 91 days after discharge",
            subtitle = NULL,
            caption = "Source: NHS Digital",
            x = NULL,
            y = "Percent",
            colour = NULL
          ) +
          theme_x()
        
        x <- girafe(ggobj = gg)
        x <-
          girafe_options(x,
                         opts_tooltip(use_fill = TRUE),
                         opts_toolbar(saveaspng = FALSE))
        x
        
      }
      
    })
    
    output$home_after_discharge_box <- renderUI({
      div(
        class = "col-sm-12 col-md-6 col-lg-4",
        box(
          width = '100%',
          hr(style = "border-top: 1px solid #757575;"),
          title = "At home 91 days after discharge",
          withSpinner(
            ggiraphOutput("home_after_discharge_plot"),
            type = 4,
            color = "#bdbdbd",
            size = 1
          )
        ),
        br(),
        div(
          style = "position: absolute; left: 1.5em; bottom: 0.5em;",
          dropdown(
            radioGroupButtons(
              inputId = "home_after_discharge_selection",
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
            includeMarkdown("data/targeted/metadata/home_after_discharge.md"),
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
    
# Care homes --------------------------------------------------

    care_homes <- read_csv("data/targeted/care_homes.csv") %>% 
      filter(rating != "NULL") %>% 
      mutate(rating = fct_relevel(as_factor(rating),
                                  level = c("Outstanding", "Good", "Requires improvement", "Inadequate")))
    
    output$care_homes_map = renderLeaflet({
      
      sf <- filter(care_homes, rating == input$care_homes_selection)
      
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Positron,
                         options = tileOptions(minZoom = 11, maxZoom = 17)) %>% 
        addPolygons(data = boundary,
                    fillOpacity = 0, color = "#212121", weight = 2, opacity = 1) %>% 
        addCircleMarkers(data = sf,
                         lng = ~lon, lat = ~lat,
                         stroke = TRUE, color = "#212121", weight = 2, 
                         fillColor = "#00AFBB", fillOpacity = 0.5, radius = 4,
                         popup = paste("<strong>", sf$name, "</strong><br />",
                                       "<em>", sf$rating, "</em><br />",
                                       "Last inspection date:",  sf$inspection_date))
      
    })
    
    output$care_homes_box <- renderUI({
      div(class = "col-sm-12 col-md-6 col-lg-4",
          box(width = '100%', 
              hr(style = "border-top: 1px solid #757575;"),
              title = "Care homes",
              withSpinner(
                leafletOutput("care_homes_map"),
                type = 4,
                color = "#bdbdbd",
                size = 1
              ),
              div(
                style = "position: absolute; left: 1.5em; bottom: 0.5em;",
                dropdown(
                  radioButtons(inputId = "care_homes_selection", 
                               tags$h4("Inspection rating:"),
                               choices = unique(levels(care_homes$rating)), 
                               selected = "Outstanding"
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
                  includeMarkdown("data/targeted/metadata/care_homes.md"),
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
      )
      
    })
    
    
    