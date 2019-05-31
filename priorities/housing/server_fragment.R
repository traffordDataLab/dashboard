# Housing #

# Net additional dwellings --------------------------------------------------
net_additional_dwellings <- read_csv("data/housing/net_additional_dwellings.csv") %>%
  mutate(area_name = factor(area_name)) %>%
  filter(period >= "2007-08") %>%
  group_by(area_name) %>%
  mutate(index = round(100 * value / value[1], 0),
    tooltip = 
      paste0("<strong>", comma(value), "</strong>", " dwellings", "<br/>",
             "<em>", area_name, "</em><br/>",
             period))

output$net_additional_dwellings_plot <- renderggiraph({
  
  gg <- ggplot(
    filter(net_additional_dwellings, area_name %in% input$net_additional_dwellings_selection),
    aes(x = period, y = index, colour = area_name, fill = area_name, group = area_name)) +
    geom_hline(aes(yintercept = 100), colour = "#212121", linetype = "dashed") +
    geom_line(size = 1) +
    geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, colour = "white") +
    scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(
      title = "Housing supply",
      subtitle = NULL,
      caption = "Source: MHCLG",
      x = NULL,
      y = "Index of values (Base year = 2007-08)",
      colour = NULL
    ) +
    guides(fill = FALSE) +
    theme_x()
  
  gg <- girafe(ggobj = gg)
  girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
  
})

output$net_additional_dwellings_box <- renderUI({
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","net additional dwellings","</b>", "  not set.", "</h5>")),
          style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
    title = "Net additional dwellings",
    withSpinner(
      ggiraphOutput("net_additional_dwellings_plot"),
      type = 4,
      color = "#bdbdbd",
      size = 1
    ),
    div(
      style = "position: absolute; left: 1.5em; bottom: 0.5em;",
      dropdown(
        checkboxGroupInput(
          inputId = "net_additional_dwellings_selection",
          label = tags$h4("Select area:"),
          choices = unique(levels(net_additional_dwellings$area_name)),
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
        includeMarkdown("data/housing/metadata/net_additional_dwellings.md"),
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

# Long-term vacant properties --------------------------------------------------
vacant_properties <- read_csv("data/housing/vacant_properties.csv") %>%
  mutate(area_name = factor(area_name),
         tooltip = 
           paste0("<strong>", paste(round(value*100, 2)), "</strong>", "%", "<br/>",
                  "<em>", area_name, "</em><br/>",
                  period))

output$vacant_properties_plot <- renderggiraph({
  
  gg <-
    ggplot(
      filter(vacant_properties, area_name %in% input$vacant_properties_selection),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(size = 1) +
    geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, colour = "white") +
    scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_y_continuous(limits = c(0, NA), labels = scales::percent_format(accuracy = 0.1)) +
    labs(
      title = "Proportion of long-term vacant dwellings",
      subtitle = NULL,
      caption = "Source: MHCLG",
      x = NULL,
      y = "Percentage",
      colour = NULL
    ) +
    guides(fill = FALSE) +
    theme_x()
  
  gg <- girafe(ggobj = gg)
  girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
  
})

output$vacant_properties_box <- renderUI({
  
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","long-term vacant properties","</b>", "  not set.", "</h5>")),
          style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
    title = "Long-term vacant properties",
    withSpinner(
      ggiraphOutput("vacant_properties_plot"),
      type = 4,
      color = "#bdbdbd",
      size = 1
    ),
    div(
      style = "position: absolute; left: 1.5em; bottom: 0.5em;",
      dropdown(
        checkboxGroupInput(
          inputId = "vacant_properties_selection",
          label = tags$h4("Select area:"),
          choices = unique(levels(vacant_properties$area_name)),
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
        includeMarkdown("data/housing/metadata/vacant_properties.md"),
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

# New properties by council tax band --------------------------------------------------
council_tax_bands <- read_csv("data/housing/council_tax_bands.csv") %>%
  mutate(area_name = as_factor(area_name),
         band = as_factor(band),
         tooltip = 
           paste0("<strong>", paste(round(value*100, 1)), "</strong>", "%", "<br/>",
                  "<em>", area_name, "</em><br/>",
                  period))

output$council_tax_bands_plot <- renderggiraph({
  
  gg <-
    ggplot(
      filter(council_tax_bands, area_name %in% input$council_tax_bands_selection),
      aes(x = band, y = value, fill = area_name)) +
    geom_bar_interactive(aes(tooltip = tooltip), stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_y_continuous(limits = c(0, NA), labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "Properties by council tax band",
      subtitle = NULL,
      caption = "Source: Valuation Office Agency ",
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    theme_x() +
    theme(axis.text.x = element_text(angle = 0))
  
  gg <- girafe(ggobj = gg)
  girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
  
})

output$council_tax_bands_box <- renderUI({
  
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","the proportion of new builds with low council tax bands","</b>", "  not set.", "</h5>")),
          style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
      title = "New properties by council tax band",
      withSpinner(
        ggiraphOutput("council_tax_bands_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          checkboxGroupInput(
            inputId = "council_tax_bands_selection",
            label = tags$h4("Select area:"),
            choices = unique(levels(council_tax_bands$area_name)),
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
          includeMarkdown("data/housing/metadata/council_tax_bands.md"),
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

# Affordability ratio --------------------------------------------------
affordability_ratio <- read_csv("data/housing/affordability_ratio.csv") %>%
  mutate(area_name = factor(area_name),
         tooltip = 
           paste0("<strong>", round(value, 2), "</strong><br/>",
                  "<em>", area_name, "</em><br/>",
                  period))

output$affordability_ratio_plot <- renderggiraph({
  
  gg <-
    ggplot(
      filter(affordability_ratio, area_name %in% input$affordability_ratio_selection),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(size = 1) +
    geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, colour = "white") +
    scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(
      title = "Ratio of median house prices to median earnings",
      subtitle = NULL,
      caption = "Source: Office for National Statistics",
      x = NULL,
      y = "Ratio",
      colour = NULL
    ) +
    guides(fill = FALSE) +
    theme_x()
  
  gg <- girafe(ggobj = gg)
  girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
  
})

output$affordability_ratio_box <- renderUI({
  
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","the affordability ratio","</b>", "  not set.", "</h5>")),
                     style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
      title = "Affordability ratio",
      withSpinner(
        ggiraphOutput("affordability_ratio_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          checkboxGroupInput(
            inputId = "affordability_ratio_selection",
            label = tags$h4("Select area:"),
            choices = unique(levels(affordability_ratio$area_name)),
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
          includeMarkdown("data/housing/metadata/affordability_ratio.md"),
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

# Licensed Houses of Multiple Occupation --------------------------------------------------
licensed_hmos <- read_csv("data/housing/licensed_hmos.csv") %>%
  mutate(households = as.integer(households))

boundary <- st_read("data/geospatial/trafford_local_authority.geojson")

output$licensed_hmos_map = renderLeaflet({
  
  sf <- filter(licensed_hmos, households >= input$licensed_hmos_selection[1] & 
                 households <= input$licensed_hmos_selection[2])
  
  leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addPolygons(data = boundary,
                fillColor = "#DDDDCC", 
                color = "#DDDDCC", weight = 3) %>% 
    addCircleMarkers(data = sf,
                     lng = ~lon, lat = ~lat,
                     stroke = TRUE, color = "#212121", weight = 2, 
                     fillColor = "#00AFBB", fillOpacity = 0.5, radius = 4,
                     popup = paste("<strong>", sf$address, "</strong><br>",
                                   "<em>","Households:", "</em>", sf$households, "<br>",
                                   "<em>","Expiry date:", "</em>", sf$expiry))
  
})

output$licensed_hmos_box <- renderUI({
  
  box(width = 4, div(HTML(paste0("<h5>", "Target for ", "<b>","licensed HMOs","</b>", "  not set.", "</h5>")),
          style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
      br(),
      title = "Licensed HMOs",
      withSpinner(
        leafletOutput("licensed_hmos_map"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          sliderInput(
            inputId = "licensed_hmos_selection",
            label = tags$h4("Number of households:"),
            min = min(as.numeric(licensed_hmos$households), na.rm = TRUE), 
            max = max(as.numeric(licensed_hmos$households), na.rm = TRUE), 
            value = c(5,10), 
            step = 1, 
            ticks = FALSE
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
          includeMarkdown("data/housing/metadata/licensed_hmos.md"),
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



