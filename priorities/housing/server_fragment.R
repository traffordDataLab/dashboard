# Housing #

# Net additional dwellings --------------------------------------------------
net_additional_dwellings <- read_csv("data/housing/net_additional_dwellings.csv") %>%
  mutate(area_name = factor(area_name)) %>%
  filter(period >= "2007-08") %>%
  group_by(area_name) %>%
  mutate(index = round(100 * value / value[1], 0),
    tooltip = paste0(
      "Area: ", area_name, "<br/>",
      "Period: ", period, "<br/>",
      "Index: ", index, "<br/>",
      "Net dwellings: ", comma(value)
    )
  )

output$net_additional_dwellings_plot <- renderggiraph({
  
  gg <- ggplot(
    filter(net_additional_dwellings, area_name %in% input$net_additional_dwellings_area_name),
    aes(x = period, y = index, colour = area_name, fill = area_name, group = area_name)) +
    geom_hline(aes(yintercept = 100), colour = "#212121", linetype = "dashed") +
    geom_line(size = 1) +
    geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, colour = "white") +
    scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(
      title = NULL,
      subtitle = "Housing supply",
      caption = "Source: MHCLG",
      x = "",
      y = "Index of values (Base year = 2007-08)",
      colour = NULL
    ) +
    guides(fill = FALSE) +
    theme_minimal(base_family = "Open Sans") +
    theme(
      panel.grid.major.x = element_blank(),
      axis.title.y = element_text(size = 7, hjust = 1),
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "bottom"
    )
  
  gg <- girafe(ggobj = gg)
  girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
  
})

output$net_additional_dwellings_box <- renderUI({
  box(
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
          inputId = "net_additional_dwellings_area_name",
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
         tooltip = paste0(
           "Area: ", area_name, "<br/>",
           "Period: ", period, "<br/>",
           "Percentage: ", round(value*100, 2)))

output$vacant_properties_plot <- renderggiraph({
  
  gg <-
    ggplot(
      filter(vacant_properties, area_name %in% input$vacant_properties_area_name),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(size = 1) +
    geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, colour = "white") +
    scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_y_continuous(limits = c(0, NA), labels = scales::percent_format(accuracy = 0.1)) +
    labs(
      title = NULL,
      subtitle = "Proportion of all dwellings that have been empty for at least six months",
      caption = "Source: MHCLG",
      x = "",
      y = "Percentage",
      colour = NULL
    ) +
    guides(fill = FALSE) +
    theme_minimal(base_family = "Open Sans") +
    theme(
      panel.grid.major.x = element_blank(),
      axis.title.y = element_text(size = 7, hjust = 1),
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "bottom"
    )
  
  gg <- girafe(ggobj = gg)
  girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
  
})

output$vacant_properties_box <- renderUI({
  
  box(
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
          inputId = "vacant_properties_area_name",
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

# Indicator x --------------------------------------------------
