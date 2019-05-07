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
      title = "Housing supply",
      subtitle = NULL,
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
  box(div(HTML(paste0("<h5>", "By x, more than x ", "<b>","net additional dwellings","</b>", "  will be built each year.", "</h5>")),
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

# Rough sleeping --------------------------------------------------
rough_sleeping <- read_csv("data/housing/rough_sleeping.csv") %>%
  mutate(area_name = factor(area_name),
         tooltip = paste0(
           "Area: ", area_name, "<br/>",
           "Period: ", period, "<br/>",
           "Count: ", round(value, 2)))

output$rough_sleeping_plot <- renderggiraph({
  
  if (input$rough_sleeping_area_name == "GM boroughs") {
    
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
        x = "",
        y = "Count",
        colour = NULL
      ) +
      facet_wrap(~area_name, nrow = 2) +
      theme_minimal(base_family = "Open Sans") +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 7, hjust = 1),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none"
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
        title = NULL,
        subtitle = "Rough sleeping estimates",
        caption = "Source: MHCLG",
        x = "",
        y = "Count",
        colour = NULL
      ) +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 7, hjust = 1),
        axis.text.x = element_text(angle = 90, hjust = 1)
      )
    
    x <- girafe(ggobj = gg)
    x <- girafe_options(x, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
    x
    
  }
  
})
  
  output$rough_sleeping_box <- renderUI({
    
    box(div(HTML(paste0("<h5>", "By x, the number of ", "<b>","rough sleepers","</b>", "  will be zero.", "</h5>")),
            style = "background-color: #E7E7E7; border: 1px solid #FFFFFF; padding-left:1em;"),
        br(),
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
              inputId = "rough_sleeping_area_name",
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

# Long-term vacant properties --------------------------------------------------
vacant_properties <- read_csv("data/housing/vacant_properties.csv") %>%
  mutate(area_name = factor(area_name),
         tooltip = paste0(
           "Area: ", area_name, "<br/>",
           "Period: ", period, "<br/>",
           "Percentage: ", paste(round(value*100, 2)), "%"))

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
      title = "Proportion of long-term vacant dwellings",
      subtitle = NULL,
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
  
  box(div(HTML(paste0("<h5>", "By x, the proportion of ", "<b>","long-term vacant properties","</b>", "  will be below x.", "</h5>")),
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

# New properties by council tax band --------------------------------------------------
council_tax_bands <- read_csv("data/housing/council_tax_bands.csv") %>%
  mutate(area_name = factor(area_name),
         band = fct_rev(factor(band)),
         tooltip = paste0(
           "Area: ", area_name, "<br/>",
           "Band: ", band, "<br/>",
           "Percentage: ", paste0(round(value*100, 1), "%")))

output$council_tax_bands_plot <- renderggiraph({
  
  gg <-
    ggplot(
      filter(council_tax_bands, area_name %in% input$council_tax_bands_area_name),
      aes(x = band, y = value, fill = area_name)) +
    geom_bar_interactive(aes(tooltip = tooltip), stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_y_continuous(limits = c(0, NA), labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "Properties by council tax band",
      subtitle = NULL,
      caption = "Source: Valuation Office Agency ",
      x = "",
      y = "",
      fill = NULL
    ) +
    coord_flip() +
    theme_minimal(base_family = "Open Sans") +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.y = element_text(size = 7, hjust = 1),
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "bottom"
    )
  
  gg <- girafe(ggobj = gg)
  girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
  
})

output$council_tax_bands_box <- renderUI({
  
  box(div(HTML(paste0("<h5>", "By x, the proportion of new builds with ", "<b>", " low council tax bands","</b>", "  will be above x.", "</h5>")),
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
            inputId = "council_tax_bands_area_name",
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
         tooltip = paste0(
           "Area: ", area_name, "<br/>",
           "Period: ", period, "<br/>",
           "Ratio: ", round(value, 2)))

output$affordability_ratio_plot <- renderggiraph({
  
  gg <-
    ggplot(
      filter(affordability_ratio, area_name %in% input$affordability_ratio_area_name),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(size = 1) +
    geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, colour = "white") +
    scale_colour_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_fill_manual(values = c("Trafford" = "#00AFBB", "Greater Manchester" = "#E7B800", "England" = "#757575")) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(
      title = "Ratio of median house prices to median earnings",
      subtitle = NULL,
      caption = "Source: ONS",
      x = "",
      y = "Ratio",
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

output$affordability_ratio_box <- renderUI({
  
  box(div(HTML(paste0("<h5>", "By x, the ratio of ", "<b>","median house prices to median earnings","</b>", "  will be below x.", "</h5>")),
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
            inputId = "affordability_ratio_area_name",
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

# Long-term vacant properties --------------------------------------------------
