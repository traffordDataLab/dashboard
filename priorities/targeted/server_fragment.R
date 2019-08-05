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



