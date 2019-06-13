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
    ggplot(employment_rate, aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
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
  
  box(width = 4, 
      hr(style = "border-top: 1px dashed #757575;"),
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
    ggplot(median_resident_earnings, aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
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
  
  box(width = 4, 
      hr(style = "border-top: 1px dashed #757575;"),
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
  
  box(width = 4, 
      hr(style = "border-top: 1px dashed #757575;"),
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

# Apprenticeships  --------------------------------------------------

apprenticeships <- read_csv("data/places/apprenticeships.csv") %>% 
  mutate(area_name = factor(area_name),
         period = factor(period),
         tooltip = 
           paste0("<strong>", comma(value), "</strong><br/>",
                  "<em>", area_name, "</em><br/>",
                  period))

output$apprenticeships_plot <- renderggiraph({
  
  if (input$apprenticeships_selection == "GM boroughs") {
    
    gg <-
      ggplot(
        apprenticeships, aes(x = period, y = value, fill = area_name)) +
      geom_bar_interactive(aes(tooltip = tooltip), stat = "identity") +
      scale_fill_manual(values = c("Bolton" = "#E7B800", "Bury" = "#E7B800", "Manchester" = "#E7B800", 
                                   "Oldham" = "#E7B800", "Rochdale" = "#E7B800", "Salford" = "#E7B800", 
                                   "Tameside" = "#E7B800", "Stockport" = "#E7B800", "Trafford" = "#00AFBB", 
                                   "Wigan" = "#E7B800")) +
      scale_y_continuous(limits = c(0, NA), label = comma) +
      labs(
        title = "Apprenticeship starts",
        subtitle = NULL,
        caption = "Source: Department for Education",
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
        filter(apprenticeships, area_name == "Trafford"),
        aes(x = period, y = value)) +
      geom_bar_interactive(aes(tooltip = tooltip), stat = "identity", fill = "#00AFBB") +
      scale_y_continuous(limits = c(0, NA), label = comma) +
      labs(
        title = "Apprenticeship starts",
        subtitle = NULL,
        caption = "Source: Department for Education",
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

output$apprenticeships_box <- renderUI({
  
  box(width = 4, 
      hr(style = "border-top: 1px dashed #757575;"),
      title = "Apprenticeships",
      withSpinner(
        ggiraphOutput("apprenticeships_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioGroupButtons(
            inputId = "apprenticeships_selection",
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
          includeMarkdown("data/places/metadata/apprenticeships.md"),
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

# Real living wage  --------------------------------------------------

real_living_wage <- read_csv("data/places/real_living_wage.csv") %>% 
  # filter(area_name %in% c("England", "Greater Manchester", "Trafford")) %>% 
  spread(period, value) %>% 
  mutate(diff = round(`2018`-`2017`,1)) %>% 
  select(area_name, `2017`, `2018`, diff)

output$real_living_wage_table <- DT::renderDataTable({
  DT::datatable(real_living_wage, 
                caption = 'Proportion of employee jobs with hourly pay below the living wage',
                extensions = "Scroller", 
                style = "bootstrap", 
                class = "compact", 
                width = "100%", 
                rownames = FALSE,
                options = list(dom = 't',
                               deferRender = TRUE, 
                               scrollX = TRUE, 
                               scrollY = 250, 
                               scroller = TRUE, 
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#F8F8F8', 'color': '#000'});", 
                                 "}")),
                colnames = c(
                  "Area" = "area_name",
                  "2017" = "2017",
                  "2018" = "2018",
                  "Change" = "diff")
  )
})

output$real_living_wage_box <- renderUI({
  
  box(width = 4, 
      hr(style = "border-top: 1px dashed #757575;"),
      title = "Employee jobs below the living wage",
      withSpinner(
        DT::dataTableOutput("real_living_wage_table"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ), 
      br(),br(),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          includeMarkdown("data/places/metadata/real_living_wage.md"),
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

# GVA  --------------------------------------------------

gva <- read_csv("data/places/gva.csv") %>% 
  mutate(group = fct_relevel(group, "All industries"),
         tooltip = 
           paste0("<strong>", "£", comma(value), "m", "</strong>", "<br/>",
                  "<strong><em>", group, "</strong></em><br/>",
                  "<em>", area_name, "</em><br/>",
                  period))


output$gva_plot <- renderggiraph({
  
  gg <- ggplot(filter(gva, group %in% input$gva_selection),
               aes(x = period, y = value, fill = group)) +
    geom_line(size = 1, colour = "#00AFBB") +
    geom_point_interactive(aes(tooltip = tooltip), shape = 21, size = 2.5, colour = "white", fill = "#00AFBB") +
    scale_y_continuous(limits = c(0, NA), labels = comma) +
    labs(title = "Total GVA by industry sector in 2016",
         subtitle = input$gva_selection,
         caption = "Source: Office for National Statistics",
         x = NULL,
         y = "Millions (£)",
         fill = NULL) +
    theme_x() 
  
  gg <- girafe(ggobj = gg)
  girafe_options(gg, opts_tooltip(use_fill = TRUE), opts_toolbar(saveaspng = FALSE))
  
})

output$gva_box <- renderUI({
  
  box(width = 4, 
      hr(style = "border-top: 1px dashed #757575;"),
      title = "Gross Value Added",
      withSpinner(
        ggiraphOutput("gva_plot"),
        type = 4,
        color = "#bdbdbd",
        size = 1
      ),
      div(
        style = "position: absolute; left: 1.5em; bottom: 0.5em;",
        dropdown(
          radioButtons(
            inputId = "gva_selection",
            label = tags$h4("Select industry:"),
            choices = unique(levels(gva$group)),
            selected = "All industries"
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
          includeMarkdown("data/places/metadata/gva.md"),
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
