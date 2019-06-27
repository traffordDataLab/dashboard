ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  titlePanel(
    div(
      class = "headerContainer",
      a(
        img(
          src = "https://github.com/traffordDataLab/traffordDataLab.github.io/raw/master/images/trafford_council_logo_black_on_white_100px.png",
          style = "position: relative; top: -5px;",
          height = 60
        ),
        href = "https://www.trafford.gov.uk",
        target = "_blank"
      ),
      "Trafford Together"
    ),
    windowTitle = "Trafford Together"
  ),
  tagList(
    tags$head(tags$style(type = 'text/css', '.navbar-brand{display:none;}')),
    navbarPage(
      title = "",
      collapsible = TRUE,
      id = "tabs",
      tabPanel(
        title = icon("home"),
        width = 11,
        style = "margin-left:4%; margin-right:4%",
        br(),
        fluidRow(
          h4(
            "Trafford Council has identified 7 strategic priorities in its", tags$a(href = "https://www.trafford.gov.uk/about-your-council/strategies-plans-and-policies/docs/Corporate-Plan-2019.pdf", target = "_blank", "Corporate Plan"), "that it believes are key to enabling Trafford's residents, businesses and staff to thrive."
          )
        ),
        tags$br(),
        fluidRow(
          column(
            width = 5,
            offset = 1,
            div(
              tags$button(
                id = "housing_tab",
                class = "btn action-button",
                style = "color: #7C7C7C; background-color: #fff; border: none; width: 100%;",
                tags$img(
                  src = "icons/housing.png",
                  height = "50px",
                  style = "float: left; margin-right: 1em;"
                ),
                HTML(
                  paste(
                    '<p class="homeMenuButtonText"><strong>',
                    "Affordable and quality homes",
                    '</strong><br />',
                    "Trafford has a choice of quality homes that people can afford.",
                    '</p>'
                  )
                )
              )
            ),
            div(
              tags$button(
                id = "health_tab",
                class = "btn action-button",
                style = "color: #7C7C7C; background-color: #fff; border: none; width: 100%;",
                tags$img(
                  src = "icons/health.png",
                  height = "50px",
                  style = "float: left; margin-right: 1em;"
                ),
                HTML(
                  paste(
                    '<p class="homeMenuButtonText"><strong>',
                    "Health and wellbeing",
                    '</strong><br />',
                    "Trafford has improved health and wellbeing, and reduced health inequalities.",
                    '</p>'
                  )
                )
              )
            ),
            div(
              tags$button(
                id = "places_tab",
                class = "btn action-button",
                style = "color: #7C7C7C; background-color: #fff; border: none; width: 100%;",
                tags$img(
                  src = "icons/places.png",
                  height = "50px",
                  style = "float: left; margin-right: 1em;"
                ),
                HTML(
                  paste(
                    '<p class="homeMenuButtonText"><strong>',
                    "Successful and thriving places",
                    '</strong><br />',
                    "Trafford has successful and thriving town centres and communities.",
                    '</p>'
                  )
                )
              )
            ),
            div(
              tags$button(
                id = "children_tab",
                class = "btn action-button",
                style = "color: #7C7C7C; background-color: #fff; border: none; width: 100%;",
                tags$img(
                  src = "icons/children.png",
                  height = "50px",
                  style = "float: left; margin-right: 1em;"
                ),
                HTML(
                  paste(
                    '<p class="homeMenuButtonText"><strong>',
                    "Children and young people",
                    '</strong><br />',
                    "All children and young people in Trafford will have a fair start.",
                    '</p>'
                  )
                )
              )
            )
          ),
          column(
            width = 5,
            div(
              tags$button(
                id = "pride_tab",
                class = "btn action-button",
                style = "color: #7C7C7C; background-color: #fff; border: none; width: 100%;",
                tags$img(
                  src = "icons/pride.png",
                  height = "50px",
                  style = "float: left; margin-right: 1em;"
                ),
                HTML(
                  paste(
                    '<p class="homeMenuButtonText"><strong>',
                    "Pride in our area",
                    '</strong><br />',
                    "People in Trafford will take pride in their local area.",
                    '</p>'
                  )
                )
              )
            ),
            div(
              tags$button(
                id = "green_tab",
                class = "btn action-button",
                style = "color: #7C7C7C; background-color: #fff; border: none; width: 100%;",
                tags$img(
                  src = "icons/green.png",
                  height = "50px",
                  style = "float: left; margin-right: 1em;"
                ),
                HTML(
                  paste(
                    '<p class="homeMenuButtonText"><strong>',
                    "Green and connected",
                    '</strong><br />',
                    "Trafford will maximise its green spaces, transport and digital connectivity.",
                    '</p>'
                  )
                )
              )
            ),
            div(
              tags$button(
                id = "targeted_tab",
                class = "btn action-button",
                style = "color: #7C7C7C; background-color: #fff; border: none; width: 100%;",
                tags$img(
                  src = "icons/targeted.png",
                  height = "50px",
                  style = "float: left; margin-right: 1em;"
                ),
                HTML(
                  paste(
                    '<p class="homeMenuButtonText"><strong>',
                    "Targeted support",
                    '</strong><br />',
                    "People in Trafford will get support when they need it most.",
                    '</p>'
                  )
                )
              )
            )
          )
        ),
        tags$br(),
        tags$h4(
          "This dashboard allows you to browse a range of indicators that relate to each of the strategic priorities. All of the indicators are provided at local authority level with many benchmarked against Greater Manchester and England. Some of the visualisations can be filtered by variable, area and chart type. Information is provided about each indicator with a link to the original data source."
        ),
        tags$br(),
        tags$br()
      ),
      source("priorities/housing/ui.R", local = TRUE)$value,
      source("priorities/health/ui.R", local = TRUE)$value,
      source("priorities/places/ui.R", local = TRUE)$value,
      source("priorities/children/ui.R", local = TRUE)$value,
      source("priorities/pride/ui.R", local = TRUE)$value,
      source("priorities/green/ui.R", local = TRUE)$value,
      source("priorities/targeted/ui.R", local = TRUE)$value
    )
  ),
  tags$footer(
    fluidRow(
      "Developed in ",
      a(href = "https://cran.r-project.org/", target = "_blank", "R"),
      " by the ",
      a(href = "https://www.trafforddatalab.io", target = "_blank", "Trafford Data Lab"),
      " under the ",
      a(href = "https://www.trafforddatalab.io/LICENSE.txt", target = "_blank", "MIT"),
      " licence"
    ),
    style = "position:fixed; text-align:center; left: 0; bottom:0; width:100%; z-index:1000; height:30px; color: #7C7C7C; padding: 5px 20px; background-color: #E7E7E7"
  )
)
