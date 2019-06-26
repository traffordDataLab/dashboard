ui <- fluidPage(
  HTML('<meta name="viewport" content="width=1024">'),
  theme = shinytheme("yeti"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  titlePanel(
    title = div(class = "headerContainer",
      tags$a(
        tags$img(src = "https://github.com/traffordDataLab/traffordDataLab.github.io/raw/master/images/trafford_council_logo_black_on_white_100px.png", height = 60),
        href = "https://www.trafford.gov.uk", target = "_blank"
      ),
      style = "position: relative; top: -5px;",
      "Trafford Together"
    ),
    windowTitle = "Trafford's priorities"
  ),
  tabsetPanel(
    id = "tabs",
    tabPanel(
      title = icon("home"),
      width = 11,
      style = "margin-left:4%; margin-right:4%",
      br(),
      fluidRow(
        h4(
          "The Council has identified seven strategic priorities that we believe are key to enabling Trafford residents, businesses and staff to thrive. Our priorities set out our aspirations for our people, place and communities, and how they can affect and improve their daily lives."
        )
      ),
      br(),
      fluidRow(
        column(11,
        div(
          tags$button(
            id = "housing_tab",
            class = "btn action-button",
            style = "color: #212121; background-color: #fff; border: none; width: 100%;",
            tags$img(src = "icons/housing.png", height = "50px", style = "float: left; margin-right: 1em;"),
            HTML(paste('<p align="left">', "Affordable and quality homes", '<br />', em("Trafford has a choice of quality homes that people can afford."), '</p>'))
          )
        ),
        div(
          tags$button(
            id = "health_tab",
            class = "btn action-button",
            style = "color: #212121; background-color: #fff; border: none; width: 100%;",
            tags$img(src = "icons/health.png", height = "50px", style = "float: left; margin-right: 1em;"),
            HTML(paste('<p align="left">', "Health and wellbeing", '<br />', em("Trafford has improved health and wellbeing, and reduced health inequalities."), '</p>'))
          )
        ),
        div(
          tags$button(
            id = "places_tab",
            class = "btn action-button",
            style = "color: #212121; background-color: #fff; border: none; width: 100%;",
            tags$img(src = "icons/places.png", height = "50px", style = "float: left; margin-right:1 em;"),
            HTML(paste('<p align="left">', "Successful and thriving places", '<br />', em("Trafford has successful and thriving town centres and communities."), '</p>'))
          )
        ),
        div(
          tags$button(
            id = "children_tab",
            class = "btn action-button",
            style = "color: #212121; background-color: #fff; border: none; width: 100%;",
            tags$img(src = "icons/children.png", height = "50px", style = "float: left; margin-right: 1em;"),
            HTML(paste('<p align="left">', "Children and young people", '<br />', em("All children and young people in Trafford will have a fair start."), '</p>'))
          )
        ),
        div(
          tags$button(
            id = "pride_tab",
            class = "btn action-button",
            style = "color: #212121; background-color: #fff; border: none; width: 100%;",
            tags$img(src = "icons/pride.png", height = "50px", style = "float: left; margin-right: 1em;"),
            HTML(paste('<p align="left">', "Pride in our area", '<br />', em("People in Trafford will take pride in their local area."), '</p>'))
          )
        ),
        div(
          tags$button(
            id = "green_tab",
            class = "btn action-button",
            style = "color: #212121; background-color: #fff; border: none; width: 100%;",
            tags$img(src = "icons/green.png", height = "50px", style = "float: left; margin-right: 1em;"),
            HTML(paste('<p align="left">', "Green and connected", '<br />', em("Trafford will maximise its green spaces, transport and digital connectivity."), '</p>'))
          )
        ),
        div(
          tags$button(
            id = "targeted_tab",
            class = "btn action-button",
            style = "color: #212121; background-color: #fff; border: none; width: 100%;",
            tags$img(src = "icons/targeted.png", height = "50px", style = "float: left; margin-right: 1em;"),
            HTML(paste('<p align="left">', "Targeted support", '<br />', em("People in Trafford will get support when they need it most."), '</p>'))
          )
        )
        
      )), tags$br(), tags$br(),
      HTML(paste("Trafford Council's Corporate Plan (2018-2022) can be found", '<a href="https://www.trafford.gov.uk/about-your-council/strategies-plans-and-policies/docs/Corporate-Plan-2019.pdf" target="_blank">here</a>.')),
      tags$br(), tags$br(), tags$br(), tags$br()
    ),
    source("priorities/housing/ui.R", local = TRUE)$value,
    source("priorities/health/ui.R", local = TRUE)$value,
    source("priorities/places/ui.R", local = TRUE)$value,
    source("priorities/children/ui.R", local = TRUE)$value,
    source("priorities/pride/ui.R", local = TRUE)$value,
    source("priorities/green/ui.R", local = TRUE)$value,
    source("priorities/targeted/ui.R", local = TRUE)$value
  ),
  tags$footer(
    fluidRow(
      "Developed by the ",
      a(href = "https://www.trafforddatalab.io", target = "_blank", "Trafford Data Lab"),
      " under the ",
      a(href = "https://www.trafforddatalab.io/LICENSE.txt", target = "_blank", "MIT"),
      " licence"
    ),
    style = "position:fixed; text-align:center; left: 0; bottom:0; width:100%; z-index:1000; height:30px; color: #212121; padding: 5px 20px; background-color: #E7E7E7"
  )
)
