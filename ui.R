ui <- fluidPage(
  theme = shinytheme("yeti"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  titlePanel(
    title = div(
      tags$a(
        img(src = "https://github.com/traffordDataLab/traffordDataLab.github.io/raw/master/images/trafford_council_logo_black_on_white_100px.png", height = 60),
        href = "https://www.trafford.gov.uk", target = "_blank"
      ),
      style = "position: relative; top: -5px;",
      "Trafford Council's strategic priorities"
    ),
    windowTitle = "Trafford's priorities"
  ),
  tabsetPanel(
    id = "tabs",
    tabPanel(
      title = "Home",
      width = 11,
      style = "margin-left:4%; margin-right:4%",
      br(),
      fluidRow(
        h4(
          "The Council has identified seven strategic priorities that we believe are key to enabling Trafford residents, businesses and staff to thrive. Our priorities set out our aspirations for our people, place and communities, and how they can affect and improve their daily lives."
        )
      ),
      br(),
      fluidRow(column(
        4,
        div(
          div(),
          tags$button(
            id = "housing_tab",
            class = "btn action-button",
            style = "color: #212121; background-color: #fff; border: none;",
            tags$img(src = "icons/housing.png", height = "50px"),
            "Affordable and quality homes"
          )
        ),
        div(
          div(),
          tags$button(
            id = "health_tab",
            class = "btn action-button",
            style = "color: #212121; background-color: #fff; border: none;",
            tags$img(src = "icons/health.png", height = "50px"),
            "Health and wellbeing"
          )
        ),
        div(
          div(),
          tags$button(
            id = "places_tab",
            class = "btn action-button",
            style = "color: #212121; background-color: #fff; border: none;",
            tags$img(src = "icons/places.png", height = "50px"),
            "Successful and thriving places"
          )
        ),
        div(
          div(),
          tags$button(
            id = "children_tab",
            class = "btn action-button",
            style = "color: #212121; background-color: #fff; border: none;",
            tags$img(src = "icons/children.png", height = "50px"),
            "Children and young people"
          )
        ),
        div(
          div(),
          tags$button(
            id = "pride_tab",
            class = "btn action-button",
            style = "color: #212121; background-color: #fff; border: none;",
            tags$img(src = "icons/pride.png", height = "50px"),
            "Pride in our area"
          )
        ),
        div(
          div(),
          tags$button(
            id = "green_tab",
            class = "btn action-button",
            style = "color: #212121; background-color: #fff; border: none;",
            tags$img(src = "icons/green.png", height = "50px"),
            "Green and connected"
          )
        ),
        div(
          div(),
          tags$button(
            id = "targeted_tab",
            class = "btn action-button",
            style = "color: #212121; background-color: #fff; border: none;",
            tags$img(src = "icons/targeted.png", height = "50px"),
            "Targeted support"
          )
        )
        
      )), tags$br(), tags$br()
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
