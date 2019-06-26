# Pride in our area #

tabPanel("Pride in our area", width = 11, style="margin-left:4%; margin-right:4%",
         HTML(paste(h2("Pride in our area"))),
         includeHTML("help.html"),
         fluidPage(
           br(),
           fluidRow(
             uiOutput("green_flags_box"),
             uiOutput("crime_rate_box"),
             uiOutput("crime_severity_box"),
             uiOutput("potholes_box"),
             uiOutput("flytipping_box"),
             uiOutput("recycling_box")
           ),
           br(),br()
         )
)