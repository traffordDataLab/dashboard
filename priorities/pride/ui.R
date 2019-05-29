# Pride in our area #

tabPanel("Pride in our area", width = 11, style="margin-left:4%; margin-right:4%",
         HTML(paste('<br/>', em("People in Trafford will take pride in their local area."))),
         fluidPage(
           br(),
           fluidRow(
             uiOutput("green_flags_box"),
             uiOutput("crime_rate_box"),
             uiOutput("crime_severity_box")
           ),
           fluidRow(
             uiOutput("potholes_box"),
             uiOutput("flytipping_box")
           ),
           br(),br()
         )
)