# Targeted support #

tabPanel("Targeted support", width = 11, style="margin-left:4%; margin-right:4%",
         HTML(paste(h2("Targeted support"))),
         includeHTML("help.html"),
         fluidPage(
           br(),
           fluidRow(
             uiOutput("rough_sleeping_box"),
             uiOutput("dtoc_box"),
             uiOutput("residential_home_admissions_box"),
             div(class="clearfix visible-lg"),
             uiOutput("home_after_discharge_box"),
             uiOutput("care_homes_box")
           ),
           br(),br()
         )
)