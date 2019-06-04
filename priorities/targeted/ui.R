# Targeted support #

tabPanel("Targeted support", width = 11, style="margin-left:4%; margin-right:4%",
         HTML(paste(h2("Targeted support"))),
         includeHTML("help.html"),
         fluidPage(
           br(),
           fluidRow(
             uiOutput("rough_sleeping_box")
           ),
           br(),br()
         )
)