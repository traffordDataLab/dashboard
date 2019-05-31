# Targeted support #

tabPanel("Targeted support", width = 11, style="margin-left:4%; margin-right:4%",
         HTML(paste('<br/>', em("People in Trafford will get support when they need it most."))),
         fluidPage(
           br(),
           fluidRow(
             uiOutput("rough_sleeping_box")
           ),
           br(),br()
         )
)