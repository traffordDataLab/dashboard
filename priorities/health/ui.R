# Health and wellbeing #

tabPanel("Health and wellbeing", width = 11, style="margin-left:4%; margin-right:4%",
         HTML(paste('<br/>', em("Trafford has improved health and wellbeing, and reduced health inequalities."))),
         fluidPage(
           br(),
           fluidRow(
             uiOutput("alcohol_related_admissions_box")
           ),
           fluidRow(
          
           ),
           fluidRow(
           ),
           br(),br()
         )
)