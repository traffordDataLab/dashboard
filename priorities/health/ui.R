# Health and wellbeing #

tabPanel("Health and wellbeing", width = 11, style="margin-left:4%; margin-right:4%",
         HTML(paste('<br/>', em("Trafford has improved health and wellbeing, and reduced health inequalities."))),
         fluidPage(
           br(),
           fluidRow(
             uiOutput("alcohol_related_admissions_box"),
             uiOutput("alcohol_related_mortality_box")
           ),
           fluidRow(
             uiOutput("healthy_life_expectancy_at_birth_box")
           ),
           fluidRow(
           ),
           br(),br()
         )
)