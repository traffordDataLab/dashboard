# Children and young people #

tabPanel("Children and young people", width = 11, style="margin-left:4%; margin-right:4%",
         HTML(paste('<br/>', em("All children and young people in Trafford will have a fair start."))),
         fluidPage(
           br(),
           fluidRow(
             uiOutput("excess_weight_reception_box"),
             uiOutput("excess_weight_year6_box"),
             uiOutput("low_income_families_box")
           ),
           fluidRow(
             uiOutput("admissions_self_harm_young_people_box")
           ),
           br(),br()
         )
)