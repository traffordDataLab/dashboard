# Health and wellbeing #

tabPanel("Health and wellbeing", width = 11, style="margin-left:4%; margin-right:4%",
         HTML(paste('<br/>', em("Trafford has improved health and wellbeing, and reduced health inequalities."))),
         fluidPage(
           br(),
           fluidRow(
             uiOutput("healthy_life_expectancy_at_birth_box"),
             uiOutput("slope_index_of_inequality_box"),
             uiOutput("alcohol_related_admissions_box")
           ),
           fluidRow(
             uiOutput("alcohol_related_mortality_box"),
             uiOutput("smoking_adults_box"),
             uiOutput("smoking_adults_manual_box")
           ),
           fluidRow(
             uiOutput("admissions_falls_box"),
             uiOutput("deaths_at_home_box"),
             uiOutput("preventable_mortality_from_cancer_box")
           ),
           fluidRow(
             uiOutput("physically_inactive_box"),
             uiOutput("dementia_diagnosis_box"),
             uiOutput("mortality_serious_mental_illness_box")
           ),
           fluidRow(
             uiOutput("admissions_self_harm_box")
           ),
           br(),br()
         )
)