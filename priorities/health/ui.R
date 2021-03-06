# Health and wellbeing #

tabPanel("Health and wellbeing", width = 11, style="margin-left:4%; margin-right:4%",
         HTML(paste(h2("Health and wellbeing"))),
         includeHTML("help.html"),
         fluidPage(
           br(),
           fluidRow(
             uiOutput("healthy_life_expectancy_at_birth_box"),
             uiOutput("slope_index_of_inequality_box"),
             uiOutput("alcohol_related_admissions_box"),
             div(class="clearfix visible-lg"),
             uiOutput("alcohol_related_mortality_box"),
             uiOutput("smoking_adults_box"),
             uiOutput("smoking_adults_manual_box"),
             div(class="clearfix visible-lg"),
             uiOutput("admissions_falls_box"),
             uiOutput("deaths_at_home_box"),
             uiOutput("physically_inactive_box"),
             div(class="clearfix visible-lg"),
             uiOutput("preventable_mortality_from_cancer_box"),
             uiOutput("dementia_diagnosis_box"),
             uiOutput("mortality_serious_mental_illness_box"),
             div(class="clearfix visible-lg"),
             uiOutput("admissions_self_harm_box")
           ),
           br(),br()
         )
)