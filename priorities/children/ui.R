# Children and young people #

tabPanel("Children and young people", width = 11, style="margin-left:4%; margin-right:4%",
         HTML(paste('<br/>', em("All children and young people in Trafford will have a fair start."))),
         fluidPage(
           br(),
           fluidRow(
             uiOutput("childcare_providers_box"),
             uiOutput("primary_schools_box"),
             uiOutput("secondary_schools_box")
           ),
           fluidRow(
             uiOutput("special_schools_box"),
             uiOutput("excess_weight_reception_box"),
             uiOutput("excess_weight_year6_box")
           ),
           fluidRow(
             uiOutput("low_income_families_box"),
             uiOutput("admissions_self_harm_young_people_box"),
             uiOutput("dental_decay_box")
           ),
           fluidRow(
             uiOutput("school_readiness_fsm_box"),
             uiOutput("child_protection_plans_box"),
             uiOutput("neets_box")
           ),
           fluidRow(
             uiOutput("children_in_care_box")
           ),
           br(),br()
         )
)