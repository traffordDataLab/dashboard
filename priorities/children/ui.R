# Children and young people #

tabPanel("Children and young people", width = 11, style="margin-left:4%; margin-right:4%",
         HTML(paste(h2("Children and young people"))),
         includeHTML("help.html"),
         fluidPage(
           br(),
           fluidRow(
             uiOutput("excess_weight_reception_box"),
             uiOutput("excess_weight_year6_box"),
             uiOutput("low_income_families_box"),
             uiOutput("admissions_self_harm_young_people_box"),
             uiOutput("dental_decay_box"),
             uiOutput("school_readiness_fsm_box"),
             uiOutput("child_protection_plans_box"),
             uiOutput("neets_box"),
             uiOutput("children_in_care_box"),
             uiOutput("childcare_providers_box"),
             uiOutput("primary_schools_box"),
             uiOutput("secondary_schools_box"),
             uiOutput("special_schools_box")
           ),
           br(),br()
         )
)