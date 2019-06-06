# Successful and thriving places #

tabPanel("Successful and thriving places", width = 11, style="margin-left:4%; margin-right:4%",
         HTML(paste(h2("Successful and thriving places"))),
         includeHTML("help.html"),
         fluidPage(
           br(),
           fluidRow(
             uiOutput("employment_rate_box"),
             uiOutput("claimant_count_box"),
             uiOutput("median_resident_earnings_box")
           ),
           fluidRow(
             uiOutput("apprenticeships_box"),
             uiOutput("real_living_wage_box")
           ),
           br(),br()
         )
)