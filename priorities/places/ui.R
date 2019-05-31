# Successful and thriving places #

tabPanel("Successful and thriving places", width = 11, style="margin-left:4%; margin-right:4%",
         HTML(paste('<br/>', em("Trafford has successful and thriving town centres and communities."))),
         fluidPage(
           br(),
           fluidRow(
             uiOutput("employment_rate_box"),
             uiOutput("median_resident_earnings_box")
           ),
           br(),br()
         )
)