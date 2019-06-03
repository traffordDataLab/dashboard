# Green and connected #

tabPanel("Green and connected", width = 11, style="margin-left:4%; margin-right:4%",
         HTML(paste('<br/>', em("Trafford will maximise its green spaces, transport and digital connectivity."))),
         fluidPage(
           br(),
           fluidRow(
             uiOutput("nitrogen_dioxide_box")
           ),
           br(),br()
         )
)