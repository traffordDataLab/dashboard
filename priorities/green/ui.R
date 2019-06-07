# Green and connected #

tabPanel("Green and connected", width = 11, style="margin-left:4%; margin-right:4%",
         HTML(paste(h2("Green and connected"))),
         includeHTML("help.html"),
         fluidPage(
           br(),
           fluidRow(
             uiOutput("gmal_box"),
             uiOutput("licensed_vehicles_box"),
             uiOutput("nitrogen_dioxide_box")
           ),
           fluidRow(
             uiOutput("particulate_matter_box"),
             uiOutput("co2_emissions_box")
           ),
           br(),br()
         )
)