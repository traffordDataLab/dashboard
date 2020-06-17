# Green and connected #

tabPanel("Green and connected", width = 11, style="margin-left:4%; margin-right:4%",
         HTML(paste(h2("Green and connected"))),
         includeHTML("help.html"),
         fluidPage(
           br(),
           fluidRow(
             uiOutput("nitrogen_dioxide_box"),
             uiOutput("particulate_matter_box"),
             uiOutput("co2_emissions_box"),
             div(class="clearfix visible-lg"),
             uiOutput("licensed_vehicles_box"),
             uiOutput("car_charging_points_box"),
             uiOutput("green_flags_box")
             #,uiOutput("gmal_box")
           ),
           br(),br()
         )
)