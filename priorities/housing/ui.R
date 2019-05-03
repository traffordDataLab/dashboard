# Affordable and quality homes #

tabPanel("Affordable and quality homes", width = 11, style="margin-left:4%; margin-right:4%",
         #HTML(paste("<br/>", em("Trafford has a choice of quality homes that people can afford."))),
         fluidPage(
           br(),
           fluidRow(
             uiOutput("net_additional_dwellings_box"),
             uiOutput("vacant_properties_box")
           ),
           fluidRow(
            
           )
           )
           )