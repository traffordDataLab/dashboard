# Affordable and quality homes #

tabPanel("Affordable and quality homes", width = 11, style="margin-left:4%; margin-right:4%",
         #HTML(paste("<br/>", em("Trafford has a choice of quality homes that people can afford."))),
         fluidPage(
           br(),
           fluidRow(
             column(6,
                   HTML(paste0("<h5>", "By x, more than x ", "<b>","net additional dwellings","</b>", "  will be built each year.", "</h5>")),
                   style = "background-color: #E7E7E7; border: 1px solid #FFFFFF;"
             ),
             column(6,
                    HTML(paste0("<h5>", "By x, the proportion of ", "<b>","long-term vacant properties","</b>", "  will be below x.", "</h5>")),
                    style = "background-color: #E7E7E7; border: 1px solid #FFFFFF;"
             )
           ),
           fluidRow(
             uiOutput("net_additional_dwellings_box"),
             uiOutput("vacant_properties_box")
           ),
           fluidRow(
             box(title = "Box 3"),
             box(title = "Box 4")
           )
           )
           )