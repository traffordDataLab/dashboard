# Housing #

tabPanel("Affordable and quality homes", width = 11, style="margin-left:4%; margin-right:4%",
         HTML(paste("<br/>", em("Trafford has a choice of quality homes that people can afford."))),
         fluidPage(
           br(),
           fluidRow(
             uiOutput("net_additional_dwellings_box"),
             uiOutput("rough_sleeping_box")
           ),
           fluidRow(
             uiOutput("council_tax_bands_box"),
             uiOutput("vacant_properties_box")
           ),
           fluidRow(
             uiOutput("affordability_ratio_box")
           ),
           br(),br()
           )
           )