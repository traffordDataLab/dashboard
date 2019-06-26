# Housing #

tabPanel("Affordable and quality homes", width = 11, style="margin-left:4%; margin-right:4%",
         HTML(paste(h2("Affordable and quality homes"))),
         includeHTML("help.html"),
         fluidPage(
           br(),
           fluidRow(
             uiOutput("net_additional_dwellings_box"),
             uiOutput("council_tax_bands_box"),
             uiOutput("licensed_hmos_box"),
             uiOutput("affordability_ratio_box"),
             uiOutput("vacant_properties_box")
           ),
           br(),br()
           )
           )