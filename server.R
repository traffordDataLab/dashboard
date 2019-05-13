server <- function(input, output, session) {
  
  source("priorities/housing/server_fragment.R", local = TRUE)$value
  source("priorities/health/server_fragment.R", local = TRUE)$value
  
  ## Landing page -------------------
  
  observeEvent(input$housing_tab, {
    updateTabsetPanel(session, "tabs", selected = "Affordable and quality homes")
  })
  
  observeEvent(input$health_tab, {
    updateTabsetPanel(session, "tabs", selected = "Health and wellbeing")
  })
  
  observeEvent(input$places_tab, {
    updateTabsetPanel(session, "tabs", selected = "Successful and thriving places")
  })
  
  observeEvent(input$children_tab, {
    updateTabsetPanel(session, "tabs", selected = "Children and young people")
  })
  
  observeEvent(input$pride_tab, {
    updateTabsetPanel(session, "tabs", selected = "Pride in our area")
  })
  
  observeEvent(input$green_tab, {
    updateTabsetPanel(session, "tabs", selected = "Green and connected")
  })
  
  observeEvent(input$targeted_tab, {
    updateTabsetPanel(session, "tabs", selected = "Targeted support")
  })
  
}