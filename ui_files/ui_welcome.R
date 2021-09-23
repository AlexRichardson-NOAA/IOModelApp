
ui.welcome <- function() {
  
  
  
  
  tabItem(
    tabName = "welcome",
    
    # req(credentials()$user_auth),
    
    fluidRow(
      HTML("<html lang='en'>"), #Always have this as your first line
      
      h1("Welcome"),

      column(
      
        width = 12,
        tags$p(glue("Welcome to the NOAA Marine Fisheries Science Center Input/Output Modell app for estimating the economic impact of state and national fish harvests.
                    To get started, click the button below. You must have the required priors file to use this model. An optional catch dataset may also be applied.")),
        actionButton("get_started", "Get Started")
        )
      )
    )
}