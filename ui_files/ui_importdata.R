
ui.importdata <- function() {
  tabItem(
    tabName = "importdata",
    fluidRow(
      box(  
        tags$p(glue("OPTIONAL: Upload commercial harvest data to work with existing catch numbers.")),
        
        # Catch data input
        fileInput(
          "catch_data",
          "Catch Data",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv",
            ".xls",
            ".xlsx",
            ".RData"
          )
        ),
        
        tags$p(glue("REQUIRED: A prior inputs file containing economic multipliers and other required data.
                    Contact ST5 to obtain the correct file if you do not have it already.")),
        
        
        # File input for the priors file
        fileInput("all_priors", "Prior Inputs",
                  accept = c(".RData",
                             ".rda"))
      ))
  )
}