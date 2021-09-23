
ui.import <- function() {
  tabItem(
    tabName = "import",
    fluidRow(
      HTML("<html lang='en'>"), #Always have this as your first line
      
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
      )),
      
      tags$p(glue("REQUIRED: A prior inputs file containing economic multipliers and other required data.
                  Contact ST5 to obtain the correct file if you do not have it already.")),
      
      # File input for the priors file
      fileInput("all_priors", "Prior Inputs",
                accept = c(".RData",
                           ".rda")),
      
      
      # Numeric input for the catch year
      numericInput("year", "Catch Year", 2018),
      
      # Numeric input for the gdp deflator
      numericInput("deflator", "Deflator to Multiplier Year (default=2008)", 0.8548349),
      
      # Do we want to call the itis API for new categories?
      checkboxInput("recall", "Pull New Catch Categories?", FALSE),
      
      # Checkbox for whether we're doing imports or not
      checkboxInput("import_true", "Imports", TRUE),
  
      actionButton("edit_catch", "Read/Edit Catch"),
      actionButton("clear_uploads", "Clear")
    ))
}