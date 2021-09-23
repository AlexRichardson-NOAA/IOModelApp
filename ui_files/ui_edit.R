ui.edit <- function() {
  tabItem(
    tabName = "edit",
    fluidRow(
      HTML("<html lang='en'>"), #Always have this as your first line

      div(id = "loader",
          
          fluidRow(
            shinycssloaders::withSpinner(DT::dataTableOutput("categories"), type = 6)
            )
          
      ),
      
      shinyjs::hidden(div(id = "selectors",
        
                          fluidRow(
                            
                            column(4,
                                   numericInput("shrimp", "Shrimp", 0),
                                   numericInput("crab", "Crab", 0),
                                   numericInput("lobster", "Lobster", 0),
                                   numericInput("east_coast", "East Coast Groundfish", 0),
                                   numericInput("hms", "HMS", 0),
                                   numericInput("reef_fish", "Reef Fish", 0),
                                   selectInput("fips", "State:",
                                               c(
                                                 "United States" = "US",
                                                 "Alabama" = "AL",
                                                 "Florida" = "FL",
                                                 "Louisiana" = "LA",
                                                 "Mississippi" = "MS",
                                                 "Texas" = "TX",
                                                 "Delaware" = "DE",
                                                 "Maryland" = "MD",
                                                 "New Jersey" = "NJ",
                                                 "New York" = "NY",
                                                 "Virginia" = "VA",
                                                 "Connecticut" = "CT",
                                                 "Maine" = "ME",
                                                 "Massachusetts" = "MA",
                                                 "New Hampshire" = "NH",
                                                 "Rhode Island" = "RI",
                                                 "Alaska" = "AK",
                                                 "California" = "CA",
                                                 "Oregon" = "OR",
                                                 "Washington" = "WA",
                                                 "Georgia" = "GA",
                                                 "North Carolina" = "NC",
                                                 "South Carolina" = "SC",
                                                 "Hawai`i" = "HI"
                                               ))),
                            
                            column(4,
                                   numericInput("west_coast", "West Coast Groundfish", 0),
                                   numericInput("halibut", "Halibut", 0),
                                   numericInput("menhaden", "Menhaden/Industrial", 0),
                                   numericInput("salmon", "Salmon", 0),
                                   numericInput("scallop", "Sea Scallop", 0),
                                   numericInput("clam_quahog", "Surf Clam/Ocean Quahog", 0),
                                   numericInput("import_num", "Imports", 0)),
                            
                            column(4,
                                   numericInput("trawl", "Other Trawl", 0),
                                   numericInput("finfish", "All Other Finfish", 0),
                                   numericInput("shellfish", "All Other Shellfish", 0),
                                   numericInput("freshwater", "Freshwater", 0),
                                   numericInput("inshore", "Inshore/Miscellaneous", 0),
                                   numericInput("bait", "Bait", 0),
                                   
                                   # Option list for the output type
                                   selectInput(
                                     "output",
                                     "Output Type:",
                                     c(
                                       "All" = "all",
                                       "National" = "national",
                                       "State Summary" = "state summary",
                                       "Economic Category" = "sector",
                                       "States" = "states",
                                       "Impact Type" = "impact",
                                       "FEUS" = "FEUS"
                                     )
                                   ))
           )
      )),
      
      radioButtons("location","",choices = c('National','Selected State Only')),
      
      actionButton("back_edit", "Back"),
      actionButton("reset_edit", "Reset"),
      actionButton("run_model", "Run Model")
      
    )
  )
}