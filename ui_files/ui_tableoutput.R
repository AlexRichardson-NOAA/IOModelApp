
ui.tableoutput <- function() {
  tabItem(
    tabName = "tableoutput",
    fluidRow(
      HTML("<html lang='en'>"), #Always have this as your first line
      
      selectInput(
        "table_out",
        "Choose Table",
        c("Totals" = "placeholder")
      ),
      
      DT::dataTableOutput("impacts"),
      
      downloadButton("downloadData", "Download Data")
        
      
    ))
}