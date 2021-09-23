
###########PACKAGES###################
list_of_packages = c(# Essential
                     "shiny", 
                     "devtools", 
                     
                     # Design
                     "shinydashboard", 
                     "shinythemes", 
                     # devtools::install_github("paulc91/shinyauthr")
                     "shinyauthr", 
                     "shinycssloaders", 
                     
                     # Shiny v. Javascript
                     "shinyjs",
                     "shinyBS",
                     "V8",
                     
                     # Utilities
                     "magrittr", 
                     "readr", 
                     "readxl", 
                     "dplyr", 
                     "DT", 
                     "writexl", 
                     "here",
                     "tidyr",
                     "glue",
                     "ggplot2",
                     "extrafont",
                     # loadfonts()
                     # extrafont::font_import()
                     #windowsFonts()
                     "knitr",
                     "markdown", #https://stackoverflow.com/questions/33499651/rmarkdown-in-shiny-application
                     
                     #Obtaining Data
                     "fredr",
                     "feusIO"
                     #devtools::install_github("AlexRichardson-NOAA/feusIO)
)

lapply(list_of_packages, function(x) if(!require(x,character.only = TRUE)) install.packages(x))


licence0 <- "Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105). The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States."


########COLORS#########
NOAAFisheries.Colors<-list(
  
  Oceans = list(
  "Process Blue" = "#0093D0", 
  "Reflex Blue" = "#0055A4", #Nav Bar Hover
  "PMS 541" = "#00467F", # Nav Bar
  "White" = "#FFFFFF"
  ), 
  
  Waves = list(
  "PMS 319" = "#1ECAD3", 
  "PMS 321" = "#008998", 
  "PMS 322" = "#00708", 
  "Gray 10%" = "#E8E8E8"
  ),

  Seagrass = list(
  "PMS 375" = "#93D500", 
  "PMS 362" = "#4C9C2E", 
  "PMS 322" = "#007078", 
  "Gray 20%" = "#D0D0D0"
  ), 
  
  Urchin = list(
  "Custom" = "#7F7FFF", 
  "PMS 2725" = "#625BC4", 
  "PMS 7670" = "#575195",
  "Gray 40%" = "#9A9A9A"
  ), 
  
  Crustacean = list(
    "PMS 151" = "#FF8300", 
    "PMS 717" = "#D65F00", 
    "PMS 7670" = "#575195", 
    "Gray 50%" = "#7B7B7B"
  ), 
  
  Coral = list(
    "Warm Red" = "#FF4438", 
    "PMS 711" = "D02C2F", 
    "PMS 1805" = "#B2292E", 
    "Gray 70%" = "#646464"
  ),
  
  "NOAA Colors" = list(
  
  #Primary Colors
  "REFLEX BLUE" = "#0A4595", 
  "PROCESS BLUE" = "#0099D8", 
  "DARK SLATE GREY" = "#333333", 
  "WHITE" = "#FFFFFF", 
  
  #Secondary Colors
  "DARK GREY" = "#575757", 
  "MEDIUM GREY" = "#666666",
  "LIGHT GREY" = "#ACACAC",
  "FADED BLUE" = "#6B84B4",
  "RICH BLUE GREY" = "#28282A"
  )

)

NOAA.Fonts<-"Proxima Nova"

