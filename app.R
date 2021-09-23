

rm(list = ls())

##########SOURCE DATA####################
source("Reference.R") # Universal Documents
source("Functions.R") # App-specific files
### ui code (lists, instructions, etc) used in multiple tabs
# source(file.path("ui_files", "ui_common.R"), local = TRUE, echo = FALSE, chdir = TRUE)
# source(file.path("ui_files", "ui_functions.R"), local = TRUE, echo = FALSE, chdir = TRUE)


### ui code parsed by tabName
source(file.path("ui_files", "ui_edit.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_import.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_tableoutput.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_plots.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_licencing.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_manual.R"), local = TRUE, echo = FALSE, chdir = TRUE)
# source(file.path("ui_files", "ui_login.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_welcome.R"), local = TRUE, echo = FALSE, chdir = TRUE)


##########DEFINE####################
title0<-" | I/O Model "
require.login<-F 

user_base <- tibble(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"), 
  password_hash = sapply(c("pass1", "pass2"), sodium::password_store), 
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

##########ui - USER INTERFACE###########
# Define UI for application that draws a histogram
ui <- tagList(
  dashboardPage(skin = "black",

  #####* Title######
  title = tags$head(tags$title(paste0("NOAA Fisheries ",title0," | NOAA Fisheries")), 
                    tags$link(rel="shortcut icon", 
                              href="https://www.noaa.gov/sites/all/themes/custom/noaa/favicon.ico", 
                              type="image/vnd.microsoft.icon")), 
  
  #####* Header######
  # toolong<-function(text, MinLineLength, break0){
  #   if (nchar(text)>MinLineLength) {
  #     text0<-c()
  #     text00<-strsplit(x = text, split = " ")[[1]]
  #     
  #     # for (i in 1:ceiling(x = nchar(text)/MinLineLength)) {
  #     #   text0<-c(text0, 
  #     #            [grep(pattern = " ",  
  #     #                 x = substr(x = text, start = i, stop = i+MinLineLength))])
  #     # } 
  #   } else {
  #     text0<-text
  #   }
  #   return(text)
  # }
  
  header = dashboardHeader(title = 
                             tags$a(href = 'https://www.fisheries.noaa.gov/',
                                    tags$img(src="FISHERIES-Logo WEB ONLY.png", width = '130'), 
                                    HTML(title0), 
                                    style = paste0("text-align: right; 
                                    color: #10497e; 
                                    font-weight: bold; 
                                    font-size: 20px;
                                    font-family:'Arial Narrow';")
                                    ), 
                           titleWidth = nchar(title0)*17,


                           
                           
                           #For login
                           tags$li(class = "dropdown", 
                                   style = paste0("padding: 8px;"),
                                   shinyauthr::logoutUI("logout", class = "btn-primary", 
                                                        style = "background-color: #1f93d0; border: none; color: #ffffff")),
                           
                           
                           #Other Icons

                           
                            dropdownMenu(
                             tags$li(tags$style(HTML('color: #10497e;}'))),                          
                             type = "notifications",
                             icon = icon("question-circle"),
                             # icon = tags$a(icon("question-circle")#,
                             #               # href = "https://github.com/AlexRichardson-NOAA/IOModelApp",
                             #               # title = "Also see:",
                             #               # style = "color: #1f93d0;"
                             #               ),
                             badgeStatus = NULL,
                             headerText = "See also:",
                             # style = "color: #1f93d0;")
                             notificationItem("shiny", icon = icon("file"), status = "info",
                                              href = "http://shiny.rstudio.com/"),
                             notificationItem("shinydashboard", icon = icon("file"), status = "info",
                                              href = "https://rstudio.github.io/shinydashboard/")
                           ),
                           # tags$li(class = "dropdown",
                           #         tags$a(icon("question-circle"),
                           #                href = "https://rstudio.github.io/shinydashboard/",
                           #                title = "Also see:",
                           #                style = "color: #1f93d0;")),
                           
                           
                           
                           tags$li(class = "dropdown", 
                                   tags$a(icon("github"), 
                                          href = "https://github.com/AlexRichardson-NOAA/IOModelApp",
                                          title = "See the code on github", 
                                          style = "color: #1f93d0;"))
                           ), 
  
  #####* Sidebar######
  sidebar = dashboardSidebar(
    collapsed = FALSE, 
    width = nchar(title0)*17, 
    
    #Login
    div(textOutput("welcome"), 
        style = 
        "padding-top: 40px; 
         padding-bottom: 20px; 
          text-align:center; 
        color: #10497e; 
        font-weight: bold; 
        font-size: 20px;
        font-family:'Arial Narrow';"), # for Login
    
    sidebarMenu(
      id = "tabs",
      menuItem(HTML(paste0("Welcome")),
               tabName = "welcome", icon = icon("address-card")), #icon("sitemap")
      menuItem("Import Data", 
               tabName = "import", icon = icon("cloud-upload")),
      menuItem(HTML(paste0("Edit Data")),
               tabName = "edit", icon = icon("cogs")), #icon("sitemap")
      menuItem("Table Output", 
               tabName = "tableoutput", icon = icon("book")),
      menuItem("Plots", 
               tabName = "plots", icon = icon("file-image-o")),
      menuItem("Licencing", 
               tabName = "licencing", icon = icon("list-alt")),
      menuItem("Manual", 
               tabName = "manual", icon = icon("book"),
               menuSubItem("Sub Menu Item 1", tabName = "sub_1"), 
               menuSubItem("Sub Menu Item 2", tabName = "sub_2")
      )
    )
  ),
  
  #####* Body######
  body = dashboardBody(
    
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML('
      .main-header .sidebar-toggle:before {
                              color: #10497e;}'))),
    tags$head(tags$style(".table{margin: 0 auto;}"),
              tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                          type="text/javascript"),
              includeScript("www/returnClick.js"), 
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "./www/custom.js"),
    # ),
    
    
    # tags$head(
     tags$style(HTML('
    /* logo */
      .skin-black .main-header .logo {
        background-color: #ffffff;
        height: 65px;
      }

      /* logo when hovered */
      .skin-black .main-header .logo:hover {
        background-color: #ffffff;
          color: #000000;
      }

      /* navbar (rest of the header) */
      .skin-black .main-header .navbar {
      background-image: linear-gradient(to right, #ffffff , #d9effa);
          color: #000000;
      }

      /* main sidebar */
      .skin-black .main-sidebar {
        background-color: #d9effa;
      }

      /* active selected tab in the sidebarmenu */
      .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
        background-color: #1f93d0;
          color: #ffffff ;
      }

      /* other links in the sidebarmenu */
      .skin-black .main-sidebar .sidebar .sidebar-menu a{
        background-color: #d9effa;
          color: #10497e;
      }

      /* other links in the sidebarmenu when hovered */
      .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
        background-color: #1f93d0;
          color: #ffffff;
      }

      /* toggle button when hovered  */
      .skin-black .main-header .navbar .sidebar-toggle:hover{
        background-color: #1f93d0;
          color: #10497e;
      }

      /* body */
      .content-wrapper, .right-side {
        background-color: #ffffff;
          color: #000000;

      }

      .content-wrapper,
      .right-side {
      background-color: #ffffff;
          color: #000000;
      padding: 30px;
      }

     .content-wrapper {
     background-color: #ffffff !important;
          color: #000000;
     }



                                    '))),
    # div {
    #   padding-left: 5px;
    #   padding-top: 10px;
    # }
    
    
    #Login
    shinyauthr::loginUI("login"),
    uiOutput("user_table"),
    HTML('<div data-iframe-height></div>'),
    
    tabItems(
      # tabItem(
      #   tabName = "welcome", 
      #   uiOutput("ui.welcome")),
      ui.welcome(),      # Welcome
      ui.import(),   # Import Data
      ui.edit(),
      ui.tableoutput(),  # Evaluation Metrics
      ui.plots(),   # High Quality Maps
      ui.licencing(),       # Export Predictions
      ui.manual()        # Manual
    )
  )), 
  
  #####* Footer######
  
  tags$footer("U.S. Department of Commerce | National Oceanic and Atmospheric Administration | National Marine Fisheries Service", 
              align = "center", 
              style = "
              position:fixed;
              bottom:0;
              width:100%;
              height:25px;   /* Height of the footer */
              color: #10497e; 
              font-size: 15px;
              padding: 5px;
              font-family:'Arial Narrow';
              background-color: #ffffff;
              z-index: 1000;
              ")
  
)

server <- function(input, output, session) {
  
  
  #######* Welcome Server ##########
  
  observeEvent(input$get_started, {
    updateTabsetPanel(session, "tabs", selected = "import")
  })

  #######* Import Server ##########
  
  # Null out Read/Edit Catch button until priors 
  observe({
    if (is.null(input$all_priors)) {
      shinyjs::disable("edit_catch")
    } else {
      shinyjs::enable("edit_catch")
    }
  })
  
  # Null out Clear button until something is in there
  observe({
    if (is.null(input$all_priors) & is.null(input$catch_data)) {
      shinyjs::disable("clear_uploads")
    } else {
      shinyjs::enable("clear_uploads")
    }
  })
  
  # Reset infrastructure
  rv <- reactiveValues(
    catch_data = NULL,
    Comm.Catch.Spp.List = NULL,
    fp = NULL,
    imports = NULL,
    imports_states = NULL,
    multipliers = NULL,
    tsn_id = NULL,
    edit_nums = NULL,
    edit_imports = NULL,
    base_nums = NULL,
    base_imports = NULL,
    clear = FALSE
  )
  
  
  # Read in Catch Data
  observe({
    req(input$catch_data)
    req(!rv$clear)
    
    ext <- tools::file_ext(input$catch_data$datapath)
    
    if (ext == "csv") {
      rv$catch_data = readr::read_csv(input$catch_data$datapath)
    }
    
    if (stringr::str_detect(ext, "xls")) {
      rv$catch_data = readxl::read_excel(input$catch_data$datapath)
    }
    
    if (ext == "RData" | ext == "rda") {
      e = new.env()
      name <- load(input$catch_data$datapath, envir = e)
      assign(rv$catch_data, e[[name]])
    }
    
  })
  
  
  # Read in priors data
  observe({
    req(input$all_priors)
    req(!rv$clear)
    
    e = new.env()
    name <- load(input$all_priors$datapath, envir = e)
    
    rv$Comm.Catch.Spp.List <- e$Comm.Catch.Spp.List
    rv$fp <- e$fp
    rv$imports <- e$imports
    rv$imports_states <- e$imports_states
    rv$multipliers <- e$multipliers
    rv$tsn_id <- e$tsn_id

  })
  
  # More reset infrastructure
  observeEvent(input$catch_data, {
    rv$clear <- FALSE
  }, priority = 1000)
  
  observeEvent(input$all_priors, {
    rv$clear <- FALSE
  }, priority = 1000)
  
  observeEvent(input$clear_uploads, {
    rv$catch_data <- NULL
    rv$Comm.Catch.Spp.List = NULL
    rv$fp = NULL
    rv$imports = NULL
    rv$imports_states = NULL
    rv$multipliers = NULL
    rv$tsn_id = NULL
    rv$impacts = NULL
    rv$clear <- TRUE
    reset('catch_data')
    reset('all_priors')
  }, priority = 1000)
  
  # Calling the FRED API to get deflators - I may want to change this.
  api.key = "71604655b73da6ec59cfa3593d99d0cf"
  fredr_set_key(api.key)
  gdp <- fredr(series_id = 'USAGDPDEFAISMEI')
  gdp$YEAR<-as.numeric(substr(x = gdp$date, start = 1, stop = 4))
  gdp = as.data.frame(gdp) %>%
    filter(!is.na(value)) %>%
    select(YEAR, value) %>%
    mutate(inflator = value/gdp$value[gdp$YEAR==2008]) %>%
    select(-value) %>%
    mutate(deflator = 1/inflator)
  
  
  # Suggest a deflator
  observeEvent(input$year, {
    
    current_year <- input$year
    
    if (current_year %in% gdp$YEAR) {
      deflat <- gdp$deflator[gdp$YEAR == current_year]
    } else {
      deflat <- gdp$deflator[gdp$YEAR == max(gdp$YEAR)]
    }
    
    updateNumericInput(inputId = "deflator", value = deflat)
    
  })
  
  
  # When the button is pressed
  observeEvent(input$edit_catch, {

    # Grab the Year
    current_year = input$year

    # Grab the deflator
    defl = input$deflator

    # Deal with imports
    
    if(input$import_true == F){
      imports = F
    } else {imports = rv$imports}

    # Move to next tab
    updateTabsetPanel(session, "tabs", selected = "edit")

    # Classify Species using itis_reclassify
    base_cat = io_classifier(data = rv$catch_data,  
                             species = rv$Comm.Catch.Spp.List, 
                             year = input$year, 
                             recall = input$recall, 
                             tsn = rv$tsn_id)
    
    shinyjs::hide(id = "loader", anim = T)
    shinyjs::show(id = "selectors", anim = T)
    
    # Limiting the presented data to the FIPS chosen
    if(input$fips == "US"){
      base_nums = base_cat %>%
        group_by(`Species Category`) %>%
        summarize(catch = sum(base_catch))
    } else {
      base_nums = base_cat %>%
        group_by(`Species Category`, fips) %>%
        summarize(catch = sum(base_catch)) %>%
        left_join(rv$fp) %>%
        filter(state_abbr == input$fips) %>%
        select(-state_abbr) %>%
        mutate(catch = case_when(
          (`Species Category` == "East Coast Groundfish" & coast == 2) ~ 0,
          (`Species Category` == "West Coast Groundfish " & coast == 1) ~ 0,
          TRUE ~ catch
        ))
    }
    
    
      
      updateNumericInput(inputId = "shrimp", value = base_nums$catch[base_nums$`Species Category`=="Shrimp"])
      updateNumericInput(inputId = "crab", value = base_nums$catch[base_nums$`Species Category`=="Crab"])
      updateNumericInput(inputId = "lobster", value = base_nums$catch[base_nums$`Species Category`=="Lobster"])
      updateNumericInput(inputId = "east_coast", value = base_nums$catch[base_nums$`Species Category`=="East Coast Groundfish"])
      updateNumericInput(inputId = "hms", value = base_nums$catch[base_nums$`Species Category`=="HMS"])
      updateNumericInput(inputId = "reef_fish", value = base_nums$catch[base_nums$`Species Category`=="Reef Fish"])
      updateNumericInput(inputId = "west_coast", value = base_nums$catch[base_nums$`Species Category`=="West Coast Groundfish "])
      updateNumericInput(inputId = "halibut", value = base_nums$catch[base_nums$`Species Category`=="Halibut"])
      updateNumericInput(inputId = "menhaden", value = base_nums$catch[base_nums$`Species Category`=="Menhaden and Industrial"])
      updateNumericInput(inputId = "salmon", value = base_nums$catch[base_nums$`Species Category`=="Salmon"])
      updateNumericInput(inputId = "scallop", value = base_nums$catch[base_nums$`Species Category`=="Sea Scallop"])
      updateNumericInput(inputId = "clam_quahog", value = base_nums$catch[base_nums$`Species Category`=="Surf Clam and Ocean Quahog "])
      updateNumericInput(inputId = "trawl", value = base_nums$catch[base_nums$`Species Category`=="Other Trawl"])
      updateNumericInput(inputId = "finfish", value = base_nums$catch[base_nums$`Species Category`=="All Other Finfish"])
      updateNumericInput(inputId = "shellfish", value = base_nums$catch[base_nums$`Species Category`=="All Other Shellfish  "])
      updateNumericInput(inputId = "freshwater", value = base_nums$catch[base_nums$`Species Category`=="Freshwater "])
      updateNumericInput(inputId = "inshore", value = base_nums$catch[base_nums$`Species Category`=="Inshore and Miscellaneous"])
      updateNumericInput(inputId = "bait", value = base_nums$catch[base_nums$`Species Category`=="Bait"])
      
      filtered_import = imports[imports$state_abbr == input$fips,]
      basic_import = filtered_import$imports[1]
      
      updateNumericInput(inputId = "import_num", value = basic_import)

      output$categories <- renderDataTable(as.data.frame(base_nums))
      
       edit_nums_states <- base_cat %>%
           left_join(rv$fp) %>% 
           mutate(base_catch = case_when(
             (`Species Category` == "East Coast Groundfish" & coast == 2) ~ 0,
             (`Species Category` == "West Coast Groundfish " & coast == 1) ~ 0,
             TRUE ~ base_catch
           )) %>%
           group_by(`Species Category`, state_abbr) %>%
           summarize(catch = sum(base_catch)) %>%
           mutate(category = `Species Category`)
       
       edit_nums_us <- base_cat %>%
         left_join(rv$fp) %>%
         mutate(base_catch = case_when(
           (`Species Category` == "East Coast Groundfish" & coast == 2) ~ 0,
           (`Species Category` == "West Coast Groundfish " & coast == 1) ~ 0,
           TRUE ~ base_catch
         )) %>%
         group_by(`Species Category`) %>%
         summarize(catch = sum(base_catch)) %>%
         mutate(category = `Species Category`) %>%
         mutate(state_abbr = "US")
       
       
      rv$edit_nums <- edit_nums_us %>% 
        bind_rows(edit_nums_states)
      

      temp = as.data.frame(
        expand.grid(
          state_abbr = c("AK", "AL", "CA", "CT", "DE", "FL", "GA","HI", "LA", "MA", "MD", "ME", "MS", "NC", "NH", "NJ", "NY","OR","RI", "SC", "TX", "VA", "WA", "US"),
          category = c("All Other Finfish", "Shrimp", "Crab", "Lobster", "East Coast Groundfish", "HMS" , "Reef Fish", "West Coast Groundfish ", "West Coast Whiting", "Halibut", "Menhaden and Industrial", "Salmon", "Sea Scallop", "Pelagic Herring and Mackerel", "Surf Clam and Ocean Quahog ", "Other Trawl", "All Other Shellfish  ", "Freshwater ", "Inshore and Miscellaneous", "Bait"),
          catch = 0
        )
      ) %>%
        mutate(state_abbr = as.character(state_abbr), category = as.character(category)) %>%
        anti_join(rv$edit_nums, by = c("state_abbr", "category"))
      
      rv$edit_nums = rv$edit_nums %>%
        bind_rows(temp) %>%
        mutate(`Species Category` = category)
        
       
      rv$base_nums = rv$edit_nums
      

      rv$edit_imports = imports
      rv$base_imports = rv$edit_imports
  })
  
  #######* Edit Server ##########

  observeEvent(input$fips, {

    if (!is.null(rv$edit_nums) & !is.null(rv$edit_imports)) {
      updateNumericInput(inputId = "shrimp", value = rv$edit_nums$catch[rv$edit_nums$`Species Category` ==
                                                                       "Shrimp" & rv$edit_nums$state_abbr == input$fips])
      updateNumericInput(inputId = "crab", value = rv$edit_nums$catch[rv$edit_nums$`Species Category` ==
                                                                     "Crab" & rv$edit_nums$state_abbr == input$fips])
      updateNumericInput(inputId = "lobster", value = rv$edit_nums$catch[rv$edit_nums$`Species Category` ==
                                                                        "Lobster" & rv$edit_nums$state_abbr == input$fips])
      updateNumericInput(inputId = "east_coast", value = rv$edit_nums$catch[rv$edit_nums$`Species Category` ==
                                                                           "East Coast Groundfish" & rv$edit_nums$state_abbr == input$fips])
      updateNumericInput(inputId = "hms", value = rv$edit_nums$catch[rv$edit_nums$`Species Category` ==
                                                                    "HMS" & rv$edit_nums$state_abbr == input$fips])
      updateNumericInput(inputId = "reef_fish", value = rv$edit_nums$catch[rv$edit_nums$`Species Category` ==
                                                                          "Reef Fish" & rv$edit_nums$state_abbr == input$fips])
      updateNumericInput(inputId = "west_coast", value = rv$edit_nums$catch[rv$edit_nums$`Species Category` ==
                                                                           "West Coast Groundfish " & rv$edit_nums$state_abbr == input$fips])
      updateNumericInput(inputId = "halibut", value = rv$edit_nums$catch[rv$edit_nums$`Species Category` ==
                                                                        "Halibut" & rv$edit_nums$state_abbr == input$fips])
      updateNumericInput(inputId = "menhaden", value = rv$edit_nums$catch[rv$edit_nums$`Species Category` ==
                                                                         "Menhaden and Industrial" & rv$edit_nums$state_abbr == input$fips])
      updateNumericInput(inputId = "salmon", value = rv$edit_nums$catch[rv$edit_nums$`Species Category` ==
                                                                       "Salmon" & rv$edit_nums$state_abbr == input$fips])
      updateNumericInput(inputId = "scallop", value = rv$edit_nums$catch[rv$edit_nums$`Species Category` ==
                                                                        "Sea Scallop" & rv$edit_nums$state_abbr == input$fips])
      updateNumericInput(inputId = "clam_quahog", value = rv$edit_nums$catch[rv$edit_nums$`Species Category` ==
                                                                            "Surf Clam and Ocean Quahog " & rv$edit_nums$state_abbr == input$fips])
      updateNumericInput(inputId = "trawl", value = rv$edit_nums$catch[rv$edit_nums$`Species Category` ==
                                                                      "Other Trawl" & rv$edit_nums$state_abbr == input$fips])
      updateNumericInput(inputId = "finfish", value = rv$edit_nums$catch[rv$edit_nums$`Species Category` ==
                                                                        "All Other Finfish" & rv$edit_nums$state_abbr == input$fips])
      updateNumericInput(inputId = "shellfish", value = rv$edit_nums$catch[rv$edit_nums$`Species Category` ==
                                                                          "All Other Shellfish  " & rv$edit_nums$state_abbr == input$fips])
      updateNumericInput(inputId = "freshwater", value = rv$edit_nums$catch[rv$edit_nums$`Species Category` ==
                                                                           "Freshwater " & rv$edit_nums$state_abbr == input$fips])
      updateNumericInput(inputId = "inshore", value = rv$edit_nums$catch[rv$edit_nums$`Species Category` ==
                                                                        "Inshore and Miscellaneous" & rv$edit_nums$state_abbr == input$fips])
      updateNumericInput(inputId = "bait", value = rv$edit_nums$catch[rv$edit_nums$`Species Category` ==
                                                                     "Bait" & rv$edit_nums$state_abbr == input$fips])
      updateNumericInput(inputId = "import_nums", value = rv$edit_imports$imports[rv$edit_imports$state_abbr == input$fips])
    }
  })
  
  
  observeEvent(input$shrimp, {
    if (!is.null(rv$edit_nums)) {
      rv$edit_nums$catch[rv$edit_nums$category == "Shrimp" &
                        rv$edit_nums$state_abbr == input$fips] <- input$shrimp
    }
  })
  observeEvent(input$crab, {
    if (!is.null(rv$edit_nums)) {
      rv$edit_nums$catch[rv$edit_nums$category == "Crab" &
                        rv$edit_nums$state_abbr == input$fips] <- input$crab
    }
  })
  observeEvent(input$lobster, {
    if (!is.null(rv$edit_nums)) {
      rv$edit_nums$catch[rv$edit_nums$category == "Lobster" &
                        rv$edit_nums$state_abbr == input$fips] <- input$lobster
    }
  })
  observeEvent(input$east_coast, {
    if (!is.null(rv$edit_nums)) {
      rv$edit_nums$catch[rv$edit_nums$category == "East Coast Groundfish" &
                        rv$edit_nums$state_abbr == input$fips] <- input$east_coast
    }
  })
  observeEvent(input$hms, {
    if (!is.null(rv$edit_nums)) {
      rv$edit_nums$catch[rv$edit_nums$category == "HMS" &
                        rv$edit_nums$state_abbr == input$fips] <- input$hms
    }
  })
  observeEvent(input$reef_fish, {
    if (!is.null(rv$edit_nums)) {
      rv$edit_nums$catch[rv$edit_nums$category == "Reef Fish" &
                        rv$edit_nums$state_abbr == input$fips] <- input$reef_fish
    }
  })
  observeEvent(input$west_coast, {
    if (!is.null(rv$edit_nums)) {
      rv$edit_nums$catch[rv$edit_nums$category == "West Coast Groundfish " &
                        rv$edit_nums$state_abbr == input$fips] <- input$west_coast
    }
  })
  observeEvent(input$halibut, {
    if (!is.null(rv$edit_nums)) {
      rv$edit_nums$catch[rv$edit_nums$category == "Halibut" &
                        rv$edit_nums$state_abbr == input$fips] <- input$halibut
    }
  })
  observeEvent(input$menhaden, {
    if (!is.null(rv$edit_nums)) {
      rv$edit_nums$catch[rv$edit_nums$category == "Menhaden and Industrial" &
                        rv$edit_nums$state_abbr == input$fips] <- input$menhaden
    }
  })
  observeEvent(input$salmon, {
    if (!is.null(rv$edit_nums)) {
      rv$edit_nums$catch[rv$edit_nums$category == "Salmon" &
                        rv$edit_nums$state_abbr == input$fips] <- input$salmon
    }
  })
  observeEvent(input$scallop, {
    if (!is.null(rv$edit_nums)) {
      rv$edit_nums$catch[rv$edit_nums$category == "Sea Scallop" &
                        rv$edit_nums$state_abbr == input$fips] <- input$scallop
    }
  })
  observeEvent(input$clam_quahog, {
    if (!is.null(rv$edit_nums)) {
      rv$edit_nums$catch[rv$edit_nums$category == "Surf Clam and Ocean Quahog " &
                        rv$edit_nums$state_abbr == input$fips] <- input$clam_quahog
    }
  })
  observeEvent(input$trawl, {
    if (!is.null(rv$edit_nums)) {
      rv$edit_nums$catch[rv$edit_nums$category == "Other Trawl" &
                        rv$edit_nums$state_abbr == input$fips] <- input$trawl
    }
  })
  observeEvent(input$finfish, {
    if (!is.null(rv$edit_nums)) {
      rv$edit_nums$catch[rv$edit_nums$category == "All Other Finfish" &
                        rv$edit_nums$state_abbr == input$fips] <- input$finfish
    }
  })
  observeEvent(input$shellfish, {
    if (!is.null(rv$edit_nums)) {
      rv$edit_nums$catch[rv$edit_nums$category == "All Other Shellfish  " &
                        rv$edit_nums$state_abbr == input$fips] <- input$shellfish
    }
  })
  observeEvent(input$freshwater, {
    if (!is.null(rv$edit_nums)) {
      rv$edit_nums$catch[rv$edit_nums$category == "Freshwater " &
                        rv$edit_nums$state_abbr == input$fips] <- input$freshwater
    }
  })
  observeEvent(input$inshore, {
    if (!is.null(rv$edit_nums)) {
      rv$edit_nums$catch[rv$edit_nums$category == "Inshore and Miscellaneous" &
                        rv$edit_nums$state_abbr == input$fips] <- input$inshore
    }
  })
  observeEvent(input$bait, {
    if (!is.null(rv$edit_nums)) {
      rv$edit_nums$catch[rv$edit_nums$category == "Bait" &
                        rv$edit_nums$state_abbr == input$fips] <- input$bait
    }
  })
  observeEvent(input$import_num, {
    if (!is.null(rv$edit_imports)) {
      rv$edit_imports$imports[rv$edit_imports$state_abbr == input$fips] <- input$import_num
    }
  })
  
  
  observeEvent(input$back_edit, {
    updateTabsetPanel(session, "tabs", selected = "import")
  })
  
  observeEvent(input$reset_edit, {
    
    rv$edit_nums = rv$base_nums
    rv$edit_imports = rv$base_imports
    updateSelectInput(inputId = "fips", selected = "US")
    
  })
  
#### * Table Output Server ####
  
  observeEvent(input$run_model, {
    
    updateTabsetPanel(session, "tabs", selected = "tableoutput")
    
    
    # Run the model (FEUS has to be run differently)
    if(input$output == "FEUS"){
      
      edited_nums = rv$edit_nums %>% 
        left_join(rv$fp) %>%
        mutate(Year = input$year) %>%
        rename(State = state_abbr, base_catch = catch)
      
      # Calculate the impacts
      impacts.i <- feusIO::io_calculator(catch = edited_nums, import_numbers = rv$edit_imports, implan_multipliers = rv$multipliers, deflator = input$deflator, import_state_multipliers = rv$imports_states)
      impacts.ni <- feusIO::io_calculator(catch = edited_nums, import_numbers = F, implan_multipliers = rv$multipliers, deflator = input$deflator, import_state_multipliers = rv$imports_states)
      
      # Clean the impacts
      impacts.commercial.i <- io_cleaner(impact = impacts.i, format = "FEUS", xlsx = F, fp = rv$fp, maxyr = input$year)
      impacts.commercial.ni <- io_cleaner(impact = impacts.ni, format = "FEUS", xlsx = F, fp = rv$fp, maxyr = input$year)
      impacts.commercial= bind_rows(impacts.commercial.i, impacts.commercial.ni) %>%
        mutate(Index = row_number())
      
      
      # Render the impacts
      output$impacts <- renderDataTable(as.data.frame(impacts.commercial))
      rv$impacts <- as.data.frame(impacts.commercial)
      
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste0("io_impacts_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          writexl::write_xlsx(impacts.commercial, path = file)
        }
      )
    } else {
      
      edited_nums = rv$edit_nums %>% 
        left_join(rv$fp) %>%
        mutate(Year = input$year) %>%
        rename(State = state_abbr, base_catch = catch)
      
      impacts <- feusIO::io_calculator(catch = edited_nums, import_numbers = rv$edit_imports, implan_multipliers = rv$multipliers, deflator = input$deflator, import_state_multipliers = rv$imports_states)
      
      impacts.commercial <- io_cleaner(impact = impacts, format = input$output, xlsx = F, fp = rv$fp, maxyr = input$year)
      
      names = names(impacts.commercial)
      
      updateSelectInput(session, "table_out", choices = names)
      
      # Render the impacts
      observeEvent(input$table_out,{
        output$impacts <- DT::renderDataTable(DT::datatable({impacts.commercial[input$table_out][[1]]}))
      })
      
      rv$impacts <- impacts.commercial[input$table_out][[1]]
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste0("io_impacts_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          writexl::write_xlsx(impacts.commercial, path = file)
        }
      )
      
    }
    
  })
  
  
  #######* Images##########
  output$ImageFull <- renderImage({
    filename <- normalizePath(file.path("./www/noaa_fisheries_small.png"))
    list(src = filename,
         width = session$clientData$output_ImageFull_width,
         height = session$clientData$output_ImageFull_height
    )
  }, deleteFile = FALSE)

  output$Image <- renderImage({
    filename <- normalizePath(file.path("./www/noaa_logo.gif"))
    list(src = filename,
         width = session$clientData$output_Image_width,
         height = session$clientData$output_Image_height
    )
  }, deleteFile = FALSE)
  
  ########* Plots###########
  
  datasetInput <- reactive({
    switch(input$dataset,
           "faithful" = faithful,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  output$table <- renderDataTable(input$datasetInput)

  # output$table.login <- renderDataTable(DT::renderDT(user_data(), options = list(scrollX = TRUE)))
  
    
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- as.numeric(data.frame(datasetInput())[, 2]) 
    x <- x[!(is.na(x))]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = input$color, border = 'white')
    
  })
  
  ###*CSV Download####
  output$downloadData <- downloadHandler(
    # filename <- paste0("NOAAAcousticThresholds_", Sys.Date(), ".csv"),
    filename = #function() {
      "downloadData.csv",
    # },
    contentType = "text/csv",
    content = function(file) {
      
      filename0<-file#"downloadData.csv"#file.path(getwd(), "downloadData.csv")

      # Threshold Isopleths Results WARNINGS   
      
      write.table(input$dataset, 
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
      
      write.table("Data",
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
      
      write.table(input$datasetInput,
                  file=filename0,
                  sep=",", row.names=TRUE, col.names=FALSE, append=TRUE)
      
      write.table("", #Space
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
      
      # DISCLAIMER  
      write.table("LICENCE",
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
            
      write.table(licence0,
                  file=filename0,
                  sep=",", row.names=TRUE, col.names=FALSE, append=TRUE)
      

    }
  )
  
  ########* R Markdown Report #########
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    contentType = "text/html",
    
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(getwd(), "report4.Rmd")
      file.copy(from = "report4.Rmd", "report2.Rmd", overwrite = TRUE)
      file.copy("report2.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        ProjectName = input$ProjectName, 
        distPlot = input$distPlot,
        table = input$table
      )
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
    
    
  )
}

shinyApp(ui, server)