#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load R packages
library(shiny)
library(shinythemes)
library(rgdal)

popdata <- read_csv("../data/respopagesextod2011to2020.csv")
population20 <- popdata %>%
  filter(Time == 2020)

population20_tidy <- population20 %>%
  group_by(PA, SZ, AG) %>%
  summarise(`POP` = sum(`Pop`)) %>%
  ungroup() %>%
  pivot_wider(names_from = AG, values_from = POP) %>%
  mutate(TOTAL = rowSums(.[3:21]))

pop_65above <- population20_tidy %>%
  mutate(`65_Above` = rowSums(.[16:21])) %>%
  dplyr::select(PA, SZ, `65_Above`)

mpsz <- st_read(dsn = "../data",
                layer = "MP14_SUBZONE_WEB_PL")
mpsz3414 <- st_transform(mpsz, 3414)
community_club <- st_read("../data/community-clubs-kml.kml")
community_club3414 <- st_transform(community_club, 3414)
mpsz3414$`COMMUNITY_CLUBS` <- lengths(st_intersects(mpsz3414, community_club3414))

community_club_supply <- mpsz3414 %>%
  dplyr::select(SUBZONE_N, PLN_AREA_N, COMMUNITY_CLUBS)

community_club_supply <- st_set_geometry(community_club_supply,NULL)

eldercare <- st_read("../data/ELDERCARE.kml")
eldercare3414 <- st_transform(eldercare, 3414)
mpsz3414$`ELDERCARE` <- lengths(st_intersects(mpsz3414, eldercare3414))

eldercare_supply <- mpsz3414 %>%
  dplyr::select(SUBZONE_N, PLN_AREA_N, ELDERCARE)

eldercare_supply <- st_set_geometry(eldercare_supply,NULL)

chas_clinics <- st_read("../data/chas-clinics-kml.kml")
chas_clinics3413 <- st_transform(chas_clinics, 3414)

mpsz3414$`CHASCLINIC`<- lengths(st_intersects(mpsz3414, chas_clinics3413))

chas_supply <- mpsz3414 %>%
  dplyr::select(SUBZONE_N, PLN_AREA_N,CHASCLINIC)

chas_supply <- st_set_geometry(chas_supply,NULL)



# Define UI
ui <- fluidPage(theme = shinytheme("paper"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Old but Gold",
                  tabPanel("Upload Data",
                           sidebarPanel(
                             
                             # Input: Select a file ----
                             
                             fileInput(inputId = "filemap",
                                       label = "Upload Shapefile Here:",
                                       multiple = TRUE,
                                       accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
                             
                             fileInput("file1", "Upload CSV File:",
                                       multiple = FALSE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")),
                             
                             # Horizontal line ----
                             tags$hr(),
                             
                             # Input: Checkbox if file has header ----
                             checkboxInput("header", "Header", TRUE),
                             
                             # Input: Select separator ----
                             radioButtons("sep", "Separator",
                                          choices = c(Comma = ",",
                                                      Semicolon = ";",
                                                      Tab = "\t"),
                                          selected = ","),
                             
                             # Input: Select quotes ----
                             radioButtons("quote", "Quote",
                                          choices = c(None = "",
                                                      "Double Quote" = '"',
                                                      "Single Quote" = "'"),
                                          selected = '"'),
                             
                             # Horizontal line ----
                             tags$hr(),
                             
                             # Input: Select number of rows to display ----
                             radioButtons("disp", "Display",
                                          choices = c(Head = "head",
                                                      All = "all"),
                                          selected = "head")
                             
                           ),
                           mainPanel(
                             tableOutput("contents")
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  
                  tabPanel("Data",
                           
                           sidebarPanel(
                             
                             # Input: Choose dataset ----
                             selectInput("dataset", "Choose a dataset:",
                                         choices = c("Population (65 Above)", "CHAS Clinics", "Eldercare Services","Community Clubs"))
          
                             
                           ),
                           
                           # Main panel for displaying outputs ----
                           mainPanel(
                             
                             tableOutput("table")
                             
                           ))
                  
                  
                ) # navbarPage
) # fluidPage


# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    


  })
  
  output$poptable <- DT::renderDataTable(
    DT::datatable(
      pop_65above, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
      )
    )
  )
  
  output$community_clubs <- DT::renderDataTable(
    DT::datatable(
      community_club_supply, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
      )
    )
  )
  output$eldercare <- DT::renderDataTable(
    DT::datatable(
      eldercare_supply, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
      )
    )
  )
  output$chas <- DT::renderDataTable(
    DT::datatable(
      chas_supply, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
      )
    )
  )
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Population (65 Above)" = pop_65above,
           "CHAS Clinics" = chas_supply,
           "Eldercare Services" = eldercare_supply,
           "Community Clubs" = community_club_supply)
  })
  
  output$table <- renderTable({
    datasetInput()
  })
  

  
  
  
  
}

# shinyApp()
shinyApp(ui = ui, server = server)






