#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load R packages
packages = c('shiny', 'shinythemes', 'leaflet', 'DT', 'sp', 'rgdal', 'rgeos', 'sf', 'tidyverse', 'tmap', 'maptools', 'raster','spatstat')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

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
  mutate(`65_Above` = as.integer(rowSums(.[16:21]))) %>%
  mutate_at(.vars = vars(PA,SZ), .funs = funs(toupper))%>%
  dplyr::select(PA, SZ, `65_Above`) 

mpsz <- st_read(dsn = "../data",
                layer = "MP14_SUBZONE_WEB_PL")
mpsz3414 <- st_transform(mpsz, 3414)

community_club <- st_read("../data/community-clubs-kml.kml")
community_club3414 <- st_transform(community_club, 3414)
mpsz3414$`COMMUNITY_CLUBS` <- lengths(st_intersects(mpsz3414, community_club3414))

community_club_supply <- mpsz3414 %>%
  dplyr::select(SUBZONE_N, PLN_AREA_N, COMMUNITY_CLUBS)

community_club_supply_nogeo <- st_set_geometry(community_club_supply,NULL)

eldercare <- st_read("../data/ELDERCARE.kml")
eldercare3414 <- st_transform(eldercare, 3414)
mpsz3414$`ELDERCARE` <- lengths(st_intersects(mpsz3414, eldercare3414))

eldercare_supply <- mpsz3414 %>%
  dplyr::select(SUBZONE_N, PLN_AREA_N, ELDERCARE)

eldercare_supply_nogeo <- st_set_geometry(eldercare_supply,NULL)

chas_clinics <- st_read("../data/chas-clinics-kml.kml")
chas_clinics3413 <- st_transform(chas_clinics, 3414)

mpsz3414$`CHAS_CLINIC`<- lengths(st_intersects(mpsz3414, chas_clinics3413))

chas_supply <- mpsz3414 %>%
  dplyr::select(SUBZONE_N, PLN_AREA_N,CHAS_CLINIC)

chas_supply_nogeo <- st_set_geometry(chas_supply,NULL)

mpsz3414_65Above <- left_join(mpsz3414,pop_65above,
                              by=c("SUBZONE_N" = "SZ"))


# mpsz3414_65Above_sp <- as_Spatial(mpsz3414_65Above)
# 
# # Converting the spatial point data frame into generic sp format
# mpsz3414_65Above_sp <- as(mpsz3414_65Above_sp, "SpatialPoints")
# 
# #Converting the generic sp format into spatstat's ppp format
# ppp_mpsz3414_65Above <- as(mpsz3414_65Above_sp, "ppp")
# 
# # Create owin to confine analysis 
# mpsz_owin <- as(mpsz3414_65Above, "owin")
# 
# mpsz3414_65Above_ppp <- ppp_mpsz3414_65Above[mpsz_owin]
# 
# kde_mpsz3414_65Above <- density(mpsz3414_65Above_ppp, sigma=bw.diggle, edge=TRUE, kernel="gaussian")
# gridded_kde_mpsz3414_65Above <- as.SpatialGridDataFrame.im(kde_mpsz3414_65Above)
# kde_mpsz3414_65Above_raster <- raster(gridded_kde_mpsz3414_65Above)
# projection(kde_mpsz3414_65Above_raster) <- CRS("+init=EPSG:3414")



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
                             selectInput("mapset", "Choose a dataset:",
                                         choices = c("Community Clubs", "CHAS Clinics", "Eldercare Services")),

                             tags$hr(),
                             
                             # Input: Checkbox if file has header ----
                             # checkboxInput("header", "Header", TRUE),
                             
                             # Input: Select separator ----
                             # radioButtons("sep", "Separator",
                             #              choices = c(Comma = ",",
                             #                          Semicolon = ";",
                             #                          Tab = "\t"),
                             #              selected = ","),
                             
                             # Input: Select quotes ----
                             # radioButtons("quote", "Quote",
                             #              choices = c(None = "",
                             #                          "Double Quote" = '"',
                             #                          "Single Quote" = "'"),
                             #              selected = '"'),
                             
                             # Horizontal line ----
                             # tags$hr(),
                             
                             # Input: Select number of rows to display ----
                             # radioButtons("disp", "Display",
                             #              choices = c(Head = "head",
                             #                          All = "all"),
                             #              selected = "head")
                             
                           ),
                           mainPanel(fluid=TRUE,
                                     fluidRow(
                                       column(6,class="well",
                                              leafletOutput(
                                                outputId = "supplymap"
                                              )
                                       ),
                                       column(6,class="well",
                                              leafletOutput(
                                                outputId = "demandmap"
                                              )
                                       )
                                     )
                                     
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  
                  tabPanel("View Data",
                           
                           sidebarPanel(
                             
                             # Input: Choose dataset ----
                             selectInput("dataset", "Choose a dataset:",
                                         choices = c("Population (65 Above)", 
                                                     "CHAS Clinics", 
                                                     "Eldercare Services",
                                                     "Community Clubs"))
          
                             
                           ),
                           
                           # Main panel for displaying outputs ----
                           mainPanel(
                             
                             DT::dataTableOutput('table')
                             
                           )),
                  tabPanel("Visualization", "This panel is intentionally left blank")
                  
                  
                ) # navbarPage
       
                
) # fluidPage


# Define server logic to read selected file ----
server <- function(input, output) {
  # output$map <- renderLeaflet({
  #   leaflet() %>%
  #     addTiles()%>%
  #     addMarkers(data=community_club$geometry)
  # })
  
  datasetInput <- reactive({
    switch(input$mapset,
           "Population (65 Above)" = pop_65above,
           "CHAS Clinics" = chas_supply_nogeo,
           "Eldercare Services" = eldercare_supply_nogeo,
           "Community Clubs" = community_club_supply_nogeo)
  })
  
  
  output$supplymap <- renderTmap (
    tm_shape(mpsz3414)+
      tm_polygons("ELDERCARE")
  )
  output$demandmap <- renderTmap (
    tm_shape(mpsz3414_65Above)+
      tm_polygons("65_Above")
  )
  
  
  
  
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
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Population (65 Above)" = pop_65above,
           "CHAS Clinics" = chas_supply_nogeo,
           "Eldercare Services" = eldercare_supply_nogeo,
           "Community Clubs" = community_club_supply_nogeo)
  })
  
  output$table <- DT::renderDataTable(
    DT::datatable(
      datasetInput(), options = list(
        lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')),
        pageLength = 10
      )
    )
  )
}

# shinyApp()
shinyApp(ui = ui, server = server)






