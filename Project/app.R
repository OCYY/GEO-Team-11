# Load R packages
packages = c('shiny', 'shinythemes', 'leaflet', 'DT', 'sp', 'rgeos', 'sf', 'rgdal', 'tidyverse', 'tmap', 'maptools', 'raster','spatstat', 'httr', 'rvest')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

# Import Data
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

mpsz <- st_read(dsn = "../data", layer = "MP14_SUBZONE_WEB_PL")
mpsz3414 <- st_transform(mpsz, 3414)

eldercare <- st_read(dsn = "../data", layer = "ELDERCARE")
eldercare3414 <- st_transform(eldercare, 3414)

community_club <- st_read("../data/community-clubs-kml.kml")
community_club3414 <- st_transform(community_club, 3414)

chas_clinics <- st_read("../data/chas-clinics-kml.kml")
chas_clinics3413 <- st_transform(chas_clinics, 3414)

chas_clinics_attributes <- lapply(X = 1:nrow(chas_clinics), 
                                  FUN = function(x) {
                                    chas_clinics %>% 
                                      slice(x) %>%
                                      pull(Description) %>%
                                      read_html() %>%
                                      html_node("table") %>%
                                      html_table(header = TRUE, trim = TRUE, dec = ".", fill = TRUE) %>%
                                      as_tibble(.name_repair = ~ make.names(c("Attribute", "Value"))) %>% 
                                      pivot_wider(names_from = Attribute, values_from = Value)
                                  })

chas_clinics_attributes <- chas_clinics %>%
  bind_cols(bind_rows(chas_clinics_attributes)) %>%
  dplyr::select(-Description)

community_club_attributes <- lapply(X = 1:nrow(community_club), 
                                    FUN = function(x) {
                                      community_club %>% 
                                        slice(x) %>%
                                        pull(Description) %>%
                                        read_html() %>%
                                        html_node("table") %>%
                                        html_table(header = TRUE, trim = TRUE, dec = ".", fill = TRUE) %>%
                                        as_tibble(.name_repair = ~ make.names(c("Attribute", "Value"))) %>% 
                                        pivot_wider(names_from = Attribute, values_from = Value)
                                    })

community_club_attributes <- community_club %>%
  bind_cols(bind_rows(community_club_attributes)) %>%
  dplyr::select(-Description)

mpsz3414$`COMMUNITY_CLUBS` <- lengths(st_intersects(mpsz3414, community_club3414))
mpsz3414$`ELDERCARE` <- lengths(st_intersects(mpsz3414, eldercare3414))
mpsz3414$`CHAS_CLINIC`<- lengths(st_intersects(mpsz3414, chas_clinics3413))
mpsz3414_65Above <- left_join(mpsz3414, pop_65above, by=c("SUBZONE_N" = "SZ"))

# Define UI
ui <- fluidPage(theme = shinytheme("paper"),
                navbarPage(
                  "Old but Gold",
                  tabPanel("Overview",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("txt1", "Given Name:", ""),
                             textInput("txt2", "Surname:", ""),
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Header 1"),
                             
                             h4("Output 1"),
                             verbatimTextOutput("txtout"),
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("View Data",
                           # Input: Choose dataset ----
                           selectInput("dataset", "Select Data to View:",
                                       choices = c("Population (65 Above)", 
                                                   "CHAS Clinics",
                                                   "Community Clubs",
                                                   "Eldercare Services")),
                           
                           
                           # Main panel for displaying outputs ----
                           mainPanel(
                             DT::dataTableOutput('table')
                           )),
                  
                  tabPanel("Supply and Demand Analysis", 
                           selectInput("mapset", "Select Data to Analyse:",
                                       choices = c("CHAS Clinics", 
                                                   "Community Clubs",
                                                   "Eldercare Services")),
                           mainPanel(fluid=TRUE,
                                     fluidRow(
                                       column(
                                         6, class = "well",
                                         leafletOutput(
                                           outputId = "supplymap"
                                         )
                                       ),
                                       column(
                                         6, class = "well",
                                         leafletOutput(
                                           outputId = "demandmap"
                                         )
                                       )
                                     )
                           ))
                ) # navbarPage
) # fluidPage

# Define server function  
server <- function(input, output) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Population (65 Above)" = pop_65above,
           "CHAS Clinics" = chas_clinics_attributes,
           "Community Clubs" = community_club_attributes,
           "Eldercare Services" = eldercare)
  })
  
  output$table <- DT::renderDataTable(
    DT::datatable(
      datasetInput(), options = list(
        lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')),
        pageLength = 10
      )
    )
  )

  observe({
    
    x<-input$mapset
    
    if(is.null(x))
      x<-character(0)
    
    if(x =="Community Clubs"){
      output$supplymap <- renderTmap(
        tm_shape(mpsz3414_65Above)+
          tm_polygons("COMMUNITY_CLUBS")
      )
    }
    
    else if (x =="CHAS Clinics"){
      output$supplymap <- renderTmap(
        tm_shape(mpsz3414_65Above)+
          tm_polygons("CHAS_CLINIC")
      )
    }
    
    else{
      output$supplymap <- renderTmap(
        tm_shape(mpsz3414_65Above)+
          tm_polygons("ELDERCARE")
      )
    }
  })
  
  output$demandmap <- renderTmap (
    tm_shape(mpsz3414_65Above)+
      tm_polygons("65_Above")
  )
  
} # server

# Run the app ----
shinyApp(ui = ui, server = server)
