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

#<----------------------------------------------for kde---------------------------------------->
mpszB <- readOGR(dsn = "../data", layer="MP14_SUBZONE_WEB_PL")

sp_chas <- as_Spatial(chas_clinics3413)
sp_eldercare <- as_Spatial(eldercare3414)
sp_cc <- as_Spatial(community_club3414)

chas_sp <- as(sp_chas, "SpatialPoints")
eldercare_sp <- as(sp_eldercare, "SpatialPoints")
cc_sp <- as(sp_cc, "SpatialPoints")

ppp_chas <- as(chas_sp, "ppp")
ppp_eldercare <- as(eldercare_sp, "ppp")
ppp_cc <- as(cc_sp, "ppp")

ppp_chas_jit <- rjitter(ppp_chas, retry=TRUE, nsim=1, drop=TRUE)
ppp_eldercare_jit <- rjitter(ppp_eldercare, retry=TRUE, nsim=1, drop=TRUE)
ppp_cc_jit <- rjitter(ppp_cc, retry=TRUE, nsim=1, drop=TRUE)

sp_mpszB <- as(mpszB, "SpatialPolygons")

mpsz_owin <- as(mpszB, "owin")

chas_ppp = ppp_chas_jit[mpsz_owin]
eldercare_ppp = ppp_eldercare_jit[mpsz_owin]
cc_ppp = ppp_cc_jit[mpsz_owin]


#<--------------------------------------------end for kde---------------------------------------->

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
                           mainPanel(width=10,
                             DT::dataTableOutput('table')
                           )),
                  
                  tabPanel("Supply and Demand Analysis", 
                           selectInput("mapset", "Select Data to Analyse:",
                                       choices = c("CHAS Clinics", 
                                                   "Community Clubs",
                                                   "Eldercare Services")),
                           mainPanel(width=10,fluid=TRUE,
                                     fluidRow(
                                       column(6, tmapOutput(
                                           outputId = "supplymap"
                                         )
                                       ),
                                       column(6,tmapOutput(
                                           outputId = "demandmap"
                                         )
                                       )
                                     )
                           )),
                  tabPanel("Kernel Density Estimation", 
                           fluidRow(
                             column(4, 
                                    selectInput("kdefacilityselect", "Select Facility to Analyse:",
                                                choices = c("CHAS Clinics", 
                                                            "Community Clubs",
                                                            "Eldercare Services"))
                             ),
                             column(4,
                                    selectInput("kdezoneselect", "Select Planning Area to Analyse:",
                                                choices = c(unique(mpsz3414_65Above$PLN_AREA_N)))
                                    
                             ),
                             column(4,
                                    selectInput("analysis_method", "Analysis Method:",
                                                  choices = c("G-Function", 
                                                              "F-Function",
                                                              "K-Function",
                                                              "L-Function")),
                             )
                           ),
                           
                           mainPanel(fluid=TRUE,
                                     fluidRow(
                                       column(width=6,tmapOutput(
                                         outputId = "kdemap"
                                       )
                                       ),
                                       column(width=6,plotOutput(
                                         outputId = "secondordermap"
                                       )
                                       )
                                     )
                           )
                           
                  )
                ) # navbarPage
) # fluidPage

# Define server function  
server <- function(input, output) {
#<------------------------View Data--------------------------->
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
  
  
#<-------------------Supply and Demand------------------------->
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
#<-----------------------------first and second order analysis-------------------------------->
  # kdefacilityselect <- reactive({
  #   if (input$kdefacilityselect=="CHAS Clinics") {
  #     chas_ppp
  #   }
  #   else if (input$kdefacilityselect=="Community Clubs") {
  #     cc_ppp
  #   }else {
  #     eldercare_ppp 
  #   }
  # })
  observe({
    label<-input$kdefacilityselect
    facility<-input$kdefacilityselect
    analysis<-input$analysis_method
    zone <- input$kdezoneselect
    
    if(is.null(facility))
      facility<-character(0)
    
    if(facility =="Community Clubs"){
      facility= cc_ppp
    }else if(facility =="CHAS Clinics"){
      facility= chas_ppp
    }else{
      facility= eldercare_ppp
    }
    area = mpszB[mpszB@data$PLN_AREA_N == zone,]
    area_sp = as(area, "SpatialPolygons")
    area_owin = as(area_sp, "owin")
    facility_area_ppp = facility[area_owin]
    
    #<--------------------------------kde---------------------------------->
    facility_area_ppp.km <- rescale(facility_area_ppp, 1000, "km")
    kde_facility_area <- adaptive.density(facility_area_ppp.km, method="kernel")
    gridded_kde_facility_area <- as.SpatialGridDataFrame.im(kde_facility_area)
    kde_facility_area_raster <- raster(gridded_kde_facility_area)
    projection(kde_facility_area_raster) <- CRS("+init=EPSG:3414")
    extent(kde_facility_area_raster) <- extent(c(xmin(kde_facility_area_raster), xmax(kde_facility_area_raster), ymin(kde_facility_area_raster), ymax(kde_facility_area_raster))*1000)
    projection(kde_facility_area_raster) <- gsub("units=km", "units=m", projection(kde_facility_area_raster))
    
    output$kdemap <- renderTmap(
      tm_shape(kde_facility_area_raster)+
        tm_raster(colorNA="lightgrey", showNA=FALSE, alpha=0.5, title= paste0(label," Count in ", str_to_title(zone)))+
        tm_shape(area)+
        tm_borders(lwd = 1)+
        tm_basemap(group = "OpenStreetMap")+
        tm_layout(main.title="KDE of Facility")
    )
    
    #<--------------------------------g,f,k,l function---------------------------------->
    if(is.null(analysis))
      anlaysis<-character(0)
    
    if(analysis == "G-Function"){
      facility_area.csr <- envelope(facility_area_ppp, Gest, correction = "all", nsim = 99)
      output$secondordermap <- renderPlot({
        plot(facility_area.csr)
      })
    }else if(analysis == "F-Function"){
      facility_area.csr <- envelope(facility_area_ppp, Fest, correction = "all", nsim = 99)
      output$secondordermap <- renderPlot({
        plot(facility_area.csr)
      })
    }else if(analysis == "K-Function"){
      facility_area.csr <- envelope(facility_area_ppp, Kest, nsim = 99) 
      output$secondordermap <- renderPlot({
        plot(facility_area.csr, . - r ~ r, 
             xlab="d", ylab="K(d)-r", xlim=c(0,500))
      })
    }else if(analysis == "L-Function"){
      facility_area.csr <- envelope(facility_area_ppp, Lest, nsim = 99)
      output$secondordermap <- renderPlot({
        plot(facility_area.csr, . - r ~ r, 
             xlab="d", ylab="L(d)-r", xlim=c(0,500))
      })
    }
    
    output$secondordermap <- renderPlot({
      plot(facility_area.csr)
    })
    
    
  })
} # server

# Run the app ----
shinyApp(ui = ui, server = server)
