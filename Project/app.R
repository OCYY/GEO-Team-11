# Load R packages
packages = c('shiny', 'shinythemes', 'leaflet', 'DT', 'sp', 'rgeos', 'sf', 'rgdal', 'tidyverse', 'tmap', 'maptools', 'raster','spatstat', 'httr', 'rvest','GWmodel','lctools','RColorBrewer','shinycssloaders')
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
  mutate(`SENIOR_POPULATION` = as.integer(rowSums(.[16:21]))) %>%
  mutate_at(.vars = vars(PA,SZ), .funs = funs(toupper))%>%
  dplyr::select(PA, SZ, `SENIOR_POPULATION`) 

mpsz <- st_read(dsn = "../data", layer = "MP14_SUBZONE_WEB_PL")
mpsz3414 <- st_transform(mpsz, 3414)

eldercare <- st_read(dsn = "../data", layer = "ELDERCARE")
eldercare3414 <- st_transform(eldercare, 3414)

community_club <- st_read("../data/community-clubs-kml.kml")
community_club3414 <- st_transform(community_club, 3414)

chas_clinics <- st_read("../data/chas-clinics-kml.kml")
chas_clinics3414 <- st_transform(chas_clinics, 3414)

rc <- st_read(dsn = "../data", layer = "REsIDENTSCOMMITTEE")
rc3414 <- st_transform(rc, 3414)

gym <- st_read("../data/gyms-sg-kml.kml")
gym3414 <- st_transform(gym, 3414)

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

gym_attributes <- lapply(X = 1:nrow(gym), 
                                    FUN = function(x) {
                                      gym %>% 
                                        slice(x) %>%
                                        pull(Description) %>%
                                        read_html() %>%
                                        html_node("table") %>%
                                        html_table(header = TRUE, trim = TRUE, dec = ".", fill = TRUE) %>%
                                        as_tibble(.name_repair = ~ make.names(c("Attribute", "Value"))) %>% 
                                        pivot_wider(names_from = Attribute, values_from = Value)
                                    })

gym_attributes <- gym %>%
  bind_cols(bind_rows(gym_attributes)) %>%
  dplyr::select(-Description)

mpsz3414$`COMMUNITY_CLUBS` <- lengths(st_intersects(mpsz3414, community_club3414))
mpsz3414$`ELDERCARE` <- lengths(st_intersects(mpsz3414, eldercare3414))
mpsz3414$`CHAS_CLINIC`<- lengths(st_intersects(mpsz3414, chas_clinics3414))
mpsz3414$`RC`<- lengths(st_intersects(mpsz3414, rc3414))
mpsz3414$`GYM`<- lengths(st_intersects(mpsz3414, gym3414))

mpsz3414_65Above <- left_join(mpsz3414, pop_65above, by=c("SUBZONE_N" = "SZ"))
mpsz3414_65Above <- mpsz3414_65Above[!is.na(mpsz3414_65Above$'SENIOR_POPULATION'), ]
#<----------------------------------------------for kde---------------------------------------->
mpszB <- readOGR(dsn = "../data", layer="MP14_SUBZONE_WEB_PL")

sp_chas <- as_Spatial(chas_clinics3414)
sp_eldercare <- as_Spatial(eldercare3414)
sp_cc <- as_Spatial(community_club3414)
sp_rc <- as_Spatial(rc3414)
sp_gym <- as_Spatial(gym3414)


chas_sp <- as(sp_chas, "SpatialPoints")
eldercare_sp <- as(sp_eldercare, "SpatialPoints")
cc_sp <- as(sp_cc, "SpatialPoints")
rc_sp <- as(sp_rc, "SpatialPoints")
gym_sp <- as(sp_gym, "SpatialPoints")

ppp_chas <- as(chas_sp, "ppp")
ppp_eldercare <- as(eldercare_sp, "ppp")
ppp_cc <- as(cc_sp, "ppp")
ppp_rc <- as(rc_sp, "ppp")
ppp_gym <- as(gym_sp, "ppp")

ppp_chas_jit <- rjitter(ppp_chas, retry=TRUE, nsim=1, drop=TRUE)
ppp_eldercare_jit <- rjitter(ppp_eldercare, retry=TRUE, nsim=1, drop=TRUE)
ppp_cc_jit <- rjitter(ppp_cc, retry=TRUE, nsim=1, drop=TRUE)
ppp_rc_jit <- rjitter(ppp_rc, retry=TRUE, nsim=1, drop=TRUE)
ppp_gym_jit <- rjitter(ppp_gym, retry=TRUE, nsim=1, drop=TRUE)

sp_mpszB <- as(mpszB, "SpatialPolygons")

mpsz_owin <- as(mpszB, "owin")

chas_ppp = ppp_chas_jit[mpsz_owin]
eldercare_ppp = ppp_eldercare_jit[mpsz_owin]
cc_ppp = ppp_cc_jit[mpsz_owin]
rc_ppp = ppp_rc_jit[mpsz_owin]
gym_ppp = ppp_gym_jit[mpsz_owin]


#<--------------------------------------------for gwc---------------------------------------->
mpsz3414_65Above$'SENIOR_POPULATION'[is.na(mpsz3414_65Above$'SENIOR_POPULATION')] = 0
sp_mpsz3414_65Above <- st_set_geometry(mpsz3414_65Above, NULL)
sp_mpsz3414_65Above <- SpatialPointsDataFrame(data=sp_mpsz3414_65Above,coords=sp_mpsz3414_65Above[,12:13])
proj4string(sp_mpsz3414_65Above) <- CRS("+init=epsg:3414")

drawmap2 <- function(spdf,var) {
  rescale <- function(x) (x + max(abs(x)))/(2*max(abs(x)))
  colmap <- colorRamp(brewer.pal(9,'RdYlBu'))
  # This part plots the map
  sgmap <- openmap(localstats1[["SDF"]]@coords)
  plot(sgmap)
  plot(spdf,pch='',add=TRUE)
  plot(mpsz,col='lightgrey', add=TRUE)
  plot(spdf,pch=16,cex=0.8,col=rgb(colmap(rescale(var)),max=255),add=TRUE,height=600)
  # This part plots the legend
  p1 <- rep(-10.94,5)
  p2 <- 63.305 -  seq(0,2.4,l=5)
  varvals <- seq(min(var),max(var),l=5)
  varlabs <- sprintf('%5.1f',varvals)
  legend("topleft", legend=c(varlabs),title="Correlation",col=rgb(colmap(rescale(var)),max=255),pch=16,cex=1.2)  
  # rect("topleft",col='lightgrey')
  # varvals <- seq(min(var),max(var),l=5)
  # varlabs <- sprintf('%5.1f',varvals)
  # varcols <- rgb(colmap(rescale(varvals)),max=255)
  # text(0,100,-10,10,cex=0.8)
  # points(p1,p2,pch=16,col=varcols)
  # box(which = "plot",lty = "solid")
}



# Define UI
# title = div(img(src="../public/images/icon.png"),
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage("Old but Gold",windowTitle="Old but Gold",
                           tabPanel("Overview",icon=icon("home"),
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("txt1", "Given Name:", ""),
                             textInput("txt2", "Surname:", ""),
                             
                           ), # sidebarPanel
                           mainPanel(
                             h3("IS415 Geospatial Analytics and Applications"),
                             br(),
                             h3("Old but Gold"),
                             p("Aging population has always been a prominent issue in Singapore. The percentage of 
                                individuals over age 65 between 2010 and 2020 increased from 9% to 15.2%. By 2030, 
                                one in four Singaporeans will be aged 65 and above. While there are many efforts by 
                                the government to solve this problem, it is inevitable that people will age and all 
                                of us will grow old one day."),
                             br(),
                             strong("It is essential that relevant facilities and amenities are strategically built 
                                    and readily available for elderly in this land-scarce Singapore."),
                             br(),
                             br(),
                             p("Through this application, it is intended to help policymakers to determine the availability
                               and adequecy of essential amenities to support and empower the seniors in Singapore."),
                             p("1. Comparison of supply and demand of various facilities and the aging population 
                               (aged 65 and above)."),
                             p("2. Identify areas with high distribution of specific facility in a specific subzone."),
                             p("3. Visualisation of geographically weighted correlation between specific facility across
                               Singapore."),
                             p("4. Facilities included in this application are: CHAS clinics, community clubs, eldercare 
                               services, gyms, as well as residents committee."),
                             verbatimTextOutput("txtout"),
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("View Data",icon=icon("search"),
                           # Input: Choose dataset ----
                           selectInput("dataset", "Select Data to View:",
                                       choices = c("Population (65 Above)", 
                                                   "CHAS Clinics",
                                                   "Community Clubs",
                                                   "Eldercare Services",
                                                   "Gyms",
                                                   "Residents Committee")),
                           
                           
                           # Main panel for displaying outputs ----
                           mainPanel(width=10,
                                     withSpinner(DT::dataTableOutput('table'))
                           )),
                  
                  tabPanel("Supply and Demand Analysis", icon=icon("globe-asia"),
                           selectInput("mapset", "Select Data to Analyse:",
                                       choices = c("CHAS Clinics", 
                                                   "Community Clubs",
                                                   "Eldercare Services",
                                                   "Gyms",
                                                   "Residents Committee")),
                           mainPanel(width=10,fluid=TRUE,
                                     fluidRow(
                                       column(6, withSpinner(tmapOutput(
                                           outputId = "supplymap"
                                         ))
                                       ),
                                       column(6,withSpinner(tmapOutput(
                                           outputId = "demandmap"
                                         ))
                                       )
                                     )
                           )),
                  tabPanel("Kernel Density Estimation", icon=icon("chart-line"),
                           fluidRow(
                             column(4, 
                                    selectInput("kdefacilityselect", "Select Facility to Analyse:",
                                                choices = c("CHAS Clinics", 
                                                            "Community Clubs",
                                                            "Eldercare Services",
                                                            "Gyms",
                                                            "Residents Committee"))
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
                                       column(width=6,withSpinner(tmapOutput(
                                         outputId = "kdemap"
                                       ))
                                       ),
                                       column(width=6,withSpinner(plotOutput(
                                         outputId = "secondordermap"
                                       ))
                                       )
                                     )
                           )
                           
                  ),
                  tabPanel("GWC", icon=icon("laptop-code"),
                           selectInput("gwc_data", "Select Data to Analyse:",
                                       choices = c("CHAS Clinics", 
                                                   "Community Clubs",
                                                   "Eldercare Services",
                                                   "Gyms",
                                                   "Residents Committee")),
                           radioButtons("gwc_type", "GWC type:",
                                        c("Pearson Correlation" = "pearson",
                                          "Spearman Correlation" = "spearman")),
                           sliderInput("bandwidth", "Bandwidth:",
                                       min = 0, max = 50,
                                       value = 5),
                           mainPanel(fluid=TRUE,
                                     fluidRow(
                                       column(12,withSpinner(plotOutput(
                                         outputId = "gwcmap"
                                       )))
                                     )
                           ))
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
           "Eldercare Services" = eldercare,
           "Gyms" = gym,
           "Residents Committee" = rc
           )
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
    else if (x =="Eldercare Services"){
      output$supplymap <- renderTmap(
        tm_shape(mpsz3414_65Above)+
          tm_polygons("ELDERCARE")
      )
    }
    else if (x == "Residents Committee"){
      output$supplymap <- renderTmap(
        tm_shape(mpsz3414_65Above)+
          tm_polygons("RC")
      )
    }
    else {
      output$supplymap <- renderTmap(
        tm_shape(mpsz3414_65Above)+
          tm_polygons("GYM")
      )
    }
  })
  
  output$demandmap <- renderTmap (
    tm_shape(mpsz3414_65Above)+
      tm_polygons("SENIOR_POPULATION")
  )
#<-----------------------------first and second order analysis-------------------------------->
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
    }else if(facility =="Eldercare Services"){
      facility= eldercare_ppp
    }else if(facility =="Residents Committee") {
      facility= rc_ppp
    }else {
      facility= gym_ppp
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
  
  #<-------------------------------------gwc------------------------------------->
  observe({
    gwc_type<-input$gwc_type
    gwc_data<-input$gwc_data
    bandwidth <- input$bandwidth
    localstats1 <- gwss(sp_mpsz3414_65Above,vars=c("SENIOR_POPULATION","CHAS_CLINIC","COMMUNITY_CLUBS","ELDERCARE","RC","GYM"),bw=bandwidth,adaptive = TRUE,quantile = TRUE)
    proj4string(localstats1$SDF) <- CRS("+init=epsg:3414")
    
    if(is.null(gwc_data))
      gwc_data<-character(0)
    if(is.null(gwc_type))
      gwc_type<-character(0)
    if(gwc_data =="Community Clubs"){
      if(gwc_type =="pearson"){
        column_name <- 'Corr_SENIOR_POPULATION.COMMUNITY_CLUBS'
      }else {
        column_name <- 'Spearman_rho_SENIOR_POPULATION.COMMUNITY_CLUBS'
      }
    }else if(gwc_data =="CHAS_Clinics"){
      if(gwc_type =="pearson"){
        column_name <- 'Corr_SENIOR_POPULATION.CHAS_CLINIC'
      }else {
        column_name <- 'Spearman_rho_SENIOR_POPULATION.CHAS_CLINIC'
      }
    }else if(gwc_data =="Eldercare Services"){
      if(gwc_type =="pearson"){
        column_name <- 'Corr_SENIOR_POPULATION.ELDERCARE'
      }else {
        column_name <- 'Spearman_rho_SENIOR_POPULATION.ELDERCARE'
      }
    }else if(gwc_data =="Residents Committee"){
      if(gwc_type =="pearson"){
        column_name <- 'Corr_SENIOR_POPULATION.RC'
      }else {
        column_name <- 'Spearman_rho_SENIOR_POPULATION.RC'
      }
    }else{
      if(gwc_type =="pearson"){
        column_name <- 'Corr_SENIOR_POPULATION.GYM'
      }else {
        column_name <- 'Spearman_rho_SENIOR_POPULATION.GYM'
      }
    }
    dud <- is.nan(localstats1$SDF[[column_name]])
    output$gwcmap <- renderPlot(
      drawmap2(localstats1$SDF[!dud,],localstats1$SDF[[column_name]][!dud])
    )
  })
} # server

# Run the app ----
shinyApp(ui = ui, server = server)
