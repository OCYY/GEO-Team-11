#<--------------------Load R packages-------------------->
 
library('shiny')
library('shinythemes')
library('DT')
library('sp')
library('rgeos')
library('sf')
library('rgdal')
library('tidyverse')
library('tmap')
library("maptools")
library('raster')
library('spatstat')
library('httr')
library('rvest')
library("GWmodel")
library('lctools')
library('RColorBrewer')
library('shinycssloaders')

#<--------------------Import Data-------------------->

popdata <- read_csv("data/respopagesextod2011to2020.csv")
mpsz <- st_read("data/master-plan-2019-subzone-boundary-no-sea-kml.kml")
eldercare <- st_read(dsn = "data", layer = "ELDERCARE")
community_club <- st_read("data/community-clubs-kml.kml")
chas_clinics <- st_read("data/chas-clinics-kml.kml")
rc <- st_read(dsn = "data", layer = "REsIDENTSCOMMITTEE")
gym <- st_read("data/gyms-sg-kml.kml")

#<--------------------Transforming Projections-------------------->

mpsz_3414 <- st_transform(mpsz, 3414)
eldercare3414 <- st_transform(eldercare, 3414)
community_club3414 <- st_transform(community_club, 3414)
chas_clinics3414 <- st_transform(chas_clinics, 3414)
rc3414 <- st_transform(rc, 3414)
gym3414 <- st_transform(gym, 3414)

#<--------------------Data Wrangling-------------------->

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

mpsz_3414_attributes <- lapply(X = 1:nrow(mpsz_3414), 
                                 FUN = function(x) {
                                   mpsz_3414 %>% 
                                     slice(x) %>%
                                     pull('Description') %>%
                                     read_html() %>%
                                     html_node("table") %>%
                                     html_table(header = TRUE, trim = TRUE, dec = ".", fill = TRUE) %>%
                                     as_tibble(.name_repair = ~ make.names(c("Attribute", "Value"))) %>% 
                                     pivot_wider(names_from = Attribute, values_from = Value)
                                 })

mpsz_3414 <- mpsz_3414 %>%
  bind_cols(bind_rows(mpsz_3414_attributes)) %>%
  dplyr::select(-"Description")

chas_clinics_attributes <- lapply(X = 1:nrow(chas_clinics), 
                                  FUN = function(x) {
                                    chas_clinics %>% 
                                      slice(x) %>%
                                      pull('Description') %>%
                                      read_html() %>%
                                      html_node("table") %>%
                                      html_table(header = TRUE, trim = TRUE, dec = ".", fill = TRUE) %>%
                                      as_tibble(.name_repair = ~ make.names(c("Attribute", "Value"))) %>% 
                                      pivot_wider(names_from = Attribute, values_from = Value)
                                  })

chas_clinics_attributes <- chas_clinics %>%
  bind_cols(bind_rows(chas_clinics_attributes)) %>%
  dplyr::select("HCI_NAME","POSTAL_CD","STREET_NAME","BLK_HSE_NO")
st_geometry(chas_clinics_attributes) <- NULL

community_club_attributes <- lapply(X = 1:nrow(community_club), 
                                    FUN = function(x) {
                                      community_club %>% 
                                        slice(x) %>%
                                        pull('Description') %>%
                                        read_html() %>%
                                        html_node("table") %>%
                                        html_table(header = TRUE, trim = TRUE, dec = ".", fill = TRUE) %>%
                                        as_tibble(.name_repair = ~ make.names(c("Attribute", "Value"))) %>% 
                                        pivot_wider(names_from = Attribute, values_from = Value)
                                    })

community_club_attributes <- community_club %>%
  bind_cols(bind_rows(community_club_attributes)) %>%
  dplyr::select("NAME","ADDRESSPOSTALCODE","ADDRESSSTREETNAME","ADDRESSBLOCKHOUSENUMBER")
st_geometry(community_club_attributes) <- NULL

gym_attributes <- lapply(X = 1:nrow(gym), 
                                    FUN = function(x) {
                                      gym %>% 
                                        slice(x) %>%
                                        pull('Description') %>%
                                        read_html() %>%
                                        html_node("table") %>%
                                        html_table(header = TRUE, trim = TRUE, dec = ".", fill = TRUE) %>%
                                        as_tibble(.name_repair = ~ make.names(c("Attribute", "Value"))) %>% 
                                        pivot_wider(names_from = Attribute, values_from = Value)
                                    })

gym_attributes <- gym %>%
  bind_cols(bind_rows(gym_attributes)) %>%
  dplyr::select("NAME","ADDRESSPOSTALCODE","ADDRESSSTREETNAME","ADDRESSBLOCKHOUSENUMBER")
st_geometry(gym_attributes) <- NULL

eldercare_attributes <- eldercare %>%
  dplyr::select("NAME","ADDRESSPOS","ADDRESSSTR")
st_geometry(eldercare_attributes) <- NULL

rc_attributes <- rc %>%
  dplyr::select("NAME","ADDRESSPOS","ADDRESSSTR","ADDRESSBLO")
st_geometry(rc_attributes) <- NULL

#<--------------------Joining the Data-------------------->

mpsz_3414$`COMMUNITY_CLUBS` <- lengths(st_intersects(mpsz_3414, community_club3414))
mpsz_3414$`ELDERCARE_SERVICES` <- lengths(st_intersects(mpsz_3414, eldercare3414))
mpsz_3414$`CHAS_CLINICS`<- lengths(st_intersects(mpsz_3414, chas_clinics3414))
mpsz_3414$`RESIDENTS_COMMITTEES`<- lengths(st_intersects(mpsz_3414, rc3414))
mpsz_3414$`GYMS`<- lengths(st_intersects(mpsz_3414, gym3414))

mpsz3414_65Above <- left_join(mpsz_3414, pop_65above, by=c("SUBZONE_N" = "SZ"))
# mpsz3414_65Above <- mpsz3414_65Above[!is.na(mpsz3414_65Above$'SENIOR_POPULATION'), ]

#<--------------------Preprocessing for KDE-------------------->

mpszB <- readOGR("data/master-plan-2019-subzone-boundary-no-sea-kml.kml","URA_MP19_SUBZONE_NO_SEA_PL")
mpszB <- spTransform(mpszB,CRS("+init=epsg:3414"))

mpszB_attributes <- lapply(X = 1:nrow(mpszB@data), 
                               FUN = function(x) {
                                 mpszB@data %>% 
                                   slice(x) %>%
                                   pull(Description) %>%
                                   read_html() %>%
                                   html_node("table") %>%
                                   html_table(header = TRUE, trim = TRUE, dec = ".", fill = TRUE) %>%
                                   as_tibble(.name_repair = ~ make.names(c("Attribute", "Value"))) %>% 
                                   pivot_wider(names_from = Attribute, values_from = Value)
                               })

mpszB@data <- mpszB@data %>%
  bind_cols(bind_rows(mpszB_attributes)) %>%
  dplyr::select(-"Description")

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

mpsz_owin <- as(sp_mpszB, "owin")

chas_ppp = ppp_chas_jit[mpsz_owin]
eldercare_ppp = ppp_eldercare_jit[mpsz_owin]
cc_ppp = ppp_cc_jit[mpsz_owin]
rc_ppp = ppp_rc_jit[mpsz_owin]
gym_ppp = ppp_gym_jit[mpsz_owin]

#<--------------------Preprocessing for GWC-------------------->

mpsz3414_65Above$'SENIOR_POPULATION'[is.na(mpsz3414_65Above$'SENIOR_POPULATION')] = 0
sp_mpsz3414_65Above <- st_set_geometry(mpsz3414_65Above, NULL)
sp_mpsz3414_65Above <- SpatialPointsDataFrame(data=sp_mpsz3414_65Above,coords=coordinates(mpszB))
proj4string(sp_mpsz3414_65Above) <- CRS("+init=epsg:3414")

drawmap2 <- function(spdf,var) {
  rescale <- function(x) (x + max(abs(x)))/(2*max(abs(x)))
  colmap <- colorRamp(brewer.pal(9,'RdYlBu'))
  # This part plots the map
  plot(spdf,pch='')
  plot(mpsz_3414,col='lightgrey', add=TRUE)
  plot(spdf,pch=16,cex=0.8,col=rgb(colmap(rescale(var)),max=255),add=TRUE,height=600)
  # This part plots the legend
  p1 <- rep(-10.94,5)
  p2 <- 63.305 -  seq(0,2.4,l=5)
  varvals <- seq(min(var),max(var),l=5)
  varlabs <- sprintf('%5.1f',varvals)
  legend("topleft", legend=c(varlabs),title="Correlation",col=rgb(colmap(rescale(varvals)),max=255),pch=16,cex=1.4)  
  # rect("topleft",col='lightgrey')
  # varvals <- seq(min(var),max(var),l=5)
  # varlabs <- sprintf('%5.1f',varvals)
  # varcols <- rgb(colmap(rescale(varvals)),max=255)
  # text(0,100,-10,10,cex=0.8)
  # points(p1,p2,pch=16,col=varcols)
  # box(which = "plot",lty = "solid")
}

#<---------------------------------------->

facilities <- c("CHAS Clinics",
                "Community Clubs",
                "Eldercare Services",
                "Gyms",
                "Residents Committee")

#<--------------------UI-------------------->

# title = div(img(src="../public/images/icon.png"),
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage("Old but Gold",windowTitle="Old but Gold",
                           tabPanel("Overview",icon=icon("home"),
                            img(src="Overview.png", width = '100%')
                           
                  ),
                  
                  tabPanel("View Data", icon = icon("search"),
                           column(
                             3,
                             wellPanel(
                               selectInput(
                                 "dataset", "Select Data to View:",
                                 choices = c("Population (65 Above)", 
                                             "CHAS Clinics",
                                             "Community Clubs",
                                             "Eldercare Services",
                                             "Gyms",
                                             "Residents Committee")
                               )
                             )
                           ),
                           mainPanel(
                             fluidRow(
                               withSpinner(
                                 DT::dataTableOutput('table')
                               )
                             )
                           )
                  ), 
                  
                  tabPanel("Supply and Demand Analysis", icon=icon("globe-asia"),
                           column(3,
                           wellPanel(
                                    selectInput("mapset", "Select Facility to Analyse:",
                                                choices = facilities),
                                    
                                    selectInput("binning_method", "Binning Method:",
                                                choices=c("Standard Deviation"="sd",
                                                                    "Equal"="equal",
                                                                    "Pretty"="pretty",
                                                                    "Quantile"="quantile",
                                                                    "K-means Cluster"="kmeans",
                                                                    "Hierarchical Cluster"="hclust",
                                                                    "Bagged Cluster"="bclust",
                                                                    "Fisher"="fisher",
                                                                    "Jenks"="jenks"
                                                )),
                             )
                           ),
                           
                           mainPanel(fluid=TRUE,
                                     fluidRow(
                                       column(6,h4("Supply Analysis", align = "center"), 
                                              withSpinner(tmapOutput(
                                           outputId = "supplymap"
                                         ))
                                       ),
                                       column(6,h4("Demand Analysis", align = "center"),
                                              withSpinner(tmapOutput(
                                           outputId = "demandmap"
                                         ))
                                       )
                                     )
                           )),

                  tabPanel("Spatial Point Pattern Analysis", icon=icon("chart-line"),
                           
                           column(
                             3,
                             wellPanel(
                               selectInput(
                                 "kdefacilityselect", "Select Facility to Analyse:",
                                 choices = facilities
                               ),
                               selectInput(
                                 "kdezoneselect", "Select Planning Area to Analyse:",
                                 choices = c(unique(mpsz3414_65Above$PLN_AREA_N))
                               ),
                               selectInput(
                                 "analysis_method", "Analysis Method:",
                                 choices = c("G-Function", 
                                             "F-Function",
                                             "K-Function",
                                             "L-Function")
                               )
                             )
                           ),

                           mainPanel(
                             fluidRow(
                               column(
                                 width = 6,
                                 h4("First Order Analysis", align = "center"),
                                 withSpinner(
                                   tmapOutput(
                                     outputId = "kdemap"
                                   )
                                 )
                               ),
                               column(
                                 width = 6,
                                 h4("Second Order Analysis", align = "center"),
                                 withSpinner(
                                   plotOutput(
                                     outputId = "secondordermap"
                                   )
                                 )
                               )
                             )
                           )
                  ),

                  tabPanel("Geographically Weighted Correlation", icon = icon("laptop-code"),
                           column(3,
                             wellPanel(
                               selectInput(
                                 "gwc_data", "Select Data to Analyse:",
                                 choices = facilities
                               ),
                               radioButtons(
                                 "gwc_type", "GWC type:",
                                 c("Pearson Correlation" = "pearson",
                                   "Spearman's Rank Correlation" = "spearman")
                               ),
                               sliderInput(
                                 "bandwidth", "Bandwidth:",
                                 min = 0, max = 50,
                                 value = 10
                               )
                             )
                           ),
                             
                           column(9,
                             withSpinner(plotOutput(
                               outputId = "gwcmap"
                             ))
                           )
                ))
)

#<--------------------Server-------------------->

server <- function(input, output) {
  
#<--------------------View Data-------------------->
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Population (65 Above)" = pop_65above,
           "CHAS Clinics" = chas_clinics_attributes,
           "Community Clubs" = community_club_attributes,
           "Eldercare Services" = eldercare_attributes,
           "Gyms" = gym_attributes,
           "Residents Committee" = rc_attributes
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
  
#<--------------------Supply and Demand Analysis-------------------->
  
  observe({
    x<-input$mapset
    bin<-input$binning_method
    
    if(is.null(x))
      x<-character(0)
    
    if(x =="Community Clubs"){
      output$supplymap <- renderTmap(
        tm_shape(mpsz3414_65Above)+
          tm_polygons("COMMUNITY_CLUBS",style=bin)+
          tm_view(view.legend.position = c('right', 'bottom'))
      )
      output$demandmap <- renderTmap (
        tm_shape(mpsz3414_65Above)+
          tm_polygons("SENIOR_POPULATION",style=bin)+
          tm_view(view.legend.position = c('right', 'bottom'))
      )
    }
    else if (x =="CHAS Clinics"){
      output$supplymap <- renderTmap(
        tm_shape(mpsz3414_65Above)+
          tm_polygons("CHAS_CLINICS", style=bin)+
          tm_view(view.legend.position = c('right', 'bottom'))
      )
      output$demandmap <- renderTmap (
        tm_shape(mpsz3414_65Above)+
          tm_polygons("SENIOR_POPULATION",style=bin)+
          tm_view(view.legend.position = c('right', 'bottom'))
      )
    }
    else if (x =="Eldercare Services"){
      output$supplymap <- renderTmap(
        tm_shape(mpsz3414_65Above)+
          tm_polygons("ELDERCARE_SERVICES", style=bin)+
          tm_view(view.legend.position = c('right', 'bottom'))
      )
      output$demandmap <- renderTmap (
        tm_shape(mpsz3414_65Above)+
          tm_polygons("SENIOR_POPULATION",style=bin)+
          tm_view(view.legend.position = c('right', 'bottom'))
      )
    }
    else if (x == "Residents Committee"){
      output$supplymap <- renderTmap(
        tm_shape(mpsz3414_65Above)+
          tm_polygons("RESIDENTS_COMMITTEES", style=bin)+
          tm_view(view.legend.position = c('right', 'bottom'))
      )
      output$demandmap <- renderTmap (
        tm_shape(mpsz3414_65Above)+
          tm_polygons("SENIOR_POPULATION",style=bin)+
          tm_view(view.legend.position = c('right', 'bottom'))
      )
    }
    else {
      output$supplymap <- renderTmap(
        tm_shape(mpsz3414_65Above)+
          tm_polygons("GYMS", style=bin)+
          tm_view(view.legend.position = c('right', 'bottom'))
      )
      output$demandmap <- renderTmap (
        tm_shape(mpsz3414_65Above)+
          tm_polygons("SENIOR_POPULATION",style=bin)+
          tm_view(view.legend.position = c('right', 'bottom'))
      )
    }
  })

#<--------------------Spatial Point Pattern Analysis-------------------->
  
  observe({
    label<-input$kdefacilityselect
    facility<-input$kdefacilityselect
    analysis<-input$analysis_method
    zone <- input$kdezoneselect
    
    if(is.null(facility))
      facility<-character(0)
    
    if(facility =="Community Clubs"){
      facility= cc_ppp
    }
    else if(facility =="CHAS Clinics"){
      facility= chas_ppp
    }
    else if(facility =="Eldercare Services"){
      facility= eldercare_ppp
    }
    else if(facility =="Residents Committee") {
      facility= rc_ppp
    }
    else {
      facility= gym_ppp
    }
    
    area = mpszB[mpszB@data$PLN_AREA_N == zone,]
    area_sp = as(area, "SpatialPolygons")
    area_owin = as(area_sp, "owin")
    facility_area_ppp = facility[area_owin]
    
#<--------------------Kernel Density Estimation-------------------->
    
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
        tm_layout(main.title="KDE of Facility")+
        tm_view(view.legend.position = c('right', 'bottom'))
    )
    
#<--------------------Second Order Analysis-------------------->
    
    if(is.null(analysis))
      anlaysis<-character(0)
    
    if(analysis == "G-Function"){
      facility_area.csr <- envelope(facility_area_ppp, Gest, correction = "all", nsim = 99)
      output$secondordermap <- renderPlot({
        plot(facility_area.csr)
      })
    }
    else if(analysis == "F-Function"){
      facility_area.csr <- envelope(facility_area_ppp, Fest, correction = "all", nsim = 99)
      output$secondordermap <- renderPlot({
        plot(facility_area.csr)
      })
    }
    else if(analysis == "K-Function"){
      facility_area.csr <- envelope(facility_area_ppp, Kest, nsim = 99) 
      output$secondordermap <- renderPlot({
        plot(facility_area.csr, . - r ~ r, 
             xlab="d", ylab="K(d)-r", xlim=c(0,500))
      })
    }
    else if(analysis == "L-Function"){
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
  
#<--------------------Geographically Weighted Correlation-------------------->
  
  observe({
    gwc_type<-input$gwc_type
    gwc_data<-input$gwc_data
    bandwidth <- input$bandwidth
    localstats1 <- gwss(sp_mpsz3414_65Above,vars=c("SENIOR_POPULATION","CHAS_CLINICS","COMMUNITY_CLUBS","ELDERCARE_SERVICES","RESIDENTS_COMMITTEES","GYMS"),bw=bandwidth,adaptive = TRUE,quantile = TRUE)
    proj4string(localstats1$SDF) <- CRS("+init=epsg:3414")
    
    if(is.null(gwc_data))
      gwc_data<-character(0)
    
    if(is.null(gwc_type))
      gwc_type<-character(0)
    
    if(gwc_data =="Community Clubs"){
      if(gwc_type =="pearson"){
        column_name <- 'Corr_SENIOR_POPULATION.COMMUNITY_CLUBS'
      }
      else {
        column_name <- 'Spearman_rho_SENIOR_POPULATION.COMMUNITY_CLUBS'
      }
    }
    else if(gwc_data =="CHAS_Clinics"){
      if(gwc_type =="pearson"){
        column_name <- 'Corr_SENIOR_POPULATION.CHAS_CLINICS'
      }
      else {
        column_name <- 'Spearman_rho_SENIOR_POPULATION.CHAS_CLINICS'
      }
    }
    else if(gwc_data =="Eldercare Services"){
      if(gwc_type =="pearson"){
        column_name <- 'Corr_SENIOR_POPULATION.ELDERCARE_SERVICES'
      }
      else {
        column_name <- 'Spearman_rho_SENIOR_POPULATION.ELDERCARE_SERVICES'
      }
    }
    else if(gwc_data =="Residents Committee"){
      if(gwc_type =="pearson"){
        column_name <- 'Corr_SENIOR_POPULATION.RESIDENTS_COMMITTEES'
      }
      else {
        column_name <- 'Spearman_rho_SENIOR_POPULATION.RESIDENTS_COMMITTEES'
      }
    }
    else{
      if(gwc_type =="pearson"){
        column_name <- 'Corr_SENIOR_POPULATION.GYMS'
      }
      else {
        column_name <- 'Spearman_rho_SENIOR_POPULATION.GYMS'
      }
    }
    
    dud <- is.nan(localstats1$SDF[[column_name]])
    output$gwcmap <- renderPlot(
      drawmap2(localstats1$SDF[!dud,],localstats1$SDF[[column_name]][!dud])
    )
  })
} 

#<--------------------Run the App-------------------->

shinyApp(ui = ui, server = server)
