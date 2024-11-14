# Katelyn N Diaz
# R Shiny Dashboard

library(shiny)
library(leaflet)
library(tidyverse)
library(shinycssloaders)
library(shinythemes)
library(readr)
library(tidycensus) #for api from nevi tract info
library(sf) #for nevi geo data
library(viridis) #not sure if i need
library(colorspace) #for red to blue for nevi



ui <- fluidPage(
  tags$style(HTML("
                  .navbar {
                  margin: 0px;
                  padding: 0px;
                  border: 0px;
                  }
                  #sideBar {
                  background-color: white;
                  border-color: none;
                  margin: 0px;
                  padding: 0px;
                  border: 0px;
                  border-left: 15px;
                  margine-left: 5px;
                  }
                  ")
  ),
  navbarPage("NYC Neighborhood Vulnerability and DPP Referrals", header = "", theme = shinytheme("cosmo"),
             tabPanel("Map", fluid = TRUE,
                      icon = icon("globe-americas"),

                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        sidebarPanel(id = "sideBar",

                                     titlePanel("Filters"),
                                     fluidRow(
                                       selectInput(inputId = "viewCat",
                                                   label = "View:",
                                                   choices = c("New York City",
                                                               "Bronx",
                                                               "Brooklyn",
                                                               "Manhattan",
                                                               "Queens",
                                                               "Staten Island"),
                                                   selected = "New York City",
                                                   width = "75%"
                                       ),


                                       h5("NYC Boundaries:"),
                                       # borough Checkbox
                                       checkboxInput(inputId = "boroughData",
                                                     label = "NYC boroughs", value=TRUE
                                       ),




                                       h5("NEVI Scores:"),
                                       # NEVI Checkbox
                                       checkboxInput(inputId = "neviData",
                                                     label = "NEVI Score"
                                       ),

                                       # nevi sub scores
                                       checkboxInput(inputId = "demoData",
                                                     label = "NEVI Demographic Score"
                                       ),
                                       checkboxInput(inputId = "econData",
                                                     label = "NEVI Economic Score"
                                       ),
                                       checkboxInput(inputId = "resData",
                                                     label = "NEVI Residential Score"
                                       ),
                                       checkboxInput(inputId = "healthData",
                                                     label = "NEVI Health Status Score"
                                       ),


                                       h5("Study-Specific Data"),

                                       # nevi binary
                                       checkboxInput(inputId = "neviBinaryData",
                                                     label = "Dichotomous NEVI Score", value=TRUE
                                       ),

                                       checkboxInput(inputId = "neviParticipantScoreData",
                                                     label = "Dichotomous NEVI Participant Scores"
                                       ),


                                       checkboxInput(inputId = "monteData",
                                                     label = "Montefiore Locations"
                                       ),

                                       # participant census tracts
                                       checkboxInput(inputId = "participantData",
                                                     label = "Study Participant Density", value=TRUE
                                       )

                                     ), # Fluid Row
                                     fluid = TRUE,
                                     width = 2

                        ), # Side bar panel
                        mainPanel(id = "main-panel",
                                  fluidRow(
                                    column(3, offset = 0, #width
                                    ),
                                    leafletOutput("map", height = 850) #orig 550
                                  ),
                                  fluid = TRUE,
                                  width = 10
                        )
                      ), # sidebar layout
                      tags$style(HTML(".leaflet-control legend{ font-size: 8px !important; }")) # Custom CSS to adjust legend font size
             ),# Main map panel close


             # Data tab start
             tabPanel ("Data",
                       fluid = TRUE,

                       br(),
                       h2("Abstract"),
                       br(),

                       p("Research on the determinants of Diabetes Prevention Programs (DPP) referral typically focuses on individual-level factors like patient and provider characteristics, neglecting neighborhood context. We aim to examine neighborhood environment vulnerability as a factor influencing likelihood of referral to a health system DPP in the Bronx, NY."),
                       br(),
                       p("NEVI is a neighborhood vulnerability index based on composite measures reflecting 4 primary neighborhood-level domains (demographic, economic, residential, and health status) measured at the census tract-level. High and low NEVI was determined by a median split (median=0.48; range: 0.19-0.58). This study used an observational cross-sectional design and recorded patient demographic, anthropometric, and DPP referral data from the electronic health records. Eligibility was based on hemoglobin A1c (HbA1c) between 5.7-6.4 and body mass index (BMI) ≥25 (kg/m2), a NYC residential address, and seen for a PCP visit between January 2018 and February 2020. The association between NEVI and DPP referral was examined using logistic regression, adjusted for age, sex, insurance type, preferred language, HbA1c (mg/dL), and BMI."),
                       br(),
                       p("Of 9,319 eligible patients, 66% were female, 78% were Hispanic or non-Hispanic Black, and the mean age was 53 years (SD: 13.9 years). Of referred patients, approximately 18% (n=851) have a higher NEVI while 12% (n=542) have a lower NEVI (p=2.2e-16). In the adjusted analysis, higher odds of referral were observed for patients residing in high NEVI neighborhoods than low NEVI neighborhoods (OR=1.50, 95% CI, 1.28-1.65)."),
                       br(),
                       p("We show a 50% higher likelihood of DPP referral among patients who reside in a high-vulnerability neighborhood. The reasons contributing referrals to DPP are likely multifactoral, including individual patient characteristics and PCP characteristics (e.g. knowledge & discretion). Our study highlights the importance of neighborhood-level context as a potential factor influencing PCP referrals to DPP."),
                       br(),
                       br(),



                       h2("References"),
                       br(),

                       h3("Research References"), #subtitle
                       br(),
                       a("Uong, S.P., Zhou, J., Lovinsky-Desir, S. et al. The Creation of a Multidomain Neighborhood Environmental Vulnerability Index Across New York City. J Urban Health 100, 1007–1023 (2023).", href= "https://doi.org/10.1007/s11524-023-00766-3"),
                       br(),

                       h3("Dashboard References"), #subtitle
                       # citation("tidycensus")
                       #if don't want citations as links, use p(""), not a()
                       br(),
                       a("Walker K, Herman M (2023). tidycensus: Load US Census Boundary and Attribute Data as 'tidyverse' and 'sf'-Ready Data Frames. R package version 1.5,", href= "https://CRAN.R-project.org/package=tidycensus"),
                       br(),
                       a("Cambon J, Hernangómez D, Belanger C, Possenriede D (2021). tidygeocoder: An R package for geocoding. Journal of Open Source Software, 6(65), 3544, (R package version 1.0.5)", href = "https://doi.org/10.21105/joss.03544"),
                       br(),
                       a(" Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A, Borges B (2022). shiny: Web Application Framework for R. R package version 1.7.3,", href= "https://CRAN.R-project.org/package=shiny"),
                       br(),
                       a(" Cheng J, Karambelkar B, Xie Y (2022). leaflet: Create Interactive Web Maps with the JavaScript 'Leaflet' Library. R package version 2.1.1,", href="https://CRAN.R-project.org/package=leaflet"),
                       br(),
                       a("Diaz K, Weden S, Goldmann T (2023). ggRtsy: Add Some Van Gogh Colors and Overlay Colors on Your 'ggplot()'. R package version 0.1.0,", href="<https://CRAN.R-project.org/package=ggRtsy"),
                       br(),
                       a("Pebesma, E., & Bivand, R. (2023). Spatial Data Science: With Applications in R. Chapman and Hall/CRC.", href= "https://doi.org/10.1201/9780429459016"),
                       br(),
                       br(),
                       br(),


                       br()

             )


  ) # navBarPage
) # FluidPage



server <- function(input, output, session) {

  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>%
      addProviderTiles("CartoDB.Positron", #where map type can be changed

                       # give the layer a name
                       group = "streetMap") %>%

      # Fit to NYC area
      fitBounds(lng1 = -74.257159, lat1 = 40.496010,
                lng2 = -73.699215, lat2 = 40.915568)
  })


  ## PULLING THE DATA

  #NYC geography
  # API key. Run when using a new session of R
  # census_api_key("d8635ccd895aae1a02b62eba946d956d797e4b26", install=TRUE) # add overwrite=TRUE

  options(tigris_use_cache = TRUE) #used to cache the downloaded geographic data

  bronx_pop <- get_acs(state="NY", county = "Bronx", geography = "tract", variables = "B01003_001", geometry = TRUE, year = 2015) #have to call a variable from the table for the code to run, even if i don't use it
  bk_pop <- get_acs(state="NY", county = "Kings County", geography = "tract", variables = "B01003_001", geometry = TRUE, year = 2015)
  man_pop <- get_acs(state="NY", county = "New York", geography = "tract", variables = "B01003_001", geometry = TRUE, year = 2015)
  SI_pop <- get_acs(state="NY", county = "Richmond County", geography = "tract", variables = "B01003_001", geometry = TRUE, year = 2015)
  queens_pop <- get_acs(state="NY", county = "Queens", geography = "tract", variables = "B01003_001", geometry = TRUE, year = 2015)

  #bind datasets together
  nyc_bind <- bind_rows(bronx_pop, bk_pop, man_pop, SI_pop, queens_pop)
  nyc_bind$GEOID <- as.double(nyc_bind$GEOID)

  #Nevi dataset
  nevi_tract_final <- read_csv("nevi_tract_final.csv")

  ### NEVI Binary
  # Find half
  vhalf = quantile(nevi_tract_final$nevi, c(0:2/2))

  # classify values
  nevi_tract_final$nevi_binary = with(nevi_tract_final,
                                      cut(nevi,
                                          vhalf,
                                          include.lowest = T,
                                          labels = c("Low","High")))


  # Combine

  nevi_tract_final <- nevi_tract_final %>%
    mutate(
      nevi_binary = factor(  # Convert to factor
        nevi_binary,
        levels = c("Low", "High")
      )
    )


  #Joining
  nycpop <- nyc_bind %>%
    left_join(nevi_tract_final, by = c("GEOID" = "Tract_FIPS"))

  ## MAKE GEO CONSISTENT DATUM

  # Convert geometry column to sf object
  nevi_sf <- st_as_sf(nycpop, wkt = "geometry")

  # Transform the CRS to WGS84 (EPSG:4326)
  nevi_sf <- st_transform(nevi_sf, crs = st_crs(4326))

  ## NEVI COUNT DATA

  participant_count <- read.csv("nevi_count.csv")
  participant_count$GEOID <- as.double(participant_count$GEOID)

  # Join participant count data with nevi_sf dataset
  nevi_with_count <- left_join(nevi_sf, participant_count, by = "GEOID")

  # Calculate centroids of census tracts
  tract_centroids <- st_centroid(nevi_with_count)

  ### New 4/9/24 KND
  centroid_coords <- st_coordinates(tract_centroids$geometry)

  # Create a data frame with coordinates and count of participants
  centroid_data <- data.frame(longitude = centroid_coords[, "X"],
                              latitude = centroid_coords[, "Y"],
                              n = tract_centroids$n,
                              referral_mean = tract_centroids$referral_mean,
                              referral_percentage = tract_centroids$referral_percentage,
                              ones_count = tract_centroids$ones_count,
                              nevi_binary_part = tract_centroids$nevi_binary_part,
                              nevi_binary_parta = tract_centroids$nevi_binary_parta,
                              GEOID = tract_centroids$GEOID,
                              NAME = tract_centroids$NAME,
                              borough = tract_centroids$borough)

  ### end



  # Change Zoom level to different states
  observeEvent(input$viewCat, {
    print(input$viewCat)
    print(typeof(input$viewCat))
    #   print(input$viewCat == "MA") #change KND
    proxy <- leafletProxy("map")

    if (input$viewCat == "New York City") {
      proxy <- proxy %>%
        setView(-73.978,40.698, zoom = 10.6)
    } else if (input$viewCat == "Bronx") {
      proxy <- proxy %>%
        setView(-73.852939,40.848711, zoom = 12)
    } else if (input$viewCat == "Brooklyn") {
      proxy <- proxy %>%
        setView(-73.950777,40.635133, zoom = 11.5)
    } else if (input$viewCat == "Manhattan") {
      proxy <- proxy %>%
        setView(-73.970174,40.776557, zoom = 11.5)
    } else if (input$viewCat == "Queens") {
      proxy <- proxy %>%
        # setView(-73.837929,40.658557, zoom = 11.5)
        setView(-73.837929,40.69, zoom = 11.5) #moved UP!
    }else {
      proxy <- proxy %>%
        setView(-74.137063,40.563855, zoom = 11.5) #longlat for SI KND
    }
  })



  # Called when the montedata checkbox is interacted with, adds/removes monte location data
  observeEvent(input$monteData, {

    # Use leaflet proxy instead of rerendering the map all the time
    proxy <- leafletProxy("map")

    if (input$monteData == TRUE) {
      #loads data
      monte_locations <- read_csv("monte_location_geo.csv")

      # Creates a circle marker for all monte locations
      proxy <- proxy %>%
        addCircleMarkers(stroke = FALSE, radius = 5, fillOpacity = 1, fillColor = "#1F1BED",
                         data = monte_locations, group = "Montefiore Locations",
                         popup = paste0(monte_locations$name_location, "<br>", monte_locations$address, ",", monte_locations$longitude, ",", monte_locations$latitude)
        )
    } else {
      # Removes only monte data
      proxy <- proxy %>%
        clearGroup(group = "Montefiore Locations")
    }
  })


  # adds/removes boroughs
  # `BoroName` for borough names
  observeEvent(input$boroughData, {
    proxy <- leafletProxy("map")

    if (input$boroughData == TRUE) {
      # Load and clean data
      boroughs_outline <- sf::read_sf("NYC_Borough_Boundary/NYC_Borough_Boundary.shp")

      # Transform CRS to WGS84
      boroughs_outline <- sf::st_transform(boroughs_outline, crs = 4326)

      # Add polylines to the map
      proxy %>%
        addPolylines(data = boroughs_outline,
                     group = "NYC boroughs", #unsure if need
                     color = "navy",
                     weight = 2) %>% #was 1.5
        addLabelOnlyMarkers(data = st_centroid(boroughs_outline),
                            group = "NYC boroughs",
                            label = ~BoroName,
                            labelOptions = labelOptions(noHide = TRUE),


        )



    } else {
      proxy <- proxy %>%
        clearGroup(group = "NYC boroughs") # %>%
    }
  })


  #dataset: neviData, group: nevi

  observeEvent(input$neviData, {
    proxy <- leafletProxy("map")

    if (input$neviData == TRUE) {
      ## PULLING THE DATA done above in lines 170 - 225

      # Define a continuous color palette based on the nevi variable
      color_palette <- colorNumeric(
        palette = "RdBu",
        domain = nevi_tract_final$nevi  # Use the range of nevi values as the domain
      )

      # Create a legend for the color palette
      legend_values <- seq(min(nevi_tract_final$nevi), max(nevi_tract_final$nevi), length.out = 5) # Adjust the number of bins
      legend_colors <- color_palette(legend_values)

      # Title for the legend
      legend_title <- "NEVI distribution"

      # Add polygons to the map with fill color based on the nevi variable
      proxy %>%
        addPolygons(
          data = nevi_sf,
          fillColor = ~ifelse(is.na(nevi), "#FFFFFF", color_palette(nevi)),
          color = "black",
          fillOpacity = 0.5,
          weight = .2, #change boundary outlines thin/thick
          group = "NEVI Score"  # Assign the polygons to a group for layer control
        ) %>%

        addLegend(
          position = "bottomright",
          pal = color_palette,
          values = legend_values,
          title = legend_title,
          labFormat = labelFormat(suffix = ""),
          opacity = 0.7,
          layerId = "neviLegend"
        )

    } else {
      proxy <- proxy %>%
        clearGroup(group = "NEVI Score") %>%
        removeControl(layerId = "neviLegend")
    }
  })


  #dataset: neviBinaryData, var: nevi_binary, group: Binary NEVI Score

  observeEvent(input$neviBinaryData, {
    proxy <- leafletProxy("map")

    if (input$neviBinaryData == TRUE) {
      # Define a discrete color palette based on the nevi_binary variable
      color_palette <- colorFactor(
        palette = c("#C11654", "#187DD5"),
        domain = c("Low", "High")
      )

      # Title for the legend
      legend_title <- "Dichotomous NEVI Distribution"

      # Add polygons to the map with fill color based on the nevi_binary variable
      proxy %>%
        addPolygons(
          data = nevi_sf,
          fillColor = ~ifelse(is.na(nevi_binary), "#FFFFFF", color_palette(nevi_binary)),
          color = "black",
          fillOpacity = 0.8, #from .5
          weight = .2,  # Change boundary outlines thin/thick; was .2
          group = "Binary NEVI Score"  # Assign the polygons to a group for layer control
        ) %>%

        addLegend(
          position = "bottomright",
          pal = color_palette,
          values = c("Low", "High"), # NEVI split at 0.48 median
          title = legend_title,
          labFormat = labelFormat(suffix = ""),
          opacity = 0.7,
          layerId = "binarynevi"
        )

    } else {
      proxy %>%
        clearGroup(group = "Binary NEVI Score") %>%
        removeControl(layerId = "binarynevi")
      # clearControls()
    }
  })


  #dataset: neviParticipantScoreData, var: nevi_binary_part, group: Binary NEVI Participant Scores
  observeEvent(input$neviParticipantScoreData, {
    proxy <- leafletProxy("map")

    if (input$neviParticipantScoreData == TRUE) {
      # Define a discrete color palette based on the nevi_binary variable
      color_palette <- colorFactor(
        palette = c("red", "navy"),
        domain = c("Low", "High")
      )

      # Title for the legend
      legend_title <- "Dichotomous NEVI Participant Distribution"

      # Add polygons to the map with fill color based on the nevi_binary variable
      proxy %>%
        addPolygons(
          data = nevi_with_count,
          fillColor = ~ifelse(is.na(nevi_binary_part), "#FFFFFF", color_palette(nevi_binary_part)),
          color = "black",
          fillOpacity = 0.7,
          weight = .2,  # Change boundary outlines thin/thick
          group = "Binary NEVI Participant Scores"  # Assign the polygons to a group for layer control
        ) %>%

        addLegend(
          position = "bottomright",
          pal = color_palette,
          values = c("Low", "High"), # NEVI split at 0.48 median
          title = legend_title,
          labFormat = labelFormat(suffix = ""),
          opacity = 0.7,
          layerId = "participantscore"
        )

    } else {
      proxy %>%
        clearGroup(group = "Binary NEVI Participant Scores") %>%
        removeControl(layerId = "participantscore")
    }
  })






  #dataset: demoData, group: nevi demographic score

  observeEvent(input$demoData, {
    proxy <- leafletProxy("map")

    if (input$demoData == TRUE) {

      # Define a continuous color palette based on the nevi variable
      color_palette <- colorNumeric(
        palette = "Blues",
        domain = nevi_tract_final$score_demo  # Use the range of nevi values as the domain
      )

      # Create a legend for the color palette
      legend_values <- seq(min(nevi_tract_final$score_demo), max(nevi_tract_final$score_demo), length.out = 5) # Adjust the number of bins
      legend_colors <- color_palette(legend_values)

      # Title for the legend
      legend_title <- "Demographic Score"

      # Add polygons to the map with fill color based on the nevi variable
      proxy %>%
        addPolygons(
          data = nevi_sf,
          fillColor = ~ifelse(is.na(score_demo), "#FFFFFF", color_palette(score_demo)),
          color = "black",
          fillOpacity = 0.5,
          weight = .2, #change boundary outlines thin/thick
          group = "NEVI Demographic Score"  # Assign the polygons to a group for layer control
        ) %>%

        addLegend(
          position = "bottomright",
          pal = color_palette,
          values = legend_values,
          title = legend_title,
          labFormat = labelFormat(suffix = ""),
          opacity = 0.7,
          layerId = "demoscore"
        )

    } else {
      proxy <- proxy %>%
        clearGroup(group = "NEVI Demographic Score") %>%
        removeControl(layerId = "demoscore")
    }
  })



  #dataset: econData, group: nevi economic score

  observeEvent(input$econData, {
    proxy <- leafletProxy("map")

    if (input$econData == TRUE) {

      # Define a continuous color palette based on the nevi variable
      color_palette <- colorNumeric(
        palette = "Greens",
        domain = nevi_tract_final$score_economic  # Use the range of nevi values as the domain
      )

      # Create a legend for the color palette
      legend_values <- seq(min(nevi_tract_final$score_economic), max(nevi_tract_final$score_economic), length.out = 5) # Adjust the number of bins
      legend_colors <- color_palette(legend_values)

      # Title for the legend
      legend_title <- "Economic Score"

      # Add polygons to the map with fill color based on the nevi variable
      proxy %>%
        addPolygons(
          data = nevi_sf,
          fillColor = ~ifelse(is.na(score_economic), "#FFFFFF", color_palette(score_economic)),
          color = "black",
          fillOpacity = 0.5,
          weight = .2, #change boundary outlines thin/thick
          group = "NEVI Economic Score"  # Assign the polygons to a group for layer control
        ) %>%

        addLegend(
          position = "bottomright",
          pal = color_palette,
          values = legend_values,
          title = legend_title,
          labFormat = labelFormat(suffix = ""),
          opacity = 0.7,
          layerId= "econscore"
        )

    } else {
      proxy <- proxy %>%
        clearGroup(group = "NEVI Economic Score") %>%
        removeControl(layerId = "econscore")
    }
  })


  #dataset: econData, group: nevi residential score

  observeEvent(input$resData, {
    proxy <- leafletProxy("map")

    if (input$resData == TRUE) {

      # Define a continuous color palette based on the nevi variable
      color_palette <- colorNumeric(
        palette = "Oranges",
        domain = nevi_tract_final$score_residential  # Use the range of nevi values as the domain
      )

      # Create a legend for the color palette
      legend_values <- seq(min(nevi_tract_final$score_residential), max(nevi_tract_final$score_residential), length.out = 5) # Adjust the number of bins
      legend_colors <- color_palette(legend_values)

      # Title for the legend
      legend_title <- "Residential Score"

      # Add polygons to the map with fill color based on the nevi variable
      proxy %>%
        addPolygons(
          data = nevi_sf,
          fillColor = ~ifelse(is.na(score_residential), "#FFFFFF", color_palette(score_residential)),
          color = "black",
          fillOpacity = 0.5,
          weight = .2, # Change boundary outlines thin/thick
          group = "NEVI Residential Score"  # Assign the polygons to a group for layer control
        ) %>%

        addLegend(
          position = "bottomright",
          pal = color_palette,
          values = legend_values,
          title = legend_title,
          labFormat = labelFormat(suffix = ""),
          opacity = 0.7,
          layerId = "resscore"
        )

    } else {
      proxy <- proxy %>%
        clearGroup(group = "NEVI Residential Score") %>%
        removeControl(layerId = "resscore")
      # clearControls()
    }
  })


  #dataset: healthData, group: nevi health status score

  observeEvent(input$healthData, {
    proxy <- leafletProxy("map")

    if (input$healthData == TRUE) {

      # Define a continuous color palette based on the nevi variable
      color_palette <- colorNumeric(
        palette = "Reds",
        domain = nevi_tract_final$score_healthstatus  # Use the range of nevi values as the domain
      )

      # Create a legend for the color palette
      legend_values <- seq(min(nevi_tract_final$score_healthstatus), max(nevi_tract_final$score_healthstatus), length.out = 5) # Adjust the number of bins
      legend_colors <- color_palette(legend_values)

      # Title for the legend
      legend_title <- "Health Status Score"

      # Add polygons to the map with fill color based on the nevi variable
      proxy %>%
        addPolygons(
          data = nevi_sf,
          fillColor = ~ifelse(is.na(score_healthstatus), "#FFFFFF", color_palette(score_healthstatus)),
          color = "black",
          fillOpacity = 0.5,
          weight = .2, #change boundary outlines thin/thick
          group = "NEVI Health Status Score"  # Assign the polygons to a group for layer control
        ) %>%

        addLegend(
          position = "bottomright",
          pal = color_palette,
          values = legend_values,
          title = legend_title,
          labFormat = labelFormat(suffix = ""),
          opacity = 0.7,
          layerId="healthscore"
        )

    } else {
      proxy <- proxy %>%
        clearGroup(group = "NEVI Health Status Score") %>%
        removeControl(layerId = "healthscore")
      # clearControls()
    }
  })

  #grouped participant data marker based on census tract (checkbox)
  observeEvent(input$participantData, {
    # Use leaflet proxy instead of rerendering the map all the time
    proxy <- leafletProxy("map")

    if (input$participantData == TRUE) {
      centroid_data <- centroid_data %>%
        filter(n != 0)

      proxy <- proxy %>%
        # clearMarkers() %>%
        addCircleMarkers(
          data = centroid_data,
          group = "Study Participant Grouping",
          lng = ~longitude,
          lat = ~latitude,
          radius = ~sqrt(n) / 2 +.1, # Adjust the size of markers based on 'n'
          color = "#00FF00",
          fillOpacity = 1,
          popup = ~paste("Participant Count:", n, "<br>",
                         "Borough: ", centroid_data$borough, "<br>",
                         "Census Tract: ", centroid_data$NAME)
        )%>%
        addLegend(
          position = "bottomright",
          colors = "#00FF00",
          labels = "Participant Density",
          # title = "Legend",
          opacity = 1,
          layerId="partdens"
        )
    } else {
      # Removes only monte data
      proxy <- proxy %>%
        clearGroup(group = "Study Participant Grouping")%>%
        removeControl(layerId = "partdens")
    }
  })


}

shinyApp(ui, server)
