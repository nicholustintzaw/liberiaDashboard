################################################################################
#
#
#
# This is a Shiny web application to support the implementation of health and
# nutrition coverage surveys in Liberia.
# 
# This code is for the server logic function of the Shiny web aplication.
#
#
################################################################################


################################################################################
#
# Server logic for web application
#
################################################################################
#
# Define server logic for application
#
server <- function(input, output, session) {
  ##
  ifaHex <- reactive({
    xx <- gmHexGrid
    xx@data <- data.frame(xx@data, ifaInt)
    xx
  })
  ##
  output$mapGM <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = mapbox.street,
        attribution = "Maps by <a href='http://www.mapbox.com/'>Mapbox</a>"
      ) %>%
      #fitBounds(lng1 = gmLng1, lat1 = gmLat1,
      #          lng2 = gmLng2, lat2 = gmLat2)
      setView(lng = gmLng, lat = gmLat, zoom = 11)
  })
  ##
  observeEvent(input$round == "r1" & input$indicators == "ifa", {
    pal <- colorNumeric(palette = brewer.pal(n = 10, name = "RdYlGn"),
                        domain = c(0,1))
    ##
    leafletProxy("mapGM") %>%
      addPolygons(
        data = ifaHex(),
        fillColor = pal(ifaHex()$ifa1),
        weight = 1,
        opacity = 1,
        color = pal(ifaHex()$ifa1),
        dashArray = "",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = paste(round(ifaHex()$ifa1 * 100, digits = 1), "%", sep = ""),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto"),
        group = "Attended ANC"
      ) %>%
      addPolygons(
        data = ifaHex(),
        fillColor = pal(ifaHex()$ifa2),
        weight = 1,
        opacity = 1,
        color = pal(ifaHex()$ifa2),
        dashArray = "",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = paste(round(ifaHex()$ifa2 * 100, digits = 1), "%", sep = ""),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto"),
        group = "Know/heard of IFA"
      ) %>%
      addPolygons(
        data = ifaHex(),
        fillColor = pal(ifaHex()$ifa3),
        weight = 1,
        opacity = 1,
        color = pal(ifaHex()$ifa3),
        dashArray = "",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = paste(round(ifaHex()$ifa3 * 100, digits = 1), "%", sep = ""),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto"),
        group = "Received/purchased IFA"
      ) %>%
      addPolygons(
        data = ifaHex(),
        fillColor = pal(ifaHex()$ifa4),
        weight = 1,
        opacity = 1,
        color = pal(ifaHex()$ifa4),
        dashArray = "",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = paste(round(ifaHex()$ifa4 * 100, digits = 1), "%", sep = ""),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto"),
        group = "Took IFA"
      ) %>%
      addLegend(pal = pal, 
                opacity = 0.7,
                values = c(0,1),
                position = "bottomleft", 
                labFormat = labelFormat(suffix = "%", transform = function(x) x * 100),
                layerId = "legend") %>%
      addLayersControl(
        baseGroups = c("Attended ANC", "Know/heard of IFA", "Received/purchased IFA", "Took IFA"),
        position = "topright",
        options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
      )
  })
  
}
