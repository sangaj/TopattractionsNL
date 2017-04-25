#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(leaflet)
library(shiny)

ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  h4("Use the box to locate the attractions."),
  
  fluidRow(
    
    column(
      
      1
      
    ),
    
    column(
      
      5,
      
      selectInput(
        
        inputId = "location",
        label = "Select Attactions",
        choices = c("All", "Van Gogh Museum", "Keukenhof", "Heineken Experience", "Rijksmuseum", "Giethoorn","Hoge Veluwe National Park","Designer Outlet Roermond","Red Light District","Zaanse Schans","Kinderdijk","Castle De Haar","Peace Palace"),
        selected = "All"
        
      )
      
    ),
    column(
      
      5,
      
      h5("Website:"),
      
      htmlOutput("mySite")
      
    )
    
  ) 
)


server <- function(input, output, session) {
  
  coor <- data.frame(location = c("Van Gogh Museum", "Keukenhof","Heineken Experience","Rijksmuseum","Giethoorn","Hoge Veluwe National Park","Designer Outlet Roermond", "Red Light District","Zaanse Schans","Kinderdijk","Castle De Haar","Peace Palace"),
                     lat = c(52.3584192, 52.2700188,52.3578313,52.3599976,52.733479,52.0871235,51.1996913,52.3740008,52.4747847,51.8861194,52.1214529,52.0865911),
                     long = c(4.8788869, 4.5463893,4.8918249,4.8852188,5.9813278,5.8074264,5.9885547,4.8955779,4.8162885,4.6261411,4.9860063,4.2956021),
                     site=c("https://www.vangoghmuseum.nl/en","https://keukenhof.nl/en/","http://www.heineken.com/","https://www.rijksmuseum.nl/en","https://giethoorntourism.com/","https://www.hogeveluwe.nl/en","http://www.mcarthurglen.com/nl/designer-outlet-roermond/en/","http://www.iamsterdam.com","https://www.dezaanseschans.nl/en/","https://www.kinderdijk.com/","https://www.kasteeldehaar.nl/english","https://www.vredespaleis.nl/"),
                     loc = c("VanGoghMuseum", "Keukenhof","HeinekenExperience","Rijksmuseum","Giethoorn","HogeVeluweNationalPark","DesignerOutletRoermond", "RedLightDistrict","ZaanseSchans","Kinderdijk","CastleDeHaar","PeacePalace"))
  dir <- "Icons"
  dir.icons <- paste(dir, "/", as.character(coor$loc), ".png", sep = "")
  output$mymap <- renderLeaflet({
    position <- c(1:12)
    if(input$location != "All") position <- which(coor$location == input$location)
    
    icons <-  makeIcon(iconUrl = dir.icons[position], 
                       iconWidth = 31*215/230, 
                       iconHeight = 31,
                       iconAnchorX = 31*215/230/2, 
                       iconAnchorY = 16)
    map <- addMarkers(addTiles(leaflet()), 
                      icon =  icons, 
                      popup = coor$location[position], 
                      lat = as.numeric(coor$lat[position]), 
                      lng = as.numeric(coor$long[position]))
    
    map
    
    
    
    
  })
  
  output$mySite <- renderUI({
    if(input$location != "All") {
      position <- which(coor$location == input$location)
      website <- coor$site[position]
      tags$a(href = website, website)
    } else {
      tags$a(href = NA, NA)
    }
  })
  
  
  
  
  
}

shinyApp(ui = ui, server = server, options = list(height = 1080))
