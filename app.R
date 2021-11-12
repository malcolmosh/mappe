#load libraries
library(shiny)
library(googlesheets4)
library(dplyr)
library(shinyWidgets)
library(DT)
library(googledrive)
library(googleway)
library(tidyr)


#auth for reading drive files
drive_auth(path = "secret/clientsecret.json")
gs4_auth(path = "secret/clientsecret.json")

#import data
hikedata <- read_sheet("154FeA3kVqpMPwS9DgOOuZmd5rKVVEYjsBjAfoyk8-VQ")

# key for googleway package (Google Maps API)
fileName <- "secret/googlemaps.txt"
key <- readChar(fileName, file.info(fileName)$size)
set_key(key)

#liste des régions et des difficultés
listeregions=unique(hikedata$region)
listediff=list("Facile"=1, "Intermédiaire"=2, "Difficile"=3)

# Define UI ----
ui <- fluidPage(
    titlePanel(
      h1("Mappe 0.1"),
    ),
    
    sidebarLayout(
      sidebarPanel(
        helpText("Filtres / Filters"),
       #h2("Critères"),
        
        #textInput("text", h4("Recherche textuelle"), 
         #         value = ""),
        
      textInput(inputId = "my_address", label = "Point de départ (au Canada) / Start Point (in Canada)")    
      
      ,HTML(paste0(" <script> 
  
                function initAutocomplete() {

                 var autocomplete =   new google.maps.places.Autocomplete(document.getElementById('my_address'),{types: ['geocode']});
                 autocomplete.setFields(['address_components', 'formatted_address',  'geometry', 'icon', 'name']);
                 autocomplete.addListener('place_changed', function() {
                 var place = autocomplete.getPlace();
                 if (!place.geometry) {
                 return;
                 }

                 var addressPretty = place.formatted_address;
                 var address = '';
                 if (place.address_components) {
                 address = [
                 (place.address_components[0] && place.address_components[0].short_name || ''),
                 (place.address_components[1] && place.address_components[1].short_name || ''),
                 (place.address_components[2] && place.address_components[2].short_name || ''),
                 (place.address_components[3] && place.address_components[3].short_name || ''),
                 (place.address_components[4] && place.address_components[4].short_name || ''),
                 (place.address_components[5] && place.address_components[5].short_name || ''),
                 (place.address_components[6] && place.address_components[6].short_name || ''),
                 (place.address_components[7] && place.address_components[7].short_name || '')
                 ].join(' ');
                 }
                 var address_number =''
                 address_number = [(place.address_components[0] && place.address_components[0].short_name || '')]
                 var coords = place.geometry.location;
                 //console.log(address);
                 Shiny.onInputChange('jsValue', address);
                 Shiny.onInputChange('jsValueAddressNumber', address_number);
                 Shiny.onInputChange('jsValuePretty', addressPretty);
                 Shiny.onInputChange('jsValueCoords', coords);});}
                 </script> 
                 <script src='https://maps.googleapis.com/maps/api/js?key=", key,"&libraries=places&callback=initAutocomplete' async defer></script>"))
      
      
      #heures de route
      ,numericInput("slider_route", h4("Max. heures de route / Max Driving Dist."),
                    value=1000)

      #choix régions
      ,pickerInput("region",h4("Région/Region"), 
                  choices=listeregions, selected=NULL,
                  options = list(
                                 `deselect-all-text` = "Aucune / None",
                                 `select-all-text` = "Toutes / All", 
                                 `none-selected-text` = "Toutes / All",
                                 `selected-text-format` = paste0("count > ", length(listeregions) -1),`count-selected-text` = "Multiples"),
                                  multiple = T)
      
      #choix difficulté
      ,pickerInput("difficulte",h4("Difficulté/Difficulty"), 
                  choices = listediff, 
                  selected=NULL,
                  options = list(
                    `deselect-all-text` = "Aucune / None",
                    `select-all-text` = "Toutes / All", 
                    `none-selected-text` = "Toutes / All",
                    `selected-text-format` = paste0("count > ", length(listediff) -1),`count-selected-text` = "Multiples"),
                  multiple = T)
        
      #choix distance
      ,sliderInput("dist", h4("Longueur (km) / Hike Length (km)"),
                    min = 0, max = 100, value = c(0, 100))
      #choix durée
      ,sliderInput("duree", h4("Durée (heures) / Hike duration (hours)"),
                    min = 0, max = 20, value = c(0, 15))
        
      #autres choix varia
        ,h4("Extra")
        ,checkboxInput("boucle", "Boucle / Loop", value = FALSE)
        ,checkboxInput("chiens", "Chiens permis / Dogs allowed", value = FALSE)
        ,checkboxInput("famille", "Familiale / For Families", value = FALSE)
        ,checkboxInput("overnight", "Longue randonnée / Overnight ", value = FALSE)
        
      #bouton soumettre
        ,submitButton("GO!")
        
      #option de dons
        # br(),
        # br(),
        # h4("Contribuez"),
        # p("Mappe est un outil gratuit qui n'utilise pas la publicité.",
        #   a("Donate", 
        #     href="www.paypal.ca"))
        ),
      
      mainPanel(
        h2("Trouvez un sentier au Québec. Find a hike in Quebec."),
         textOutput("hikes"),
         textOutput(outputId = "full_address"),
        #tests pour imprimer l'addresse et les coordonnées de l'adresse saisie avant le tableau
         #textOutput(outputId = "test2"),
         DTOutput("hikefinal")
      )
    )
  )

# Define server logic ----
server <- function(input, output) {
  
  #détails de l'addresse saisie
  my_address <- reactive({
    
    if(!is.null(input$jsValueAddressNumber)){
      if(length(grep(pattern = input$jsValueAddressNumber, x = input$jsValuePretty ))==0){
        final_address<- c(input$jsValueAddressNumber, input$jsValuePretty)
      } else{
        final_address<- input$jsValuePretty
      }
      final_address
    }
  })
  
  #coordonnées de l'addresse saisie
  my_coords2 <- reactive({
    if(!is.null(my_address())){
      georesult <- google_geocode(my_address())
      my_coords <- geocode_coordinates(georesult)
      my_coords2 <- c(my_coords$lat[1], my_coords$lng[1])
    }
  })
  
  #objet texte addresse
  # output$full_address <- renderText({
  #   if(!is.null(my_address())){
  #     paste("Point de départ/Starting point:",my_address())
  #   }
  #   
  # })
  
  #objet texte coordonnées
  output$test2  <- renderText({ 
    my_coords2()
    
  })
  
  #texte descriptif des résultats
  # output$hikes <- renderText({ 
  #   paste("Vous cherchez un sentier dans la région de", input$region, "de niveau", input$difficulte)
  # })
  
  #premier filtre selon les choix
  hikedata_filt <- reactive({
    hikedata %>%
      filter(
        longueur >= input$dist[1],
        longueur <= input$dist[2],
        duree >= input$duree[1],
        duree <= input$duree[2]) %>%
      { if (!is.null(input$region)) filter(., region %in% input$region) else . } %>%
      { if (!is.null(input$difficulte)) filter(., difficulte %in% input$difficulte) else . } %>%
      { if (input$boucle==TRUE) filter(., boucle==1) else . } %>%
      { if (input$chiens==TRUE) filter(., chien==1) else . } %>%
      { if (input$famille==TRUE) filter(., famille==1) else . }%>%
      { if (input$overnight==TRUE) filter(., overnight==1) else . }

  })
  
  #trouver les distances avec les sentiers filtrés et construire table 
  hikedata2 <- reactive({
      
      hikedata_filt <- hikedata_filt()
      
      my_coords2 <- my_coords2()
      
      validate(need(my_coords2, "Entrez votre adresse de départ / Fill in your starting point first"))
      
      #coordonnées géographique des destinations
      dests=c(hikedata_filt$coordA)
      
      calcul <- google_distance(origins = my_coords2,
                                destinations = dests,
                                language="fr")
      
      #vecteur des distances aux destinations en heures 
      distances_heures <- (calcul$rows$elements[[1]]$duration$value/3600)
      distances_text <- calcul$rows$elements[[1]]$duration$text
      
      #créé tibble avec distances entre origine et toutes les destinations, affiche durée en heures
      #autre option = extraire texte de durée
      calculfinal=tibble(ident=hikedata_filt$ident,dist_heur=distances_heures, dist_text=distances_text)
      
      #remplacer valeurs vides par string NA
      calculfinal <-
        calculfinal %>%
        replace_na(list(dist_text = "NA"))
      
      #fusionner avec table originale
      hikedata2 <- 
        merge(hikedata_filt,calculfinal,by="ident") %>% 
        arrange(dist_heur) %>% 
        filter(dist_heur <= input$slider_route) %>% 
        select(nom,parc,region,longueur,duree,dist_text) %>% 
        `names<-`(.,c("Sentier - Trail ","Parc national - National Park","Région - Region","Longueur - Length","Durée - Duration","Temps de conduite - Driving time"))
  })
  
    output$hikefinal <- renderDT ({
      
      # datatable(hikedata2(), options=list(columnDefs = list(list(visible=FALSE, targets=c(8:18)))))
      datatable(hikedata2(), options = list(dom = 't'))
      
    })
    

}

# Run the app ----
shinyApp(ui=ui,server=server)
