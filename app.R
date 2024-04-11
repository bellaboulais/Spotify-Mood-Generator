#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# Load Shiny library
library(shiny)
library(tidyverse)
library(spotifyr)
install.packages("plotly")
library(plotly)

# Define UI
ui <- fluidPage(
  # Custom CSS to enhance the appearance
  tags$head(
    tags$style(HTML("
      body { 
        background-color: #191414; 
        color: #FFFFFF; 
      }
      .well { 
        background-color: rgba(25, 20, 20, 0.8); 
        border: none; 
        border-radius: 15px;
      }
      .slider-input .slider-track { 
        background-color: #1DB954; 
      }
      .slider-input .slider-handle { 
        background-color: #1DB954; 
      }
      .shiny-input-radiogroup .radio { 
        padding: 5px; 
      }
      .shiny-input-radiogroup .radio label { 
        color: #FFFFFF; 
      }
      .shiny-input-container { 
        padding-bottom: 15px; 
      }
      #moodText, #genreText { 
        color: #1DB954; 
        font-weight: bold; 
      }
      h3 { 
        border-bottom: 1px solid #1DB954; 
        color: #1DB954;
        tex-align: left;
      }
      .title { 
        color: #1DB954; 
        text-align: center;
      }
      .spotify-logo {
        display: block;
        margin: 20px auto;
        width: 100px; /* Adjust based on your preference */
        opacity: 0.8;
      }
    "))
  ),
  
  # Application title
  titlePanel(div(class = "title", "Spotify Mood and Genre Selector"), windowTitle = "Spotify Selector"),
  
  # Sidebar layout with input controls
  sidebarLayout(
    sidebarPanel(
      h3("Your Selections:"),
      radioButtons("mood",
                   "Mood Level:",
                   choices = mood_choices,
                   inline = TRUE),
      
      
      # Spotify logo under the slider
      tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/1/19/Spotify_logo_without_text.svg", class = "spotify-logo"),
      
      # Radio buttons for genre selection
      radioButtons("genre",
                   "Select Genre:",
                   choices = genre_choices,
                   inline = TRUE),
      
      # Submit button to update song list
      actionButton("submit", "Submit"),
      actionButton("showGraph", "Zoom Into Visualization", icon = icon("bar-chart"))
      
    ),
    
    # Main panel to display outputs (using text outputs as placeholders)
    mainPanel(
      fluidRow(
        column(
          width = 6,
          h3("Your Songs:"),
          textOutput("moodText"),
          textOutput("genreText"),
          uiOutput("spotifyTracks")
        ),
        column(
          width = 6,
          h3("Visualizations:"),
          plotlyOutput("spotifyPlot", height = "100vh")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  detailed_tracks_data <- reactive({
    # Code below runs only after the submit button is clicked
    req(input$submit)
    
    Sys.setenv(SPOTIFY_CLIENT_ID = "________")
    Sys.setenv(SPOTIFY_CLIENT_SECRET = "________")
    
    # Authenticate with Spotify's API
    access_token <- get_spotify_access_token()
    
    # Get the selected genre from the input
    selected_genre <- input$genre 
    
    # Get top artists for the selected genre
    genre_artists <- get_genre_artists(genre = selected_genre, limit = 20, authorization = access_token)
    
    # Initialize an empty dataframe to store random songs
    random_songs <- data.frame()
    
    # Loop through each artist to get their top tracks
    for (artist in genre_artists$id) {
      artist_tracks <- get_artist_top_tracks(artist, 'US')
      if (nrow(artist_tracks) > 0) {
        shuffled_tracks <- artist_tracks[sample(nrow(artist_tracks)), ]
        num_tracks_to_select <- min(5, nrow(shuffled_tracks))
        selected_tracks <- shuffled_tracks[1:num_tracks_to_select, ]
        random_songs <- rbind(random_songs, selected_tracks)
      }
    }
    
    # Initialize an empty dataframe to store track information and audio features
    detailed_tracks <- data.frame()
    
    # Loop through each track in the random_songs dataframe
    for (track_id in random_songs$id) {
      track_features <- get_track_audio_features(track_id)
      if (nrow(track_features) > 0) {
        track_info <- merge(random_songs[random_songs$id == track_id, c("name", "id")], track_features, by = "id")
        detailed_tracks <- rbind(detailed_tracks, track_info)
      }
    }
    
    if(nrow(detailed_tracks) > 0) {
      min_loudness <- min(detailed_tracks$loudness, na.rm = TRUE)
      max_loudness <- max(detailed_tracks$loudness, na.rm = TRUE)
      min_tempo <- min(detailed_tracks$tempo, na.rm = TRUE)
      max_tempo <- max(detailed_tracks$tempo, na.rm = TRUE)
      
      detailed_tracks %>%
        mutate(loudness_scaled = (loudness - min_loudness)/(max_loudness - min_loudness),
               tempo_scaled = (tempo - min_tempo)/(max_tempo - min_tempo)) %>%
        select(id, name, uri, danceability, energy, duration_ms, loudness_scaled, tempo_scaled, valence) %>%
        mutate(overall_mood = (danceability + energy + loudness_scaled + tempo_scaled) / 4,
               mood_type = case_when(
                 overall_mood > 0.5 & valence > 0.5 ~ "happy",
                 overall_mood > 0.5 & valence <= 0.5 ~ "angry",
                 overall_mood <= 0.5 & valence <= 0.5 ~ "sad",
                 overall_mood <= 0.5 & valence > 0.5 ~ "chill",
                 TRUE ~ "unknown"
               ))
    } else {
      # Return an empty data frame if detailed_tracks is empty
      return(data.frame())
    }
  })
  
  filtered_tracks <- reactiveVal(NULL)
  
  output$moodText <- renderText({
    paste("Mood Level:", input$mood)
  })
  
  output$genreText <- renderText({
    paste("Genre:", input$genre)
  })
  
  output$spotifyTracks <- renderUI({
    # Ensure the submit button was clicked
    req(input$submit)
    
    selected_mood <- input$mood
    # Access the reactive detailed_tracks_data
    tracks <- detailed_tracks_data()
    
    if(nrow(tracks) == 0) {
      # Return nothing if tracks is empty
      return(NULL)
    }
    
    filtered <- tracks %>%
      filter(mood_type == selected_mood)
    
    filtered_tracks(filtered)
    
    spotifyURIs <- head(filtered$uri, 10) 
    
    trackEmbeds <- map(spotifyURIs, function(uri) {
      spotifyEmbedURL <- sprintf("https://open.spotify.com/embed/track/%s", gsub("spotify:track:", "", uri))
      iframe <- tags$iframe(src = spotifyEmbedURL, width = "300", height = "80", frameborder = "0", allowtransparency = "true", allow = "encrypted-media")
      tags$div(iframe, style = "padding-bottom: 20px;")
    })
    
    do.call(tagList, trackEmbeds)
  })
  output$spotifyPlot <- renderPlotly({
    req(input$submit) 
    
    tracks <- filtered_tracks() 
    
    if(is.null(tracks) || nrow(tracks) == 0) {
      return(NULL)
    }
    
    p <- ggplot(tracks[1:10, ], aes(x = valence, y = overall_mood, color = name)) +
      geom_point() +
      geom_vline(xintercept = 0.5) +
      geom_hline(yintercept = 0.5) +
      labs(x = "Valence", y = "Overall Mood", color = "Song Title") +
      theme_minimal() +
      scale_x_continuous(limits = c(0, 1)) +
      scale_y_continuous(limits = c(0, 1))
    
    p <- ggplotly(p) |>
      layout(legend = list(orientation = "h", y = -0.4))
    
    p
  })
  
  
  
  observeEvent(input$showGraph, {  
    browser()  # Execution will pause here in an interactive R session
    
    showModal(modalDialog(
      title = "Statistical Analysis",
      plotlyOutput("spotifyPlotModal", width = "100%", height = "600px"),
      size = "l" # Set modal size to large
    ))
  })
  
  output$spotifyPlotModal <- renderPlotly({
    req(filtered_tracks()) 
    tracks <- filtered_tracks()
    
    if(nrow(tracks) == 0) {
      return(NULL) 
    }
    
    p <- ggplot(tracks[1:10, ], aes(x = valence, y = overall_mood, color = name)) +
      geom_point() +
      geom_vline(xintercept = 0.5) +
      geom_hline(yintercept = 0.5) +
      labs(x = "Valence", y = "Overall Mood", color = "Song Title") +
      theme_minimal() +
      scale_x_continuous(limits = c(0, 1)) +
      scale_y_continuous(limits = c(0, 1))
    
    p <- ggplotly(p) |>
      layout(legend = list(orientation = "h", y = -0.4))
    
    p
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
