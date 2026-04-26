#outdated ideas


# ==============================================================================
# UNIVERSAL LOADER FUNCTION (HANDLES BOTH .GPX AND .FIT)
# ==============================================================================

read_strava_universal <- function(file) {
  
  ext <- tools::file_ext(file)
  
  # --- CASE A: IF IT IS A .FIT FILE ---
  if (tolower(ext) == "fit") {
    tryCatch({
      # 1. Read the file
      fit <- readFitFile(file)
      
      # 2. Extract "session" info (to get the Sport Type)
      # This usually returns a list, so we grab the first element
      session_msg <- getMessagesByType(fit, "session")
      sport_type <- "Unknown"
      if(!is.null(session_msg) && nrow(session_msg[[1]]) > 0) {
        # 'sport' column usually holds "running", "cycling", etc.
        sport_type <- as.character(session_msg[[1]]$sport[1])
      }
      
      # 3. Extract "record" info (the GPS points)
      records_msg <- records(fit)
      if(is.null(records_msg)) return(NULL)
      
      # FITfileR can return a list of tibbles (if parameters changed mid-ride)
      # We bind them all together
      points <- bind_rows(records_msg) %>% 
        select(any_of(c("timestamp", "position_lat", "position_long"))) %>% 
        rename(time = timestamp, lat = position_lat, lon = position_long) %>% 
        mutate(
          type = sport_type,
          activity_id = paste0(basename(file), "_fit")
        ) %>% 
        drop_na(lat, lon, time)
      
      return(points)
      
    }, error = function(e) return(NULL))
    
    # --- CASE B: IF IT IS A .GPX FILE ---
  } else if (tolower(ext) == "gpx") {
    tryCatch({
      # 1. Read tracks for Metadata (Sport Type)
      meta <- st_read(file, layer = "tracks", quiet = TRUE)
      sport_type <- if("type" %in% names(meta)) as.character(meta$type[1]) else "Unknown"
      
      # 2. Read points
      points <- st_read(file, layer = "track_points", quiet = TRUE) %>% 
        mutate(
          lat = st_coordinates(.)[,2],
          lon = st_coordinates(.)[,1],
          type = sport_type,
          activity_id = paste0(basename(file), "_gpx")
        ) %>% 
        select(time, lat, lon, type, activity_id) %>% 
        st_drop_geometry() # We drop simple features to match the FIT dataframe structure
      
      return(points)
      
    }, error = function(e) return(NULL))
  }
}
# ===========================