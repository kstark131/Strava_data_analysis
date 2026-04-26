# ==============================================================================
# PROJECT: Strava Data Visualization & Animation (Berkeley)
# DESCRIPTION: Retrieves activities via API, clips to city boundary, calculates 
#              statistics, and generates static & animated maps.
# AUTHOR: KYLE STARK
# ORIGINAL VERSION: 1/27/2026
# 
# ==============================================================================

library(sf)
library(tidyverse)
library(gganimate)
library(ggspatial)
library(rStrava)
library(googledrive)
library(elevatr)
library(raster)
library(rayshader) 

# ==============================================================================
# SECTION 1: AUTHENTICATION & SETUP
# ==============================================================================

outpath <- "C:\\Users\\kstar\\Desktop\\R_projects\\Strava_data_analysis\\figures"

# API Credentials
app_name      <- "DataImporter" 
app_client_id <- "198319"  
app_secret    <- "acfb06d616562aeaffe72cdba12c01d3d0318216"

# Create authentication token (Browser will open to authorize)
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope = "activity:read_all"))

# ==============================================================================
# SECTION 2: DATA RETRIEVAL & PROCESSING
# ==============================================================================

# 1. Fetch Summary List
my_acts <- get_activity_list(stoken)

# 2. Compile & Filter (Runs in 2024+)
acts_data <- compile_activities(my_acts) %>% 
  filter(start_date_local >= "2024-01-01") %>% 
  filter(type == "Run")

# 3. Download Streams (Lat/Lon)
get_stream_safe <- function(act_id) {
  tryCatch({
    get_activity_streams(my_acts, stoken, id = act_id, 
                         types = c("latlng", "time", "altitude", "grade_smooth", "distance")) %>% 
      mutate(activity_id = as.character(act_id)) 
  }, error = function(e) return(NULL))
}

# 2. Batch Download
batch_data <- map_dfr(acts_data$id, get_stream_safe)

# 3. Clean & Prepare
final_data <- batch_data %>% 
  rename(lat = lat, lon = lng) %>% 
  drop_na(lat, lon) %>% 
  # Join metadata
  left_join(select(acts_data, id, start_date, name), by = c("activity_id" = "id")) %>% 
  mutate(date = as.Date(start_date)) %>%
  # PRE-CALCULATION: Sort strictly by time to ensure delta calcs are right later
  arrange(activity_id, time)

# ==============================================================================
# SECTION 3: SPATIAL CLIPPING (BERKELEY)
# ==============================================================================

# 1. Prepare Boundary
boundary_path <- "C:\\Users\\kstar\\OneDrive\\Desktop\\strava\\California_Incorporated_Cities"
boundary_shp <- st_read(boundary_path, quiet = TRUE) %>% 
  filter(NAME == "Berkeley") %>% 
  st_transform(4326)

# 2. Convert & Clip
berkeley_data <- final_data %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>% 
  st_filter(boundary_shp) %>% 
  st_drop_geometry() # Drop immediately to return to fast Dataframe operations

# ==============================================================================
# SECTION 4: SUMMARY STATISTICS
# ==============================================================================
# ------------------------------------------------------------------------------
# 4.1. Point-Level Calculations
# ------------------------------------------------------------------------------
point_stats <- berkeley_data %>%
  arrange(activity_id, time) %>%
  group_by(activity_id) %>%
  mutate(
    # A. Time Delta
    time_diff = as.numeric(difftime(time, lag(time, default = first(time)), units = "secs")),
    
    # B. Distance Delta
    # Your 'distance' stream is in KILOMETERS. We calculate the delta in KM.
    raw_dist_diff_km = distance - lag(distance, default = first(distance)),
    
    # C. Gap Correction (Teleportation fix)
    # If gap > 30s, we assume 0 distance gained (prevents lines streaking across map)
    is_gap = time_diff > 30,
    dist_inc_km = ifelse(is_gap, 0, raw_dist_diff_km),
    
    # D. Elevation Gain
    # Your 'altitude' stream is in METERS.
    alt_diff_m = altitude - lag(altitude, default = first(altitude)),
    elev_gain_m = ifelse(!is_gap & alt_diff_m > 0, alt_diff_m, 0),
    
    # E. Grade Categories
    grade_category = case_when(
      grade_smooth < -2 ~ "Downhill",
      grade_smooth >= -2 & grade_smooth <= 2 ~ "Flat",
      grade_smooth > 2 & grade_smooth <= 8 ~ "Climb",
      grade_smooth > 8 ~ "Steep Climb",
      TRUE ~ "Flat"
    )
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# 4.2. Per-Run Summaries
# ------------------------------------------------------------------------------
run_stats <- point_stats %>%
  group_by(activity_id) %>%
  summarise(
    date = min(date),
    
    # DISTANCE (Input is KM)
    total_km = sum(dist_inc_km, na.rm = TRUE),
    dist_miles = total_km * 0.621371, # Corrected Factor (KM -> Miles)
    
    # ELEVATION (Input is Meters)
    total_elev_gain_m = sum(elev_gain_m, na.rm = TRUE),
    total_elev_gain_ft = total_elev_gain_m * 3.28084, # Meters -> Feet
    
    # INTENSITY
    avg_grade = mean(grade_smooth, na.rm = TRUE),
    
    # HILLINESS SCORE (Meters climbed per KM traveled)
    # 0-5 = Flat, 10-20 = Hilly, 20+ = Mountainous
    hilliness_score = ifelse(total_km > 0, total_elev_gain_m / total_km, 0)
  ) %>%
  arrange(desc(dist_miles)) %>% 
  mutate(rank_distance = row_number()) 

# ------------------------------------------------------------------------------
# 4.3. Global Summaries
# ------------------------------------------------------------------------------
global_stats <- run_stats %>%
  summarise(
    Count_Runs = n(),
    
    # Totals
    Total_Miles = sum(dist_miles),
    Total_Climb_Feet = sum(total_elev_gain_ft),
    
    # Averages
    Avg_Run_Length_Miles = mean(dist_miles),
    Median_Run_Length_Miles = median(dist_miles),
    Avg_Climb_Feet = mean(total_elev_gain_ft),
    
    # Overall Berkeley "Hilliness" (Global m/km ratio)
    # This should now be a normal number (e.g. 10-25), not 18,000
    Overall_Hilliness = sum(total_elev_gain_m) / sum(total_km)
  )



# ------------------------------------------------------------------------------
# 4.4. Top 10 Lists (For Plotting later)
# ------------------------------------------------------------------------------

# Top 10 Longest
top_10_longest <- run_stats %>% 
  slice_head(n = 10) %>% 
  mutate(label = paste0(round(dist_miles, 1), " mi"))

# Top 10 Hardest (Most Elevation Gain)
top_10_hardest <- run_stats %>% 
  arrange(desc(total_elev_gain_ft)) %>% 
  slice_head(n = 10) %>% 
  mutate(label = paste0(round(total_elev_gain_ft, 0), " ft climb"))





# ==============================================================================
# SECTION 5: STATIC VISUALIZATION (EXPANDED)
# ==============================================================================

# --- A. CLASSIC HEATMAP (All Runs) ---
# Emphasizes density: Brighter areas = Most frequented paths
p_all <- ggplot(berkeley_data, aes(x = lon, y = lat, group = activity_id)) +
  geom_sf(data = boundary_shp, fill = "gray95", size = 0.5, color = "gray80", inherit.aes = FALSE) +
  geom_path(color = "#fc4c02", linewidth = 0.2, alpha = 0.5) +
  theme_void() +
  labs(title = "Berkeley Run Density")

local_filename <- "map_1_heatmap.png"
full_local_path <- file.path(outpath, local_filename)
ggsave(local_filename, p_all,path=outpath, width = 10, height = 10, bg = "white")


# --- B. "THE PAIN MAP" (Grade/Slope) ---
# Visualizes effort: Where are the hills? 
# We filter out outliers > 20% or < -20% to keep the color scale useful
p_grade <- berkeley_data %>%
  filter(grade_smooth > -20 & grade_smooth < 20) %>% 
  ggplot(aes(x = lon, y = lat, group = activity_id, color = grade_smooth)) +
  geom_sf(data = boundary_shp, color = "gray20", inherit.aes = FALSE) +
  geom_path(linewidth = 0.5) +
  # Divergent scale: Blue (Down) -> White (Flat) -> Red (Up)
  scale_color_gradient2(low = "cyan", mid = "white", high = "red", midpoint = 0, name = "Grade %") +
  theme_void() +
  # theme(plot.background = element_rect(fill = "black"),
  #       legend.text = element_text(color = "white"),
  #       legend.title = element_text(color = "white"),
  #       plot.title = element_text(color = "white")) +
  labs(title = "The Pain Map: Gradient Analysis")


local_filename <- "map_2_grade.jpg"
full_local_path <- file.path(outpath, local_filename)
ggsave(local_filename, p_grade, path=outpath, width = 10, height = 10)


# --- C. "THE HIGH GROUND" (Altitude) ---
# Visualizes Elevation: Lowlands vs Highlands
p_elev <- ggplot(berkeley_data, aes(x = lon, y = lat, group = activity_id, color = altitude)) +
  geom_sf(data = boundary_shp, fill = NA, color = "black", inherit.aes = FALSE) +
  geom_path(linewidth = 0.5, alpha = 0.6) +
  # Turbo is excellent for elevation data
  scale_color_viridis_c(option = "turbo", name = "Elev (m)") +
  theme_void() +
  labs(title = "Elevation Profile")


local_filename <- "map_3_altitude.png"
full_local_path <- file.path(outpath, local_filename)
ggsave(local_filename, p_elev,  path=outpath, width = 10, height = 10, bg = "white")


# --- D. TOP 10 LONGEST (Faceted) ---
# Note: Using 'run_stats' from Section 4
top_10_dist_ids <- run_stats %>% arrange(desc(dist_miles)) %>% slice_head(n = 10) %>% pull(activity_id)

data_top_dist <- final_data %>% # Use unclipped data for context
  filter(activity_id %in% top_10_dist_ids) %>%
  left_join(run_stats, by = "activity_id") %>%
  mutate(facet_label = paste0("#", rank_distance, ": ", round(dist_miles, 1), " mi"))

p_facet_dist <- ggplot(data_top_dist, aes(x = lon, y = lat, group = activity_id)) +
  geom_path(color = "#fc4c02", size = 0.8, lineend = "round") +
  facet_wrap(~ reorder(facet_label, rank_distance), scales = "free", ncol = 5) + 
  theme_void() +
  theme(strip.text = element_text(face = "bold", size = 9))



local_filename <- "map_4_top10_longest.png"
full_local_path <- file.path(outpath, local_filename)
 ggsave(local_filename, p_facet_dist, path=outpath, width = 12, height = 6, bg = "white")


# --- E. TOP 10 HARDEST (Faceted by Elevation Gain) ---
top_10_climb_ids <- run_stats %>% arrange(desc(total_elev_gain_ft)) %>% slice_head(n = 10) %>% pull(activity_id)

data_top_climb <- final_data %>% 
  filter(activity_id %in% top_10_climb_ids) %>%
  left_join(run_stats, by = "activity_id") %>%
  # Create a rank for climbing specifically for this plot
  mutate(facet_label = paste0(round(total_elev_gain_ft, 0), " ft climb"))

p_facet_climb <- ggplot(data_top_climb, aes(x = lon, y = lat, group = activity_id)) +
  geom_path(color = "darkred", size = 0.8, lineend = "round") +
  facet_wrap(~ reorder(facet_label, -total_elev_gain_ft), scales = "free", ncol = 5) + 
  theme_void() +
  theme(strip.text = element_text(face = "bold", size = 9, color = "darkred")) +
  labs(title = "The Sufferfest: Top 10 Climbs")


local_filename <- "map_5_top10_climbs.png"
full_local_path <- file.path(outpath, local_filename)
 ggsave(local_filename, p_facet_climb, path=outpath, width = 12, height = 6, bg = "white")


# ==============================================================================
# SECTION 6: ANIMATED VISUALIZATION
# ==============================================================================

# --- A. ELAPSED TIME (Real Speed / Mass Start) ---
# Highlights short vs long efforts
data_elapsed <- berkeley_data %>%
  group_by(activity_id) %>%
  mutate(
    start_time = min(time),
    elapsed_seconds = as.numeric(difftime(time, start_time, units = "secs"))
  ) %>% ungroup()

anim_elapsed <- ggplot(data_elapsed, aes(x = lon, y = lat, group = activity_id)) +
  geom_sf(data = boundary_shp, fill = NA, color = "black", inherit.aes = FALSE) +
  geom_path(alpha = 0.3, color = "#fc4c02", size = 0.5) +
  theme_void() +
  transition_reveal(elapsed_seconds) +
  labs(title = "Elapsed: {round(frame_along/60)} mins")

local_filename <- "anim_elapsed.gif"
full_local_path <- file.path(outpath, local_filename)
 anim_save(local_filename, animate(anim_elapsed, nframes = 200, fps = 20, renderer = gifski_renderer()), path=outpath )


# --- B. PROPORTIONAL TIME (Normalized 0-100% / Mass Start) ---
# Highlights network coverage "drawing" itself
data_prop <- berkeley_data %>%
  group_by(activity_id) %>% 
  mutate(
    start_time = min(time),
    total_dur = as.numeric(difftime(max(time), min(time), units = "secs")),
    progress = ifelse(total_dur == 0, 0, 
                      as.numeric(difftime(time, start_time, units = "secs")) / total_dur)
  ) %>% ungroup()

anim_prop <- ggplot(data_prop, aes(x = lon, y = lat, group = activity_id)) +
  geom_sf(data = boundary_shp, fill = NA, color = "black", inherit.aes = FALSE) +
  geom_path(alpha = 0.3, color = "purple", linewidth = 0.5) +
  theme_void() +
  transition_reveal(progress) +
  labs(title = "Progress: {round(frame_along * 100)}%")

local_filename <- "anim_proportional.gif"
full_local_path <- file.path(outpath, local_filename)
 anim_save(local_filename, animate(anim_prop, nframes = 200, fps = 20, renderer = gifski_renderer()), path=outpath)


# --- C. SEQUENTIAL PLAYBACK (Historical Timeline) ---
# Animates runs one by one as they actually happened in history
# We use 'point_stats' here if you want to color by grade, or 'berkeley_data' for simple color
 
 # We need to stack the runs end-to-end so there are no "rest days" in the animation.
 seq_data <- berkeley_data %>%
   arrange(date, time) %>% 
   group_by(activity_id) %>%
   mutate(
     # 1. Calculate how far into THIS specific run we are (in seconds)
     run_duration = as.numeric(difftime(time, min(time), units = "secs"))
   ) %>%
   ungroup() %>%
   
   # 2. Arrange ALL runs chronologically
   arrange(date, time) %>% 
   
   # 3. Create a look-up table for when each run should "start" in our movie
   group_by(activity_id) %>%
   mutate(run_total_time = max(run_duration)) %>% # How long is this run?
   slice(1) %>% # Keep one row per run to calculate offsets
   ungroup() %>%
   mutate(
     # The cumulative sum of all previous runs' durations is our "start offset"
     # We add a small buffer (e.g., 100 seconds) so runs don't visually overlap instantly
     start_offset = lag(cumsum(run_total_time + 100), default = 0)
   ) %>%
   select(activity_id, start_offset) %>%
   
   # 4. Join this offset back to the main data
   right_join(berkeley_data, by = "activity_id") %>%
   
   # 5. Calculate the FINAL compressed time variable
   mutate(
     run_duration = as.numeric(difftime(time, min(time), units = "secs")), # Recalculate duration
     pseudo_time = start_offset + run_duration
   ) 
 
 
 anim_seq <- ggplot(seq_data, aes(x = lon, y = lat, group = activity_id)) +
   geom_sf(data = boundary_shp, fill = NA, color = "gray10", inherit.aes = FALSE) +
   
   # The Path
   # transition_reveal keeps the history automatically!
   geom_path(color = "#fc4c02", size = 0.5, lineend = "round") +
   
   theme_void() +
   
   # Use the fake time we created. 
   # This makes Run 2 start immediately after Run 1 finishes.
   transition_reveal(pseudo_time)
   
   # We can still display the REAL date in the title!
   # view_follow() is optional: it zooms the camera to the current runner (can be dizzying)
   #labs(title = "Date: {frame_along}") # Note: frame_along might show pseudo_time number
 # To show Date, we might need a static title or complex labeling
 
 
 final_anim <- animate(
   anim_seq, 
   nframes = 400, 
   fps = 15, 
   width = 800, height = 800, # Square for maps
   renderer = gifski_renderer()
 )
 

 local_filename <- "anim_sequential.gif"
 full_local_path <- file.path(outpath, local_filename)
 anim_save(local_filename, animation = final_anim, path = outpath)
 
 
 
 
 # ==============================================================================
 # SECTION 7: 3D visualization 
 # ==============================================================================

  # 1. Create a "Hexbin" Density Plot
 # We use hex bins instead of geom_path because they extrude better into 3D spikes
 p_hex <- ggplot(berkeley_data, aes(x = lon, y = lat)) +
   # Using stat_bin_hex to count runs in small hexagonal areas
   geom_hex(bins = 80, aes(fill = ..count..)) + 
   scale_fill_viridis_c(option = "magma", name = "Run Count") +
   theme_void() +
   theme(legend.position = "none") # Remove legend for the 3D render
 
 # 2. Render in 3D
 # This pops up an interactive window you can rotate with your mouse!
 plot_gg(
   p_hex, 
   width = 5, 
   height = 5, 
   multicore = TRUE, 
   scale = 250,        # How tall the spikes are
   zoom = 0.6,         # Camera zoom
   phi = 45,           # Camera angle (0 is overhead, 90 is side view)
   theta = 30,         # Camera rotation
   windowsize = c(1000, 1000)
 )
 
 # 3. Save a Snapshot
 render_snapshot("berkeley_3d_density.png")
 
 
 
 
 
 
 
 
 
 elev_raster <- get_elev_raster(boundary_shp, z = 12, clip = "locations")
 
 # 2. Convert to Matrix for rayshader
 elev_matrix <- raster_to_matrix(elev_raster)
 
 # 3. Create the Base 3D Map
 elev_matrix %>%
   sphere_shade(texture = "desert") %>%
   add_shadow(ray_shade(elev_matrix, zscale = 3), 0.5) %>%
   add_shadow(ambient_shade(elev_matrix), 0) %>%
   plot_3d(elev_matrix, zscale = 10, fov = 0, theta = 45, zoom = 0.75, phi = 45)
 
 # 4. Overlay Your Lines
 # rayshader needs lines as a list of matrices
 # This part requires a loop to add each path, which can be slow for 1000 runs
 # Ideally, render just your "Top 10 Hardest" here to keep it fast.
 render_path(
   extent = extent(elev_raster), 
   lat = data_top_climb$lat, 
   long = data_top_climb$lon, 
   altitude = data_top_climb$altitude + 5, # Float 5m above ground
   zscale = 10, 
   color = "#fc4c02", 
   linewidth = 2
 )
 
 
 
 
 # ==============================================================================
 # Drive Upload
 # ==============================================================================
 upload_to_drive <- function(local_path, drive_folder_name = "Strava_R_outputs") {
   
   require(googledrive)
   
   # 1. Check if the folder exists on Drive; if not, create it
   # drive_ls pattern search is simple but effective
   target_folder <- drive_find(pattern = drive_folder_name, type = "folder", n_max = 1)
   
   if (nrow(target_folder) == 0) {
     message(paste("Creating new folder on Drive:", drive_folder_name))
     target_folder <- drive_mkdir(drive_folder_name)
   }
   

   drive_upload(
     media = local_path, 
     path = target_folder, 
     overwrite = TRUE # This replaces the file if you run the script again
   )
 }
 
 
 drive_target_folder <- "Strava_R_outputs"
 
 # Get a list of all files in your local figures folder
 all_plots <- list.files(outpath, full.names = TRUE)
 
 # Loop through and upload them all
 # This is great because it backs up your static maps AND animations
 for (file in all_plots) {
   upload_to_drive(file, drive_folder_name = drive_target_folder)
 }
 
 
 
 
 