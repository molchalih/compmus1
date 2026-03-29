library(tidyverse)
library(compmus)
library(patchwork)
library(ggplot2)


# tracks = c("all"), # "all" for every track
# --- configuration for per-track features generation ----
config <- list(
  tracks = c("Tha", "Avril 14th"),
  chroma = TRUE,
  timbre = TRUE,
  chroma_ssm = TRUE,
  timbre_ssm = TRUE,
  chordogram = TRUE,
  keygram = TRUE,
  novelty = TRUE,
  tempogram_act = TRUE,
  tempogram_dft = TRUE
)


# --- a caching file to reduce expensive per-track recalculations unless neccessary ----
track_cache_key <- {
  tracks_cfg <- config$tracks
  if (is.null(tracks_cfg) || (length(tracks_cfg) == 1 && tolower(tracks_cfg) == "all")) {
    "all"
  } else {
    paste(sort(tracks_cfg), collapse = "-")
  }
}


enabled_features <- names(config)[
  sapply(config, function(x) isTRUE(x))
]


cache_file <- paste0(
  "tracks_cache_",
  track_cache_key,
  "_",
  paste(enabled_features, collapse = "_"),
  ".rds"
)


# --- logging function for (time) efficiency evaluation ----
log_time <- function(label, expr) {
  start <- Sys.time()
  message("▶ ", label, " ...")
  
  result <- expr   # ← NO eval()
  
  end <- Sys.time()
  message("✔ ", label, " done in ", round(difftime(end, start, units = "secs"), 2), " sec")
  
  result
}


# --- a wrapping function for logging ----
safe_compute <- function(enabled, label, expr) {
  if (!enabled) return(NULL)
  log_time(label, expr)
}


# --- a helper function containing enums for chords evaluation ----
circshift <- function(v, n) {
  if (n == 0) v else c(tail(v, n), head(v, -n))
}

#      C     C#    D     Eb    E     F     F#    G     Ab    A     Bb    B
major_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    0,    0)
minor_chord <-
  c(   1,    0,    0,    1,    0,    0,    0,    1,    0,    0,    0,    0)
seventh_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    1,    0)

major_key <-
  c(6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
minor_key <-
  c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)

chord_templates <-
  tribble(
    ~name, ~template,
    "Gb:7", circshift(seventh_chord, 6),
    "Gb:maj", circshift(major_chord, 6),
    "Bb:min", circshift(minor_chord, 10),
    "Db:maj", circshift(major_chord, 1),
    "F:min", circshift(minor_chord, 5),
    "Ab:7", circshift(seventh_chord, 8),
    "Ab:maj", circshift(major_chord, 8),
    "C:min", circshift(minor_chord, 0),
    "Eb:7", circshift(seventh_chord, 3),
    "Eb:maj", circshift(major_chord, 3),
    "G:min", circshift(minor_chord, 7),
    "Bb:7", circshift(seventh_chord, 10),
    "Bb:maj", circshift(major_chord, 10),
    "D:min", circshift(minor_chord, 2),
    "F:7", circshift(seventh_chord, 5),
    "F:maj", circshift(major_chord, 5),
    "A:min", circshift(minor_chord, 9),
    "C:7", circshift(seventh_chord, 0),
    "C:maj", circshift(major_chord, 0),
    "E:min", circshift(minor_chord, 4),
    "G:7", circshift(seventh_chord, 7),
    "G:maj", circshift(major_chord, 7),
    "B:min", circshift(minor_chord, 11),
    "D:7", circshift(seventh_chord, 2),
    "D:maj", circshift(major_chord, 2),
    "F#:min", circshift(minor_chord, 6),
    "A:7", circshift(seventh_chord, 9),
    "A:maj", circshift(major_chord, 9),
    "C#:min", circshift(minor_chord, 1),
    "E:7", circshift(seventh_chord, 4),
    "E:maj", circshift(major_chord, 4),
    "G#:min", circshift(minor_chord, 8),
    "B:7", circshift(seventh_chord, 11),
    "B:maj", circshift(major_chord, 11),
    "D#:min", circshift(minor_chord, 3)
  )

key_templates <-
  tribble(
    ~name, ~template,
    "Gb:maj", circshift(major_key, 6),
    "Bb:min", circshift(minor_key, 10),
    "Db:maj", circshift(major_key, 1),
    "F:min", circshift(minor_key, 5),
    "Ab:maj", circshift(major_key, 8),
    "C:min", circshift(minor_key, 0),
    "Eb:maj", circshift(major_key, 3),
    "G:min", circshift(minor_key, 7),
    "Bb:maj", circshift(major_key, 10),
    "D:min", circshift(minor_key, 2),
    "F:maj", circshift(major_key, 5),
    "A:min", circshift(minor_key, 9),
    "C:maj", circshift(major_key, 0),
    "E:min", circshift(minor_key, 4),
    "G:maj", circshift(major_key, 7),
    "B:min", circshift(minor_key, 11),
    "D:maj", circshift(major_key, 2),
    "F#:min", circshift(minor_key, 6),
    "A:maj", circshift(major_key, 9),
    "C#:min", circshift(minor_key, 1),
    "E:maj", circshift(major_key, 4),
    "G#:min", circshift(minor_key, 8),
    "B:maj", circshift(major_key, 11),
    "D#:min", circshift(minor_key, 3)
  )


# --- per-track analysis ----
if (file.exists(cache_file)) {
  
  message("Loading cached tracks...")
  tracks <- readRDS(cache_file)
  
} else {
  
  message("Computing tracks from scratch...")
  
  selected_tracks <- config$tracks
  
  # --- LOAD CLEAN METADATA FROM MP3 FILES ---
  clean_string <- function(x) {
    x |>
      str_to_lower() |>
      str_replace_all("[^a-z0-9]", "")
  }
  
  songs <- tibble(
    file = list.files("songs/downloaded", "*.mp3", full.names = TRUE)
  ) |>
    mutate(
      name = basename(file) |> tools::file_path_sans_ext(),
      artist = str_split(name, " - ") |> map_chr(1),
      track  = str_split(name, " - ") |> map_chr(2),
      track_key = clean_string(track)
    )
  
  # --- LOAD CSV TRACK FILES ---
  tracks <- tibble(
    chroma_file = list.files("datasets/final/chroma", "*.csv", full.names = TRUE)
  ) |>
    mutate(
      name = basename(chroma_file),
      name_key = clean_string(tools::file_path_sans_ext(name))
    ) |>
    
    # --- MATCH CSVs TO MP3 METADATA ---
    rowwise() |>
    mutate(
      match = list(
        songs |>
          filter(str_detect(name_key, paste0(track_key, "$"))) |>
          slice(1)
      ),
      
      artist = if (nrow(match) == 0) NA else match$artist,
      track  = if (nrow(match) == 0) NA else match$track
    ) |>
    ungroup()

# tracks |>
#   distinct(track) |>
#   print(n = Inf)

if (!is.null(selected_tracks) && !(length(selected_tracks) == 1 && tolower(selected_tracks) == "all")) {
  tracks <- tracks |>
    filter(track %in% selected_tracks)
}
  
if (!is.null(selected_tracks) && !(length(selected_tracks) == 1 && tolower(selected_tracks) == "all")) {
  tracks <- tracks |>
    filter(track %in% selected_tracks)
}

tracks <- tracks |>
    
  mutate(
    timbre_file = file.path("datasets/final/timbre", name),
    
    chroma = map(chroma_file, read_csv),
    timbre = map(timbre_file, read_csv),
  ) |>
  
  # --- WRANGLE FOR LATER REUSABILITY ---
  mutate(
    chroma_wrangled = map(chroma, \(df)
                          compmus_wrangle_chroma(df)
    ),
    
    timbre_wrangled = map(timbre, \(df)
                          compmus_wrangle_timbre(df)
    )
  ) |>
  
  mutate(
    # --- CHROMA PROCESSING ---
    chroma_proc = map2(chroma_wrangled, track, \(df, name)
                       safe_compute(config$chroma, paste("Chroma proc:", name), {
                         df |>
                           mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
                           compmus_gather_chroma()
                       })
    ),
    
    # --- TIMBRE PROCESSING ---
    timbre_proc = map2(timbre_wrangled, track, \(df, name)
                       safe_compute(config$timbre, paste("Timbre proc:", name), {
                         df |>
                           mutate(timbre = map(timbre, compmus_normalise, "manhattan")) |>
                           compmus_gather_timbre()
                       })
    ),
    
    # --- TIMBRE SSM ---
    timbre_ssm = map2(timbre_wrangled, track, \(df, name)
                      safe_compute(config$timbre_ssm, paste("Timbre SSM:", name), {
                        df |>
                          slice(seq(1, n(), by = 50)) |>
                          mutate(timbre = map(timbre, compmus_normalise, "euclidean")) |>
                          compmus_self_similarity(timbre, "cosine")
                      })
    ),
    
    # --- CHROMA SSM ---
    chroma_ssm = map2(chroma_wrangled, track, \(df, name)
                      safe_compute(config$chroma_ssm, paste("Chroma SSM:", name), {
                        df |>
                          slice(seq(1, n(), by = 50)) |>   # optional optimization
                          mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
                          compmus_self_similarity(pitches, "cosine")
                      })
    ),
    
    # --- CHORDOGRAMS ---
    chordogram = map2(chroma_wrangled, track, \(df, name)
                      safe_compute(config$chordogram, paste("Chordogram:", name), {
                        df |>
                          slice(seq(1, n(), by = 20)) |>
                          compmus_match_pitch_template(
                            chord_templates,
                            method = "cosine",
                            norm = "euclidean"
                          )
                      })
    ),
    
    # --- KEYGRAMS ---
    keygram = map2(chroma_wrangled, track, \(df, name)
                   safe_compute(config$keygram, paste("Keygram:", name), {
                     df |>
                       slice(seq(1, n(), by = 50)) |>
                       compmus_match_pitch_template(
                         key_templates,
                         method = "cosine",
                         norm = "manhattan"
                       )
                   })
    ),
    
    # --- NOVELTY FUNCTION ---
    novelty = pmap(list(artist, track, name), \(artist, track, name)
                   safe_compute(config$novelty, paste("Novelty:", track), {
                     
                     read_csv(
                       paste0("datasets/final/tempo_novelty/", name),
                       show_col_types = FALSE
                     ) |>
                       select(TIME, VALUE) |>
                       rename(
                         start = TIME,
                         novelty = VALUE
                       ) |>
                       filter(start <= 30) |>
                       mutate(
                         duration = lead(start, default = last(start)) - start
                       )
                   })
    ),
    
    # --- ACT FUNCTION ---
    tempogram_act = pmap(list(artist, track, name), \(artist, track, name)
                         safe_compute(config$tempogram_act, paste("ACT:", track), {
                           
                           read_csv(
                             paste0("datasets/final/tempo_act/", name),
                             show_col_types = FALSE
                           ) |>
                             filter(TIME <= 30)
                         })
    ),
    
    # --- DFT FUNCTION ---
    tempogram_dft = pmap(list(artist, track, name), \(artist, track, name)
                         safe_compute(config$tempogram_dft, paste("DFT:", track), {
                           
                           read_csv(
                             paste0("datasets/final/tempo_dft/", name),
                             show_col_types = FALSE
                           ) |>
                             filter(TIME <= 30)
                         })
    )
    
  )

  # --- LABELS FOR UI ---
  tracks <- tracks |>
    mutate(
      artist_clean = artist,
      track_clean  = track,
      label = paste(artist_clean, "–", track_clean)
    )
  
  # --- saving the cache files with the analysis ----
  saveRDS(tracks, cache_file)
      
}

tracks
saveRDS(tracks, "data/tracks.rds")

# --- view generated results  ----
# chroma_plot <- tracks |>
#   filter(track == "xtal") |>
#   pull(chroma_proc) |>
#   purrr::pluck(1) |>
#   ggplot(
#     aes(
#       x = start + duration / 2,
#       width = duration,
#       y = pitch_class,
#       fill = value
#     )
#   ) +
#   geom_tile() +
#   labs(subtitle = "chromagram", x = "Time (s)", y = NULL) +
#   theme_minimal() +
#   scale_fill_viridis_c()
# 
# 
# timbre_plot <- tracks |>
#   filter(track == "xtal") |>
#   pull(timbre_proc) |>
#   purrr::pluck(1) |>
#   ggplot(
#     aes(
#       x = start + duration / 2,
#       width = duration,
#       y = mfcc,
#       fill = value
#     )
#   ) +
#   geom_tile() +
#   labs(subtitle = "cepstrogram", x = "Time (s)", y = NULL, fill = "Magnitude") +
#   theme_classic() +
#   scale_fill_viridis_c()
# 
# timbre_ssm_plot <- tracks |>
#   filter(track == "xtal") |>
#   pull(timbre_ssm) |>
#   purrr::pluck(1) |>
#   ggplot(
#     aes(
#       x = xstart + xduration / 2,
#       width = 50 * xduration,
#       y = ystart + yduration / 2,
#       height = 50 * yduration,
#       fill = d
#     )
#   ) +
#   geom_tile() +
#   coord_fixed() +
#   scale_fill_viridis_c(guide = "none") +
#   theme_classic() +
#   labs(subtitle = "timbre self-similarity", x = "", y = "")
# 
# chroma_ssm_plot <- tracks |>
#   filter(track == "xtal") |>
#   pull(chroma_ssm) |>
#   purrr::pluck(1) |>
#   ggplot(
#     aes(
#       x = xstart + xduration / 2,
#       width = 50 * xduration,
#       y = ystart + yduration / 2,
#       height = 50 * yduration,
#       fill = d
#     )
#   ) +
#   geom_tile() +
#   coord_fixed() +
#   scale_fill_viridis_c() +
#   theme_classic() +
#   labs(subtitle = "chroma self-similarity", x = "", y = "")
# 
# chord_plot <- tracks |>
#   filter(track == "xtal") |>
#   pull(chordogram) |>
#   purrr::pluck(1) |>
#   ggplot(
#     aes(
#       x = start + duration / 2,
#       width = 20 * duration,
#       y = name,
#       fill = d
#     )
#   ) +
#   geom_tile() +
#   scale_fill_viridis_c(guide = "none") +
#   theme_minimal() +
#   labs(subtitle = "chordogram", x = "Time (s)", y = "")
# 
# key_plot <- tracks |>
#   filter(track == "xtal") |>
#   pull(keygram) |>
#   purrr::pluck(1) |>
#   ggplot(
#     aes(
#       x = start + duration / 2,
#       width = 50 * duration,
#       y = name,
#       fill = d
#     )
#   ) +
#   geom_tile() +
#   scale_fill_viridis_c(guide = "none") +
#   theme_minimal() +
#   labs(subtitle = "keygram", x = "Time (s)", y = "")
# 
# novelty_plot <- if (config$novelty) {
#   tracks |>
#     filter(track == config$tracks[1])|>
#     pull(novelty) |>
#     purrr::pluck(1) |>
#     ggplot(aes(x = start, y = novelty)) +
#     geom_line() +
#     theme_minimal() +
#     labs(
#       subtitle = "novelty function",
#       x = "Time (s)",
#       y = "Novelty"
#     )
# } else NULL
# 
# act_plot <- tracks |>
#   pull(tempogram_act) |>
#   purrr::pluck(1) |>
#   
#   pivot_longer(-TIME, names_to = "tempo", values_to = "value") |>
#   mutate(tempo = as.numeric(tempo)) |>
#   
#   ggplot(aes(x = TIME, y = tempo, fill = value)) +
#   geom_raster() +
#   
#   scale_y_continuous(
#     transform = c("reciprocal", "reverse"),
#     breaks = seq(50, 350, 100)
#   ) +
#   
#   scale_fill_viridis_c(guide = "none") +
#   labs(
#     subtitle = "autocorrelation tempogram",
#     x = "Time (s)",
#     y = "Tempo (BPM)"
#   ) +
#   theme_classic()
# 
# dft_plot <- tracks |>
#   pull(tempogram_dft) |>
#   purrr::pluck(1) |>
#   
#   pivot_longer(-TIME, names_to = "tempo", values_to = "value") |>
#   mutate(tempo = as.numeric(tempo)) |>
#   
#   ggplot(aes(x = TIME, y = tempo, fill = value)) +
#   geom_raster() +
#   
#   scale_fill_viridis_c(guide = "none") +
#   labs(
#     subtitle = "fourier tempogram",
#     x = "Time (s)",
#     y = "Tempo (BPM)"
#   ) +
#   theme_classic()
# 
# 
# plots <- list(
#   chroma_plot + timbre_plot,
#   
#   if (config$novelty)
#     novelty_plot,
#   
#   if (config$tempogram_act)
#     act_plot,
#   
#   if (config$tempogram_dft)
#     dft_plot
# ) |> purrr::compact()