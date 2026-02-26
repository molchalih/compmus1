#install.packages('remotes')
#remotes::install_github('jaburgoyne/compmus')

library(tidyverse)
library(compmus)
library(dplyr)

# IMPORT: aphex twin avril 14th
aphex_pitches <- read_csv("datasets/avril-14th-pitches.csv")
aphex_timbre <- read_csv("datasets/avril-14th-timbre.csv")

# IMPORT: Bloed, Zweet en Tranen
bzt_pitches <- read_csv("datasets/bzt-pitches.csv")
bzt_timbre <- read_csv("datasets/bzt-timbre.csv")

# IMPORT: radiohead - knives out
radiohead_pitches <- read_csv("datasets/radiohead_pitches.csv")
radiohead_timbre <- read_csv("datasets/radiohead_timbre.csv")



# P1: TIMBRE PLOT
radiohead_timbre |>
  compmus_wrangle_timbre() |>
  mutate(timbre = map(timbre, compmus_normalise, "manhattan")) |>
  compmus_gather_timbre() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = mfcc,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude", subtitle = "cepstogram") +
  scale_fill_viridis_c() +
  theme_classic()



# P2: CHROMA PLOT
radiohead_pitches |>
  compmus_wrangle_chroma() |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude", subtitle = "chromagram") +
  theme_minimal() +
  scale_fill_viridis_c()



# P3:SELF-SIMILARITY (TIMBRE)

radiohead_timbre |>
  compmus_wrangle_timbre() |>
  filter(row_number() %% 50L == 0L) |>
  mutate(timbre = map(timbre, compmus_normalise, "euclidean")) |>
  compmus_self_similarity(timbre, "cosine") |>
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = 50 * xduration,
      y = ystart + yduration / 2,
      height = 50 * yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c() +
  theme_classic() +
  labs(x = "Radiohead - Knives Out", 
       y = "Radiohead - Knives Out", 
       subtitle = "timbre")



# P4: SELF-SIMILARITY (CHROMA)

radiohead_pitches |>
  compmus_wrangle_chroma() |>
  filter(row_number() %% 50L == 0L) |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_self_similarity(pitches, "cosine") |>
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = 50 * xduration,
      y = ystart + yduration / 2,
      height = 50 * yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c() +
  theme_classic() +
  labs(x = "Radiohead - Knives Out", 
       y = "Radiohead - Knives Out", 
       subtitle = "chroma")
