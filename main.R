library(tidyverse)
library(compmus)
library(dplyr)



# # IMPORT: aphex twin avril 14th
# aphtex_pitches <- read_csv("datasets/avril-14th-pitches.csv")
# aphtex_timbre <- read_csv("datasets/avril-14th-timbre.csv")
# 
# # IMPORT: Bloed, Zweet en Tranen
# bzt_pitches <- read_csv("datasets/bzt-pitches.csv")
# bzt_timbre <- read_csv("datasets/bzt-timbre.csv")



# TIMBRE PLOT
# aphtex_timbre |>
#   compmus_wrangle_timbre() |> 
#   mutate(timbre = map(timbre, compmus_normalise, "manhattan")) |>
#   compmus_gather_timbre() |>
#   ggplot(
#     aes(
#       x = start + duration / 2,
#       width = duration,
#       y = mfcc,
#       fill = value
#     )
#   ) +
#   geom_tile() +
#   labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
#   scale_fill_viridis_c() +                              
#   theme_classic()



# CHROMA PLOT
bzt_pitches |>
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
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()