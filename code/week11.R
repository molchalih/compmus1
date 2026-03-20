library(tidyverse)
library(compmus)
library(dplyr)

# IMPORTS (CLASSROOM):

pata_pata_novelty <- read_csv("datasets/week11/pata-pata-novelty.csv")
graveola_act <- read_csv("datasets/week11/graveola-act.csv")
graveola_dft <- read_csv("datasets/week11/graveola-dft.csv")

# IMPORTS (HOMEWORK):

square_novelty <- read_csv("datasets/week11/square-novelty.csv")
square_act <- read_csv("datasets/week11/square-act.csv")
square_dft <- read_csv("datasets/week11/square-dft.csv")

# CLASSROOM TRACKS:

# square_novelty |>
#   ggplot(aes(x = TIME, y = VALUE)) +
#   geom_line() +
#   xlim(0, 30) +                         # Adjust the limits to the desired time range
#   theme_minimal() +
#   labs(x = "Time (s)", y = "Novelty")

# graveola_act |> 
#   pivot_longer(-TIME, names_to = "tempo") |> 
#   mutate(tempo = as.numeric(tempo)) |> 
#   ggplot(aes(x = TIME, y = tempo, fill = value)) +
#   geom_raster() +
#   scale_fill_viridis_c(guide = "none") +
#   labs(x = "Time (s)", y = "Tempo (BPM)") +
#   theme_classic()

# graveola_dft |> 
#   pivot_longer(-TIME, names_to = "tempo") |> 
#   mutate(tempo = as.numeric(tempo)) |> 
#   ggplot(aes(x = TIME, y = tempo, fill = value)) +
#   geom_raster() +
#   scale_fill_viridis_c(guide = "none") +
#   labs(x = "Time (s)", y = "Tempo (BPM)") +
#   theme_classic()

# HOMEWORK TRACKS:

square_novelty |>
  ggplot(aes(x = TIME, y = VALUE)) +
  geom_line() +
  xlim(0, 30) +                         # Adjust the limits to the desired time range
  theme_minimal() +
  labs(x = "Time (s)", y = "Novelty")

square_act |>
  pivot_longer(-TIME, names_to = "tempo") |>
  mutate(tempo = as.numeric(tempo)) |>
  ggplot(aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()

square_dft |>
  pivot_longer(-TIME, names_to = "tempo") |>
  mutate(tempo = as.numeric(tempo)) |>
  ggplot(aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()