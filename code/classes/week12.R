#install.packages('remotes')
#remotes::install_github('jaburgoyne/compmus')

library(tidyverse)
library(tidymodels)
library(ggdendro)
library(heatmaply)
library(compmus)
# library(cluster)
# library(protoclust)
library(dplyr)


# CLASSROOM TRACKS:

# IMPORT: NRC playlist (class)
nrc <- read_csv("datasets/week12/nrc.csv")
pop <- read_csv("datasets/week12/pop.csv")


# HOMEWORK TRACKS:

# IMPORT: warp playlist (class)
warp <- read_csv("datasets/final/warp_playlist.csv")

library(compmus)

get_conf_mat <- function(fit) {
  outcome <- .get_tune_outcome_names(fit)
  fit |>
    collect_predictions() |>
    conf_mat(truth = outcome, estimate = .pred_class)
}

get_pr <- function(fit) {
  fit |>
    conf_mat_resampled() |>
    group_by(Prediction) |> mutate(precision = Freq / sum(Freq)) |>
    group_by(Truth) |> mutate(recall = Freq / sum(Freq)) |>
    ungroup() |> filter(Prediction == Truth) |>
    select(class = Prediction, precision, recall)
}

nrc_juice <-
  recipe(
    `Track Name` ~
      Danceability +
      Energy +
      Loudness +
      Speechiness +
      Acousticness +
      Instrumentalness +
      Liveness +
      Valence +
      Tempo +
      `Duration (ms)`,
    data = warp
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors()) |>
  # step_range(all_predictors()) |>
  prep(warp |> mutate(`Track Name` = str_trunc(`Track Name`, 36))) |>
  juice() |>
  column_to_rownames("Track Name")

nrc_dist <- dist(nrc_juice, method = "euclidean")

nrc_dist |>
  hclust(method = "single") |> # Try single, average, and complete.
  dendro_data() |>
  ggdendrogram()

heatmaply(
  nrc_juice,
  hclustfun = hclust,
  hclust_method = "average",  # Change for single, average, or complete linkage.
  dist_method = "euclidean",
)