library(targets)
library(tarchetypes)

targets::tar_manifest()

targets::tar_visnetwork(targets_only = TRUE,
                        degree_from = 5L,
                        degree_to = 5L)

targets::tar_make()

