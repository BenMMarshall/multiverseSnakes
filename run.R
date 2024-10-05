#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.

# targets::tar_make(names = matches("Brms$"))
# targets::tar_make_clustermq(names = contains("modelsBrms"), workers = 4) # nolint
# targets::tar_make_clustermq(workers = 2) # nolint