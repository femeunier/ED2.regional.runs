rm(list = ls())

# Libraries
library(dplyr)
library(tidyr)
library(ED2scenarios)
library(ED2.regional.runs)
library(purrr)

# Directories

ref_dir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/CB/run/grid"

rundir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/CB/run/grid"
outdir <- "/kyukon/scratch/gent/gvo000/gvo00074/felicien/CB/out/"

lats <- seq(2.5,3.5,1)
lons <- seq(20,21,1)
coords <- expand.grid(lon = lons,
                      lat = lats)

for (icoord in seq(1,nrow(coords))){
  clat <- coords$lat[icoord]
  clon <- coords$lon[icoord]

  # Directories
  run_name <- paste0("CB_X_",abs(clon),ifelse(sign(clon) > 0,"E","W"),
                     "_Y_",abs(clat),ifelse(sign(clat) > 0,"N","S"))

  run_ref <- file.path(rundir,run_name)
  out_ref <- file.path(outdir,run_name)

  dir.create(run_ref,showWarnings = FALSE)
  dir.create(out_ref,showWarnings = FALSE)
  dir.create(file.path(out_ref,"analy"),showWarnings = FALSE)
  dir.create(file.path(out_ref,"histo"),showWarnings = FALSE)

  cleanup.ED2.directory(Dir.analy = file.path(out_ref,"analy"),
                        Dir.hist = file.path(out_ref,"histo"),
                        prefix.analy = "analysis",
                        prefix.histo = "history",
                        years2keep.full.analy = c(2014:2024),
                        years2keep.month.histo = c(1400:2024),

                        month2keep = 1)

}

# scp /Users/felicien/Documents/projects/ED2.regional.runs/scripts/cleanup.all.directories.R hpc:/data/gent/vo/000/gvo00074/felicien/R

# Initial directories
# inputs/climate
# 629M	./ED2
# 713M	.
# outputs
# 9.0G	./histo
# 68G	./analy
# 77G	.


