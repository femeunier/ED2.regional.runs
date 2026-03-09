rm(list = ls())

# Libraries
library(dplyr)
library(tidyr)
library(ED2scenarios)
library(PEcAn.ED2)
library(purrr)

# Directories

ref_dir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/CB/run/grid"

rundir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/CB/run/grid"
outdir <- "/kyukon/scratch/gent/gvo000/gvo00074/felicien/CB/out/"

lats <- seq(2.5,3.5,1)
lons <- seq(20,21,1)
coords <- expand.grid(lon = lons,
                      lat = lats)

list_dir <- list()

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


  # job.sh

  ED2scenarios::write_joblauncher_Ronly(file = file.path(run_ref,
                                                         "jobR.sh"),
                                        nodes = 1, ppn = 1, mem=50, walltime = 12,
                                        prerun = "ml purge ; ml R-bundle-Bioconductor/3.20-foss-2024a-R-4.4.2",
                                        CD = run_ref,
                                        date.init = "2014/01/01", date.end = "2024/01/01",
                                        ED2IN = "ED2IN_historical",
                                        Rplot_function = "/data/gent/vo/000/gvo00074/felicien/R/read_and_plot_ED2.2_all_tspft.r",
                                        firstjob = TRUE)


  list_dir[[run_name]] = run_ref

}

dumb <- write_bash_submission(file = file.path(rundir,"all_jobs_R.sh"),
                              list_files = list_dir,
                              job_name = "jobR.sh")


# scp /Users/felicien/Documents/projects/ED2.regional.runs/scripts/generate_all_datum.R hpc:/data/gent/vo/000/gvo00074/felicien/R


