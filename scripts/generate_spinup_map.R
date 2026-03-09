rm(list = ls())

# Libraries
library(dplyr)
library(tidyr)
library(ED2scenarios)
library(PEcAn.ED2)
library(purrr)

# Directories

ref_dir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/CB/run/grid"
ed2in <- read_ed2in(
  file.path(ref_dir,
            "ED2IN_spinup"))

# No -T- Files
ed2in$ITOUTPUT = 0
ed2in$IYEARZ   = 1400
ed2in$IYEARZ   = 1700

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

  # ED2IN
  ed2in_scenar <- ed2in
  ed2in_scenar$FFILOUT = file.path(out_ref,"analy","analysis")
  ed2in_scenar$SFILOUT = file.path(out_ref,"histo","history")

  # Met driver
  ed2in_scenar$ED_MET_DRIVER_DB =
    paste0('/data/gent/vo/000/gvo00074/ED_common_data/met/CB/ERA5_ED2/ERA5_lat_',
           clat,'_lon_',clon,'/ERA5_lat_',clat,'_lon_',clon,'_1/ED2/ED_MET_DRIVER_HEADER')

  write_ed2in(ed2in_scenar,
              filename = file.path(run_ref,"ED2IN_spinup"))

  # job.sh

  write_job_noR(file = file.path(run_ref,
                                 "job_CB_spinup.sh"),
                    nodes = 1,ppn = 18,mem = 16,walltime = 72,
                    prerun = "ml purge ; ml HDF5/1.14.3-iimpi-2023b ; ml imkl-FFTW/2023.2.0-iimpi-2023b; ulimit -s unlimited",
                    CD = run_ref,
                    ed_exec = "/kyukon/scratch/gent/vo/000/gvo00074/felicien/ED2.2/ED2/ED/build/ed_2.2-opt-master-bae4504d",
                    ED2IN = "ED2IN_spinup")

  list_dir[[run_name]] = run_ref

}

dumb <- write_bash_submission(file = file.path(rundir,"all_jobs_spinup.sh"),
                              list_files = list_dir,
                              job_name = "job_CB_spinup.sh")


# scp /Users/felicien/Documents/projects/ED2.regional.runs/scripts/generate_spinup_map.R hpc:/data/gent/vo/000/gvo00074/felicien/R


