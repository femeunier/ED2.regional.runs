rm(list = ls())

# ============================================================
# Merge monthly ERA5 "archives" (named .nc but actually ZIPs)
# Each archive contains TWO NetCDFs:
#   - instant variables (e.g., t2m, sp, d2m, u10, v10)
#   - accumulated variables (e.g., tp, ssrd, strd)
#
# Goal: For each year (1940..2024), produce ONE yearly NetCDF:
#   ERA5_CB_YYYY.nc
# containing BOTH instant + accumulated variables on the same time axis.
#
# Tools required in PATH: unzip, ncrcat, ncks, ncrename (NCO)
# ============================================================

years  <- 1940:2024
# years <- 1940  # for testing

in_dir  <- "/data/gent/vo/000/gvo00074/ED_common_data/met/CB/ERA5/test/"  # where monthly archives live
out_dir <- "/data/gent/vo/000/gvo00074/ED_common_data/met/CB/ERA5/"       # where yearly files are written

# ---- NCO tools
ncrcat    <- Sys.which("ncrcat")
ncks      <- Sys.which("ncks")
ncrename  <- Sys.which("ncrename")
unzip_cmd <- Sys.which("unzip")

if (ncrcat == "" || ncks == "" || unzip_cmd == "") stop("Need NCO (ncrcat,ncks) and unzip in PATH.")
if (ncrename == "") stop("ncrename not found in PATH (NCO incomplete).")

# ---- Make sure no global NCO overwrite/append flags leak in from env/modules
Sys.unsetenv(c("NCO_FORCE_OVERWRITE", "NCO_OVERWRITE", "NCO_FORCE_APPEND"))

# ---- System runner (prints tool output so NCO errors are visible)
run_cmd <- function(cmd, args) {
  out <- system2(cmd, args = args, stdout = TRUE, stderr = TRUE)
  if (length(out)) message(paste(out, collapse = "\n"))
  out
}

# ---- Helpers to detect whether a name exists in the file metadata
has_name <- function(nc_file, name) {
  meta <- run_cmd(ncks, c("-m", nc_file))
  any(grepl(paste0("\\b", name, "\\b"), meta))
}

# ---- Harmonize latitude/longitude naming between acc and inst
# (only runs if mismatch is detected)
harmonize_latlon <- function(acc_file, inst_file) {

  inst_has_latitude  <- has_name(inst_file, "latitude")
  inst_has_longitude <- has_name(inst_file, "longitude")
  inst_has_lat <- has_name(inst_file, "lat")
  inst_has_lon <- has_name(inst_file, "lon")

  acc_has_latitude  <- has_name(acc_file, "latitude")
  acc_has_longitude <- has_name(acc_file, "longitude")
  acc_has_lat <- has_name(acc_file, "lat")
  acc_has_lon <- has_name(acc_file, "lon")

  # inst: latitude/longitude ; acc: lat/lon  -> rename acc
  if (inst_has_latitude && inst_has_longitude &&
      acc_has_lat && acc_has_lon &&
      !(acc_has_latitude || acc_has_longitude)) {

    run_cmd(ncrename, c("-O",
                        "-d", "lat,latitude", "-d", "lon,longitude",
                        "-v", "lat,latitude", "-v", "lon,longitude",
                        acc_file))
  }

  # inst: lat/lon ; acc: latitude/longitude -> rename acc
  if (inst_has_lat && inst_has_lon &&
      acc_has_latitude && acc_has_longitude &&
      !(acc_has_lat || acc_has_lon)) {

    run_cmd(ncrename, c("-O",
                        "-d", "latitude,lat", "-d", "longitude,lon",
                        "-v", "latitude,lat", "-v", "longitude,lon",
                        acc_file))
  }

  invisible(TRUE)
}

# ---- Ensure record dimension exists for concatenation (ncrcat needs a record dim)
# Your case: valid_time is the correct leading dimension for data variables.
# We convert it to an unlimited (record) dimension in-place.
make_rec_dim <- function(nc_file, dim_name = "valid_time") {
  run_cmd(ncks, c("--mk_rec_dmn", dim_name, "-O", nc_file, nc_file))
}

# ---- Variables to append from accumulated file into instant file
acc_vars <- c("tp", "ssrd", "strd")

for (yy in years) {

  out_file <- file.path(out_dir, sprintf("ERA5_CB_%d.nc", yy))
  if (file.exists(out_file)) {
    message(sprintf("[skip] %d exists -> %s", yy, out_file))
    next
  }

  tmp_year <- file.path(tempdir(), sprintf("era5_cb_%d_%s", yy, format(Sys.time(), "%Y%m%d%H%M%S")))
  dir.create(tmp_year, recursive = TRUE, showWarnings = FALSE)

  inst_month <- character(0)
  acc_month  <- character(0)

  # ------------------------------------------------------------
  # 1) Unzip each monthly archive into its own folder and
  #    capture the two extracted NetCDFs (instant + accum).
  # ------------------------------------------------------------
  for (mm in 1:12) {

    zip_file <- file.path(in_dir, sprintf("ERA5_CB_%d_%02d.nc", yy, mm))  # archive is misnamed .nc
    if (!file.exists(zip_file)) {
      message(sprintf("[warn] %d-%02d missing archive: %s", yy, mm, zip_file))
      inst_month <- character(0); acc_month <- character(0)
      break
    }

    tmp_m <- file.path(tmp_year, sprintf("%d_%02d", yy, mm))
    dir.create(tmp_m, recursive = TRUE, showWarnings = FALSE)

    # unzip to month-specific folder (avoids overwriting generic filenames)
    run_cmd(unzip_cmd, c("-o", "-qq", zip_file, "-d", tmp_m))

    nc_files <- list.files(tmp_m, pattern = "\\.nc$", full.names = TRUE)

    # Identify inst vs accum by filename keywords from CDS/ERA5 exports
    inst_f <- nc_files[grepl("instant", basename(nc_files), ignore.case = TRUE)]
    acc_f  <- nc_files[grepl("accum",   basename(nc_files), ignore.case = TRUE)]

    if (length(inst_f) != 1 || length(acc_f) != 1) {
      message(sprintf("[err ] %d-%02d cannot uniquely identify instant/accum among: %s",
                      yy, mm, paste(basename(nc_files), collapse = ", ")))
      inst_month <- character(0); acc_month <- character(0)
      break
    }

    # Rename to stable month-specific filenames
    inst_out <- file.path(tmp_m, sprintf("ERA5_CB_%d_%02d_inst.nc", yy, mm))
    acc_out  <- file.path(tmp_m, sprintf("ERA5_CB_%d_%02d_acc.nc",  yy, mm))

    file.rename(inst_f, inst_out)
    file.rename(acc_f,  acc_out)

    # Ensure concatenation dimension is record/unlimited (valid_time in your case)
    make_rec_dim(inst_out, "valid_time")
    make_rec_dim(acc_out,  "valid_time")

    inst_month <- c(inst_month, inst_out)
    acc_month  <- c(acc_month,  acc_out)
  }

  if (length(inst_month) != 12 || length(acc_month) != 12) {
    message(sprintf("[skip] %d incomplete year (need 12 inst + 12 acc).", yy))
    unlink(tmp_year, recursive = TRUE, force = TRUE)
    next
  }

  # ------------------------------------------------------------
  # 2) Concatenate months into yearly inst and yearly acc
  # ------------------------------------------------------------
  inst_year <- file.path(tmp_year, sprintf("ERA5_CB_%d_inst_year.nc", yy))
  acc_year  <- file.path(tmp_year, sprintf("ERA5_CB_%d_acc_year.nc",  yy))

  message(sprintf("[run ] %d: ncrcat inst (dim0=valid_time)", yy))
  log1 <- run_cmd(ncrcat, c("-O", "-4", "-L", "4", inst_month, inst_year))
  if (!file.exists(inst_year) || any(grepl("ERROR", log1, ignore.case = TRUE))) {
    message(sprintf("[err ] %d: inst-year failed", yy))
    unlink(tmp_year, recursive = TRUE, force = TRUE)
    next
  }

  message(sprintf("[run ] %d: ncrcat acc  (dim0=valid_time)", yy))
  log2 <- run_cmd(ncrcat, c("-O", "-4", "-L", "4", acc_month, acc_year))
  if (!file.exists(acc_year) || any(grepl("ERROR", log2, ignore.case = TRUE))) {
    message(sprintf("[err ] %d: acc-year failed", yy))
    unlink(tmp_year, recursive = TRUE, force = TRUE)
    next
  }

  # ------------------------------------------------------------
  # 3) Harmonize coords if needed, then append accumulated vars
  #    into the inst-year file (single yearly output)
  #
  # IMPORTANT:
  # - Do NOT pass -O with -A (mutually exclusive)
  # - Some environments set global FORCE_OVERWRITE; we unset env vars
  #   and use --no_cll_msr to avoid reading user/default NCO rc.
  # ------------------------------------------------------------
  message(sprintf("[run ] %d: ncks append acc vars -> inst", yy))

  # re-unset in case modules/scripts set it mid-session
  Sys.unsetenv(c("NCO_FORCE_OVERWRITE", "NCO_OVERWRITE", "NCO_FORCE_APPEND"))

  harmonize_latlon(acc_year, inst_year)

  log3 <- run_cmd(ncks, c("--no_cll_msr",
                          "-A",
                          "-v", paste(acc_vars, collapse = ","),
                          acc_year, inst_year))

  if (any(grepl("ERROR", log3, ignore.case = TRUE))) {
    message(sprintf("[err ] %d: append failed; not writing final yearly file.", yy))
    unlink(tmp_year, recursive = TRUE, force = TRUE)
    next
  }

  # ------------------------------------------------------------
  # 4) Write final yearly file
  # ------------------------------------------------------------
  ok <- file.copy(inst_year, out_file, overwrite = TRUE)
  if (!ok) {
    message(sprintf("[err ] %d: failed to copy output to %s", yy, out_file))
    unlink(tmp_year, recursive = TRUE, force = TRUE)
    next
  }

  message(sprintf("[done] %d -> %s", yy, out_file))
  unlink(tmp_year, recursive = TRUE, force = TRUE)
}

# scp /Users/felicien/Documents/projects/ED2.regional.runs/scripts/merge.monthly.files.ERA5.R hpc:/data/gent/vo/000/gvo00074/felicien/R
