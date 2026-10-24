source("R/00_config.R")

zenodo_miller_jiang <- data.table::data.table(
  source = "zenodo_miller_jiang",
  title = "Congressional Tweets",
  file = "Congress_Tweets_20220630.xlsx",
  size_bytes = 531856785,
  url = "https://zenodo.org/api/records/8215566/files/Congress_Tweets_20220630.xlsx/content",
  destination = file.path(dir_raw, "zenodo_miller_jiang", "Congress_Tweets_20220630.xlsx"),
  note = "Large Excel file. Use as the clean 117th Congress backbone and validation corpus."
)

data.table::fwrite(zenodo_miller_jiang, file.path(dir_outputs, "public_download_manifest.csv"))

download_if_missing <- function(url, dest, expected_size = NA_real_) {
  if (file.exists(dest)) {
    actual_size <- file.info(dest)$size
    if (is.na(expected_size) || actual_size >= expected_size) {
      message("Already exists: ", dest)
      return(invisible(dest))
    }
    message("Found incomplete file: ", dest, " (", actual_size, " bytes). Redownloading.")
    file.remove(dest)
  }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(3600, old_timeout))
  message("Downloading ", url)
  utils::download.file(url, destfile = dest, mode = "wb", quiet = FALSE)
  if (!is.na(expected_size) && file.info(dest)$size < expected_size) {
    stop("Downloaded file is smaller than expected: ", dest)
  }
  invisible(dest)
}

check_public_downloads <- function() {
  status <- data.table::copy(zenodo_miller_jiang)
  status[, exists := file.exists(destination)]
  status[, local_size_bytes := ifelse(exists, file.info(destination)$size, NA_real_)]
  status[, complete := exists & local_size_bytes >= size_bytes]
  data.table::fwrite(status, file.path(dir_outputs, "public_download_status.csv"))
  status
}

download_zenodo <- function() {
  download_if_missing(
    zenodo_miller_jiang$url,
    zenodo_miller_jiang$destination,
    zenodo_miller_jiang$size_bytes
  )
  check_public_downloads()
}

message("Wrote outputs/public_download_manifest.csv.")
message("Run download_zenodo() to fetch the 532 MB Zenodo file.")
