options(stringsAsFactors = FALSE)

needed_packages <- c(
  "data.table", "lubridate", "stringr", "jsonlite", "yaml", "readxl", "fixest",
  "ggplot2", "broom", "modelsummary"
)

missing_packages <- needed_packages[!vapply(needed_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0 && !isTRUE(getOption("covid_misinfo_pipeline.warned_packages"))) {
  options(covid_misinfo_pipeline.warned_packages = TRUE)
  message(
    "Missing R packages: ", paste(missing_packages, collapse = ", "), "\n",
    "Install them with install.packages(c(",
    paste(sprintf('\"%s\"', missing_packages), collapse = ", "),
    ")) before running the full pipeline."
  )
}

root_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
dir_raw <- file.path(root_dir, "data", "raw")
dir_intermediate <- file.path(root_dir, "data", "intermediate")
dir_processed <- file.path(root_dir, "data", "processed")
dir_outputs <- file.path(root_dir, "outputs")

invisible(lapply(c(dir_raw, dir_intermediate, dir_processed, dir_outputs), dir.create, recursive = TRUE, showWarnings = FALSE))

read_any_table <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("csv", "txt")) {
    return(data.table::fread(path, showProgress = FALSE))
  }
  if (ext %in% c("tsv")) {
    return(data.table::fread(path, sep = "\t", showProgress = FALSE))
  }
  if (ext %in% c("jsonl", "ndjson")) {
    return(data.table::rbindlist(jsonlite::stream_in(file(path), verbose = FALSE), fill = TRUE))
  }
  if (ext == "json") {
    parsed <- jsonlite::fromJSON(path, flatten = TRUE)
    if (is.data.frame(parsed)) return(data.table::as.data.table(parsed))
    if (is.list(parsed)) return(data.table::rbindlist(parsed, fill = TRUE))
  }
  if (ext %in% c("xlsx", "xls")) {
    sheets <- readxl::excel_sheets(path)
    first_sheet <- sheets[1]
    return(data.table::as.data.table(readxl::read_excel(path, sheet = first_sheet)))
  }
  stop("Unsupported file type: ", path)
}

coalesce_columns <- function(dt, candidates, default = NA_character_) {
  present <- intersect(candidates, names(dt))
  if (length(present) == 0) return(rep(default, nrow(dt)))
  out <- dt[[present[1]]]
  if (length(present) > 1) {
    for (col in present[-1]) out <- data.table::fcoalesce(out, dt[[col]])
  }
  out
}

safe_month <- function(x) {
  x_chr <- as.character(x)
  parsed <- suppressWarnings(lubridate::ymd_hms(x_chr, quiet = TRUE, tz = "UTC"))
  parsed_date <- suppressWarnings(lubridate::ymd(x_chr, quiet = TRUE))
  out <- data.table::fcoalesce(data.table::as.IDate(parsed), data.table::as.IDate(parsed_date))
  data.table::as.IDate(format(out, "%Y-%m-01"))
}

state_abb <- c(
  AL = "Alabama", AK = "Alaska", AZ = "Arizona", AR = "Arkansas", CA = "California",
  CO = "Colorado", CT = "Connecticut", DE = "Delaware", FL = "Florida", GA = "Georgia",
  HI = "Hawaii", ID = "Idaho", IL = "Illinois", IN = "Indiana", IA = "Iowa",
  KS = "Kansas", KY = "Kentucky", LA = "Louisiana", ME = "Maine", MD = "Maryland",
  MA = "Massachusetts", MI = "Michigan", MN = "Minnesota", MS = "Mississippi",
  MO = "Missouri", MT = "Montana", NE = "Nebraska", NV = "Nevada", NH = "New Hampshire",
  NJ = "New Jersey", NM = "New Mexico", NY = "New York", NC = "North Carolina",
  ND = "North Dakota", OH = "Ohio", OK = "Oklahoma", OR = "Oregon", PA = "Pennsylvania",
  RI = "Rhode Island", SC = "South Carolina", SD = "South Dakota", TN = "Tennessee",
  TX = "Texas", UT = "Utah", VT = "Vermont", VA = "Virginia", WA = "Washington",
  WV = "West Virginia", WI = "Wisconsin", WY = "Wyoming", DC = "District of Columbia"
)

normalize_state <- function(x) {
  x <- toupper(trimws(as.character(x)))
  names_by_upper <- setNames(names(state_abb), toupper(unname(state_abb)))
  out <- ifelse(x %in% names(state_abb), x, names_by_upper[x])
  out
}
