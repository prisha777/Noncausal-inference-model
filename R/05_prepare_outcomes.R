source("R/00_config.R")

outcomes_dir <- file.path(dir_raw, "outcomes")
files <- list.files(outcomes_dir, pattern = "\\.(csv|tsv|txt|xlsx|xls)$", recursive = TRUE, full.names = TRUE)

normalize_outcome <- function(path) {
  dt <- read_any_table(path)
  nm <- tolower(names(dt))
  names(dt) <- nm
  state_col <- intersect(c("state", "state_abbr", "location", "jurisdiction"), names(dt))[1]
  date_col <- intersect(c("month", "date", "week", "period", "year"), names(dt))[1]
  if (is.na(state_col) || is.na(date_col)) {
    message("Skipping outcome without state/date columns: ", basename(path))
    return(data.table::data.table())
  }
  dt[, state := normalize_state(get(state_col))]
  if (date_col == "year") {
    dt[, month := as.IDate(paste0(get(date_col), "-11-01"))]
  } else {
    dt[, month := safe_month(get(date_col))]
  }
  value_cols <- setdiff(names(dt), c(state_col, date_col, "state", "month"))
  keep <- c("state", "month", value_cols)
  out <- dt[, ..keep]
  prefix <- tools::file_path_sans_ext(basename(path))
  value_cols <- setdiff(names(out), c("state", "month"))
  data.table::setnames(out, value_cols, paste(prefix, value_cols, sep = "__"))
  out
}

if (length(files) == 0) {
  message("No outcome files found. Add CDC, MIT Election Lab, ACLED, FBI, or Census-derived files to data/raw/outcomes/.")
  outcomes <- data.table::data.table()
} else {
  pieces <- lapply(files, normalize_outcome)
  pieces <- pieces[vapply(pieces, nrow, integer(1)) > 0]
  if (length(pieces) == 0) {
    outcomes <- data.table::data.table()
  } else {
    outcomes <- Reduce(function(x, y) merge(x, y, by = c("state", "month"), all = TRUE), pieces)
    data.table::fwrite(outcomes, file.path(dir_processed, "state_month_outcomes.csv"))
    print(outcomes[, .N, by = state][order(state)])
  }
}
