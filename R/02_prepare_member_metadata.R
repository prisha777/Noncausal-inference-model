source("R/00_config.R")

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x
metadata_dir <- file.path(dir_raw, "member_metadata")
congress_dir <- file.path(dir_raw, "congress_members")

read_legislators_yaml <- function(path) {
  records <- yaml::read_yaml(path)
  data.table::rbindlist(lapply(records, function(x) {
    terms <- x$terms
    latest_term <- if (length(terms)) terms[[length(terms)]] else list()
    ids <- x$id %||% list()
    data.table::data.table(
      bioguide_id = as.character(ids$bioguide %||% NA), screen_name = NA_character_,
      member_name = paste(na.omit(c(x$name$first, x$name$middle, x$name$last)), collapse = " "),
      state = normalize_state(latest_term$state %||% NA), party = as.character(latest_term$party %||% NA),
      chamber = as.character(latest_term$type %||% NA), term_start = as.character(latest_term$start %||% NA),
      term_end = as.character(latest_term$end %||% NA), metadata_source = basename(path)
    )
  }), fill = TRUE)
}

read_congress_members_json <- function() {
  social_path <- file.path(congress_dir, "legislators-social-media.JSON")
  people_paths <- c(file.path(congress_dir, "legislators-current.JSON"), file.path(congress_dir, "legislators-historical.JSON"))
  people_paths <- people_paths[file.exists(people_paths)]
  if (!file.exists(social_path) || !length(people_paths)) return(data.table::data.table())

  social <- data.table::as.data.table(jsonlite::fromJSON(social_path, flatten = TRUE))
  social <- social[!is.na(`social.twitter`) & nzchar(`social.twitter`), .(
    bioguide_id = as.character(`id.bioguide`), screen_name = as.character(`social.twitter`)
  )]
  people <- data.table::rbindlist(lapply(people_paths, function(path) {
    raw <- jsonlite::fromJSON(path, flatten = TRUE)
    data.table::rbindlist(lapply(seq_len(nrow(raw)), function(i) {
      terms <- raw$terms[[i]]
      if (is.null(terms) || !nrow(terms)) return(NULL)
      terms <- data.table::as.data.table(terms)
      data.table::data.table(
        bioguide_id = as.character(raw[["id.bioguide"]][i]),
        member_name = as.character(raw[["name.official_full"]][i]),
        state = normalize_state(coalesce_columns(terms, "state")),
        party = as.character(coalesce_columns(terms, "party")),
        chamber = as.character(coalesce_columns(terms, "type")),
        term_start = as.character(coalesce_columns(terms, "start")),
        term_end = as.character(coalesce_columns(terms, "end")),
        metadata_source = basename(path)
      )
    }), fill = TRUE)
  }), fill = TRUE)
  people <- people[term_start <= "2022-06-30" & (is.na(term_end) | term_end >= "2020-01-01")]
  people <- unique(people, by = c("bioguide_id", "state", "party", "chamber", "term_start", "term_end"))
  out <- merge(social, people, by = "bioguide_id", all.x = TRUE, allow.cartesian = TRUE)
  out[is.na(metadata_source), metadata_source := "social_media_only"]
  # A handle can span several terms. Keep a one-row-per-handle crosswalk;
  # tweet-level state and party remain authoritative when already available.
  data.table::setorderv(out, c("screen_name", "term_end", "term_start"), c(1, -1, -1), na.last = TRUE)
  unique(out, by = "screen_name")
}

read_flat_metadata <- function(path) {
  dt <- read_any_table(path)
  data.table::data.table(
    bioguide_id = as.character(coalesce_columns(dt, c("bioguide_id", "bioguide", "BioGuideID", "id.bioguide"))),
    screen_name = as.character(coalesce_columns(dt, c("screen_name", "twitter", "twitter_id", "handle", "social.twitter"), default = NA)),
    member_name = as.character(coalesce_columns(dt, c("member_name", "name", "full_name", "Name", "name.official_full"))),
    state = normalize_state(coalesce_columns(dt, c("state", "State"))), party = as.character(coalesce_columns(dt, c("party", "Party"))),
    chamber = as.character(coalesce_columns(dt, c("chamber", "type", "Chamber"))),
    term_start = as.character(coalesce_columns(dt, c("term_start", "start"), default = NA)),
    term_end = as.character(coalesce_columns(dt, c("term_end", "end"), default = NA)), metadata_source = basename(path)
  )
}

metadata_files <- if (dir.exists(metadata_dir)) list.files(metadata_dir, pattern = "\\.(csv|tsv|txt|yaml|yml|json)$", recursive = TRUE, full.names = TRUE) else character()
flat_members <- data.table::rbindlist(lapply(metadata_files, function(path) {
  message("Reading member metadata: ", basename(path))
  if (tolower(tools::file_ext(path)) %in% c("yaml", "yml")) read_legislators_yaml(path) else read_flat_metadata(path)
}), fill = TRUE)
congress_members <- read_congress_members_json()
members <- data.table::rbindlist(list(congress_members, flat_members), fill = TRUE)

if (!nrow(members)) {
  message("No member metadata found. Add a crosswalk or the congress_members JSON files.")
} else {
  members[, screen_name := sub("^@", "", trimws(screen_name))]
  members <- members[(!is.na(bioguide_id) & nzchar(bioguide_id)) | (!is.na(screen_name) & nzchar(screen_name))]
  members <- unique(members, by = c("bioguide_id", "screen_name"))
  data.table::fwrite(members, file.path(dir_intermediate, "members_normalized.csv"))
  data.table::fwrite(members[, .(members = .N, handles = data.table::uniqueN(screen_name)), by = metadata_source],
                     file.path(dir_outputs, "member_metadata_coverage.csv"))
  message("Wrote ", nrow(members), " member metadata records with ", data.table::uniqueN(members$screen_name), " Twitter handles.")
}
