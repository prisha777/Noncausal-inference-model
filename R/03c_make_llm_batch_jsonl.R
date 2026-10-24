source("R/00_config.R")

candidate_path <- file.path(dir_outputs, "llm_labeling_batch.csv")
batch_dir <- file.path(dir_intermediate, "openai_label_batches")
dir.create(batch_dir, recursive = TRUE, showWarnings = FALSE)

label_model <- Sys.getenv("LLM_LABEL_MODEL", unset = "gpt-5.5")
max_rows <- as.integer(Sys.getenv("LLM_LABEL_MAX_ROWS", unset = "0"))
rows_per_shard <- as.integer(Sys.getenv("LLM_LABEL_ROWS_PER_SHARD", unset = "25000"))

if (!file.exists(candidate_path)) {
  stop("Missing outputs/llm_labeling_batch.csv. Run R/03_prepare_labels.R first.")
}

candidates <- data.table::fread(candidate_path)
if (max_rows > 0) candidates <- candidates[seq_len(min(.N, max_rows))]

label_schema <- list(
  name = "congressional_covid_misinfo_label",
  strict = TRUE,
  schema = list(
    type = "object",
    additionalProperties = FALSE,
    properties = list(
      is_covid_related = list(type = "boolean"),
      category = list(
        type = "string",
        enum = list(
          "covid_misinformation",
          "vaccine_hesitancy_promotion",
          "mask_mandate_opposition",
          "health_institution_delegitimization",
          "accurate_health_information",
          "policy_opinion_or_normative_claim",
          "unrelated_or_insufficient_context"
        )
      ),
      misinformation_score = list(
        type = "integer",
        minimum = 0,
        maximum = 5,
        description = "0 unrelated/accurate; 1 neutral COVID mention; 2 policy disagreement; 3 misleading or unsupported; 4 false factual claim; 5 severe harmful falsehood or conspiracy."
      ),
      confidence = list(type = "number", minimum = 0, maximum = 1),
      rationale = list(type = "string")
    ),
    required = list("is_covid_related", "category", "misinformation_score", "confidence", "rationale")
  )
)

system_prompt <- paste(
  "You are coding tweets by U.S. members of Congress for an academic study.",
  "Classify only what is supported by the tweet text.",
  "Do not label ordinary criticism, sarcasm, or policy disagreement as misinformation unless the tweet makes or strongly implies a false or unsupported factual health claim.",
  "Treat claims about vaccines, masks, COVID origins/treatments, public health institutions, mandates, and pandemic severity as relevant.",
  "Use the 0-5 score consistently: 0 unrelated or accurate; 1 COVID mention with no misinfo; 2 normative policy disagreement; 3 misleading framing or unsupported health claim; 4 clear false factual claim; 5 severe harmful falsehood, conspiracy, or dangerous medical advice."
)

make_request <- function(tweet_id, tweet_text) {
  list(
    custom_id = paste0("tweet_", tweet_id),
    method = "POST",
    url = "/v1/chat/completions",
    body = list(
      model = label_model,
      temperature = 0,
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = paste0("Tweet ID: ", tweet_id, "\nTweet text:\n", tweet_text))
      ),
      response_format = list(
        type = "json_schema",
        json_schema = label_schema
      )
    )
  )
}

num_shards <- ceiling(nrow(candidates) / rows_per_shard)
manifest <- vector("list", num_shards)

for (shard in seq_len(num_shards)) {
  start <- (shard - 1) * rows_per_shard + 1
  end <- min(shard * rows_per_shard, nrow(candidates))
  shard_dt <- candidates[start:end]
  out_path <- file.path(batch_dir, sprintf("congress_covid_labels_%03d.jsonl", shard))
  con <- file(out_path, open = "wt", encoding = "UTF-8")
  for (i in seq_len(nrow(shard_dt))) {
    req <- make_request(shard_dt$tweet_id[i], shard_dt$text[i])
    writeLines(jsonlite::toJSON(req, auto_unbox = TRUE, null = "null"), con)
  }
  close(con)
  manifest[[shard]] <- data.table::data.table(
    shard = shard,
    path = out_path,
    rows = nrow(shard_dt),
    file_size_bytes = file.info(out_path)$size,
    model = label_model
  )
}

manifest <- data.table::rbindlist(manifest)
data.table::fwrite(manifest, file.path(dir_outputs, "openai_label_batch_manifest.csv"))

message("Wrote OpenAI Batch JSONL shards to ", batch_dir)
message("Upload each JSONL file with purpose='batch', create a batch for /v1/chat/completions, then save returned output JSONL files in ", batch_dir)
print(manifest)
