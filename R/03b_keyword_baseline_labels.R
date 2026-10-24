# Provisional keyword baseline for pipeline debugging only.
# This script never overwrites tweet_labels_normalized.csv unless explicitly
# requested with ALLOW_KEYWORD_BASELINE_OVERWRITE=1.
source("R/00_config.R")

tweets_path <- file.path(dir_intermediate, "tweets_normalized.csv")
baseline_path <- file.path(dir_intermediate, "tweet_labels_keyword_baseline.csv")
final_path <- file.path(dir_intermediate, "tweet_labels_normalized.csv")

if (!file.exists(tweets_path)) {
  message("Missing tweets_normalized.csv. Run R/01_ingest_tweets.R first.")
} else {
  tweets <- data.table::fread(tweets_path, select = c("tweet_id", "text_norm"))
  txt <- stringr::str_to_lower(tweets$text_norm)

  covid_pattern <- "\\b(covid(?:[- ]?19)?|coronavirus|pandemic|vaccin(?:e|es|ated|ation|ating)|booster|mask(?:s|ing)?|fauci|cdc|mandate|lockdown|operation warp speed|hydroxychloroquine|ivermectin)\\b"
  institution_pattern <- "\\b(fauci|cdc|world health organization|@who|nih|public health bureaucracy|health bureaucrats)\\b"
  anti_mandate_pattern <- "\\b(mask mandate|vaccine mandate|forced vaccination|medical freedom|lockdown|shutdown)\\b"
  hesitancy_pattern <- "\\b(experimental vaccine|rushed vaccine|vaccine injury|natural immunity|my body my choice|do not comply)\\b"
  severe_pattern <- "\\b(plandemic|scamdemic|bioweapon|microchip|depopulation|hoax|fake pandemic|ivermectin|hydroxychloroquine)\\b"
  accurate_pattern <- "\\b(get vaccinated|safe and effective|wear a mask|follow cdc|vaccines save lives|booster shot|public health guidance)\\b"

  labels <- data.table::data.table(
    tweet_id = tweets$tweet_id, label_source = "keyword_baseline_not_for_final_inference",
    is_covid_related = stringr::str_detect(txt, covid_pattern), category = "unrelated",
    misinformation_score = 0, confidence = 0.35
  )
  labels[is_covid_related == TRUE, `:=`(category = "policy_or_health_covid_related", misinformation_score = 1)]
  labels[stringr::str_detect(txt, accurate_pattern), `:=`(category = "accurate_health_information", misinformation_score = 0)]
  labels[stringr::str_detect(txt, anti_mandate_pattern), `:=`(category = "mask_or_vaccine_mandate_opposition", misinformation_score = 2)]
  labels[stringr::str_detect(txt, institution_pattern) & is_covid_related == TRUE, `:=`(category = "health_institution_delegitimization", misinformation_score = pmax(misinformation_score, 3))]
  labels[stringr::str_detect(txt, hesitancy_pattern), `:=`(category = "vaccine_hesitancy_promotion", misinformation_score = pmax(misinformation_score, 3))]
  labels[stringr::str_detect(txt, severe_pattern), `:=`(category = "covid_misinformation", misinformation_score = 5)]
  labels <- labels[is_covid_related == TRUE | misinformation_score > 0]
  data.table::fwrite(labels, baseline_path)
  data.table::fwrite(labels[, .N, by = .(category, misinformation_score)][order(-misinformation_score, -N)], file.path(dir_outputs, "keyword_label_distribution.csv"))
  if (identical(Sys.getenv("ALLOW_KEYWORD_BASELINE_OVERWRITE"), "1")) {
    data.table::fwrite(labels, final_path)
    message("Explicit override enabled: wrote provisional labels to ", final_path)
  } else {
    message("Wrote provisional keyword baseline to ", baseline_path, ". The structured-label file was not changed.")
  }
}
