library(DBI)
library(RSQLite)
library(tidyverse)
library(psych)
library(itembankr)


# custom functions to handle this particular dataset

get_phrase <- function(mel_id, start, end, get = "pitch") {

  # get phrases based on segmentation information
  melodies %>%
    filter(melid == mel_id) %>%
    slice(start:end) %>%
    select(pitch, duration) %>%
    mutate(N = length(pitch),
           duration = round(duration, 2),
           melody = paste0(diff(pitch), collapse = ",")) %>%
    cbind(tibble(melid = mel_id)) %>%
    musicassessr::to_string_df(exclude_cols = c("melid", "N", "melody")) %>%
    dplyr::rename(abs_melody = pitch, durations = duration)

}

# tests
# t <- phrases %>% slice(1)
# tt <- get_phrase(t$melid, t$start+1, t$end+1)

# import the WJD in its original SQLite format

con <- dbConnect(RSQLite::SQLite(), "/Users/sebsilas/WJD/data-raw/wjazzd.db")

# Show List of Tables
as.data.frame(dbListTables(con))

# get melody table
melodies <- dbReadTable(con, 'melody')

solo_info <- dbReadTable(con, 'solo_info')

# get section information
sections <-dbReadTable(con, 'sections')

dbDisconnect(con)

rm(con)

# the WJD already has phrase information
phrases <- sections[sections$type=="PHRASE", ]

# tests
t <- phrases %>% slice(1)
tt <- get_phrase(t$melid, t$start+1, t$end+1)


melodic_phrases <- pmap_dfr(phrases, function(melid, type, start, end, value) {
  get_phrase(melid, start+1, end+1)
})


# how many unique melodies?

length(unique(melodic_phrases$melody))
# 10,357

relative.phrases.factor <- as.factor(unlist(melodic_phrases$melody))
no_unique <- length(levels(relative.phrases.factor))


# remove empty/weird mels

melodic_phrases <- melodic_phrases %>%
  filter(!melody %in% c(
    "", "0", "0,0", "0,0,0",
    "-6,6" # there was probably a good reason for removing this one, this which I haven't documented..
  ))


#
# melodic_phrases$row_id <- 1:nrow(melodic_phrases)
#
# # join with tempo
# melodic_phrases <- melodic_phrases %>%
#   dplyr::left_join(select(solo_info, melid, avgtempo), by = "melid")
#
#
# # we want a min duration of .25 seconds
#
# melodic_phrases <- melodic_phrases %>%
#   mutate(crotchet_speed_in_seconds = 60/avgtempo) %>%
#   rowwise() %>%
#   mutate(
#     durations_v = list(itembankr::str_mel_to_vector(durations)),
#     min_duration = min(unlist(durations_v), na.rm = TRUE),
#     ratio_to_quarter_ms = .25/min_duration
#   ) %>%
#   filter(
#     !is.infinite(ratio_to_quarter_ms)
#   ) %>%
#   mutate(
#     durations_v_corrected = case_when(ratio_to_quarter_ms > 1 ~ list(durations_v * ratio_to_quarter_ms), TRUE ~ list(durations_v)),
#     max_dur_corrected = max(unlist(durations_v_corrected), na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   filter(max_dur_corrected < 4)
#
#
#
# # check
# melodic_phrases %>%
#   pull(durations_v_corrected) %>%
#   unlist() %>%
#   as_tibble() %>%
#   filter(value < .3) %>%
#     ggplot(aes(x = value)) +
#       geom_histogram()
#
#
# melodic_phrases <- melodic_phrases %>%
#   rowwise() %>%
#   mutate(
#    durations_original = durations,
#    durations = paste0(unlist(round(durations_v_corrected, 2)), collapse = ",")
#   ) %>%
#   ungroup() %>%
#   select(orig_abs_melody, durations) %>%
#   rename(abs_melody = orig_abs_melody)
#
#
#
#
# rm(melodies)

melodic_phrases <- melodic_phrases %>%
  select(abs_melody, durations)

melodic_phrases_test <- melodic_phrases %>%
  slice_sample(n = 5)


create_item_bank("WJD_test",
                input = "phrase_df",
                output = "all",
                input_df = melodic_phrases_test,
                launch_app = FALSE)


load('WJD_test_ngram.rda')
load('WJD_test_phrase.rda')
load('WJD_test_combined.rda')

# See what creates the smallest file
save(WJD_test, file = "data-raw/WJD_test_gzip.rda", compress = "gzip")
save(WJD_test, file = "data-raw/WJD_test_bzip2.rda", compress = "bzip2")
save(WJD_test, file = "data-raw/WJD_test_xz.rda", compress = "xz")






create_item_bank("WJD",
                 input = "phrase_df",
                 output = "all",
                 input_df = melodic_phrases, launch_app = FALSE)

# Started 24/01/2023, 23:23
# Finished 26/01/2023, 09:23
# Took: 1 day, 10 hours

# Started: 05/06/2023, 08.53
# Similarity Computation complete: 16.36
# Finished: 08/06/2023, 2.04


load('WJD_ngram.rda')
load('WJD_phrase.rda')
load('WJD_combined.rda')


# See what creates the smallest file
save(ngram_item_bank, file = "data-raw/WJD_ngram_gzip.rda", compress = "gzip")
save(ngram_item_bank, file = "data-raw/WJD_ngram_bzip2.rda", compress = "bzip2")
save(ngram_item_bank, file = "data-raw/WJD_ngram_xz.rda", compress = "xz")

save(phrase_item_bank, file = "data-raw/WJD_phrase_gzip.rda", compress = "gzip")
save(phrase_item_bank, file = "data-raw/WJD_phrase_bzip2.rda", compress = "bzip2")
save(phrase_item_bank, file = "data-raw/WJD_phrase_xz.rda", compress = "xz")

save(combined_item_bank, file = "data-raw/WJD_combined_gzip.rda", compress = "gzip")
save(combined_item_bank, file = "data-raw/WJD_combined_bzip2.rda", compress = "bzip2")
save(combined_item_bank, file = "data-raw/WJD_combined_xz.rda", compress = "xz")



# Winners
load(file = "data-raw/WJD_combined_xz.rda")
load(file = "data-raw/WJD_phrase_xz.rda")
load(file = "data-raw/WJD_ngram_xz.rda")

# xz always wins here

combined_item_bank <- combined_item_bank %>% itembankr::set_item_bank_class(extra = "combined_item_bank")
ngram_item_bank <- ngram_item_bank %>% itembankr::set_item_bank_class(extra = "ngram_item_bank")
phrase_item_bank <- phrase_item_bank %>% itembankr::set_item_bank_class(extra = "phrase_item_bank")


use_data(combined_item_bank, ngram_item_bank, phrase_item_bank, compress = "xz", overwrite = TRUE)


# Remove redundancy and resave manually


# Phrase

load('data/phrase_item_bank.rda')

phrase_item_bank %>%
  tibble::as_tibble() %>%
  itembankr::remove_redundancy(remove_redundancy = TRUE) %>%
  itembankr::save_item_bank(name = "WJD", type = "phrase")


load('WJD_phrase.rda')

phrase_item_bank <- item_bank
rm(item_bank)

# N-gram

load('data/ngram_item_bank.rda')


ngram_item_bank %>%
  tibble::as_tibble() %>%
  itembankr::remove_redundancy(remove_redundancy = TRUE) %>%
  itembankr::save_item_bank(name = "WJD", type = "ngram")


load('WJD_ngram.rda')

attributes(item_bank)

ngram_item_bank <- item_bank
rm(item_bank)




# Combined
load('data/combined_item_bank.rda')


combined_item_bank %>%
  tibble::as_tibble() %>%
  itembankr::remove_redundancy(remove_redundancy = TRUE) %>%
  itembankr::save_item_bank(name = "WJD", type = "combined")


load('WJD_combined.rda')

attributes(item_bank)

combined_item_bank <- item_bank
rm(item_bank)


use_data(combined_item_bank, ngram_item_bank, phrase_item_bank, compress = "xz", overwrite = TRUE)

rm(combined_item_bank)

# Add to DB

# itembankr::store_item_bank_in_db(phrase_item_bank, item_bank_name = "WJD_phrase", item_bank_description = "The WJD corpus as a phrase item bank.")
# itembankr::store_item_bank_in_db(ngram_item_bank, item_bank_name = "WJD_ngram", item_bank_description = "The WJD corpus as an N-gram item bank.")



