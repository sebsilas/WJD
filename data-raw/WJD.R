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
    rename(orig_abs_melody = pitch, durations = duration)

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


# remove the empty mel
melodic_phrases <- melodic_phrases[!melodic_phrases$melody=="", ]
# remove the random repeated note melodies
melodic_phrases <- melodic_phrases[!melodic_phrases$melody=="0", ]
melodic_phrases <- melodic_phrases[!melodic_phrases$melody=="0,0", ]
melodic_phrases <- melodic_phrases[!melodic_phrases$melody=="0,0,0", ]
melodic_phrases <- melodic_phrases[!melodic_phrases$melody=="-6,6", ] # there was probably a good reason for this which I haven't documented..

melodic_phrases$row_id <- 1:nrow(melodic_phrases)

# join with tempo
melodic_phrases_scaled <- melodic_phrases %>% dplyr::left_join(solo_info, by = "melid") %>%
  mutate(bpm_80_scale = avgtempo/80,
         durations_original = durations) %>% rowwise() %>%
  mutate(durations = paste0(round(itembankr::str_mel_to_vector(durations_original) * bpm_80_scale, 2), collapse = ","),
         mean_duration = mean(itembankr::str_mel_to_vector(durations), na.rm = TRUE)) %>%
  ungroup() %>%
  select(orig_abs_melody, durations, melody, N, row_id)




#hist(phrases_dbs2$mean_duration)
#mean(phrases_dbs2$mean_duration, na.rm = TRUE)
#mean(itembankr::WJD("main")$mean_duration, na.rm = TRUE)

# now use the itembankr function
# there are no midi/musicxml files (this the WJD are preprocessed everything for us)
# we can give our dataframe to the corpus_to_item_bank function, which them simply computes features for us
# as well as makes an ngram database


melodic_phrases_test <- melodic_phrases %>% slice(1:2)

WJD <- itembankr::corpus_to_item_bank(corpus_name = "WJD",
                                      corpus_df = melodic_phrases,
                                      output_type = "both",
                                      phrases_db = melodic_phrases, launch_app = FALSE)


t1 <- WJD("files")
t2 <- WJD("ngram")
t3 <- WJD("main")
t4 <- WJD("phrases")

tt <- purrr::map_dfr(t4$durations, function(x) tibble::tibble(dur_length = length(itembankr::str_mel_to_vector(x))))
tt2 <- purrr::map_dfr(t4$orig_abs_melody, function(x) tibble::tibble(mel_length = length(itembankr::str_mel_to_vector(x))))

tt3 <- cbind(tt, tt2)

tt3$same <- apply(tt3, MARGIN = 1, function(row) {
  dur <- row['dur_length']
  mel <- row['mel_length']
  dur == mel
})


#usethis::use_data(WJD, overwrite = TRUE)



# make minimal version


# load('data/WJD.rda')

files_db <- NA
ngram_db <- NA
main_db <- WJD("main")
phrases_db <- WJD("phrases")


WJD <- function(key) {
  l <- list("files" = files_db,
            "ngram" = ngram_db,
            "main" = main_db,
            "phrases" = phrases_db)
  l[[key]]
}

usethis::use_data(WJD, overwrite = TRUE)

WJD("phrases")



