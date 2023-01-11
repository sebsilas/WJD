

## check this! haven't used/properly looked..

WJD_IRT_arrhythmic <- WJD("main") %>%
  dplyr::filter(N > 2)

WJD_IRT_arrhythmic_phrases <- WJD("phrases") %>%
  dplyr::filter(N > 2)


WJD_IRT_arrhythmic <- WJD_IRT_arrhythmic %>%
  select(melody, N, step.cont.loc.var, tonalness, log_freq)

WJD_IRT_arrhythmic_phrases <- WJD_IRT_arrhythmic_phrases %>%
  select(melody, N, step.cont.loc.var, tonalness, log_freq)


WJD_IRT_arrhythmic <- WJD_IRT_arrhythmic %>%
  rbind(WJD_IRT_arrhythmic_phrases)


# normal

difficulty <- predict(Berkowitz::lm2.2,
                      newdata = WJD_IRT_arrhythmic,
                      re.form = NA) # this instructs the model to predict without random effects )

difficulty <- difficulty %>% as.numeric %>%
  multiply_by(-1) %>%
  scales::rescale()

WJD_IRT_arrhythmic$difficulty <- as.numeric(difficulty)

WJD_IRT_arrhythmic <- WJD_IRT_arrhythmic %>% unique()

use_data(WJD_IRT_arrhythmic, overwrite = TRUE)

# scaled

WJD_IRT_arrhythmic_scaled <- WJD_IRT_arrhythmic %>%
  select(melody, N, step.cont.loc.var, tonalness, log_freq) %>%
  mutate(across(N:log_freq, scale))

difficulty <- predict(WJD::lm2.2_scaled,
                      newdata = WJD_IRT_arrhythmic_scaled,
                      re.form = NA) # this instructs the model to predict without random effects )

difficulty <- difficulty %>% as.numeric %>%
  multiply_by(-1) %>%
  scale()

WJD_IRT_arrhythmic_scaled$difficulty <- as.numeric(difficulty)

WJD_IRT_arrhythmic_scaled <- WJD_IRT_arrhythmic_scaled %>% unique()

WJD_IRT_arrhythmic_scaled %>% select(-melody) %>%
  itembankr::hist_item_bank()

use_data(WJD_IRT_arrhythmic_scaled, overwrite = TRUE)
