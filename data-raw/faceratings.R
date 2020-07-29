library(dplyr)
library(readr)
library(tidyr)

faceinfo <- readr::read_csv("data-raw/london_faces_info.csv")
faceratings <- 
  readr::read_csv("data-raw/london_faces_ratings.csv") %>%
  dplyr::mutate(rater_id = make_id(nrow(.))) %>%
  tidyr::gather(face_id, rating, X001:X173) %>%
  dplyr::left_join(faceinfo, by = "face_id") %>%
  dplyr::mutate(face_id = str_replace(face_id, "X", "I")) %>%
  dplyr::select(rating, rater_id, face_id, 
                rater_sex, rater_age, rater_sexpref, 
                face_sex, face_age, face_eth)

usethis::use_data(faceratings, overwrite = T)

rater24 <- faceratings %>%
  filter(rater_sex %in% c("male", "female"),
         rater_sexpref %in% c("men", "women", "either"),
         !is.na(rater_age)) %>%
  group_by(rater_id, rater_sex, rater_sexpref) %>%
  summarise() %>%
  ungroup() %>%
  group_by(rater_sex, rater_sexpref) %>%
  arrange(sample(nrow(.))) %>%
  filter(row_number() < 5) %>%
  ungroup() %>%
  pull(rater_id)

face32 <- faceratings %>%
  filter(face_eth %in% c("black", "white", "east_asian", "west_asian"),
         !is.na(face_age)) %>%
  group_by(face_id, face_sex, face_eth) %>%
  summarise() %>%
  ungroup() %>%
  group_by(face_sex, face_eth) %>%
  arrange(sample(nrow(.))) %>%
  filter(row_number() < 5) %>%
  ungroup() %>%
  pull(face_id)
  
fr4 <- faceratings %>%
  filter(face_id %in% face32, rater_id %in% rater24)

usethis::use_data(fr4, overwrite = T)

range(fr4$face_age)
range(fr4$rater_age)
