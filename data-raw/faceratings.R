faceinfo <- readr::read_csv("data-raw/london_faces_info.csv")
faceratings <- 
  readr::read_csv("data-raw/london_faces_ratings.csv") %>%
  dplyr::mutate(rater_id = paste0("S", formatC(row_number(), digits = 3, flag = "0"))) %>%
  tidyr::gather(face_id, rating, X001:X173) %>%
  dplyr::left_join(faceinfo, by = "face_id") %>%
  dplyr::mutate(face_id = str_replace(face_id, "X", "I")) %>%
  dplyr::select(rating, rater_id, face_id, 
                rater_sex, rater_age, rater_sexpref, 
                face_sex, face_age, face_eth)

usethis::use_data(faceratings, overwrite = T)
