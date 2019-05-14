#' Attractiveness ratings of faces
#'
#' A dataset containing attractiveness ratings (on a 1-7 scale from "much less attractiveness than average" to "much more attractive than average") for the neutral front faces from 2513 people (ages 17-90) 
#'
#' @format A data frame with 256326 rows and 9 variables:
#' \describe{
#'   \item{rater_id}{rater's ID}
#'   \item{rater_sex}{rater's sex (female, male, intersex, NA)}
#'   \item{rater_age}{rater's age (17-90 years)}
#'   \item{rater_sexpref}{rater's preferred sex for romantic relationships (either, men, neither, women, NA)}
#'   \item{face_id}{face's ID}
#'   \item{face_sex}{face's sex (female, male)}
#'   \item{face_age}{face's age (in years)}
#'   \item{face_eth}{face's ethnic group}
#'   \item{rating}{attractiveness rating on a scale from 1 (much less attractive than average) to 7 (much more attractive than average)}
#' }
#' @source \url{https://figshare.com/articles/Face_Research_Lab_London_Set/5047666}
"faceratings"

#' Attractiveness ratings of 10 faces from 10 subjects
#'
#' The faceratings dataset cut down to the first 10 raters and the first 10 faces for demos
#'
#' @format A data frame with 100 rows and 9 variables:
#' \describe{
#'   \item{rater_id}{rater's ID}
#'   \item{rater_sex}{rater's sex (female, male, intersex, NA)}
#'   \item{rater_age}{rater's age (17-90 years)}
#'   \item{rater_sexpref}{rater's preferred sex for romantic relationships (either, men, neither, women, NA)}
#'   \item{face_id}{face's ID}
#'   \item{face_sex}{face's sex (female, male)}
#'   \item{face_age}{face's age (in years)}
#'   \item{face_eth}{face's ethnic group}
#'   \item{rating}{attractiveness rating on a scale from 1 (much less attractive than average) to 7 (much more attractive than average)}
#' }
#' @source \url{https://figshare.com/articles/Face_Research_Lab_London_Set/5047666}
"fr10"