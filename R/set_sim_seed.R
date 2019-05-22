#' Set seed for a simulated data set
#'
#' @param seed  a single value, interpreted as an integer, or NULL (see set.seed)
#' @param kind (see set.seed)
#' @param normal.kind (see set.seed)
#'
#' @return sets the seed
#' @keywords internal
#'
#' @examples
set_sim_seed <- function (seed, kind = "Mersenne-Twister", 
                          normal.kind = "Inversion") {
  sysSeed <- .GlobalEnv$.Random.seed
  on.exit({
    #.globals$idSeed <- .GlobalEnv$.Random.seed
    if (!is.null(sysSeed)) {
      .GlobalEnv$.Random.seed <- sysSeed 
    } else {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })
  set.seed(seed, kind = kind, normal.kind = normal.kind)
}