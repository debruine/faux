# benchmark rep speed

between <- list(pet = c("cat", "dog"))
within <- list(time = c("day", "night"))
vardesc <- c(pet = "Type of Pet",
             time = "Time of Day")
design <- check_design(within, between, n = 100, 
                       mu = 1:4, sd = 1:4, r = 0.5, 
                       vardesc = vardesc, plot = FALSE)

sim_data_bm <- system.time({ 
  sim_data_data <- purrr::map_df(1:1000, ~sim_data(design))
})

sim_design_bm <- system.time({ 
  sim_design_data <- purrr::map_df(1:1000, ~sim_design(within, between, n = 100, 
                                  mu = 1:4, sd = 1:4, r = 0.5, 
                                  vardesc = vardesc, plot = FALSE))
})

rep_design_bm <- system.time({ 
  rep_design_data <- sim_design(design, rep = 1000, plot = FALSE)
})

rep_data_bm <- system.time({ 
  rep_data_data <- sim_data(design, rep = 1000)
})
