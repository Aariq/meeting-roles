#' Generate meeting roles based on sprint number and date
#'
#' @param n_sprints how many sprints to generate?
#' @param start_sprint starting sprint number
#' @param start_date starting date, will be rounded down to previous Monday
#' @param names team member names
#'
#' @return a tibble
generate_roles <-
  function(n_sprints,
           start_sprint,
           start_date = Sys.Date(),
           names = c("Eric", "Kristina", "Chris", "Renata")) {
    
    start_monday <- lubridate::floor_date(start_date, unit = "week", week_start = 1)
    n_names = length(names)
    dplyr::tibble(
      sprint = rep(seq(start_sprint, length.out = n_sprints), each = 2),
      date = seq(start_monday, by = "week", length.out = n_sprints * 2)
    ) |> 
      dplyr::mutate(
        leader = names[sprint %% n_names + 1],
        scribe = names[(sprint + 1) %% n_names + 1],
        timer = names[(sprint + 2) %% n_names + 1],
        substitute = names[(sprint + 3) %% n_names + 1]
      )
    
  }

generate_roles(10, start_sprint = 27, start_date = as.Date("2024-02-12"), names = c("Chris", "Renata", "Eric","Kristina")) |> readr::write_csv("roles.csv")
