# details -------------------------------------------------------------------

# Assignment 3.2 [R]
# Mohammad Hamdan
# Student number: 13572687

# function-------------------------------------------------------------------

make_art <- function(x = 42){
  library(tidyverse)
  library(ambient)
  library(scico)
  library(here)

  parameters <- list(
    seed = x,
    n_paths = 500,
    n_steps = 100,
    step_size = 80
  )
  set.seed(seed = parameters$seed)
  coordinates <- tibble(
    x = runif(n = parameters$n_paths, 0, 2),
    y = runif(n = parameters$n_paths, 0, 2),
    z = 0)

  #include the path_id and step_id in the coordinates
  coordinates <- coordinates %>%
    mutate(
      path_id = 1:parameters$n_paths,
      step_id = 1
    )

  #keep track of the series of coordinates
  new_coordinates <- coordinates

  painting <- TRUE
  while(painting == TRUE){

    # next two blocks of code form the curls
    step <- curl_noise(
      generator = gen_simplex, x = coordinates$x, y = coordinates$y, z = coordinates$z,
      seed = c(1,1,1) * parameters$seed
    )

    coordinates <- coordinates %>%
      mutate(x = x + (step$x/10000) * parameters$step_size,
             y = y + (step$y/10000) * parameters$step_size,
             step_id = step_id + 1
      )

    # append the coordinates to new_coordinates // progression of painting
    new_coordinates <- bind_rows(new_coordinates, coordinates)

    # deciding when to stop
    current_step <- last(coordinates$step_id)

    if(current_step >= parameters$n_steps){
      painting <- FALSE
    }
  }

  covid <- ggplot(new_coordinates, mapping = aes(x, y, group = path_id)) +
    geom_path(size = 0.5, alpha = 0.75, color = sample(c("dark green", "green", "blue", "dark blue"), 1)
    ) + coord_equal()+ #square picture
    theme_void()

  print(covid)
}


