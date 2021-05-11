needs(tidyverse)
needs(palmerpenguins)
a <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, NA)

mn <- function(x) {
  d <- na.omit(a)
  return(
    sum(d)/length(d))
}

mn(a)
mean(a, na.rm = TRUE)

na_count <- function(x) {
  sum(is.na(x))
}

purrr::map_dbl(split(mtcars$cyl, mtcars$cyl), length) / length(mtcars$cyl)

prop_rep <- function(x) {
  map_dbl(split(x, x), length)/length(x)
}

prop_rep(mtcars$cyl)

znorm <- function(x) {
  (x - mean(x, na.rm = T)) /
    sd(x, na.rm = T)
}
znorm(mtcars$mpg)

num_df <- function(x) {
  sub <- map_lgl(x, is.numeric)
  x <- x[, sub]
  return(x)
  }

  