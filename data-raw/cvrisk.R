library(purrr)

sex <- rbinom(200, 1, .5)
smoking <- rbinom(200, 1, .15)
tx <- map_dbl(smoking, ~ rbinom(1, 1, .5 + .x*.2)) # RD of 0.2

outcome <- pmap_dbl(list(sex, smoking, tx),
                    ~ rbinom(1, 1, .24 + .07*..1 - 0.12*..3 + .2*..2))

cvrisk <- data.frame(sex = sex, smoking = smoking, tx = tx, outcome = outcome)

usethis::use_data(cvrisk, overwrite = TRUE)
