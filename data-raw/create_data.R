# These are subsets of data from:
#
# Flicker data:
# Gow EA, Wiebe KL, Fox JW (2015). Cavity use throughout the annual cycle of a
# migratory woodpecker revealed by geolocators. Ibis 157:167-170.
#
# White-throated sparrow data unpublished:
# Otter KA, Mckenna A, LaZerte SE, Ramsay SM (2017). The possible link between
# wintering grounds and continent-wide shifts in song dialects of white-throated
# sparrows (Oral). The joint meeting of the American Ornithological Society and
# the Society of Canadian Ornitholo- gists/Société des ornithologistes du Canada
# (East Lansing, MI, USA).


library(tidyverse)

light_data <- read_csv("./data-raw/examples_raw.csv", guess_max = 15000)

calib <- filter(light_data, id == "flicker_a") %>%
  select(-id)

flicker_mult <- filter(light_data, str_detect(id, "flicker_[bc]{1}"))

flicker <- filter(flicker_mult, id == "flicker_c")

wtsp <- filter(light_data, str_detect(id, "wtsp"))

usethis::use_data(calib, overwrite = TRUE)
usethis::use_data(flicker_mult, overwrite = TRUE)
usethis::use_data(flicker, overwrite = TRUE)
usethis::use_data(wtsp, overwrite = TRUE)
