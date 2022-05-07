

# admin to load a bunch of libraries
install.packages("pacman")

pacman::p_load(tidyverse,filesstrings,scales)

a <- str()
parse_number

mad

library(listviewer)
jsonedit(mad)
a <- lm(formula= cty ~ hwy,data = mpg)
b <- jsonedit(a)
summary(b)
summary(a)
pull(b,1)

library(tidyr)
install.packages("repurrrsive")
library(repurrrsive)
got_chars %>% map("name")

got_chars %>%
  map_chr('name')

set_names( )

got_chars[1:5] %>%
  set_names(map_chr(.,'name')) %>%
  listviewer::jsonedit()
