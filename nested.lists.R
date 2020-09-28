library(tidyverse)
library(purrr)

POA_CODE = c("POA101","POA102")
dogs = c(4,4) 
cats = c(3,2) 

children = c(0, 1)

salary = c(100, 120)
employed.prop = c(1,0.5)

pets <- list(POA_CODE, as.integer(dogs), as.integer(cats))

children <-list(POA_CODE, as.integer(children))

employment <-list(POA_CODE, salary, employed.prop)

census <- list(pets, children, employment)


#change all non-numeric elements to double
census_num <- census %>%
  lmap_if(is.character, as.numeric(x))


map(census, function(x) map_if(x, is.character, ~as.numeric(sub('POA', '', .x))))
#Remove "POA" prefix from every postcode
census_code <- lmap(census, function(x){
  str_replace(POA_CODE,"POA","")
})