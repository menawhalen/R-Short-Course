library(tidyverse)
library(mdsr)


for (i in 10:26){
  print(i)
}
for (i in 10:1){
  print(i)
}

for (i in c(2, 3, 5, 7, 11)){
  print(i)
}

for (i in c("mike", "mary", "tom", "sue")){
  print(i)
}

rand_state <- sample(SAT_2010$state, 5, replace = FALSE)

dat <- tibble()
for (s in rand_state) {
  out <- SAT_2010 %>% 
    filter(state == s) %>% 
    summarise(total_sum = read + write + math,
              is_equal = ifelse(total == total_sum, TRUE, FALSE),
              state = s)
  dat <- bind_rows(dat, out)
}


## Cherry example
yearz <- sample(unique(Cherry$year), 3)

cherry_dat <- tibble()
for (y in yearz) {
  cherry_dat <- Cherry %>%
    filter(year == y) %>% 
    mutate(age_cat = cut(
      age, 
      breaks = c(0,29,49,69,100),
      labels = c("under 30", "under 50", "under 70", "old")
    )) %>% 
    group_by(age_cat) %>% 
    summarise(ave = mean(gun, na.rm = TRUE),
              year = y) %>% 
    bind_rows(cherry_dat)
}

cherry_dat %>% 
  pivot_wider(id_cols = age_cat, names_from = year, values_from = ave)

Cherry %>% 
  mutate(age_cat = ifelse(age %in% 25:35, "my age", "not me"))


# Create a list of values
values <- c(2, 4, 6, 8)

# Define a function to be applied
square <- function(x) {
  return(x^2)
}

# Use map to apply the function to each element
result <- map(values, square)

# The result is a list with squared values
result
unlist(result)

for (i in values) {
  print(square(i))
}


library(palmerpenguins)
data("penguins")

penguins %>% 
  group_by(sex) %>% 
  summarise(mean_length = mean(bill_length_mm, na.rm = TRUE))

## mapping
penguins %>% 
  group_by(species) %>% 
  nest() %>% 
  mutate(mean_body_mass = map(data, ~ mean(.x$body_mass_g, na.rm = TRUE)),
         mean_bill_length = map(data, ~ mean(.x$bill_length_mm, na.rm = TRUE)),
         mean_bill_depth = map(data, ~ mean(.x$bill_depth_mm, na.rm = TRUE)))


penguins %>% 
  group_by(species) %>% 
  nest() %>% 
  mutate(mean_body_mass = map_dbl(data, ~ mean(.x$body_mass_g, na.rm = TRUE)),
         mean_bill_length = map_dbl(data, ~ mean(.x$bill_length_mm, na.rm = TRUE)),
         mean_bill_depth = map_dbl(data, ~ mean(.x$bill_depth_mm, na.rm = TRUE)))
