library(tidyverse)
library(mdsr)
## glimpse function in the tidyverse to explore the dataset
CIACountries %>% 
  glimpse()
cia <- CIACountries

CIACountries %>% 
  select(country, pop, gdp)

CIACountries %>% 
  select(country, educ) %>% 
  filter(educ > 5) %>% view()

cia <- CIACountries %>% 
  mutate(dens = pop/area)

CIACountries %>% 
  mutate(level_gdp = case_when(
    gdp < 4775 ~ "Low",
    gdp >= 4775 & gdp < 31650 ~ "Medium",
    gdp >= 31650 ~ "High"
  )) %>% 
  select(country, gdp, level_gdp) %>% 
  count(level_gdp)

CIACountries %>% 
  mutate(binary_oil = ifelse(oil_prod == 0, "No Oil", "Oil")) %>% 
  select(country, binary_oil) %>% 
  count(binary_oil)

CIACountries %>% 
  filter(!is.na(educ)) %>% view()





# Sample data frame
data <- data.frame(category = c("A", "B", "A", "B", "A"),
                   value = c(10, 20, 15, 25, 30))

# Group by "category" and compute mean and median of "value"
data %>%
  group_by(category) %>%
  summarise(mean_value = mean(value),
            median_value = median(value))

CIACountries %>% 
  mutate(high_educ = educ > 4.5) %>% 
  group_by(high_educ) %>% 
  summarise(mn_area = mean(area))

CIACountries %>% 
  group_by(net_users) %>% 
  summarise(mean_gdp = mean(gdp, na.rm = TRUE))

CIACountries %>% 
  count(net_users)

CIACountries %>% 
  group_by(net_users) %>% 
  summarise(count = n())

CIACountries %>% 
  arrange(desc(gdp)) %>% 
  slice(1:20) %>% 
  select(country, gdp)

CIACountries %>% 
  arrange(desc(pop)) %>% 
  slice(1:50) %>% 
  mutate(oil = ifelse(oil_prod == 0, TRUE, FALSE)) %>% 
  group_by(net_users) %>%
  summarise(perc = mean(oil))
  
  
CIACountries %>% 
  mutate(level_gdp = case_when(
    gdp < 66800 ~ "Low",
    TRUE ~ "High"
  )) %>% 
  arrange(-gdp) %>% 
  slice(1:25) %>% 
  mutate(pop_density = pop/area) %>% 
  group_by(level_gdp) %>% 
  summarise(min = min(pop_density),
            max = max(pop_density),
            mean = mean(pop_density),
            median = median(pop_density))

