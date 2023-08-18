library(tidyverse)

airlines <- read_csv("data/airlines.csv")
airports <- read_csv("data/airports.csv")
flights <- read_csv("data/flights.csv")
cia <- read_csv("data/CIACountries.csv")

str(cia)
glimpse(airports)


### inner join data
flights_joined <- flights %>% 
  inner_join(airlines, by = c("carrier" = "carrier"))
glimpse(flights_joined)


## left joins 
#Airports in the pacific time zone only 
airports_pt <- airports %>%
  filter(tz == -8) %>% 
  select(faa, name, tz)

#If we do inner join
nyc_dests_pt <- flights %>% 
  inner_join(airports_pt, by = c("dest" = "faa")) %>% 
  select(name, tz, dest, dep_time,sched_dep_time, arr_time, sched_arr_time, carrier)

## left join
nyc_dests <- flights %>% 
  left_join(airports_pt, by = c("dest" = "faa")) 


nyc_dests %>%
  summarize(
    num_flights = n(),
    num_flights_pt = sum(!is.na(name)),
    num_flights_not_pt = sum(is.na(name))
  )
#### example of multiple col joins
# Create emp Data Frame
emp_df=data.frame(
  emp_id=c(1,2,3,4,5,6),
  name=c("Smith","Rose","Williams","Jones","Brown","Brown"),
  superior_emp_id=c(-1,1,1,2,2,2),
  dept_id=c(10,20,10,10,40,50),
  dept_branch_id= c(101,102,101,101,104,105)
)

# Create dept Data Frame
dept_df=data.frame(
  dept_id=c(10,20,30,40),
  dept_name=c("Finance","Marketing","Sales","IT"),
  dept_branch_id= c(101,102,103,104)
)

emp_df %>% 
  inner_join(dept_df, 
             by=c('dept_id'='dept_id', 
                  'dept_branch_id'='dept_branch_id'))

### pivoting
# Sample data
data_wide <- data.frame(
  country = c("USA", "Canada"),
  `2018` = c(250, 180),
  `2019` = c(260, 190),
  `2020` = c(270, 200)
)

# Using pivot_longer to reshape data
data_wide %>%
  pivot_longer(cols = -country, names_to = "year", values_to = "value")


data_long <- data.frame(
  country = c("USA", "Canada"),
  year = rep(c("2018", "2019", "2020"), each = 2),
  value = c(250, 260, 270, 180, 190, 200)
)
# Using pivot_wider to reshape data
data_long %>%
  pivot_wider(names_from = year, values_from = value)



airports %>% 
  filter(tzone == "America/New_York") %>% 
  left_join(flights, by = c("faa" = "origin")) %>% 
  mutate(delayed = ifelse(dep_delay >0, 1, 0)) %>% 
  group_by(carrier) %>% 
  summarise(number_of_delay = sum(delayed, na.rm = TRUE),
            perc_delay = mean(delayed, na.rm = TRUE))%>% 
  arrange(-number_of_delay)


### ggplot2

library(mdsr)
data("SAT_2010")

ggplot(data = SAT_2010, aes(x = math)) +
  geom_histogram()

SAT_2010 %>% 
  ggplot() +
  geom_histogram(aes(x = math), breaks = c(400,450,550,600,700))

SAT_2010 %>% 
  ggplot() +
  geom_histogram(aes(x = math), bins = 8)

SAT_2010 %>% 
  ggplot(aes(x = math)) +
  geom_density()

SAT_2010 %>% 
  ggplot(aes(x = math)) +
  geom_density(bw = 3)

SAT_2010 %>% 
  ggplot(aes(x = math)) +
  geom_boxplot() +
  coord_flip()

## bar plot
SAT_2010 %>% 
  mutate(salary_level = case_when(
    salary < 52000 ~ "Low",
    salary >= 52000 & salary < 60000 ~ "Medium",
    salary >= 60000 ~ "High"
  )) %>% 
  ggplot(aes(x = salary_level)) +
  geom_bar()

SAT_2010 %>% 
  mutate(salary_level = case_when(
    salary < 52000 ~ "Low",
    salary >= 52000 & salary < 60000 ~ "Medium",
    salary >= 60000 ~ "High"
  )) %>% 
  mutate(salary_level = factor(salary_level, levels = c("Low", "Medium", "High"))) %>% 
  ggplot(aes(x = salary_level)) +
  geom_bar()

SAT_2010 %>% 
  ggplot(aes(x = expenditure, y = math)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) 
lm(math ~ expenditure, data = SAT_2010)

library(viridis)
SAT_2010 <- SAT_2010 %>%
  mutate(
    SAT_rate = cut(
      sat_pct, 
      breaks = c(0, 30, 60, 100), 
      labels = c("low", "medium", "high")
    )
  )
SAT_2010 %>% 
  ggplot(aes(x = expenditure, y = math, color = SAT_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d()

data("Cherry")

