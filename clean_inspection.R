library(tidyverse)
library(janitor)


inspections <- read_csv("data/Food_Inspections.csv") %>% 
  clean_names() %>% 
  mutate(inspection_date = mdy(inspection_date),
         license_number = factor(license_number), 
         results = factor(results),
         inspection_type = factor(tolower(inspection_type))) %>% 
  filter(results == "Fail" | results == "Pass" | results == "Pass w/ Conditions") %>% 
  filter(dba_name == aka_name) %>% 
  filter(inspection_type == "canvass" | inspection_type == "canvass re-inspection" |
           inspection_type == "complaint" | inspection_type == "complaint re-inspection" |
           inspection_type == "license" | inspection_type == "license re-inspection")

repeated <- tibble(name = names(table(inspections$dba_name)), count = table(inspections$dba_name)) %>% 
  filter(count > 2) %>% 
  pull(name)

subset_ins <- inspections %>% 
  filter(dba_name %in% repeated) %>%
  arrange(dba_name) %>% 
  select(inspection_id:address, zip:violations, -aka_name) %>% 
  mutate(facility_type = factor(tolower(facility_type)),
         risk = factor(risk),
         inspection_type = factor(tolower(inspection_type)))

subset_ins %>% 
  summarise(across(everything(), ~sum(is.na(.x))))

summary(subset_ins)
table(subset_ins$facility_type)[order(table(subset_ins$facility_type))]
facility<- tibble(name = names(table(subset_ins$facility_type)), count = table(subset_ins$facility_type)) %>% 
  filter(count > 100) %>% 
  pull(name)

subset_ins <- subset_ins %>% 
  filter(facility_type %in% facility) %>% 
  mutate(facility_type = case_when(
    facility_type == "daycare (2 - 6 years)" | facility_type == "daycare (under 2 years)" |
      facility_type == "children's services facility" | facility_type == "daycare above and under 2 years" | 
      facility_type == "daycare combo 1586" ~ "school",
    facility_type == "tavern" ~ "liquor",
    facility_type == "golden diner" ~ "long term care",
    facility_type == "wholesale" ~ "grocery store",
    facility_type == "mobile food dispenser" | facility_type == "mobile food preparer" |
      facility_type == "special event" ~ "catering",
    TRUE ~ as.character(facility_type)),
    inspection_type = case_when(
      inspection_type == "canvass re-inspection" ~ "canvass",
      inspection_type == "complaint re-inspection" ~ "complaint",
      inspection_type == "license re-inspection" ~ "license",
      TRUE ~ as.character(inspection_type)
    )
  )

subset_ins[is.na(subset_ins$risk),]$risk <- "Risk 2 (Medium)"

subset_ins %>% 
  summarise(across(everything(), ~sum(is.na(.x))))

write_csv(subset_ins, "data/inspections_clean.csv")
