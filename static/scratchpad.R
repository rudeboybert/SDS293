library(tidyverse)
library(janitor)
library(googledrive)
library(googlesheets)

googledrive::drive_find(n_max = 25)

datafest_participants_orig <-
  "1A6v_1RskgVDiCBMIHV0ORt2cRSEg19_ntRL6-TwqqkY" %>%
  gs_key() %>%
  gs_read(ws = "2019 Participants") %>%
  mutate(Email = tolower(Email))

datafest_participants <- datafest_participants_orig %>%
  filter(Institution == "Smith") %>%
  select(X3, `Last Name`, Email, Withdrew)

ML_students_orig <-
  "1GqeIpPxyr9Q29VgdZUn2t7cdP3XeUN4_VI833o2xUno" %>%
  gs_key() %>%
  gs_read(ws = "Roster") %>%
  mutate(Email = tolower(Email))

ML_students <-
  ML_students_orig %>%
  select(`First name`, `Last name`, Email) %>%
  left_join(datafest_participants, by = "Email") %>%
  filter(!is.na(X3)) %>%
  filter(`Last name` != "Mudanye", `Last name` != "Simplice", `Last name` != "Nawaz") %>%
  clean_names() %>%
  select(first_name, last_name) %>%
  arrange(first_name)

write_csv(ML_students, "static/ML_students.cs")
