########################################################################################################
#
# Papetiers Article
#
# Project: Mapping Print, Charting Enlightenment
#
# Script: Intitial run, exporting data from database.
#
# Authors: Rachel Hendery, Michael Falk
#
# Date: 24/10/18
#
########################################################################################################

# Load necessary packages
library(tidyverse)
library(magrittr)
library(DBI)
library(RMySQL)

# Establish connection to database
# NB: You must have a local MySQL/MariaDB server running for the script to connect to
manuscripts <- dbConnect(MySQL(), user="root", dbname="manuscripts", host="localhost")
info_schema <- dbConnect(MySQL(), user="root", dbname="information_schema", host="localhost")

# Query database, fetch necessary data
clients <- manuscripts %>%
dbSendQuery("SELECT * FROM clients") %>%
  dbFetch(n = Inf) %>%
  as_tibble()

professions <- manuscripts %>%
  dbSendQuery("SELECT * FROM professions") %>%
  dbFetch(n = Inf) %>%
  as_tibble()

clients_professions <- manuscripts %>%
  dbSendQuery("SELECT * FROM clients_professions") %>%
  dbFetch(n = Inf) %>%
  as_tibble()

placenames <- manuscripts %>%
  dbSendQuery("SELECT * FROM places") %>%
  dbFetch(n = Inf) %>%
  as_tibble()

clients_places <- manuscripts %>%
  dbSendQuery("SELECT * FROM clients_addresses") %>%
  dbFetch(n = Inf) %>%
  as_tibble()

# Filter clients by profession to get list of papermakers
papermaker_list <- filter(clients_professions, profession_code == "pf171")

# Add place data to papermakers
papermakers <- clients %>%
  filter(client_code %in% papermaker_list$client_code) %>%
  left_join(clients_places, by = "client_code") %>%
  left_join(placenames, by = "place_code") %>%
  mutate(other_documents = number_of_documents - number_of_letters)

# Export as csv for use in other programs.
write_csv(papermakers, "data/papermakers_with_places.csv")

# Find clients who were *only* papermakers.
only_papermakers <- clients_professions %>%
  group_by(client_code) %>% # group into professions
  filter(n() == 1) %>% # keep those with only one profession
  ungroup() %>%
  filter(profession_code == "pf171") %>% # only keep papermakers
  left_join(clients, by = "client_code") %>% # import the rest of the client data about these people
  left_join(clients_places, by = "client_code") %>% # link to place data
  left_join(placenames, by = "place_code") %>%
  mutate(other_documents = number_of_documents - number_of_letters) %T>% # to avoid confusion, calculate number of non-letter documents
  write_csv("data/papermakers_only_one_profession.csv")

## END
