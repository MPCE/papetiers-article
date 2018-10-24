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
library(RMySQL)

# Establish connection to database
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

## END
