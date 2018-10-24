# Use the matches found by dedupe to update the data.
library(tidyverse)
library(magrittr)

# Estbalish connection to database
library(RMySQL)
manuscripts <- dbConnect(MySQL(), user="root", dbname="manuscripts", host="localhost")
info_schema <- dbConnect(MySQL(), user="root", dbname="information_schema", host="localhost")

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

papermaker_list <- filter(clients_professions, profession_code == "pf171")

papermakers <- clients %>%
  filter(client_code %in% papermaker_list$client_code) %>%
  left_join(clients_places, by = "client_code") %>%
  left_join(placenames, by = "place_code") %>%
  mutate(other_documents = number_of_documents - number_of_letters)

write_csv(papermakers, "papermakers_with_places.csv")



# Did the intensity of correspondence decline with distance from neuchatel?
cor.test(papermakers$number_of_documents, papermakers$distance_from_neuchatel, alternative = "less", method = "spearman")

papermakers %>%
  ggplot(aes(number_of_documents)) +
  geom_histogram(binwidth = 50)

papermakers %>%
  ggplot(aes(number_of_documents, distance_from_neuchatel)) +
  geom_point()

# Does every client have a place?
papermakers %>%
  sum(is.null(.$latitude))

# Top papermakers as percentage of total documents


# Abstract question 1:

# Who were the top papermakers the STN used?
papermakers %>%
  filter(number_of_documents > 20.75) %>%
  arrange(desc(number_of_documents)) %>%
  select(client_name, number_of_letters, other_documents, name, distance_from_neuchatel) %>%
  write_csv("most_important_papetiers.csv")
# How far did the paper have to come?
papermakers %>%
  mutate(intensity = number_of_documents/sum(number_of_documents)) %>%
  summarise(paper_distance = weighted.mean(distance_from_neuchatel, intensity))
# Which towns had the most intense relationships?
papermakers %>%
  mutate(document_prop = number_of_documents/sum(number_of_documents)) %>%
  group_by(name, distance_from_neuchatel) %>%
  summarise(document_prop = sum(document_prop)) %>%
  arrange(desc(document_prop)) %>%
  ungroup() %>%
  mutate(total = cumsum(document_prop)) %>%
  write_csv("top_two_thirds_places.csv")