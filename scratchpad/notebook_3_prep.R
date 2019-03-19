# Notebook 3 prep: other suppliers of printing materials.

# Load libraries
library(DBI)
library(RMySQL)
library(tidyverse)
library(magrittr)
library(lubridate)

# Create connection object
manuscripts <- dbConnect(MySQL(), user="root", dbname="manuscripts", host="localhost")

# Get data on all printing supplies professions:
printing_supply_professions <- c(
  inkmaker = "pf100",
  ink_merchant = "pf336",
  oil_supplier = "pf120",
  pressmaker = "pf099",
  papetier = "pf171",
  papetier_director = "pf069",
  paper_merchant = "pf244"
)

# Import data
suppliers <- list()
for (i in seq_along(printing_supply_professions)) {
  query <- paste0("SELECT * FROM clients AS c",
                  "    LEFT JOIN clients_professions AS cp ",
                  "        ON cp.client_code = c.client_code ",
                  "    LEFT JOIN professions AS p ",
                  "        ON p.profession_code = cp.profession_code ",
                  "    LEFT JOIN clients_addresses AS ca ",
                  "        ON ca.client_code = c.client_code ",
                  "    LEFT JOIN places AS pl ",
                  "        ON pl.place_code = ca.place_code ",
                  "WHERE cp.profession_code LIKE '",
                  printing_supply_professions[i], "'")
  suppliers[[i]] <- manuscripts %>%
    dbSendQuery(query) %>%
    fetch(n = Inf) %>%
    .[,-c(13,14,20,21)] %>% # remove columns with duplicate names, otherwise error
    as.tibble()
}
rm(query, i) # delete global variables
suppliers %<>% bind_rows() # convert list of tibbles to single tibble

# How many documents have we got for the different professions?
suppliers %>%
  group_by(profession_type) %>%
  summarise(number_of_documents = sum(number_of_documents)) %>%
  write_csv("documents_all_profs.csv")
  
# Where are the inkmakers from?
suppliers %>%
  filter(profession_code == "pf100") %>%
  group_by(name) %>%
  summarise(n = n())

# How about the papermakers. When do they enter and leave the archive?
dates <- suppliers %>%
  filter(profession_code %in% c("pf171", "pf069", "pf244")) %>%
  mutate(
    first_date = dmy(first_date),
    last_date = dmy(last_date),
    how_long = last_date - first_date
  )

pdf("scratchpad/duration_of_letters.png", width = 9, height = 8)
thresh <- 5
dates %>%
  filter(number_of_documents > thresh) %>%
  ggplot(aes(x = fct_reorder(client_name, first_date, .desc = T))) +
  scale_y_date(date_breaks = "2 years", date_labels = "%Y") +
  geom_linerange(aes(ymin = first_date, ymax = last_date),
                 color = "dark blue", size = 1) +
  coord_flip() +
  labs(
    title = "The STN were always on the hunt for new papermakers",
    subtitle = paste0("Duration of in-correspondence for papetiers with more than ", thresh, " documents in the archive.",
                      " (n = ", nrow(dates), ")"),
    x = "Client",
    y = "Year"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
dev.off()

# Have a look at different thresholds
dates %>%
  filter(how_long > years(15))

# Book production peaked in the early 80s. Were they corresponding with more papetiers then?
pdf("scratchpad/new_papetiers_each_year.pdf", width = 7, height = 5)
dates %>%
  mutate(first_year = year(first_date)) %>%
  group_by(first_year) %>%
  summarise(n = n()) %>%
  ggplot(aes(first_year, n)) +
  geom_col() +
  labs(
    x = "Year",
    y = "New Papetiers Contacted",
    title = "The STN may have sought or found new suppliers in 1777 and 1778,\nin the build-up to their massive production in 1779-82.",
    subtitle = "New papetiers contacted each year."
  )
dev.off()

# Did partnerships last longer than sole traders?

# The partnership field is unreliable:
dates %>%
  group_by(partnership) %>%
  summarise(how_long = median(as.duration(how_long), na.rm = T))

# Do it again:
dates %>%
  mutate(partnership = str_detect(client_name, "[&]| et ")) %>%
  select(partnership, how_long) %>%
  drop_na() %>%
  group_by(partnership) %>%
  summarise(how_long = median(how_long, na.rm = T),
            n = n()) %>%
  mutate(how_long = as.duration(how_long))

# How do book suppliers compare to paper suppliers?
transactions <- manuscripts %>%
  dbSendQuery(paste0(
    "SELECT * FROM transactions AS t ",
    "    LEFT JOIN orders AS o ",
    "        ON t.order_code = o.order_code ",
    "    LEFT JOIN places AS p ",
    "        ON p.place_code = o.place_code"
  )) %>%
  fetch(n = Inf) %>%
  .[,-c(11,19)] %>%
  as.tibble()

transactions %>%
  filter("In" %in% direction_of_transaction) %>%
  group_by(place_code) %>%
  mutate(total_numer_of_volumes = abs(total_number_of_volumes),
    total_number_of_volumes = sum(total_number_of_volumes, na.rm = T)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(volume_prop = total_number_of_volumes/sum(total_number_of_volumes)) %>%
  select(name, distance_from_neuchatel, total_number_of_volumes, volume_prop) %>%
  arrange(desc(total_number_of_volumes)) %>%
  print(n = 11)

# How many papetiers were they corresponding with at any one time?
intervals <- dates %>%
  (function(x){
    na_i <- is.na(x$last_date)
    x$last_date[na_i] <- x$first_date[na_i]
    out <- x$first_date %--% x$last_date
    return(out)
  })()

years <- seq(
  from = min(year(dates$first_date), na.rm = T),
  to = max(year(dates$last_date), na.rm = T)
  ) %>%
  as.tibble() %>%
  rename(years = value) %>%
  mutate(
    year_start = paste0(years, "01-01"),
    year_end = paste0(years, "12-31")
  ) %>%
  transmute(
    yearly_intervals = year_start %--% year_end
  ) %>%
  pull(yearly_intervals)

# How many correspondents in each year?
correspondence <- lapply(years, function(x) {
  year <- year(x$start)
  num <- int_overlaps(intervals, x) %>% sum(na.rm = T)
  tibble(year = year, num = num)
}) %>%
  bind_rows()

pdf("correspondents_per_year.pdf", width = 7, height = 5)
correspondence %>%
  ggplot(aes(year, num)) +
  geom_line() +
  labs(
    title = "The STN corresponded with more and more people in the paper trade until 1777",
    subtitle = "This graph assumes that each correspondence was continuous between its start and end dates.",
    x = "Year",
    y = "Number active correspondences with papetiers and others"
  )
dev.off()

# Can we infer how gappy the correspondence was? Rephrased question: is there a correlation between length of correspondence
# and number of letters?
png("correspondence_scatter_log.png", width=650, height=550)
dates %>%
  ggplot(aes(as.numeric(how_long), number_of_letters)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_y_continuous(trans = 'log') +
  labs(
    title = "There seems to be some correlation between the length of correspondence\nand the number of letters in the archive.",
    subtitle = "The number of letters goes up exponentially with the length of correspondence in days, so on this chart\nthe number of letters is on the log scale.",
    x = "Length of Correspondence (days)",
    y = "Number of Letters in the Archive (log scale)"
  )
dev.off()

png("correspondence_scatter_raw.png", width=650, height=550)
dates %>%
  ggplot(aes(as.numeric(how_long), number_of_letters)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "There seems to be some correlation between the length of correspondence\nand the number of letters in the archive.",
    x = "Length of Correspondence (days)",
    y = "Number of Letters in the Archive"
  )
dev.off()

# Statistical correlation?
dates %>%
  select(how_long, number_of_letters) %>%
  drop_na() %>%
  mutate(how_long = as.numeric(how_long)) %>%
  summarise(cor = cor(how_long, number_of_letters))

dates %>%
  select(how_long, number_of_letters) %>%
  drop_na() %>%
  mutate(how_long = as.numeric(how_long),
         number_of_letters = log(number_of_letters)) %>%
  summarise(cor = cor(how_long, number_of_letters))
