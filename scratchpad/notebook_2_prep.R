# Prep for notebook 2

# Load libraries
library(DBI)
library(RMySQL)
library(tidyverse)
library(magrittr)
library(lubridate)

# Create connection object
manuscripts <- dbConnect(MySQL(), user="root", dbname="manuscripts", host="localhost")

# Import editions table
editions <- manuscripts %>%
  dbSendQuery("SELECT * FROM books") %>%
  fetch(n = Inf) %>%
  as_tibble()

# At first, we only want to look at editions actually manufactured by the STN. Here's how.
# The editions have all been categorised. Three of these categories entail that the STN actually manufactured the book.
stn_edition_types <- c("STN editions", "Livres en Société: STN joint editions", "Commissioned STN edition")

stn_editions <- editions %>%
  filter(edition_type %in% stn_edition_types) %>%
  # Next we need to clean the year data, which is currently stored as strings
  mutate(
    stated_year = str_match(stated_publication_years, "[0-9]{4}"), # extract earliest year from stated years
    stated_year = as.integer(stated_year), # convert to numeric
    actual_year = str_match(actual_publication_years, "[0-9]{4}"), # extract earliest year from actual years
    actual_year = as.integer(actual_year), # convert to numeric
    year = coalesce(actual_year, stated_year) # combine data, keep actual year if it is known
  ) %>%
  select(-stated_year, -actual_year) # drop columns used on the way

# Next we need to calculate the number of sheets used for each edition. There is alreay a column book_sheets, but
# it is incomplete and appears to have been done manually.

# This next function finds any page numbers in the 'pages' field and adds them up:
sum_pages <- function(pages) {
  #############
  #
  # Given a character vector of page number information, this finds any Roman or Hindu-Arabic numerals and sums them.
  #
  # depends:
  #   stringr (part of the tidyverse)
  #
  # params:
  #   pages: a character vector
  #
  # returns:
  #   num_pages: the total pages
  #
  #############
  
  require(stringr)
  
  # Search for roman numerals
  rom_reg <- regex("
                  (?<!b)    # do not find a match if there is a 'b' in front (blank pages are denoted 'bl.' in this field)
                  [ivxlcdm]+  # look for any number of the roman numerals
                   ", comments = T,
                   ignore_case = T)
  roman <- str_match_all(pages, rom_reg)
  
  # roman is a list with a column vector of strings for each book
  roman <- lapply(roman, as.roman) # interpret all roman numerals using built in R function
  roman_totals <- sapply(roman, sum) # then sum all the results to get the total roman numeral pages for each book
  
  # now extract hindu-arabic numerals
  hindu_arabic <- str_match_all(pages, "\\d+")
  hindu_arabic_totals <- sapply(hindu_arabic, function(x) sum(as.numeric(x)))
  
  # add these two amounts to get the total pages for each book
  num_pages <- roman_totals + hindu_arabic_totals
  
  return(num_pages)
}


stn_editions %<>%
  mutate(
    # First let's get the format of each book as a number:
    edition = str_replace(edition, "Folio", "2"), # if the text 'folio' has been used, replace with the number of leaves
    edition = as.integer(edition),
    # Next let's get the total pages according to the 'pages' column (which seems to be the most accurate)
    total_pages = sum_pages(pages),
    # Now let's estimate the number of sheets according to the total_pages and the format
    # NB: this is the total number of sheets across all volumes of the book
    total_sheets = total_pages / edition / 2
  )


# The final step is to estimate how many of each book was produced. A good proxy is the number of each book actually sold. In fact,
# the number sold may be more relevant to the amount of paper consumed than the number produced. A book that was produced and not sold
# could conceivably have been pulped and the paper reused. Only if a book was sold would the STN lose access to the paper from which it
# was made.

# Get order and transaction data. Each order consists of many transactions.
orders <- manuscripts %>%
  dbSendQuery("SELECT * FROM orders") %>%
  fetch(n = Inf) %>%
  as_tibble()

transactions <- manuscripts %>%
  dbSendQuery("SELECT * FROM transactions") %>%
  fetch(n = Inf) %>%
  as_tibble() %>%
  filter(str_detect(direction_of_transaction, regex("out", ignore_case = T))) %>% # only want books sent out/sold.
  rename(num_vols_exchanged = total_number_of_volumes) %>% # rename this confusing column
  mutate(num_vols_exchanged = abs(num_vols_exchanged)) %>% # all out transactions are negative numbers
  right_join(orders, by = "order_code") %>% # add order time (time, location etc. of transaction)
  mutate(order_year = as.numeric(str_extract(date, "\\d{4}"))) %>% # extract year of order from date column
  right_join(stn_editions, by = "book_code") %>% # add book information to the transaction data. The right join drops rows for non-STN editions.
  mutate(sheets_needed = num_vols_exchanged / number_of_volumes * total_sheets) %>% # calculate total sheets for each transaction
  select(transaction_code, book_code, full_book_title, total_pages, edition, total_sheets, number_of_volumes, num_vols_exchanged, sheets_needed, everything()) # reorder columns

# Okay, now for some time-series analysis.

# Number of sheets over time:
transactions %>%
  group_by(order_year) %>%
  summarise(sheets_needed = sum(sheets_needed, na.rm = T)) %T>%
  print(n = Inf) %>%
  ggplot(aes(order_year, sheets_needed)) +
  geom_line() +
  scale_y_continuous() +
  labs(
    x = "Year",
    y = "Total sheets of all books sold",
    title = "Paper requirements peaked in the early 1780s"
  )

# So messy...
edition_analysis <- transactions %>%
  mutate(
    year = coalesce(year, as.integer(order_year)),
    sheets_known = !is.na(total_sheets), # create mask for books we know the sheets of
    known_vols = num_vols_exchanged * sheets_known # Calculate number of known vols
  ) %>% # Use year of sale if year of edition not known
  group_by(year) %>%
  summarise(
    sheets_needed = sum(sheets_needed, na.rm = T), # How many sheets needed for the ones we know the amount of?
    known_vols = sum(known_vols, na.rm = T)/sum(num_vols_exchanged, na.rm = T) # What percentage of sold volumes do we not know?
  ) %>%
  mutate(revised = sheets_needed / known_vols) %>%
  select(-known_vols) %>%
  drop_na() %T>%
  write_excel_csv("data/sheets_by_edition_year.csv")

encyclopedie <- tibble(
  year = c(1777, 1778, 1779),
  encyclopedie = c(540000, 2208250, 116000)
)


# Alternative time-scale: by the year the edition was produced (also include revised estimate using below method)
pdf("data/sheets_by_edition_year.pdf", width = 7, height = 4)
edition_analysis %>%
  gather(metric, measure, -year) %>%
  mutate(measure = measure/1000) %>%
  print(n = Inf) %>%
  ggplot(aes(year, measure, colour = metric)) +
  geom_line() +
  scale_y_continuous() +
  scale_colour_discrete(labels = c("Revised\nestimate", "Raw estimate")) +
  labs(
    x = "Year produced (or sold, if production year unknown)",
    y = "Total sheets of all books produced ('000s)",
    title = "Paper requirements peaked in the late 1770s and early 1780s",
    colour = "Metric"
  )
dev.off()

pdf("data/including_encyl.pdf", width = 7, height = 4)
edition_analysis %>%
  left_join(encyclopedie, by = "year") %>%
  mutate(inc_encyc = (encyclopedie + revised)[encyclopedie != 0]) %>%
  select(-encyclopedie) %>%
  gather(metric, measure, -year) %>%
  mutate(measure = measure / 1000) %>%
  drop_na() %>%
  print(n = Inf) %>%
  ggplot(aes(year, measure, colour = metric)) +
  geom_line() +
  scale_y_continuous() +
  scale_colour_discrete(labels = c("Including\nEncyclopedie", "Revised\nestimate", "Raw estimate")) +
  labs(
    x = "Year produced (or sold, if production year unknown)",
    y = "Total sheets of all books produced ('000s)",
    title = "Paper requirements peaked in the late 1770s and early 1780s",
    colour = "Metric"
  )
dev.off()


# We can verify that these trends represent something real by seeing if the number of sheets correlates to the number of
# volumes. If so, then we can more confidently assume that the average number of sheets per volume did not alter much
# year-on-year
sheets_v_volumes <- transactions %>%
  group_by(order_year) %>%
  summarise(sheets_needed = sum(sheets_needed, na.rm = T),
            num_vols_exchanged = sum(num_vols_exchanged, na.rm = T)) %>%
  drop_na() %>%
  mutate_at(2:3, log) # convert to log scale

# There is an extremely strong correlation:
sheet_vol_cor <- cor.test(sheets_v_volumes$sheets_needed, sheets_v_volumes$num_vols_exchanged, method = "pearson", alternative = "greater")

# Is the data parametric?
transactions %>%
  select(sheets_needed, num_vols_exchanged) %>%
  mutate_all(log) %>%
  gather(metric, score) %>%
  drop_na() %>%
  ggplot(aes(score)) +
  facet_wrap(~metric) +
  geom_histogram(bins = 40) +
  labs(
    title = "The two distributions are roughly normal on the log scale.",
    subtitle = "Number of volumes exchanged and sheets needed per transaction.",
    x = "Natural logarithm of volumes (left) or sheets (right)"
  )
# The data does seem to be drawn from a more-or-less normal distribution.

# This is obvious from the graph:
model <- lm(sheets_v_volumes$sheets_needed ~ sheets_v_volumes$num_vols_exchanged)
sheets_v_volumes %>%
  ggplot(aes(num_vols_exchanged, sheets_needed)) +
  geom_point() +
  geom_abline(intercept = model$coefficients[1], slope =  model$coefficients[2], colour = "red") +
  labs(
    title = paste0(
      "There is an extremely strong correlation between\n",
      "the number of volumes actually exchanged and the estimated\n",
      "number of sheets required. (p < 10^-13)"
    )
  )

# Is this because the books of unknown sheets/unknown volumes both come from the same unrepresentative distribution? What is
# the overlap between the books whose total_sheets we don't know and the transactions whose num_volumes_exchanged we don't know?
transactions %>%
  transmute(
    sheets_known = !is.na(total_sheets),
    vols_traded_known = !is.na(num_vols_exchanged)
  ) %>%
  group_by(sheets_known, vols_traded_known) %>%
  summarise(n = n())
# It appears there is no overlap. We know the volumes traded for basically every transaction, and 

# Why is 1782 so high?
transactions %>%
  filter(order_year == 1782) %>%
  group_by(book_code, full_book_title, edition) %>%
  summarise(num_vols_exchanged = sum(num_vols_exchanged, na.rm = T),
            sheets_needed = sum(sheets_needed, na.rm = T)) %>%
  arrange(desc(sheets_needed))
  

# Does the number of books whose format we don't know vary from year to year?
transactions %>%
  group_by(order_year, sheets_known = !is.na(sheets_needed)) %>%
  summarise(num_vols_exchanged = sum(num_vols_exchanged, na.rm = T)) %>%
  ggplot(aes(order_year, num_vols_exchanged, fill = sheets_known)) +
  geom_col(position = "dodge") + 
  labs(
    title = "In all years except 1785 and 1786, we know the number of sheets\nfor more books than not.",
    y = "Number of volumes sold",
    x = "Year",
    fill = "Number of\nsheets known?"
  )
  
# How does the ratio of known to unknown change?
transactions %>%
  group_by(order_year, sheets_known = !is.na(sheets_needed)) %>%
  summarise(num_vols_exchanged = sum(num_vols_exchanged, na.rm = T)) %>%
  mutate(total_vols = sum(num_vols_exchanged)) %>%
  mutate(prop = num_vols_exchanged / total_vols) %>%
  filter(sheets_known == FALSE) %T>%
  print(n = Inf) %>%
  ggplot(aes(order_year, prop)) +
  geom_col() +
  expand_limits(y = 1) + 
  labs(
    title = "For 18 of the 24 years, we know the number of sheets\nfor at least 70% of the volumes sold.",
    x = "Year",
    y = "Proportion of sold volumes with known number of sheets"
  )

# Did they prefer to publish in different formats at different times?

# Try a column graph
transactions %>%
  mutate(order_year = cut(order_year, 5, labels = c("1770-74", "1775-79", "1780-1784", "1785-1789", "1790-94"))) %>% # split data into three-year increments to make it easier to visualise
  group_by(order_year, edition) %>%
  summarise(num_vols_exchanged = sum(num_vols_exchanged)) %>%
  replace_na(list(edition = "Unknown")) %>% # replace NAs in edition column
  drop_na() %T>% # drop the rows with missing years
  print(n = Inf) %>%
  ggplot(aes(order_year, num_vols_exchanged, fill = as.character(edition))) +
  geom_col(position = "dodge")

# Or a faceted lined graph
transactions %>%
  filter(!is.na(edition)) %>%
  group_by(order_year, edition) %>%
  summarise(num_vols_exchanged = sum(num_vols_exchanged)) %>%
  ggplot(aes(order_year, num_vols_exchanged)) +
  facet_wrap(~edition) +
  geom_line()

#indexed: (doesn't work - data too variable by far)
transactions %>%
  filter(!is.na(edition)) %>%
  group_by(order_year, edition) %>%
  summarise(num_vols_exchanged = sum(num_vols_exchanged)) %>%
  group_by(edition) %>%
  mutate(num_vols_exchanged = num_vols_exchanged / num_vols_exchanged[which.min(order_year)] * 100) %>%
  ggplot(aes(order_year, num_vols_exchanged)) +
  facet_wrap(~edition) +
  geom_line()


# Distribution of number of sheets in each year.
transactions %>%
  mutate(books = num_vols_exchanged/number_of_volumes,
         books = ceiling(books)) %>%
  filter(!is.na(books)) %>%
  uncount(books) %>%
  select(order_year, total_sheets) %>%
  filter(total_sheets < 100) %>%
  ggplot(aes(factor(order_year), total_sheets)) +
  geom_violin()

# Another way ...
transactions %>%
  mutate(sheets_per_vol = total_sheets/number_of_volumes) %>%
  select(order_year, num_vols_exchanged, sheets_per_vol) %>%
  drop_na() %>%
  uncount(num_vols_exchanged) %>%
  group_by(order_year) %>%
  summarise(median = median(sheets_per_vol)) %>%
  print(n = Inf)
  

# How does my estimate of sheets compare to Mark's more accurate assessment?
stn_editions

# Pages per volume by format:
stn_editions %>%
  filter(!is.na(edition) & !is.na(total_pages)) %>%
  group_by(edition) %>%
  mutate(ppv = total_pages/number_of_volumes) %>%
  summarise(ppv_mean = mean(ppv),
            sheets_mean = mean(total_sheets/number_of_volumes))
  
