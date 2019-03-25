# Init

library(tidyverse)
library(magrittr)
library(DBI)
library(RMySQL)

stn <- dbConnect(MySQL(), dbname = "stn", user = "root", host = "localhost")

fetch_table <- function(con, tbl_name) {
  # Helper function that selects an entire table.
  #
  # Params:
  #   con (connection): the databse where the table is
  #   tbl_name (str): the name of the table in the database
  #
  # Returns:
  #   out (tbl): a tibble of the data
  
  query <- paste0("SELECT * FROM ", tbl_name)
  
  out <- con %>%
    dbSendQuery(query) %>%
    fetch(n = Inf) %>%
    as_tibble()
  
  return(out)
}

sum_pages <- function(pages) {
  #############
  #
  # Given a character vector of page number information, this finds any Roman or
  # Hindu-Arabic numerals and sums them.
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
  
  # Preprocessing to remove errors
  pages <- str_replace_all(pages, "j", "i") # Some typos in a few titles
  pages <- str_replace_all(pages, "\\b\\w+,?\\s\\[i.\\s?e\\.", "") # Account for all the 'i.e.'s
  # Some pages use the hyphens incorrectly, and chain them all together. Fix.
  incorrect_idx <- str_detect(pages, "\\[?\\w+\\]?-\\[?\\w+\\]?-\\[?\\w+\\]?") %>% which()
  pages[incorrect_idx] <- str_replace_all(pages[incorrect_idx], "-", " ")
  
  # Search for roman numerals
  rom_reg <- regex("\\b[ivxlc]+\\b  # look for a substring comprising only Roman numerals
                  (?!\\]?-[ivxlc])  # do not find a match if it is followed by a hyphen
                   ", comments = T,
                   ignore_case = T)
  roman <- str_match_all(pages, rom_reg)
  
  # roman is a list with a column vector of strings for each book
  roman <- lapply(roman, as.roman) # interpret all roman numerals using built in R function
  roman_totals <- sapply(roman, sum) # then sum all the results
  
  # now extract hindu-arabic numerals
  # need negative lookahead for page range issue (see below)
  hindu_arabic <- str_match_all(pages, "\\d+\\b(?!\\]?-)")
  hindu_arabic_totals <- sapply(hindu_arabic, function(x) return(sum(as.numeric(x))))
  
  # Now extrac the beginnings of page ranges
  # e.g. if [3]-20 appears in the page description, then there are 20 - 3 + 1 = 18 actual pages.
  to_deduct_hindu <- str_match_all(pages, "\\d+\\b(?=]?-\\d)") %>%
    sapply(function(x) return(sum(as.numeric(x)) - length(x)))
  rom_deduct_reg <- regex("\\b[ivxlc]+\\b     # look for any number of roman numerals
                      (?=\\]?-\\[?[ivxlc])    # followed by a hyphen
                      ", comments = T,
                      ignore_case = T)
  to_deduct_roman <- str_match_all(pages, rom_deduct_reg) %>%
    lapply(as.roman) %>%
    sapply(function(x) return(sum(as.numeric(x)) - length(x)))
  
  # add these two amounts to get the total pages for each book
  num_pages <- roman_totals + hindu_arabic_totals - to_deduct_hindu - to_deduct_roman
  
  return(num_pages)
}

calculate_sheets <- function(pages, leaves) {
  # Applies the formula for calculating the number of sheets
  return(ceiling(pages / (2 * leaves)))
}
