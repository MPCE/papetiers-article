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
  
  # Search for roman numerals
  rom_reg <- regex("
                  (?<![abcdefghjknopqrstuwxyz]) # do not find a match if there are any non-roman letters or an 'e.' (i.e.)
                  [ivxlcdm]+  # look for any number of the roman numerals
                  (?![abcdefghjknopqrstuwxyz]|\\.\\s?e|,?\\s\\[i\\.|-) # do not find a match if there are any non-roman letters, an '.e', an 'i.e.' or a hyphen
                   ", comments = T,
                   ignore_case = T)
  roman <- str_match_all(pages, rom_reg)
  
  # roman is a list with a column vector of strings for each book
  roman <- lapply(roman, as.roman) # interpret all roman numerals using built in R function
  roman_totals <- sapply(roman, sum) # then sum all the results
  
  # now extract hindu-arabic numerals
  # need negative lookahead for page range issue (see below)
  # need negative lookahead to ignore numbers followed by 'i.e.', which indicates corrections
  hindu_arabic <- str_match_all(pages, "\\d+(?!]?-|,?\\s\\[i\\.\\s?e\\.\\s?)")
  hindu_arabic_totals <- sapply(hindu_arabic, function(x) return(sum(as.numeric(x))))
  
  # Now extrac the beginnings of page ranges
  # e.g. if [3]-20 appears in the page description, then there are 20 - 3 + 1 = 18 actual pages.
  to_deduct_hindu <- str_match_all(pages, "\\d+(?=]?-)") %>%
    sapply(function(x) return(sum(as.numeric(x)) - length(x)))
  rom_deduct_reg <- regex("
                      (?<![abcdefghjknopqrstuwxyz]|e\\.\\s?]) # do not find a match if there are any non-roman letters or an 'e.' (i.e.)
                      [ivxlcdm]+  # look for any number of the roman numerals
                      (?![abcdefghjknopqrstuwxyz]|\\.\\s?e) # do not find a match if there are any non-roman letters, an '.e' or a hyphen
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
