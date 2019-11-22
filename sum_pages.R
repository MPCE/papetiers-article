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
  
  # Remove beginning unnumbered page data
  # If we come across something like [3]-20, this presumably means 20 pages, three of which are blank.
  # In this case, we simply want to delete the number in square brackets.
  # Note that the digits can sometime also be roman numerals
  begin_rgx <- regex("\\[[\\divxlc]+\\]-", ignore_case = T)
  pages %<>% str_remove_all(begin_rgx)
  
  # Extract ununmbered/blank leaves data
  # In the case that a seperate number is provided in square brackets, it usually denotes blank leaves,
  # e.g. [3], xii, 24 means (3 x 2) + 12 + 24 = 42 pages
  leaves_rgx <- regex("\\[(\\d+)\\D{0,7}\\]")
  leaves <- pages %>% 
    # str_match_all outputs a list of matrices. Capture group is in second column
    str_match_all(leaves_rgx) %>% 
    # sum the pages for each book and multiply by two to get leaves
    map_dbl(function(mat) sum(as.numeric(mat[,2]))*2) %>% 
    replace_na(0)
  # Now scrub all the numbers in parentheses
  pages %<>% str_remove_all(leaves_rgx)
  
  # Search for roman numerals
  rom_reg <- regex("[ivxlc]+", ignore_case = T)
  roman <- str_extract_all(pages, rom_reg) %>% 
    # now interpret all strings in list as roman numerals
    map(as.roman) %>% 
    # and sum them together
    map_int(sum) %>% 
    replace_na(0)
  
  # now extract hindu-arabic numerals
  hindu_arabic <- str_match_all(pages, "\\d+") %>% 
    map(as.integer) %>% 
    map_int(sum) %>% 
    replace_na(0)
  
  # now we have three vectors that can simply be combined
  num_pages <- leaves + roman + hindu_arabic
  
  return(num_pages)
}
