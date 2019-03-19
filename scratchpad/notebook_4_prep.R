# Notebok 4 prep

# Part 1: How many out-letters do we have for our papermakers?
source("init.R")

clients <- fetch_table(stn, "clients")
clients_professions <- fetch_table(stn, "clients_professions")
ms <- fetch_table(stn, "clients_correspondence_manuscripts")
prof <- fetch_table(stn, "professions") %>% select(profession_code, profession_type)

papetier_codes <- c("pf171", "pf069", "pf244")

conv_ms <- function(x) {
  key <- c("CLA1"="1095", "CLA2"="1096", "CLC1"="1098", "CLC2"="1099", "CLE"="1101", "CLF"="1103",
            "CLG2"="1105", "CLH"="1108", "CLI"="1109", "CLL"="1110", "CLM"="1112")
  return(key[x])
}

# Okay, so where are all the out-letters for the papetiers?
letters <- clients %>%
  left_join(clients_professions, by = "client_code") %>%
  filter(profession_code %in% papetier_codes) %>%
  # Add other professions for each client
  select(-profession_code) %>%
  left_join(clients_professions, by = "client_code") %>%
  left_join(prof, by = "profession_code") %>%
  group_by(client_code) %>%
  mutate(
    profession_code = paste0(profession_code, collapse = ", "),
    profession_type = paste0(profession_type, collapse = ", ")
  ) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(ms, by = "client_code") %>%
  filter(str_detect(manuscript_numbers, "^CL")) %>%
  select(client_code, client_name, profession_code, profession_type, manuscript_numbers) %>%
  mutate(
    # Get FBTEE's code for the MS
    ms_code = str_extract(manuscript_numbers, "^CL[A-M]?[12]?"),
    # Get the MS number in the archive
    ms_num = conv_ms(ms_code),
    # Get the page references
    page_ref = str_remove(manuscript_numbers, "^CL[A-M]?[12]? ") %>% str_split(", ?")
  ) %>%
  select(-manuscript_numbers) %>%
  unnest() %T>%
  write_excel_csv("papetier_out_letter_locations.csv")
