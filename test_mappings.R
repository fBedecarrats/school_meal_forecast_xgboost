
# Completing school-cafet mapping -----------------------------------------------

# fetch school-cafet mapping
map_schools <- dt()$map_schools
# list school names appearing in headcounts
eff_etabs <- dt()$effs %>%
  dplyr::select(ecole) %>%
  unique()
# identify schools in headcounts that are missing in school-cafet mapping
miss_eff_etabs <- eff_etabs %>%
  dplyr::filter(!(ecole %in% map_schools$ecole))

# completing missing data
new_school_map <- dplyr::tribble(
  ~ecole,                                          ~cantine_nom,      ~cantine_type,
  "MARSAUDERIES bilingue \"\"Français/Breton\"\"", "MARSAUDERIES E",  "E",
  "BATIGNOLLES bilingue \"\"Français/Breton\"\"",  "BATIGNOLLES",     "M/E",
  "JOSEPHINE BAKER",                               "JOSEPHINE BAKER", "M/E",
  "CAMILLE CLAUDEL \"bilingue breton\"",           "CAMILLE CLAUDEL", "M",
  "LELOUP BOUHIER PRIMAIRE",                       "LELOUP BOUHIER",  "E"
)
# appending and saving
map_schools2 <- map_schools %>%
  dplyr::bind_rows(new_school_map)
map_schools2 %>%
  readr::write_csv(index$path[index$name == "map_schools"])


# Completing cafet list ---------------------------------------------------

cafets <- dt()$cafets

miss_cafets <- map_schools %>%
  dplyr::filter(!(cantine_nom %in% cafets$cantine_nom))

new_cafets <- dplyr::tribble(
  ~cantine_nom,      ~cantine_type, ~secteur,
  "JOSEPHINE BAKER", "M/E",         "Sud",
  "LELOUP BOUHIER",  "E",           "Ouest"
)
cafets2 <- cafets %>%
  dplyr::bind_rows(new_cafets)
cafets2 %>%
  readr::write_csv(index$path[index$name == "cafets"])

# Completing freq mapping -------------------------------------------------
# 1 établissement(s) mentionné(s) dans le champ cantine_nom du fichier dt()$cafets$cantine_nom mais pas dans le champ cantine_nom du fichier dt()$map_freqs$cantine_nom : LELOUP BOUHIER
map_freq <- dt()$map_freq
miss_map_freq <- cafets %>%
  dplyr::filter(!(cantine_nom %in% map_freq$cantine_nom))

# Leloup bouhier
# baker
effs <- dt()$effs

freqs_etabs <- dt()$freqs %>%
  dplyr::select(site_nom, site_type) %>%
  unique()
map_freq <- dt()$map_freq