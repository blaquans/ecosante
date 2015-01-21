# EcoSanté : Démographie des médecins
# URL : http://www.ecosante.fr/index2.php?base=DEPA&langs=FRA&langh=FRA&valeur=MDECLISAEFFMEDBIOMLISAEFFMEDORLALISAEFFMEDPNEULISAEFFMEDNEURLISAEFFMEDRRFOLISAEFFMEDENDOLISAEFFMEDANAPLISAEFFMEDNEFRLISAEFFMEDSTOMLISAEFFMEDUROOLISAEFFMEDNEUCLISAEFFMED&source=1
# URL DataGouv : https://www.data.gouv.fr/fr/datasets/demographie-des-medecins-au-1er-janvier/

library("dplyr")
library("tidyr")
library("stringr")

# Read the data
dm <- read.csv2("data/DEPAMDECLISAEFFMED572.csv", fileEncoding = "latin1", skip = 15, stringsAsFactors = FALSE)

# speciality <- str_trim(sub(pattern = "(.*)ADELI\\:Ens\\.Sal\\+lib\\.$", replacement = "\\1", x = dm[1,2:13]))
# speciality

# Renaming variables
specs <- c("medecins", "biologie_medicale", "pneumologie", "orl", "neurologie", "medecine_physique_readaptation", "nephrologie", "endocrinologie_metabolisme", "urologie", "stomatologie","anatomie_cytologie_pathologique", "neurochirurgie")
names(dm) <- c("geo", paste0("X.", sort(rep(1968:2011, 12)), ".", rep(specs, length(1968:2011))), "X.1", "X.2")

# Tidying data 
gdm <- dm %>% 
  filter(geo != "", grepl(pattern = "Eco-Santé France", x = geo) == FALSE) %>% 
  select(-X.1,-X.2) %>% 
  gather(key, n, X.1968.medecins:X.2011.neurochirurgie) %>% 
  mutate(
    n = as.numeric(n), 
    year = sub(pattern = "X\\.([[:digit:]]{4})\\.(.*)", replacement = "\\1", x = key), 
    speciality = sub(pattern = "X\\.([[:digit:]]{4})\\.(.*)", replacement = "\\2", x = key), 
    code_geo = sub(pattern = "([[:digit:][:upper:]]{3})[[:blank:]](.*)", replacement = "\\1", x = geo), 
    lib_geo = sub(pattern = "([[:digit:][:upper:]]{3})[[:blank:]](.*)", replacement = "\\2", x = geo), 
    geo_level = ifelse(grepl(pattern = "^0", x = geo), "country", 
                       ifelse(grepl(pattern = "^R", x = geo), "region", "departement"))
    ) %>%
  select(-key, -geo) %>% 
  select(geo_level, code_geo, lib_geo, year, speciality, n)

# Exporting data
gdm %>% 
  write.table(file = "data/ecosante_depa.csv", row.names = FALSE, sep = ",", dec = ".")

gdm %>% 
  filter(geo_level == "departement" & year > 2008) %>%
  write.table(file = "data/ecosante_depa_departements.csv", row.names = FALSE, sep = ",", dec = ".")

