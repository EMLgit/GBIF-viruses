

#Load DwC data with Joel Nitta's dwctaxon package
install.packages("dwctaxon")

library(dwctaxon)
library(readr)
library(tibble)
library(dplyr)


occs <- read_tsv("/Users/elizabethlombardi/Desktop/Research/ESIIL biotic interactions group/GBIF viruses/Pisuviricota_INSDC Host Organisms DB/0072899-240626123714530/occurrence.txt")

#validate
validation_occs <- dct_validate(occs, on_fail = "summary")
validation_occs


#subset full occurrence dataframe just for quick exploration

cols <- c("gbifID", "basisOfRecord", "occurrenceID", "associatedSequences", "associatedTaxa", "occurrenceRemarks", "eventDate", "year", 
          "countryCode", "locality", "decimalLatitude", "decimalLongitude", "taxonID", "scientificName", "kingdom", "phylum", "class", "order", "family", "genus")

occs.sub <- occs %>%
  select(all_of(cols))


#explore the data a bit
table(occs.sub$genus)


#table of associated taxa; the taxa are not standardized at all. This will be an issue.
host.table <- occs.sub %>%
  count(associatedTaxa) %>%
  arrange(desc(n))

#how many rows don't have associatedTaxa values?
sum(is.na(occs.sub$associatedTaxa)) #zero?! nice


#what genera exist?
genus.table <- occs.sub %>%
  count(genus) %>%
  arrange(desc(n))


#potyvirus subset to look at host range in dataset
poty <- occs.sub %>%
  filter(genus == "Potyvirus")

table(poty$associatedTaxa)

poty.hosts <- poty %>%
  count(associatedTaxa) %>%
  arrange(desc(n))

poty.viruses <- poty %>%
  count(scientificName) %>%
  arrange(desc(n))

#MAPPING?
#{r}
library(sf)
library(dplyr)
library(rnaturalearth)
library(ggplot2)
library(countrycode)

# Create world map in Robinson projection
countries <- ne_countries() %>%
  st_transform("ESRI:54030")

#spatial transformation of df
occs.sub.sf <- occs.sub %>% 
  st_as_sf(coords=c("decimalLongitude", "decimalLatitude"), crs = 4326) %>%
  st_transform("ESRI:54030")


#basic map
ggplot() +
  geom_sf(data = countries, fill = "slategrey", color = "gray") +
  geom_sf(data = occs.sub.sf, color = "black", size = 1) +
  coord_sf(crs = "+proj=robin") +
  theme_minimal() +
  labs(title = "Pisuviricota occurrences from INSDC Host Organisms DB")

