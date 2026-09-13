
#### Packages ----
rm(list = ls()); gc(); gc(); gc()
groundhog::groundhog.library(c("tidyverse", "yaml"), "2025-04-01")
cb <- function(x) {write.table(x, "clipboard-16384", sep = "\t", row.names = FALSE)}


#### Pokemon Data ----
pokemon_data <- data.frame()
fname_list <- dir("./data/pokemon/base_stats/")
for(fname in fname_list) {
  temp_fname <- paste0("./data/pokemon/base_stats/", fname)
  temp_data <- readLines(temp_fname)
  temp_data <- str_trim(temp_data)
  temp_pokemon <- substr(temp_data[1], 
                         regexpr("db ", temp_data[1], fixed = T)[1] + 3, 
                         regexpr(" ; ", temp_data[1], fixed = T)[1] - 1)
  temp_number <- substr(temp_data[1], 
                        regexpr(" ;", temp_data[1], fixed = T)[1] + 3,
                        nchar(temp_data[1]))
  temp_type <- substr(temp_data[6], 
                      regexpr("db ", temp_data[6], fixed = T)[1] + 3, 
                      regexpr(", ", temp_data[6], fixed = T)[1] - 1)
  temp_catchrate <- substr(temp_data[7], 
                           regexpr("db ", temp_data[7], fixed = T)[1] + 3, 
                           regexpr(" ; ", temp_data[7], fixed = T)[1] - 1)
  temp_baseexp <- substr(temp_data[8], 
                         regexpr("db ", temp_data[8], fixed = T)[1] + 3, 
                         regexpr(" ; ", temp_data[8], fixed = T)[1] - 1)
  temp_stats <- str_trim(substr(temp_data[3], 
                                regexpr("db ", temp_data[3], fixed = T)[1] + 3, 
                                nchar(temp_data[3])))
  temp_stats <- str_split(temp_stats, ",", simplify = T)
  colnames(temp_stats) <- c("hp", "atk", "def", "spd", "sat", "sdf")
  temp_df <- data.frame(temp_stats) %>%
    mutate(pokemon = temp_pokemon,
           pokemon_number = temp_number,
           type = temp_type,
           catchrate = temp_catchrate,
           baseexp = temp_baseexp)
  if(length(temp_df) > 0) {
    pokemon_data <- pokemon_data %>% bind_rows(temp_df)
  }
}


#### Wild Encounters Data ----
johto_grass <- str_trim(readLines("./data/wild/johto_grass.asm")[-(1:4)])
jg_encounters <- data.frame(entry = johto_grass[sort(unlist(lapply(c(4:10, 12:18, 20:26), function(x) seq(x, length(johto_grass) - 1, 28))))])
jg_encounters$map <- rep(johto_grass[seq(1, length(johto_grass) - 1, 28)], each = 21)
jg_encounters$daypart <- rep(johto_grass[sort(unlist(lapply(c(3, 11, 19), function(x) seq(x, length(johto_grass) - 1, 28))))], each = 7)
jg_encounters$fname <- "johto_grass"
kanto_grass <- str_trim(readLines("./data/wild/kanto_grass.asm")[-(1:4)])
kg_encounters <- data.frame(entry = kanto_grass[sort(unlist(lapply(c(4:10, 12:18, 20:26), function(x) seq(x, length(kanto_grass) - 1, 28))))])
kg_encounters$map <- rep(kanto_grass[seq(1, length(kanto_grass) - 1, 28)], each = 21)
kg_encounters$daypart <- rep(kanto_grass[sort(unlist(lapply(c(3, 11, 19), function(x) seq(x, length(kanto_grass) - 1, 28))))], each = 7)
kg_encounters$fname <- "kanto_grass"
johto_water <- str_trim(readLines("./data/wild/johto_water.asm")[-(1:4)])
jw_encounters <- data.frame(entry = johto_water[sort(unlist(lapply(3:5, function(x) seq(x, length(johto_water) - 1, 7))))])
jw_encounters$map <- rep(johto_water[seq(1, length(johto_water) - 1, 7)], each = 3)
jw_encounters$daypart <- "; all"
jw_encounters$fname <- "johto_water"
kanto_water <- str_trim(readLines("./data/wild/kanto_water.asm")[-(1:4)])
kw_encounters <- data.frame(entry = kanto_water[sort(unlist(lapply(3:5, function(x) seq(x, length(kanto_water) - 1, 7))))])
kw_encounters$map <- rep(kanto_water[seq(1, length(kanto_water) - 1, 7)], each = 3)
kw_encounters$daypart <- "; all"
kw_encounters$fname <- "kanto_water"
wild_maps_data <- rbind(jg_encounters, kg_encounters, jw_encounters, kw_encounters) %>%
  as_tibble() %>%
  mutate(entry = substr(entry, 4, nchar(entry)),
         map = substr(map, regexpr(" ", map, fixed = T)[1] + 1, nchar(map)),
         daypart = substr(daypart, 3, nchar(daypart))) %>%
    separate_wider_delim(cols = entry, delim = ", ", names = c("lvl", "pokemon")) %>%
    mutate(lvl = as.numeric(lvl))
wild_maps_data <- wild_maps_data %>% group_by(fname, map, daypart) %>% mutate(original_order = row_number())
wild_maps_data_summ <- wild_maps_data %>%
  group_by(pokemon) %>% 
  summarise(n_maps = n_distinct(map), n_dayparts = n_distinct(daypart), 
            avg_lvl = mean(lvl), min_lvl = min(lvl), max_lvl = max(lvl))


#### Check What's Fishable ----
fishasm <- str_trim(readLines("./data/wild/fish.asm")[-(1:26)])
fishable_pokemon <- data.frame()
for(pkm in pokemon_data$pokemon) {
  if(any(grepl(pkm, fishasm, fixed = T))) {
    fishable_pokemon <- fishable_pokemon %>% bind_rows(data.frame(pokemon = pkm, fishable = T))
  } else {
    fishable_pokemon <- fishable_pokemon %>% bind_rows(data.frame(pokemon = pkm, fishable = F))
  }
}


#### Combined Pokemon Data ----
pokemon_data_combined <- pokemon_data %>%
  left_join(wild_maps_data_summ, by = "pokemon") %>%
  left_join(fishable_pokemon, by = "pokemon") %>%
  arrange(pokemon_number, desc(hp)) %>%
  select(pokemon_number, pokemon, type, everything())
pokemon_data_combined %>% cb()


#### Estimated Map Order ----
maps_order <- wild_maps_data %>%
  group_by(fname, map) %>%
  summarise(n_pokemon = n_distinct(pokemon), 
            avg_lvl = mean(lvl), min_lvl = min(lvl), max_lvl = max(lvl),
            .groups = "drop") %>%
  arrange(substr(fname, 1, 1), min_lvl, avg_lvl, max_lvl) %>%
  mutate(map_order = row_number())


#### Duplicate Pokemon in Maps ----
wild_maps_duplicates <- wild_maps_data %>%
  left_join(maps_order %>% select(map, map_order), by = c("map")) %>%
  left_join(pokemon_data_combined %>% select(pokemon, type), by = c("pokemon")) %>%
  arrange(map_order, map, pokemon, original_order, desc(lvl)) %>%
  group_by(map, pokemon) %>%
  mutate(map_pokemon_order = row_number()) %>%
  arrange(map_order, original_order)
wild_maps_duplicates %>% cb()








