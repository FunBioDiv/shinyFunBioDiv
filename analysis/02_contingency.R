# load the needed package and functions
devtools::load_all()

# make sure no one is logged in from Google Account
googlesheets4::gs4_deauth()

## A. METADATA
# A.1 Load metadata
url0 <- "https://docs.google.com/spreadsheets/d/1Lz-IBQAPd8RykPj57Nf1Tutb_fQlXg4fG461i29BYIM/"
fullmeta <- googlesheets4::read_sheet(
  url0,
  sheet = 1,
  skip = 2,
  col_types = "c"
)

# A.2 Select relevant variables
keep <- c(1, 4, 5, 6, 9, 11:18, 20:25, 27:30)
meta <- fullmeta[, keep]

# names(meta)
lab <- c(
  "Type",
  "N_fieldyears",
  "service_plant",
  "cultivar_mixture",
  "intercropping",
  "cover_crop",
  "agroforestry",
  "crop_rotation",
  "natural_habitats",
  "crop_diversity",
  "organic",
  "tillage",
  "N_qty",
  "TFI",
  "Yield",
  "crop_type",
  "N_species"
)
names(meta)[c(4:19, 23)] <- lab


# A.3 Simplify the metadata
# table(meta$Study_ID)
meta$Study_ID <- gsub("SEBIOPAG _BVD", "SEBIOPAG_BVD", meta$Study_ID)


# select sampling 'Pest'
table(meta$Response_variables)
# not needed anymore
meta$Response_variables <- gsub("^Pest-", "Pest - ", meta$Response_variables)

meta$Pest <- ifelse(
  grepl("Pest - ", meta$Response_variables),
  gsub("Pest - ", "", meta$Response_variables),
  ""
) |>
  tolower()

meta$NE <- ifelse(
  grepl("NE - ", meta$Response_variables),
  gsub("NE - ", "", meta$Response_variables),
  ""
)

meta$Pests <- ifelse(
  as.numeric(gsub(">", "", meta$N_species)) > 1,
  paste0(meta$Pest, "s"),
  meta$Pest
)

# simplify the dataset with one line per Study_ID
smalldf <- data.frame(
  "Study_ID" = sort(unique(meta$Study_ID)),
  "Sampling" = tapply(
    firstup(meta$Pest_sampling_method),
    meta$Study_ID,
    concat
  ),
  "Resp_pest" = tapply(meta$Pest, meta$Study_ID, concat),
  "Resp_pests" = tapply(meta$Pests, meta$Study_ID, concat),
  "N_fieldyears" = tapply(
    as.numeric(meta$N_fieldyears),
    meta$Study_ID,
    max
  ),
  "Pest_abundance" = tapply(
    meta$Pest_sampling_method %in% "Pest - abundance/freq/obs",
    meta$Study_ID,
    any
  ),
  "service_plant" = as.numeric(tapply(
    !meta$service_plant %in% "no",
    meta$Study_ID,
    any
  )),
  "cultivar_mixture" = as.numeric(tapply(
    !meta$cultivar_mixture %in% "no",
    meta$Study_ID,
    any
  )),
  "intercropping" = as.numeric(tapply(
    !meta$intercropping %in% "no",
    meta$Study_ID,
    any
  )),
  "cover_crop" = as.numeric(tapply(
    !meta$cover_crop %in% "no",
    meta$Study_ID,
    any
  )),
  "agroforestry" = as.numeric(tapply(
    !meta$agroforestry %in% "no",
    meta$Study_ID,
    any
  )),
  "crop_rotation" = as.numeric(tapply(
    !meta$crop_rotation %in% "no",
    meta$Study_ID,
    any
  )),
  "natural_habitats" = as.numeric(tapply(
    !meta$natural_habitats %in% "no",
    meta$Study_ID,
    any
  )),
  "crop_diversity" = as.numeric(tapply(
    !meta$crop_diversity %in% "no",
    meta$Study_ID,
    any
  ))
)


smalldf$Pestcat <- smalldf$Resp_pest
multi <- grepl(",", smalldf$Resp_pest)
smalldf$Pestcat[multi] <- paste0("2_", smalldf$Pestcat[multi])

plural <- grepl("s$", smalldf$Resp_pests)
smalldf$Pestcat[!multi & plural] <- paste0(
  "2_",
  smalldf$Pestcat[!multi & plural]
)
smalldf$Pestcat[!multi & !plural] <- paste0(
  "1_",
  smalldf$Pestcat[!multi & !plural]
)
smalldf$Pestcat <- gsub(
  "2_insect, pathogen, weed",
  "3_insect, pathogen, weed",
  smalldf$Pestcat
)

smalldf$Pestcat <- factor(
  smalldf$Pestcat,
  levels = c(
    "1_insect",
    "1_pathogen",
    "1_weed",
    "2_insect",
    "2_pathogen",
    "2_weed",
    "2_insect, pathogen",
    "2_insect, weed",
    "2_pathogen, weed",
    "3_insect, pathogen, weed"
  )
)


write.csv(
  smalldf[, c(1, 15, 5:14)],
  file = here::here("app", "data", "cont_table.csv"),
  row.names = FALSE
)
