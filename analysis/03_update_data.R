# Read and pre-process the metadata
# The input file was sent by Axelle on 27/01/2026
# with update from Sylvie Ladet (Sebiopag_VcG) on 17/12/2026
# with update from Frederic Fabre (OSCAR) on 03/04/2026

# Creates one files for the shiny app

# load the needed package and functions
devtools::load_all()
library(sf) |> suppressWarnings()

# 1. Load data from Axelle
meta <- read.csv2("data/dataset_coordinates_wCropSpecies.csv")

# Create an ID per site
ID <- paste(meta$Study_ID, meta$Plot_ID, meta$Year, sep = "_")

# remove duplicates : SEBIOPAG_VcG 2022 and 2023
meta <- meta[!duplicated(ID), ]

# guess the coordinate system
proj <- ifelse(meta$Long > 180, "LAMB93", "WGS84")
# transform LAMB93 to WGS84
lamb93 <- meta[proj %in% "LAMB93", c("Long", "Lat")]
shp_5698 <- st_as_sf(lamb93, coords = c("Long", "Lat"), crs = 5698)
shp_4326 <- st_transform(shp_5698, crs = 4326)
coo_4326 <- st_coordinates(shp_4326)
meta[proj %in% "LAMB93", c("Long", "Lat")] <- coo_4326

# plot(meta$Long, meta$Lat)

# 2. update from Sylvie Ladet (Sebiopag_VcG) on 17/12/2026
sebiopagF <- st_read(
    "data/XYpoint1_transect1_17parcelles_Sebiopag_Toulouse_L93.shp"
)
vcg_4326 <- st_transform(sebiopagF, crs = 4326)
coo_vcg <- data.frame(
    st_coordinates(vcg_4326),
    "Site" = substr(vcg_4326$NOMPLOT, 1, 3)
)

# Replace SEBIOPAG_VcG coordinates
# but not for T08, T18 and T19 which doesn't have coordinates in Sylvie's dataset
sel <- meta$Study_ID == "SEBIOPAG_VcG" & meta$Plot_ID %in% coo_vcg$Site
m0 <- match(meta$Plot_ID[sel], coo_vcg$Site)
meta$Lat[sel] <- coo_vcg$Y[m0]
meta$Long[sel] <- coo_vcg$X[m0]


# 3. update from Frederic Fabre (OSCAR) on 03/04/2026
oscar <- readxl::read_xlsx("data/OSCAR_gps_manquant_VF.xlsx")
rm <- oscar$plot[oscar$latitude %in% c("à supprimer", "Plot arraché")]
oscar <- oscar[!oscar$plot %in% rm, ]
sel <- meta$Study_ID == "OSCAR" & meta$Plot_ID %in% oscar$plot
m1 <- match(meta$Plot_ID[sel], oscar$plot)
meta$Lat[sel] <- as.numeric(oscar$latitude[m1])
meta$Long[sel] <- as.numeric(oscar$longitude[m1])
meta <- meta[!(meta$Study_ID == "OSCAR" & meta$Plot_ID %in% rm), ]

# table(is.na(meta$Lat), meta$Study_ID, useNA = "ifany")
write.csv(
    meta,
    here::here("data", "coordinates_year_crop.csv"),
    row.names = FALSE
)

meta <- read.csv(here::here("data", "coordinates_year_crop.csv"))
dim(meta) # 2059
length(unique(paste0(meta$Long, meta$Lat, sep = "_"))) # 592

meta$ID <- paste(meta$Study_ID, meta$Plot_ID, sep = "@")
uID <- sort(unique(meta$ID))
sid <- strsplit(uID, "@")
mid <- match(uID, meta$ID)
sort_unique <- function(x) paste(sort(unique(x[!is.na(x)])), collapse = "; ")
length_unique <- function(x) length(unique(x[!is.na(x)]))
umeta <- data.frame(
    "ID" = uID,
    "Study_ID" = sapply(sid, function(x) x[[1]]),
    "Plot_ID" = sapply(sid, function(x) x[[2]]),
    "Lat" = meta$Lat[mid], #tapply(meta$Lat, meta$ID, median, na.rm = TRUE),
    "Long" = meta$Long[mid], #tapply(meta$Long, meta$ID, median, na.rm = TRUE)
    "Years" = tapply(meta$Year, meta$ID, sort_unique),
    "N_years" = tapply(meta$Year, meta$ID, length_unique),
    "Crops" = tapply(meta$Crop_species, meta$ID, sort_unique),
    "N_crops" = tapply(meta$Crop_species, meta$ID, length_unique)
)


write.csv(umeta, here::here("data", "coordinates.csv"), row.names = FALSE)

umeta[!is.na(umeta$Lat), -1] |>
    terra::vect(geom = c("Long", "Lat"), crs = "EPSG:4326") |>
    terra::writeVector(
        here::here("data", "Fields_Unique.gpkg"),
        overwrite = TRUE
    )
