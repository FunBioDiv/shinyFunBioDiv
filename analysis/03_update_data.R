# Read and pre-process the metadata
# The input file was sent by Axelle on 27/01/2026

# Creates one  files for the shiny app

# load the needed package and functions
devtools::load_all()
library(sf) |> suppressWarnings()

meta <- read.csv2("data/dataset_coordinates_wCropSpecies.csv")


# guess the coordinate system
proj <- ifelse(meta$Long > 180, "LAMB93", "WGS84")
# transform LAMB93 to WGS84
lamb93 <- meta[proj %in% "LAMB93", c("Long", "Lat")]
shp_5698 <- st_as_sf(lamb93, coords = c("Long", "Lat"), crs = 5698)
shp_4326 <- st_transform(shp_5698, crs = 4326)
coo_4326 <- st_coordinates(shp_4326)
meta[proj %in% "LAMB93", c("Long", "Lat")] <- coo_4326

# plot(meta$Long, meta$Lat)
sebiopagF <- st_read(
    "data/XYpoint1_transect1_17parcelles_Sebiopag_Toulouse_L93.shp"
)
vcg_4326 <- st_transform(sebiopagF, crs = 4326)
coo_vcg <- data.frame(
    st_coordinates(vcg_4326),
    "Site" = substr(vcg_4326$NOMPLOT, 1, 3)
)

# Replace SEBIOPAG_VcG coordinates
m0 <- match(meta$Plot_ID[meta$Study_ID == "SEBIOPAG_VcG"], coo_vcg$Site)
meta$Lat[meta$Study_ID == "SEBIOPAG_VcG"] <- coo_vcg$Y[m0]
meta$Long[meta$Study_ID == "SEBIOPAG_VcG"] <- coo_vcg$X[m0]

write.csv(meta, "coordinates_year_crop.csv", row.names = FALSE)

meta <- read.csv("coordinates_year_crop.csv")
dim(meta) # 2068
length(unique(paste0(meta$Long, meta$Lat, sep = "_")))

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

range(umeta$Long, na.rm = TRUE)

meta$ID2 <- paste(meta$Study_ID, meta$Plot_ID, meta$Lat, sep = "@")
meta$ID2[!duplicated(meta$ID2)]
table(tapply(meta$Long, meta$ID, sd) > 0)
length(unique(paste0(
    meta$Study_ID,
    meta$Plot_ID,
    meta$Long,
    meta$Lat,
    sep = "_"
)))

# check previous coordinates
# meta0 <- read.csv2("data/dataset_coordinates.csv")
# meta0$Study_ID <- gsub("SEBIOPAG _BVD", "SEBIOPAG_BVD", meta0$Study_ID)
# meta0$Study_ID[!meta0$Study_ID %in% meta$Study_ID]
# meta$Study_ID[!meta$Study_ID %in% meta0$Study_ID]

meta$ID <- paste(meta$Study_ID, meta$Plot_ID, meta$Year, sep = "@")
meta0$ID <- paste(meta0$Study_ID, meta0$Plot_ID, meta0$Year, sep = "@")
# plot(meta$Lat, meta0$Lat[match(meta$ID, meta0$ID)])
boxplot(meta$Lat - meta0$Lat[match(meta$ID, meta0$ID)])
# plot(meta$Long, meta0$Long[match(meta$ID, meta0$ID)])
boxplot(meta$Long - meta0$Long[match(meta$ID, meta0$ID)])

pts <- terra::vect("data/fields_FR.gpkg")
table(pts$Study_ID)
table(meta$Study_ID)
# pts$ID <- paste(pts$Study_ID, pts$Site, pts$Year, sep = "@")
# pts$ID[!pts$ID %in% meta$ID]
# match(meta$ID, pts$ID)

# check Lepibats coordinates

pts_SEB <- meta[meta$Study_ID == "SEBIOPAG_VcG" & meta$Year == "2023", ]
plot(coo_vcg$X - pts_SEB$Long[match(coo_vcg$Site, pts_SEB$Plot_ID)])
boxplot(coo_vcg$X - pts_SEB$Long[match(coo_vcg$Site, pts_SEB$Plot_ID)])

out <- data.frame(
    "Site" = coo_vcg$Site,
    "Long_Sylvie" = coo_vcg$X,
    "Lat_Sylvie" = coo_vcg$Y,
    "Long_Axelle" = pts_SEB$Long[match(coo_vcg$Site, pts_SEB$Plot_ID)],
    "Lat_Axelle" = pts_SEB$Lat[match(coo_vcg$Site, pts_SEB$Plot_ID)]
)
write.csv(out, "coordinates_SEBIOPAG_VcG.csv", row.names = FALSE)

# pts_m <- pts_SEB[match(coo_vcg$Site, pts_SEB$Plot_ID), ]
plot(
    out$Long_Sylvie,
    out$Lat_Sylvie,
    col = "black",
    pch = 16,
    xlab = "Longitude",
    ylab = "Latitude"
)
points(out$Long_Axelle, out$Lat_Axelle, col = "red", pch = 16)
legend("topleft", c("Sylvie", "Axelle"), pch = 16, col = c("black", "red"))


pts_TLS <- st_read(
    "data/XYpoint1_transect1_17parcelles_Sebiopag_Toulouse_L93.shp"
) |>
    st_transform(crs = 4326)
pol_TLS <- st_read(
    "data/17parcelles_Sebiopag_Toulouse_L93_Dynafor.shp"
) |>
    st_transform(crs = 4326)
pol_cts_TLS <- st_centroid(pol_TLS)

library(mapview)
mapview(terra::vect(pol_TLS)) +
    mapview(pts_TLS) +
    mapview(pol_cts_TLS)
