# Read and pre-process the metadata
# The input file is originally at :
# https://docs.google.com/spreadsheets/d/1Lz-IBQAPd8RykPj57Nf1Tutb_fQlXg4fG461i29BYIM
# Creates two files for the shiny app
#   app/data/meta.csv : simplified and corrected metadata
#   app/data/poly.shp : shapefile with convex hull of sites per dataset

# load the needed package and functions
devtools::load_all()

# make sure no one is logged in from Google Account
googlesheets4::gs4_deauth()

## A. METADATA
# A.1 Load metadata
url0 <- "https://docs.google.com/spreadsheets/d/1Lz-IBQAPd8RykPj57Nf1Tutb_fQlXg4fG461i29BYIM/"
meta <- googlesheets4::read_sheet(url0, sheet = 1, skip = 2, col_types = "c")
# meta <- readxl::read_xlsx(
#   here::here("data", "FunBioDiv_MetaData_Data.xlsx"),
#   sheet = 1,
#   skip = 2
# )

# dim(meta)
# names(meta)

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
# clean SEBIOPAG_BVD
meta$Study_ID <- gsub("SEBIOPAG _BVD", "SEBIOPAG_BVD", meta$Study_ID)

# pre-processing for years
meta$Year <- gsub(" à ", "-", meta$Year)
meta$Year <- gsub(",", ";", meta$Year)
meta$Year <- gsub("-", ";", meta$Year)
meta$Year <- gsub(" ", "", meta$Year)


allyears <- strsplit(meta$Year, ";")
df_allyear <- data.frame(
  "ID" = rep(meta$Study_ID, sapply(allyears, length)),
  "yr" = as.numeric(unlist(allyears))
)

# select sampling 'Pest'
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

# simplify the dataset with one line per Study_ID
smalldf <- data.frame(
  "Study_ID" = sort(unique(meta$Study_ID)),
  "Years" = tapply(df_allyear$yr, df_allyear$ID, minmax, na.rm = TRUE),
  "Country" = tapply(firstup(meta$Country), meta$Study_ID, concat),
  "Type" = tapply(firstup(meta$Type), meta$Study_ID, concat),
  "N_fieldyears" = tapply(meta$N_fieldyears, meta$Study_ID, max),
  "Resp_pest" = tapply(meta$Pest, meta$Study_ID, concat),
  "Resp_NE" = tapply(meta$NE, meta$Study_ID, concat),
  "Organic" = tapply(meta$organic, meta$Study_ID, concat),
  "Tillage" = tapply(meta$tillage, meta$Study_ID, concat),
  "N_qty" = tapply(meta$N_qty, meta$Study_ID, concat),
  "TFI" = tapply(meta$TFI, meta$Study_ID, concat),
  "Yield" = tapply(meta$Yield, meta$Study_ID, concat),
  "Crop_type" = tapply(meta$crop_type, meta$Study_ID, concat)
)

# get the practices
df_practices <- meta[6:13]
df_practices[is.na(df_practices)] <- "no"
df_practices[df_practices == "maybe"] <- "yes"
df_practices[df_practices == "yes (few)"] <- "yes"
ind <- which(df_practices == "yes", arr.ind = TRUE)
smallag <- data.frame(
  "ID" = meta$Study_ID[ind[, 1]],
  "practices" = names(df_practices)[ind[, 2]]
)
practices <- tapply(smallag$practices, smallag$ID, concat)

smalldf$Div_measures <- as.character(
  practices[match(smalldf$Study_ID, names(practices))]
)

# re-order columns
# names(smalldf)
smalldf <- smalldf[, c(1:7, 14, 13, 8:12)]

# A.4 Export the metadata
write.csv(
  smalldf,
  file = here::here("app", "data", "meta.csv"),
  row.names = FALSE
)


## B. Spatial coordinates
# B.1 Load GPS coordinates
library(sf)
gis <- googlesheets4::read_sheet(url0, sheet = 2, skip = 2, col_types = "c", )
# gis <- readxl::read_xlsx(
#   here::here("data", "FunBioDiv_MetaData_Data.xlsx"),
#   sheet = 2,
#   skip = 2
# )
# clean SEBIOPAG_BVD
gis$Study_ID <- gsub("SEBIOPAG _BVD", "SEBIOPAG_BVD", gis$Study_ID)

# B.2 Clean the messy coordinates
gis$X <- as.numeric(gsub(",", ".", gis$X))
gis$Y <- as.numeric(gsub(",", ".", gis$Y))

# invert latitude / longitude in OSCAR project
# table(gis$longitude>40, gis$Study_ID)
inv_coo <- c(
  "SEBIOPAG_VcG",
  "OSCAR",
  "LepiBats",
  "MUESLI",
  "SEBIOPAG_Plaine de Dijon",
  "SEBIOPAG_BVD",
  "DURUM_MIX_GM",
  "FRAMEwork_BVD",
  "PestiRed"
)
gis$longitude <- ifelse(gis$Study_ID %in% inv_coo, unlist(gis$Y), unlist(gis$X))
gis$latitude <- ifelse(gis$Study_ID %in% inv_coo, gis$X, gis$Y)

# issue some are not in WGS84, let's try the most commun
# epsg 4326 (WGS84),
# epsg 3035 (ETRS89_LAEA projection),
# epsg 5698 (LAMB93_IGN69_D066)
proj <- ifelse(gis$longitude > 180, "LAMB93", "WGS84")

# transform LAMB93 to WGS84
lamb93 <- gis[proj %in% "LAMB93", c("longitude", "latitude")]
shp_5698 <- st_as_sf(lamb93, coords = c("longitude", "latitude"), crs = 5698)
shp_4326 <- st_transform(shp_5698, crs = 4326)
coo_4326 <- st_coordinates(shp_4326)
gis[proj %in% "LAMB93", c("longitude", "latitude")] <- coo_4326


# B3. Alter coordinates to safeguard privacy
# plot(gis$longitude, gis$latitude)
fullcoo <- complete.cases(gis[, c("longitude", "latitude")])
gis_points <- gis[fullcoo, c("Study_ID", "longitude", "latitude")]
# remove duplicates (so that it doesn't appear twice when jittered)
gis_points <- gis_points[!duplicated(gis_points), ]
# round and jitter coordinates for privacy reason
gis_points$lon <- jitter(round(gis_points$longitude, 1), amount = 0.05)
gis_points$lat <- jitter(round(gis_points$latitude, 1), amount = 0.05)


# B4. Create sf object
mpt <- match(gis_points$Study_ID, smalldf$Study_ID)
gis_points$Type <- smalldf$Type[mpt]
gis_points$Resp.pest <- smalldf$Resp_pest[mpt]
gis_points$Resp.NE <- smalldf$Resp_NE[mpt]
gis_points$Diversif <- smalldf$Div_measures[mpt]
gis_points$Crop_type = smalldf$Crop_type[mpt]
shp <- st_as_sf(
  gis_points[, c(
    "Study_ID",
    "lon",
    "lat",
    "Type",
    "Resp.pest",
    "Resp.NE",
    "Diversif",
    "Crop_type"
  )],
  coords = c("lon", "lat"),
  crs = 4326
)

# check visualization
# mapview::mapview(shp, zcol = "Study_ID")

# B5. Export shapefile
st_write(
  shp,
  dsn = here::here("app", "data", "points.shp"),
  layer = "points.shp",
  driver = "ESRI Shapefile",
  append = FALSE
)

# export for gis-diversification
# keep <- c("Study_ID", "Site", "longitude", "latitude", "Year")
# gis_pts <- gis[fullcoo, keep]
# gis_pts <- gis_pts[!duplicated(gis_pts), ]
# pts <- st_as_sf(
#   gis_pts[, keep],
#   coords = c("longitude", "latitude"),
#   crs = 4326
# )

# # check visualization
# mapview::mapview(pts, zcol = "Study_ID")

# # B5. Export shapefile
# st_write(
#   pts,
#   dsn = here::here("app", "data", "all_points.gpkg")
# )

# no need to calculate convex hull anymore
# B4. calculate convex hull
# so far, simple with sf function, but could also use
# grDevices::chull(gis$X, gis$Y)
# alphahull::ahull(x, y = NULL, alpha)
# list_hull <- list()
# for (i in unique(gis$Study_ID)) {
#   coo <- gis[gis$Study_ID == i, c("longitude", "latitude")]
#   coo <- coo[complete.cases(coo), ]
#   if (nrow(coo) > 0) {
#     # if we need more precise concave hull
#     # hulli <- st_concave_hull(st_multipoint(as.matrix(coo), dim = "XY"), 0.3)
#     hulli <- st_convex_hull(st_multipoint(as.matrix(coo), dim = "XY"))
#     list_hull[[i]] <- hulli
#   }
# }

# # B.5 Combine polygon and metadata
# shp_poly <- st_sf(
#   smalldf[match(names(list_hull), smalldf$Study_ID), ],
#   geometry = st_sfc(list_hull),
#   crs = 4326
# )

# names(shp_poly)[c(1, 5, 6)] <- c("Study.ID", "Resp.var", "Practice")
# plot(gis$longitude, gis$latitude)
# plot(shp_poly, add = TRUE)
