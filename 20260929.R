library(tidyverse)
library(FactoMineR)

tuesdata <- tidytuesdayR::tt_load(2026, week = 39)

health <- tuesdata$health

# variable 	class 	description
# ID_UC_G0 	double 	Unique ID.
# GC_UCN_MAI_2025 	character 	Urban Centre Main Name.
# GC_CNT_GAD_2025 	character 	Country name. GADM.
# GC_UCA_KM2_2025 	double 	Urban centre area in 2025.
# GC_POP_TOT_2025 	double 	Urban centre total population.
# GC_DEV_WIG_2025 	character 	World Bank Income Group.
# GC_DEV_USR_2025 	character 	UN SDG Region, geographic and statistical groupings of countries used by the UN.
# HL_FCL_HOS_2024 	double 	Number of hospitals in 2024.
# HL_FCL_PHA_2024 	double 	Number of pharmacies in 2024.
# HL_FDE_HOS_2024 	double 	Number of hospitals per urban centre area (km2) in 2024.
# HL_FDE_PHA_2024 	double 	Number of pharmacies per urban centre area (km2) in 2024.
# HL_FPC_HOS_2025 	double 	Number of hospitals per capita in 2025.
# HL_FPC_PHA_2025 	double 	Number of pharmacies per capita in 2025.
# HL_POP_HOS_2025 	double 	Population living within 1 km buffer from a hospital in 2025.
# HL_POP_PHA_2025 	double 	Population living within 1 km buffer from a pharmacy in 2025.
# HL_SHP_HOS_2025 	double 	Share of the urban centre population living within 1 km buffer from a hospital in 2025.
# HL_SHP_PHA_2025 	double 	Share of the urban centre population living within 1 km buffer from a pharmacy in 2025.

health <- health |>
    rename(
        id = ID_UC_G0,
        name = GC_UCN_MAI_2025,
        country = GC_CNT_GAD_2025,
        area = GC_UCA_KM2_2025,
        pop = GC_POP_TOT_2025,
        income = GC_DEV_WIG_2025,
        region = GC_DEV_USR_2025,
        n_hos = HL_FCL_HOS_2024,
        n_pha = HL_FCL_PHA_2024,
        dens_hos = HL_FDE_HOS_2024,
        dens_pha = HL_FDE_PHA_2024,
        pc_hos = HL_FPC_HOS_2025,
        pc_pha = HL_FPC_PHA_2025,
        pop1km_hos = HL_POP_HOS_2025,
        pop1km_pha = HL_POP_PHA_2025,
        share_hos = HL_SHP_HOS_2025,
        share_pha = HL_SHP_PHA_2025
    ) |>
    mutate(
        income = fct_relevel(
            income,
            "Low income",
            "Lower Middle",
            "Upper Middle",
            "High income"
        )
    )

summary(health)


## Missing values

# 45% of cities have no hospital data, 79% no pharmacy data.
# Missingness is not random: it follows income group (and region),
# which suggests a coverage bias of the source (mapped facilities) rather than true absence.
health |>
    filter(!is.na(income)) |>
    group_by(income) |>
    summarise(
        n = n(),
        na_hos = mean(is.na(n_hos)),
        na_pha = mean(is.na(n_pha))
    )

health |>
    filter(!is.na(income)) |>
    pivot_longer(c(n_hos, n_pha), names_to = "facility", values_to = "n_fac") |>
    ggplot(aes(x = income, fill = is.na(n_fac))) +
    geom_bar(position = "fill") +
    facet_wrap(~facility) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = NULL, y = "Share of urban centres", fill = "Missing") +
    theme_minimal()

# NB: ~95% of facility counts are even (min = 2): facilities may be counted twice in the source.
mean(health$n_hos %% 2 == 0, na.rm = TRUE)


## PCA with factominer

# Choices vs. a PCA on all 10 HL_ variables:
# - raw counts (n_*) and populations within 1 km (pop1km_*) mostly measure city size,
#   they are dropped from active variables; size is projected as supplementary (log_pop, log_area)
# - densities and per capita values are very skewed -> log
# - complete cases only: PCA() would otherwise impute ~80% of pharmacy values by the mean.
#   Beware, the sample (n ~ 2,200) over-represents high income cities.
health_pca <- health |>
    drop_na(dens_hos:share_pha, income) |>
    transmute(
        id,
        across(c(dens_hos, dens_pha, pc_hos, pc_pha), log),
        share_hos,
        share_pha,
        log_pop = log(pop),
        log_area = log(area),
        income,
        region = factor(region)
    ) |>
    column_to_rownames("id")

pca <- PCA(
    health_pca,
    quanti.sup = c("log_pop", "log_area"),
    quali.sup = c("income", "region"),
    graph = FALSE
)
summary(pca)
# 2 axes are enough (85% of variance)

plot(pca, choix = "var", axes = c(1, 2))

# PC1 (55%): overall access to healthcare facilities, all variables (+)
#   slightly negatively linked to city size and area
# PC2 (30%): hospitals (+) vs. pharmacies (-)

# Income groups and regions on the PCA plane
dimdesc(pca, axes = 1:2)$Dim.1$category
dimdesc(pca, axes = 1:2)$Dim.2$category

plot(pca, choix = "ind", invisible = c("ind"), axes = c(1, 2))

pca$ind$coord |>
    as_tibble() |>
    bind_cols(health_pca |> select(income, region)) |>
    ggplot(aes(x = Dim.1, y = Dim.2)) +
    geom_hline(yintercept = 0, colour = "grey70") +
    geom_vline(xintercept = 0, colour = "grey70") +
    geom_point(alpha = 0.15, size = 0.8) +
    stat_ellipse(aes(colour = income), linewidth = 0.8) +
    labs(
        x = "PC1: overall access to healthcare",
        y = "PC2: hospitals (+) vs. pharmacies (-)",
        colour = "Income group"
    ) +
    theme_minimal()


## Maps of the PCA axes

library(sf)
library(rnaturalearth)
library(cowplot)

city_scores <- pca$ind$coord |>
    as_tibble(rownames = "id") |>
    transmute(id = as.numeric(id), pc1 = Dim.1, pc2 = Dim.2) |>
    left_join(health |> select(id, country), by = "id")

# Median city per country, countries with at least 5 cities with complete data
country_scores <- city_scores |>
    group_by(country) |>
    filter(n() >= 5) |>
    summarise(n_cities = n(), pc1 = median(pc1), pc2 = median(pc2))

# Natural Earth names differ from GADM for a few countries
world <- ne_download(scale = 110, type = "countries", category = "cultural") |>
    filter(ADMIN != "Antarctica") |>
    mutate(
        country = recode(
            ADMIN,
            "United States of America" = "United States",
            "Mexico" = "México",
            "United Republic of Tanzania" = "Tanzania",
            "Ivory Coast" = "Côte d'Ivoire",
            "Republic of Serbia" = "Serbia"
        )
    ) |>
    left_join(country_scores, by = "country")

# Globe outline with 1° segments so that edges follow the projection
globe <- st_polygon(list(cbind(
    c(seq(-180, 180, 1), rep(180, 181), seq(180, -180, -1), rep(-180, 181)),
    c(rep(-90, 361), seq(-90, 90, 1), rep(90, 361), seq(90, -90, -1))
))) |>
    st_sfc(crs = 4326)
graticule <- st_graticule(lon = seq(-180, 180, 30), lat = seq(-60, 60, 30))
globe_bbox <- st_bbox(st_transform(globe, "+proj=eqearth"))

base_map <- function(fill_var) {
    ggplot() +
        geom_sf(data = globe, fill = "#E4EFF6", colour = NA) +
        geom_sf(data = graticule, colour = "grey75", linewidth = 0.2) +
        geom_sf(data = world, aes(fill = {{ fill_var }}), colour = "white", linewidth = 0.1) +
        coord_sf(
            crs = "+proj=eqearth",
            xlim = globe_bbox[c("xmin", "xmax")],
            ylim = globe_bbox[c("ymin", "ymax")],
            default_crs = NULL,
            expand = FALSE
        ) +
        theme_void() +
        theme(
            legend.position = "bottom",
            legend.key.width = unit(1.5, "cm"),
            plot.margin = margin(10, 10, 10, 10)
        )
}

map_pc1 <- base_map(pc1) +
    scale_fill_viridis_c(option = "F", direction = -1, na.value = "gray80") +
    labs(fill = "Accès global aux soins (PC1)") +
    guides(fill = guide_colourbar(title.position = "top"))

# Symmetric scale around 0, capped at the 95th percentile so that Libya does not flatten everything
pc2_max <- quantile(abs(country_scores$pc2), 0.95)
map_pc2 <- base_map(pc2) +
    scale_fill_distiller(
        palette = "PuOr",
        direction = 1,
        limits = c(-pc2_max, pc2_max),
        oob = scales::squish,
        breaks = c(-pc2_max, 0, pc2_max),
        labels = c("Plutôt pharmacies", "Équilibré", "Plutôt hôpitaux"),
        na.value = "gray80"
    ) +
    labs(fill = "Profil de l'offre (PC2)") +
    guides(fill = guide_colourbar(title.position = "top"))

fig <- plot_grid(map_pc1, map_pc2, ncol = 1)
fig

ggsave("20260929.png", fig, width = 8, height = 9, dpi = 300, bg = "white")
