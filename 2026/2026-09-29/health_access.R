library(tidyverse)

# Access to hospitals in urban centres, measured as the share of the population
# living within 1 km of a hospital. Three questions:
# 1. Do larger (or denser) cities give better access?
# 2. Are hospitals located where people live?
# 3. Pharmacies vs hospitals: what does the source actually capture?

tuesdata <- tidytuesdayR::tt_load(2026, week = 39)

health <- tuesdata$health |>
    transmute(
        id = ID_UC_G0,
        name = GC_UCN_MAI_2025,
        country = GC_CNT_GAD_2025,
        region = GC_DEV_USR_2025,
        pop = GC_POP_TOT_2025,
        area = GC_UCA_KM2_2025,
        n_hos = HL_FCL_HOS_2024,
        n_pha = HL_FCL_PHA_2024,
        share_hos = HL_SHP_HOS_2025
    ) |>
    mutate(pop_dens = pop / area)

region_fr <- c(
    "Australia and New Zealand" = "Australie et Nouvelle-Zélande",
    "Central and Southern Asia" = "Asie centrale et du Sud",
    "Eastern and South-Eastern Asia" = "Asie de l'Est et du Sud-Est",
    "Europe" = "Europe",
    "Latin America and the Caribbean" = "Amérique latine et Caraïbes",
    "Northern Africa and Western Asia" = "Afrique du Nord et Asie de l'Ouest",
    "Northern America" = "Amérique du Nord",
    "Oceania" = "Océanie",
    "Sub-Saharan Africa" = "Afrique subsaharienne"
)


## Data quality

# 44% of urban centres have no share value. Missingness drops sharply with size
# (54% below 100k inhabitants, 0% above 5M): small centres often have no mapped hospital.
# Analyses are restricted to centres with a value, so they describe access where
# hospitals exist, not the lack of any hospital.
health |>
    mutate(size = cut(pop, c(0, 1e5, 3e5, 1e6, 5e6, Inf))) |>
    group_by(size) |>
    summarise(n = n(), na_share = mean(is.na(share_hos)))

# ~95% of hospital counts are even (min = 2): facilities seem to be counted twice.
health |>
    summarise(
        even_hos = mean(n_hos %% 2 == 0, na.rm = TRUE),
        even_pha = mean(n_pha %% 2 == 0, na.rm = TRUE)
    )

health_ok <- health |>
    drop_na(share_hos)


## 1. City size vs population density ----

# Size is barely linked to access (rho ~ -0.13), population density much more (rho ~ 0.32)
health_ok |>
    summarise(
        rho_pop = cor(log(pop), share_hos, method = "spearman"),
        rho_dens = cor(log(pop_dens), share_hos, method = "spearman")
    )

# Sprawling regions (Northern America, Australia and New Zealand) have the lowest access
region_medians <- health_ok |>
    group_by(region) |>
    summarise(
        n = n(),
        pop_dens = median(pop_dens),
        share_hos = median(share_hos)
    ) |>
    arrange(pop_dens)
region_medians

# All centres as a 2D histogram, with region medians on top
# (Oceania excluding Australia and New Zealand has too few centres)
region_labels <- region_medians |>
    filter(n >= 30) |>
    mutate(region = region_fr[region])

ggplot(health_ok, aes(x = pop_dens, y = share_hos)) +
    geom_bin_2d(bins = 45) +
    geom_point(
        data = region_labels,
        shape = 21,
        size = 3.5,
        fill = "white",
        colour = "black",
        stroke = 0.8
    ) +
    ggrepel::geom_label_repel(
        data = region_labels,
        aes(label = region),
        size = 3.2,
        fill = alpha("white", 0.85),
        min.segment.length = 0,
        box.padding = 0.6,
        seed = 1
    ) +
    scale_fill_viridis_c(option = "F", direction = -1, transform = "log10") +
    scale_x_log10(labels = scales::label_number(big.mark = " ")) +
    scale_y_continuous(
        labels = scales::label_percent(scale = 1),
        expand = c(0, 0)
    ) +
    labs(
        x = "Densité de population (hab./km², échelle log)",
        y = "Part de la population à moins de 1 km d'un hôpital",
        fill = "Nombre de\ncentres urbains"
    ) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())


## 2. Are hospitals located where people live? ----

# Null model: compare the observed coverage with the coverage expected if hospitals
# were scattered at random over the urban centre. Each hospital covers a 1 km disc
# (pi km2), so n hospitals on an area A cover on average 1 - exp(-n * pi / A) of it
# (overlapping discs included). With random placement, the covered population share
# equals the covered area share whatever the population distribution.
# Ratio observed / expected > 1: hospitals sit where people live;
# < 1: they are clustered or away from residents.
# Choices:
# - counts are halved (double counting, see above);
# - the ratio is very noisy with few hospitals: keep centres with at least 5;
# - the ratio is capped at 100 / expected: near full expected coverage, centres are
#   forced onto the diagonal and say nothing about placement. There is no clear break
#   in the data, so the threshold is set from the smallest effect we want to be able
#   to see: a centre must be able to do at least 1/3 better than random, i.e. expected
#   coverage below 100 / (4/3) = 75%. Centres above are left out of the summaries
#   and greyed out on the figure.
min_detectable_ratio <- 4 / 3
saturation <- 100 / min_detectable_ratio

placement <- health_ok |>
    mutate(
        n_hos = n_hos / 2,
        share_exp = 100 * (1 - exp(-n_hos * pi / area)),
        ratio = share_hos / share_exp,
        saturated = share_exp > saturation
    ) |>
    filter(n_hos >= 5)

placement_ok <- placement |>
    filter(!saturated)

# ~1,950 centres; median ratio ~1.15, two thirds above 1
placement_ok |>
    summarise(n = n(), median_ratio = median(ratio), above_1 = mean(ratio > 1))

# Europe is the only region at the random level, Eastern Asia the highest
placement_ok |>
    group_by(region) |>
    summarise(n = n(), median_ratio = median(ratio)) |>
    arrange(median_ratio)

# Labels: the 3 lowest and 3 highest ratios, plus a few well-known cities
known_cities <- c(
    "Paris",
    "London",
    "Moscow",
    "Tokyo",
    "Shanghai",
    "Lagos",
    "Los Angeles",
    "São Paulo"
)

city_fr <- c(
    "London" = "Londres",
    "Moscow" = "Moscou",
    "Ciudad Bolivar" = "Ciudad Bolívar"
)

placement_labels <- bind_rows(
    placement_ok |> slice_min(ratio, n = 3),
    placement_ok |> slice_max(ratio, n = 3),
    placement_ok |> filter(name %in% known_cities)
) |>
    distinct(id, .keep_all = TRUE) |>
    mutate(name = coalesce(city_fr[name], name))

placement_labels |>
    select(name, country, n_hos, share_hos, share_exp, ratio)

ggplot(placement_ok, aes(x = share_exp, y = share_hos)) +
    annotate(
        "rect",
        xmin = saturation,
        xmax = 100,
        ymin = 0,
        ymax = 100,
        fill = "grey92"
    ) +
    geom_point(
        data = placement |> filter(saturated),
        colour = "grey60",
        alpha = 0.7,
        size = 1
    ) +
    geom_abline(
        slope = 1,
        intercept = 0,
        colour = "grey40",
        linetype = "dashed"
    ) +
    geom_point(aes(colour = ratio), alpha = 0.7, size = 1) +
    geom_point(
        data = placement_labels,
        shape = 21,
        size = 2.5,
        colour = "black"
    ) +
    ggrepel::geom_text_repel(
        data = placement_labels,
        aes(label = name),
        size = 3.2,
        min.segment.length = 0,
        box.padding = 0.5,
        seed = 1
    ) +
    scale_colour_distiller(
        palette = "RdYlBu",
        direction = 1,
        transform = "log2",
        limits = c(1 / 4, 4),
        oob = scales::squish,
        breaks = c(1 / 4, 1 / 2, 1, 2, 4),
        labels = c("≤ ¼", "½", "1", "2", "≥ 4")
    ) +
    scale_x_continuous(
        labels = scales::label_percent(scale = 1),
        limits = c(0, 100)
    ) +
    scale_y_continuous(
        labels = scales::label_percent(scale = 1),
        limits = c(0, 100)
    ) +
    coord_equal() +
    labs(
        x = "Part attendue si les hôpitaux étaient placés au hasard",
        y = "Part observée de la population\nà moins de 1 km d'un hôpital",
        colour = "Observé /\nattendu"
    ) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())
ggsave(
    here::here("2026", "2026-09-29", "health_access_placement.png"),
    width = 8,
    height = 7,
    dpi = 300,
    bg = "white"
)


## 3. Pharmacies vs hospitals: what does the source capture? ----

# Number of pharmacies per hospital, by country (centres where both are known,
# countries with at least 10 such centres). The double counting cancels out in the ratio.
# Countries known for dense pharmacy networks (Spain, Italy) come out with very few
# pharmacies per hospital: the ratio likely reflects how thoroughly each type of
# facility is recorded in the source rather than the actual supply.
country_fr <- c(
    "Algeria" = "Algérie",
    "Argentina" = "Argentine",
    "Australia" = "Australie",
    "Bangladesh" = "Bangladesh",
    "Belarus" = "Biélorussie",
    "Belgium" = "Belgique",
    "Brazil" = "Brésil",
    "Cameroon" = "Cameroun",
    "Canada" = "Canada",
    "Chile" = "Chili",
    "China" = "Chine",
    "Colombia" = "Colombie",
    "Cuba" = "Cuba",
    "Ecuador" = "Équateur",
    "El Salvador" = "Salvador",
    "France" = "France",
    "Germany" = "Allemagne",
    "Guatemala" = "Guatemala",
    "India" = "Inde",
    "Indonesia" = "Indonésie",
    "Iran" = "Iran",
    "Italy" = "Italie",
    "Japan" = "Japon",
    "Kazakhstan" = "Kazakhstan",
    "México" = "Mexique",
    "Netherlands" = "Pays-Bas",
    "Nicaragua" = "Nicaragua",
    "Peru" = "Pérou",
    "Philippines" = "Philippines",
    "Poland" = "Pologne",
    "Russia" = "Russie",
    "South Korea" = "Corée du Sud",
    "Spain" = "Espagne",
    "Taiwan" = "Taïwan",
    "Thailand" = "Thaïlande",
    "Ukraine" = "Ukraine",
    "United Kingdom" = "Royaume-Uni",
    "United States" = "États-Unis",
    "Uzbekistan" = "Ouzbékistan",
    "Venezuela" = "Venezuela"
)

pharma_ratio <- health |>
    drop_na(n_hos, n_pha) |>
    group_by(country) |>
    summarise(
        n_centres = n(),
        pha_per_hos = sum(n_pha) / sum(n_hos)
    ) |>
    filter(n_centres >= 10) |>
    mutate(country_fr = fct_reorder(country_fr[country], pha_per_hos))

pharma_ratio |>
    arrange(pha_per_hos)

ggplot(pharma_ratio, aes(x = pha_per_hos, y = country_fr)) +
    geom_vline(xintercept = 1, colour = "grey40") +
    geom_segment(
        aes(x = 1, xend = pha_per_hos, yend = country_fr),
        colour = "grey70"
    ) +
    geom_point(aes(colour = pha_per_hos), size = 3) +
    scale_colour_distiller(
        palette = "BrBG",
        direction = 1,
        transform = "log10",
        limits = c(1 / 5, 5),
        oob = scales::squish,
        guide = "none"
    ) +
    scale_x_log10(
        breaks = c(0.03, 0.1, 0.3, 1, 3),
        labels = c("0,03", "0,1", "0,3", "1", "3")
    ) +
    labs(
        x = "Nombre de pharmacies par hôpital (échelle log)",
        y = NULL
    ) +
    theme_minimal() +
    theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()
    )
