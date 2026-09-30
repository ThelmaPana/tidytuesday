library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2026, week = 39)

# Question: do larger cities give better access to hospitals?
# Access = share of the urban centre population living within 1 km of a hospital.

health <- tuesdata$health |>
    transmute(
        id = ID_UC_G0,
        name = GC_UCN_MAI_2025,
        country = GC_CNT_GAD_2025,
        region = GC_DEV_USR_2025,
        pop = GC_POP_TOT_2025,
        area = GC_UCA_KM2_2025,
        n_hos = HL_FCL_HOS_2024,
        share_hos = HL_SHP_HOS_2025
    ) |>
    mutate(pop_dens = pop / area)


## Missing values

# 44% of urban centres have no share value. Missingness drops sharply with size
# (54% below 100k inhabitants, 0% above 5M): small centres often have no mapped hospital.
# The analysis below is restricted to centres with a value, so it describes access
# where hospitals exist, not the lack of any hospital.
health |>
    mutate(size = cut(pop, c(0, 1e5, 3e5, 1e6, 5e6, Inf))) |>
    group_by(size) |>
    summarise(n = n(), na_share = mean(is.na(share_hos)))

health_ok <- health |>
    drop_na(share_hos)


## Size vs density

# Size is barely linked to access (rho ~ -0.13), population density much more (rho ~ 0.32)
health_ok |>
    summarise(
        rho_pop = cor(log(pop), share_hos, method = "spearman"),
        rho_dens = cor(log(pop_dens), share_hos, method = "spearman")
    )

# Sprawling regions (Northern America, Australia and New Zealand) have the lowest access
health_ok |>
    group_by(region) |>
    summarise(
        n = n(),
        median_dens = median(pop_dens),
        median_share = median(share_hos)
    ) |>
    arrange(median_dens)


## Figure

rho <- health_ok |>
    summarise(
        pop = cor(log(pop), share_hos, method = "spearman"),
        pop_dens = cor(log(pop_dens), share_hos, method = "spearman")
    )

facet_labels <- c(
    pop = str_glue("Population (hab.)\nρ de Spearman = {format(round(rho$pop, 2), decimal.mark = ',')}"),
    pop_dens = str_glue("Densité de population (hab./km²)\nρ de Spearman = {format(round(rho$pop_dens, 2), decimal.mark = ',')}")
)

health_ok |>
    pivot_longer(c(pop, pop_dens), names_to = "variable", values_to = "value") |>
    ggplot(aes(x = value, y = share_hos)) +
    geom_point(alpha = 0.1, size = 0.6) +
    geom_smooth(method = "gam", colour = "#D95F02", linewidth = 1) +
    facet_wrap(
        ~variable,
        scales = "free_x",
        strip.position = "bottom",
        labeller = as_labeller(facet_labels)
    ) +
    scale_x_log10(labels = scales::label_number(scale_cut = c(0, k = 1e3, M = 1e6))) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    coord_cartesian(ylim = c(0, 100)) +
    labs(
        x = NULL,
        y = "Part de la population à moins de 1 km d'un hôpital"
    ) +
    theme_minimal() +
    theme(
        strip.placement = "outside",
        strip.text = element_text(size = 10),
        panel.grid.minor = element_blank()
    )

ggsave(
    here::here("2026", "2026-09-29", "health_density.png"),
    width = 9,
    height = 4.5,
    dpi = 300,
    bg = "white"
)
