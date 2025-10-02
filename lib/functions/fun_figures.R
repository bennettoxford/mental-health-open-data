# Funciont to plot figures
plot_trajectory <- function(data, title = NULL, ncol_legend = 2) {
  plot <- data |>
    mutate(semantic_tag = str_to_sentence(semantic_tag)) |>
    ggplot(
      aes(
        y = usage,
        x = end_date,
        colour = description
      )
    ) +
    geom_line(alpha = 0.2) +
    geom_point() +
    labs(
      title = title,
      x = NULL,
      y = NULL,
      colour = NULL,
      caption = "Data from NHS England SNOMED Code Usage in Primary Care."
    ) +
    scale_x_date(
      date_breaks = "1 year",
      labels = label_date_short()
    ) +
    scale_y_continuous(
      labels = label_number(accuracy = 1),
      limits = c(0, NA)
    ) +
    facet_wrap(
      ~semantic_tag,
      scales = "free_y",
      ncol = 1
    ) +
    theme_bw() +
    theme(legend.position = "bottom") +
    guides(
      colour = guide_legend(ncol = ncol_legend, byrow = TRUE),
      shape = guide_legend(ncol = ncol_legend, byrow = TRUE)
    )

  plot
}
