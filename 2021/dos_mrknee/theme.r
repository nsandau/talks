theme_plex <- function(base_size = 11.5,
                       strip_text_size = 12,
                       strip_text_margin = 5,
                       subtitle_size = 12,
                       subtitle_margin = 10,
                       plot_title_size = 16,
                       plot_title_margin = 10,
                       plot_caption_size = 9,
                       plot_caption_margin = 5,
                       axis_title_size = base_size,
                       axis_title_margin = 2,
                       axis_text_size = base_size,
                       axis_text_margin = 4,
                       y_grid = FALSE,
                       plot_margin = ggplot2::margin(t = 5, b = 5, r = 5, l = 5),
                       legend_position = "top",
                       base_family = "Arial Narrow",
                       ...) {
  ret <- ggplot2::theme_minimal(
    base_family = base_family,
    base_size = base_size, ...
  )

  # TEXT
  ret$strip.text <- ggplot2::element_text(
    hjust = 0, size = strip_text_size,
    margin = ggplot2::margin(b = strip_text_margin),
    family = base_family, face = "bold"
  )
  ret$plot.subtitle <- ggplot2::element_text(
    hjust = 0, size = subtitle_size,
    margin = ggplot2::margin(b = subtitle_margin),
    family = base_family
  )
  ret$plot.title <- ggplot2::element_text(
    hjust = 0, size = plot_title_size,
    margin = ggplot2::margin(b = plot_title_margin),
    family = base_family, face = "bold"
  )
  ret$axis.title <- ggplot2::element_text(
    size = axis_title_size,
    margin = ggplot2::margin(b = axis_title_margin),
    family = base_family, face = "bold"
  )
  ret$axis.text <- element_text(
    size = axis_text_size,
    margin = ggplot2::margin(b = axis_text_margin),
    color = "black",
    family = base_family
  )
  ret$plot.caption <- ggplot2::element_text(
    size = plot_caption_size,
    margin = ggplot2::margin(b = plot_caption_margin),
    family = base_family
  )
  # PANEL GRID LINES

  if (y_grid) {
    ret$panel.grid.major.y <- element_line(color = "#cccccc", size = 0.5)
  } else {
    ret$panel.grid.major.y <- element_blank()
  }
  ret$panel.grid.minor <- element_blank()
  ret$panel.grid.major.x <- element_blank()
  ret$panel.border <- element_blank()

  # AXIS
  ret$axis.ticks <- element_line(color = "black")
  ret$axis.ticks.length <- unit(3, "pt")
  ret$axis.line <- element_line(color = "black", size = 0.5)


  # LEGEND
  ret$legend.title <- element_blank()
  ret$legend.position <- legend_position

  # PLOT
  ret$plot.caption <- element_text(hjust = 0)
  ret$plot.background <- element_rect(color = "white")
  ret$plot.margin <- plot_margin

  # RETURN THEME
  ret
}