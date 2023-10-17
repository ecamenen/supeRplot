plot_pie <- function(
    x,
    df0 = NULL,
    hsize = 1.2,
    cex = 15,
    cex_main = cex * 1.25,
    colour = get_colors(),
    wrap = 5,
    lwd = 4,
    dec = .1,
    label = TRUE,
    threshold = 5,
    title = NULL,
    wrap_title = 20,
    legend = TRUE,
    parse = FALSE,
    collapse = FALSE,
    label_name = NULL,
    legend_name = NULL,
    top = -0.5,
    l = -1,
    r = -1,
    b = -1
) {
  df <- count_cat(x, parse = parse, wrap = wrap, collapse = collapse, label = label_name)
  if (!is.null(df0)) {
    df <- rbind(df, data.frame(f = NA, n = c(nrow(df0) - sum(df$n))))
  }
  if (!is.null(legend) && !is.logical(legend)) {
    df$f <- factor(df$f, labels = legend)
  }
  df <- mutate(
    df,
    hsize = hsize,
    pos = rev(cumsum(rev(n))),
    pos = n / 2 + lead(pos, 1),
    pos = if_else(is.na(pos), n / 2, pos),
    label = str_wrap(str_glue("{f}"), wrap),
    text = scales::percent(n / sum(n), dec)
  )
  if (is.null(title)) {
    title <- deparse(substitute(x))
  }
  i <- df$n / sum(df$n) <= threshold / 100
  df$text[i] <- ""
  if (!is.null(legend_name)) {
    df$legend <- str_replace_all(legend_name, "\\(", "\\\\(") %>% str_replace_all("\\)", "\\\\)") %>% c("NA")
    i <- sapply(df$legend, function(i) paste(df$f) %>% str_replace_all("\n", " ") %>% str_which(i))
    colour0 <- c(rev(colour), "gray")[i]
    colour <- colour0
    df$legend <- df$f[i]
  } else {
    df$legend <- df$f
    # colour0 <- c(colour[seq(length(levels(df$f)))], "gray")
    colour <- sort(levels(df$f)) %>% match(.,  levels(df$f)) %>% colour[.] %>% c("gray")
    colour0 <- colour
  }
  df$legend0 <- paste0(df$legend, ": ", df$n)
  if (!isTRUE(label)) {
    df$label <- rep("", nrow(df))
  }
  # df$text[3] <- paste("", "", "", df$text[3])
  df$legend[df$legend == "NA"] <- NA
  p <- ggplot(df, aes(x = hsize, y = n, fill = f)) +
    geom_col(width = 1, color = NA, lwd = lwd) +
    geom_text(
      color = "white",
      size = cex / 2.5,
      aes(label = text),
      position = position_stack(vjust = 0.5)
    ) +
    coord_polar(theta = "y", clip = "off") +
    scale_fill_manual(values = colour, na.value = "gray", labels = df$legend0, breaks = df$legend, name = "") +
    scale_y_continuous(breaks = df$pos, labels = df$label) +
    ggtitle(str_wrap(title, wrap_title)) +
    theme(
      plot.title = element_text(hjust = 0.5, vjust = -4, size = cex_main, face = "bold"),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      # axis.text.x = element_text(size = cex, colour = colour0),
      axis.text = element_blank(),
      legend.text = element_text(size = cex * 0.85),
      legend.key = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.margin = unit(c(top, l, r, b), "cm")
    ) +
    xlim(0.5, hsize + 0.5)
  if (is.null(legend) || legend == FALSE) {
    p + theme(legend.position = "none")
  } else {
    p
  }
}
