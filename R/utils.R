get_melt <- function(x) {
    as.data.frame(x) %>%
        pivot_longer(everything())
}

get_colors <- function() {
    c(
        brewer.pal(9, "Set1"),
        brewer.pal(7, "Set2"),
        brewer.pal(8, "Pastel1"),
        brewer.pal(8, "Set2")[-5]
    )
}
