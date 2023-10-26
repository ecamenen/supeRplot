plot_mcor <- function(
    x,
    y = NULL,
    clean_name = TRUE,
    mat = NULL,
    p_mat = NULL,
    col = brewer.pal(n = 9, name = "RdBu"),
    method = "spearman",
    p_adjust = "BH",
    cex = 1) {
    if (clean_name) {
        colnames(x) <- get_var_names(colnames(x), y)
        x <- clean_names(x)
    }
    x <- as.data.frame(x)
    if (is.null(mat)) {
        mat <- get_corr(x, TRUE, method = method)
        colnames(mat) <- colnames(x) -> rownames(mat)
    }
    if (is.null(p_mat)) {
        p_mat <- get_corr(x, FALSE, method = method) %>%
            as.vector() %>%
            p.adjust(p_adjust) %>%
            matrix(nrow = sqrt(length(.)), ncol = sqrt(length(.)))
    }

    corrplot(
        mat,
        col = col,
        type = "upper",
        order = "original",
        tl.col = "gray50",
        tl.srt = 45,
        tl.cex = 1 * cex,
        p.mat = p_mat,
        sig.level = 0.05,
        addgrid.col = NA,
        insig = "pch",
        pch = 4,
        pch.cex = 2.5 * cex,
        pch.col = "white",
        diag = FALSE,
        na.label = " ",
        cl.cex = cex,
        cl.col = "gray50"
    )
}
