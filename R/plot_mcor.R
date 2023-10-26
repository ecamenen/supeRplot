plot_mcor <- function(
    x,
    colour = brewer.pal(n = 8, name = "RdBu"),
    cex = 1,
    method = "spearman",
    method_adjust = "BH",
    mat = NULL,
    p_mat = NULL) {
    x <- as.data.frame(x)
    if (is.null(mat)) {
        mat <- get_corr(x, TRUE, method = method)
    }
    if (is.null(p_mat)) {
        p_mat <- get_corr(x, FALSE, method = method) %>%
            as.vector() %>%
            p.adjust(method_adjust) %>%
            matrix(nrow = sqrt(length(.)), ncol = sqrt(length(.)))
    }
    rownames(mat) <- rownames(p_mat) <-
        colnames(mat) <- colnames(p_mat) <- colnames(x)

    corrplot(
        mat,
        col = colour,
        type = "upper",
        order = "original",
        tl.col = "gray40",
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
        cl.cex = cex * 0.95,
        cl.align.text = "l"
    )
}
