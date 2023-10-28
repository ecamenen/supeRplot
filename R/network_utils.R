# Creates the nodes for a design matrix
get_nodes <- function(x, ratio = 12) {
get_nodes <- function(x, cex = 12) {
    res <- unlist(x[, seq(2)]) %>%
        data.frame(id = .) %>%
        group_by(id) %>%
        summarise(size = n()) %>%
        as.data.frame()
    n <- pull(res, size) %>% max()
    res$size <- res$size / n * cex
    return(res)
}

# Creates the edges for a design matrix
#
# @return A dataframe with tuples of connected x
get_edges <- function(x, C, p = NULL) {
    J <- NCOL(C)
get_edges <- function(x, y = NULL, digits = 2) {
    stopifnot(isSymmetric(x))
    if (!is.null(y)) {
        stopifnot(isSymmetric(y))
    }
    n <- NCOL(x)
    edges <- list()

    k <- 0
    for (j in seq(n)) {
        for (i in seq(n)) {
            if (i > k && abs(x[i, j]) > 0) {
                if (is.null(y)) {
                    d <- NULL
                } else {
                    d <- y[i, j]
                }
                edges[[length(edges) + 1]] <- c(colnames(x)[j], colnames(x)[i], x[i, j], d)
            }
        }
        k <- k + 1
    }

    n <- length(edges[[1]])
    edges <- as.data.frame(t(matrix(unlist(edges), n, length(edges))))
    colnames(edges) <- c("from", "to", "weight", "p")[seq(n)]
    edges[, 3] <- as.numeric(edges[, 3])
    if (!is.null(y)) {
        edges[, 4] <- as.numeric(edges[, 4])
    }
    edges$title <- edges$label <- round(edges[, 3], digits)
    return(edges)
}

#' Plot the connection between blocks
#'
#' @return A dataframe with tuples of connected blocks
#' @export
plot_network <- function(
    x = NULL,
    title = "",
    cex = 1,
    cex_main = 14 * cex,
    cex_node = 3 * cex,
    cex_edge = 2 * cex,
    color = c("#eee685", "#686868"),
    shape = "dot",
    dashed = TRUE,
    nodes = NULL,
    edges = NULL,
    dist = 1,
    label = FALSE,
    digits = 2) {
    title <- paste0(title, collapse = " ")
    if (is.null(edges)) {
        edges <- get_edges(x, digits = digits)
    }
    if (is.null(nodes)) {
        nodes <- get_nodes(edges, cex * 12)
    }
    if (!label) {
        edges$label <- ""
    }
    edges$weight <- abs(edges$weight)
    net <- graph_from_data_frame(
        d = edges,
        vertices = nodes,
        directed = FALSE
    )
    V(net)$color <- as.vector(color[1])
    V(net)$label <- nodes$id
    if (shape == "dot") {
        shape <- "circle"
    }
    V(net)$shape <- shape
    if (is.null(edges$color)) {
        edge_color <- "gray80"
    } else {
        edge_color <- edges$color
    }
    E(net)$width <- E(net)$weight * cex_edge

    plot(
        net,
        edge.color = edge_color,
        edge.lty = ifelse(dashed, 2, 1),
        edge.label.color = color[2],
        edge.label.family = "sans",
        edge.label.cex = cex * 0.95,
        vertex.frame.color = "gray80",
        vertex.label.cex = cex,
        vertex.label.color = color[2],
        vertex.label.dist = dist,
        vertex.label.degree = 1.5,
        vertex.label.family = "sans",
        vertex.size = cex_node * nodes$size * 0.5,
        vertex.frame.width = cex_node * 0.9,
        margin = c(0.1, 0, 0, 0)
    )
    title(title, cex.main = cex_main * 0.1)
}

#' Plot the connection between blocks (dynamic plot)
#'
#' @return A dataframe with tuples of connected blocks
#' @export
plot_network2 <- function(
    x,
    C = 1 - diag(length(x)),
plot_network_dyn <- function(
    x = NULL,
    title = "",
    cex = 1,
    cex_main = 14 * cex,
    cex_node = 3 * cex,
    cex_edge = 2 * cex,
    color = c("#eee685", "#686868"),
    shape = "dot",
    dashed = TRUE,
    nodes = NULL,
    edges = NULL) {
    title <- paste0(title, collapse = " ")
    if (length(color) < 2) {
        color <- c(color, "gray")
    }
    if (is.null(edges)) {
        edges <- get_edges(x)
    }
    edges$width <- edges$weight * cex_edge
    if (is.null(nodes)) {
        nodes <- get_nodes(edges, cex * 12)
        nodes$label <- nodes$id
    }
    nodes$title <- nodes$label <- nodes$id
    nodes$color.background <- rep(as.vector(color[1]), nrow(nodes))

    visNetwork(
        nodes,
        edges,
        main = list(
            text = title,
            style = paste0(
                "font-family:sans;font-weight:bold;font-size:",
                cex_main * 1.4,
                "px;text-align:center;"
            )
        )
    ) %>%
        visNodes(
            borderWidth = 2,
            shape = shape,
            shadow = TRUE,
            size = cex_node * 11,
            font = list(size = cex * 21, color = color[2]),
            color = list(
                border = "#686868",
                highlight = list(background = "black", border = "darkred")
            )
        ) %>%
        visEdges(
            smooth = FALSE,
            shadow = TRUE,
            dashes = dashed,
            color = list(color = color[2], highlight = "darkred")
        )
}

get_corr1 <- function(
    x,
    method = "spearman",
    method_adjust = "BH",
    cutoff = 0.75) {
    r <- get_corr(x, TRUE, method)
    r[abs(r) < cutoff] <- diag(r) <- 0
    r[is.na(r)] <- 0
    p <- get_corr(x, FALSE, method, method_adjust)
    r[p >= 0.05] <- 0
    return(list(r = r, p = p))
}

#' @export
plot_corr_network <- function(
plot_cor_network <- function(
    x,
    colour_edge = c("#4DAF4A", "#EE6363"),
    colour_node = c("white", "#3D3D3D"),
    cex = 1,
    method = "spearman",
    method_adjust = "BH",
    cutoff = 0.75,
    digits = 2,
    ...) {
    res <- get_corr1(x, method, method_adjust, cutoff)
    edges <- get_edges(res$r, res$p, digits = digits)
    font <- "14px arial black"
    edges$font.bold.mod <- ifelse(edges$p < 0.05, paste(font, "bold"), font)
    nodes <- get_nodes(edges, cex = cex * 12)
    edges$color <- ifelse(
        edges$weight > 0,
        colour_edge[1],
        colour_edge[2]
    )

    plot_network_dyn(
        x,
        res$r,
        dashed = FALSE,
        nodes = nodes,
        edges = edges,
        cex = cex,
        cex_edge = edges$weight * 20 * cex,
        color = c(colour_node[1], colour_node[2]),
        ...
    )
}
