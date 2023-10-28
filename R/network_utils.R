# Creates the nodes for a design matrix
#
# @return A dataframe with x in rows and the number of variables, of rows
# and tau or sparsity in columns
get_nodes <- function(x, n = 6) {
    unlist(x[, seq(2)]) %>%
        data.frame(id = .) %>%
        group_by(id) %>%
        summarise(size = n() * n) %>%
        as.data.frame()
}

# Creates the edges for a design matrix
#
# @return A dataframe with tuples of connected x
get_edges <- function(x, C, p = NULL) {
    J <- NCOL(C)
    edges <- list()

    k <- 0
    for (j in seq(J)) {
        for (i in seq(J)) {
            if (i > k && abs(C[i, j]) > 0) {
                if (is.null(p)) {
                    d <- NULL
                } else {
                    d <- p[i, j]
                }
                edges[[length(edges) + 1]] <- c(colnames(x)[j], colnames(x)[i], C[i, j], d)
            }
        }
        k <- k + 1
    }

    n <- length(edges[[1]])
    edges <- as.data.frame(t(matrix(unlist(edges), n, length(edges))))
    colnames(edges) <- c("from", "to", "weight", "p")[seq(n)]
    edges[, 3] <- as.numeric(edges[, 3])
    if (!is.null(p)) {
        edges[, 4] <- as.numeric(edges[, 4])
    }

    return(edges)
}

#' Plot the connection between blocks
#'
#' @return A dataframe with tuples of connected blocks
#' @export
plot_network <- function(
    x,
    C = 1 - diag(length(x)),
    title = paste0(
        "Common rows between blocks : ",
        length(Reduce(intersect, lapply(x, row.names)))
    ),
    cex = 1,
    cex_main = 14 * cex,
    cex_point = 3 * cex,
    cex_nodes = 2 * cex,
    color = c("#eee685", "gray60"),
    shape = "square",
    dashes = TRUE,
    nodes = NULL,
    edges = NULL,
    dist = NULL,
    label = FALSE) {
    title <- paste0(title, collapse = " ")
    if (is.null(nodes)) {
        nodes <- get_nodes(x)
    }
    if (is.null(edges)) {
        edges <- get_edges(x, C)
    }
    if (!label) {
        edges$label <- ""
    }

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
      edge_color <- color[2]
    } else {
      edge_color <- edges$color
    }

    E(net)$width <- E(net)$weight * cex_nodes
    plot(
        net,
        edge.color = edge_color,
        edge.lty = ifelse(dashes, 2, 1),
        edge.label.color = color[2],
        edge.label.family = "sans",
        edge.label.cex = cex * 0.95,
        vertex.frame.color = "gray80",
        vertex.label.cex = cex,
        vertex.label.color = color[2],
        vertex.label.dist = dist,
        vertex.label.degree = 1.5,
        vertex.label.family = "sans",
        vertex.size = cex_point * nodes$size * 0.5,
        vertex.frame.width = cex_point * 0.9,
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
    title = paste0(
        "Common rows between blocks : ",
        length(Reduce(intersect, lapply(x, row.names)))
    ),
    cex = 1,
    cex_main = 14 * cex,
    cex_point = 3 * cex,
    cex_nodes = 2 * cex,
    color = c("#eee685", "gray"),
    shape = "square",
    dashes = TRUE,
    nodes = NULL,
    edges = NULL) {
    title <- paste0(title, collapse = " ")
    if (length(color) < 2) {
        color <- c(color, "gray")
    }

    if (is.null(nodes)) {
        nodes <- get_nodes(x)
        nodes$label <- nodes$id
    }
    nodes$title <- nodes$id -> nodes$label
    nodes$color.background <- rep(as.vector(color[1]), nrow(nodes))

    if (is.null(edges)) {
        edges <- get_edges(x, C)
    }
    edges$width <- edges$weight * cex_nodes

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
            size = cex_point * 7.5,
            font = list(size = cex * 14),
            color = list(
                border = color[2],
                highlight = list(background = "black", border = "darkred")
            )
        ) %>%
        visEdges(
            smooth = FALSE,
            shadow = TRUE,
            dashes = dashes,
            color = list(color = color[2], highlight = "darkred")
        )
}

get_corr1 <- function(
    x,
    method = "spearman",
    method_adjust = "BH",
    cutoff = 0.75) {
    C <- get_corr(x, TRUE, method)
    C[abs(C) < cutoff] <- 0 -> diag(C)
    C[is.na(C)] <- 0
    p <- get_corr(x, FALSE, method) %>%
        as.vector() %>%
        p.adjust(method_adjust) %>%
        matrix(nrow = sqrt(length(.)), ncol = sqrt(length(.)))
    C[p >= 0.05] <- 0

    colnames(C) <- colnames(p) <- colnames(x) -> rownames(p) -> rownames(C)
    return(list(C = C, p = p))
}

#' @export
plot_corr_network <- function(
    x,
    method = "spearman",
    method_adjust = "BH",
    cutoff = 0.75,
    ...) {
    res <- get_corr1(x, method, method_adjust, cutoff)
    title <- round(mean(res$C, na.rm = TRUE), 2)
    edges <- get_edges(x, res$C, res$p)
    edges <- adjust_pvalue(edges, "p")
    font <- "14px arial black"
    edges$font.bold.mod <- ifelse(edges$p.adj < 0.05, paste(font, "bold"), font)
    edges$title <- round(edges[, 3], 2) -> edges$label
    nodes <- get_nodes(edges, 3)

    color_node <- ifelse(edges$weight > 0, "green", "red") -> edges$color
    edges$weight <- abs(edges$weight)

    plot_network2(
        x,
        res$C,
        shape = "dot",
        dashes = FALSE,
        nodes = nodes,
        edges = edges,
        cex_nodes = edges$weight * 20,
        color = c("white", "gray"),
        title = paste("Mean correlation between variables = ", title * 2),
        ...
    )
}
