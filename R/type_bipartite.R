# ===========================================================================
# cliomicplot: Transkingdom & Multi-Omics Bipartite Interaction Network
# Bacteria-Fungi & Microbiome-Metabolome Cross-Domain Architectures
# Inspired by ggClusterNet 2 (corBionetwork, Wen et al. iMeta 2025)
# ===========================================================================

#' Transkingdom & Multi-Omics Bipartite Network Type
#'
#' @description
#' Creates a publication-grade bipartite interaction network connecting two biological
#' domains or kingdoms (e.g., Bacteria vs Fungi, or Gut Microbiota vs Serum Metabolites)
#' with signed correlation edges and cross-domain hub highlighting (Wen et al. iMeta 2025).
#'
#' @param domain_a Character label for Domain A (default \code{"Bacteria"}).
#' @param domain_b Character label for Domain B (default \code{"Fungi"}).
#' @param layout Character string specifying spatial arrangement: \code{"bipolar"} (default,
#'   two facing vertical or circular columns), \code{"concentric"} (Domain A in inner circle,
#'   Domain B in outer ring), or \code{"parallel"}.
#' @param edge_color_pos Color for positive cross-domain correlations (default \code{"#4DBBD5"}, cyan).
#' @param edge_color_neg Color for negative cross-domain correlations (default \code{"#E64B35"}, coral red).
#' @param edge_alpha Numeric transparency for cross-domain edges (default 0.65).
#' @param edge_width_range Range of line widths mapped to absolute correlation strength (default \code{c(0.3, 1.8)}).
#' @param node_color_a Color for Domain A nodes (default \code{"#3B4992"}, deep royal blue).
#' @param node_color_b Color for Domain B nodes (default \code{"#008B45"}, emerald green).
#' @param node_size_range Numeric vector of length 2 specifying point sizes mapped to degree (default \code{c(2.5, 6.5)}).
#' @param label_top Integer. Number of top cross-domain hubs to label per domain (default 5).
#' @param label_size Numeric font size for node labels (default 2.8).
#' @param cor_cutoff Numeric minimum absolute correlation threshold to include edges (default 0.35).
#' @param p_cutoff Numeric p-value threshold for edge significance (default 0.05).
#' @param nodes Optional data frame of nodes with columns \code{node}, \code{domain}, and optional \code{phylum}.
#' @param edges Optional data frame of edges with columns \code{from}, \code{to}, \code{cor} (or \code{weight}),
#'   and optional \code{p_value}.
#'
#' @return A \code{cliplot_type} object for use with \code{\link{cliplot}}.
#'
#' @export
#' @examples
#' \dontrun{
#' cliplot(type = type_bipartite(domain_a = "Bacteria", domain_b = "Metabolites",
#'                               nodes = nodes_df, edges = edges_df))
#' }
type_bipartite = function(
    domain_a         = "Bacteria",
    domain_b         = "Fungi",
    layout           = c("bipolar", "concentric", "parallel"),
    edge_color_pos   = "#4DBBD5",
    edge_color_neg   = "#E64B35",
    edge_alpha       = 0.65,
    edge_width_range = c(0.3, 1.8),
    node_color_a     = "#3B4992",
    node_color_b     = "#008B45",
    node_size_range  = c(2.5, 6.5),
    label_top        = 5,
    label_size       = 2.8,
    cor_cutoff       = 0.35,
    p_cutoff         = 0.05,
    nodes            = NULL,
    edges            = NULL
) {
  layout <- match.arg(layout)

  cliplot_type(
    name = "bipartite",
    data = function(settings, ...) {
      settings$domain_a         <- domain_a
      settings$domain_b         <- domain_b
      settings$layout           <- layout
      settings$edge_color_pos   <- edge_color_pos
      settings$edge_color_neg   <- edge_color_neg
      settings$edge_alpha       <- edge_alpha
      settings$edge_width_range <- edge_width_range
      settings$node_color_a     <- node_color_a
      settings$node_color_b     <- node_color_b
      settings$node_size_range  <- node_size_range
      settings$label_top        <- label_top
      settings$label_size       <- label_size
      settings$cor_cutoff       <- cor_cutoff
      settings$p_cutoff         <- p_cutoff
      settings$nodes            <- nodes
      settings$edges            <- edges
      if (is.null(settings$legend)) settings$legend <- "bottom"
    },
    draw = function(data, mapping, settings, ...) {
      nodes_df <- settings$nodes
      edges_df <- settings$edges

      # Generate demo data if null
      if (is.null(nodes_df) || is.null(edges_df)) {
        set.seed(42)
        n_a <- 18
        n_b <- 14
        a_names <- paste0("Bact_", sprintf("%02d", seq_len(n_a)))
        b_names <- paste0("Fung_", sprintf("%02d", seq_len(n_b)))

        nodes_df <- data.frame(
          node   = c(a_names, b_names),
          domain = c(rep(settings$domain_a, n_a), rep(settings$domain_b, n_b)),
          phylum = c(
            sample(c("Bacteroidetes", "Firmicutes", "Actinobacteria", "Proteobacteria"), n_a, replace = TRUE),
            sample(c("Ascomycota", "Basidiomycota", "Mucoromycota"), n_b, replace = TRUE)
          ),
          stringsAsFactors = FALSE
        )

        # Generate sparse cross-domain edges
        grid <- expand.grid(from = a_names, to = b_names, stringsAsFactors = FALSE)
        grid$cor <- stats::rnorm(nrow(grid), mean = 0.05, sd = 0.45)
        grid$p_value <- stats::runif(nrow(grid), 0.001, 0.3)
        edges_df <- grid[abs(grid$cor) >= settings$cor_cutoff & grid$p_value <= settings$p_cutoff, ]
      }

      names(nodes_df) <- tolower(names(nodes_df))
      names(edges_df) <- tolower(names(edges_df))

      cor_col <- names(edges_df)[which(names(edges_df) %in% c("cor", "correlation", "weight", "r"))[1]]
      edges_df$weight <- as.numeric(edges_df[[cor_col]])
      edges_df$sign   <- factor(ifelse(edges_df$weight >= 0, "Positive (+)", "Negative (-)"),
                                levels = c("Positive (+)", "Negative (-)"))

      # Compute degree per node
      deg_table <- table(c(as.character(edges_df$from), as.character(edges_df$to)))
      nodes_df$degree <- as.numeric(deg_table[as.character(nodes_df$node)])
      nodes_df$degree[is.na(nodes_df$degree)] <- 0

      # Spatial coordinates
      nodes_a <- nodes_df[nodes_df$domain == settings$domain_a, , drop = FALSE]
      nodes_b <- nodes_df[nodes_df$domain == settings$domain_b, , drop = FALSE]

      if (settings$layout == "concentric") {
        # Domain A in inner circle, Domain B in outer ring
        th_a <- seq(0, 2*pi, length.out = nrow(nodes_a) + 1)[-(nrow(nodes_a) + 1)]
        nodes_a$x <- 2.2 * cos(th_a)
        nodes_a$y <- 2.2 * sin(th_a)

        th_b <- seq(0, 2*pi, length.out = nrow(nodes_b) + 1)[-(nrow(nodes_b) + 1)]
        nodes_b$x <- 4.2 * cos(th_b)
        nodes_b$y <- 4.2 * sin(th_b)
      } else {
        # Bipolar: two facing columns/arcs
        y_a <- seq(3.8, -3.8, length.out = nrow(nodes_a))
        nodes_a$x <- -3.2 - 0.4 * (1 - (y_a / 4)^2)
        nodes_a$y <- y_a

        y_b <- seq(3.8, -3.8, length.out = nrow(nodes_b))
        nodes_b$x <- 3.2 + 0.4 * (1 - (y_b / 4)^2)
        nodes_b$y <- y_b
      }

      nodes_placed <- rbind(nodes_a, nodes_b)
      rownames(nodes_placed) <- as.character(nodes_placed$node)

      # Map edge coordinates
      edges_df$x_start <- nodes_placed[as.character(edges_df$from), "x"]
      edges_df$y_start <- nodes_placed[as.character(edges_df$from), "y"]
      edges_df$x_end   <- nodes_placed[as.character(edges_df$to), "x"]
      edges_df$y_end   <- nodes_placed[as.character(edges_df$to), "y"]
      edges_df <- edges_df[!is.na(edges_df$x_start) & !is.na(edges_df$x_end), ]

      # Top hubs for labeling
      top_a <- utils::head(nodes_a[order(-nodes_a$degree), ], settings$label_top)
      top_b <- utils::head(nodes_b[order(-nodes_b$degree), ], settings$label_top)
      top_labels <- rbind(top_a, top_b)
      top_labels <- top_labels[top_labels$degree > 0, ]

      # Build graphic
      p <- ggplot2::ggplot()

      # Edges
      if (nrow(edges_df) > 0) {
        p <- p + ggplot2::geom_segment(
          data = edges_df,
          ggplot2::aes(
            x = .data$x_start, y = .data$y_start,
            xend = .data$x_end, yend = .data$y_end,
            color = .data$sign,
            linewidth = abs(.data$weight)
          ),
          alpha = settings$edge_alpha,
          lineend = "round"
        ) +
        ggplot2::scale_color_manual(
          values = c("Positive (+)" = settings$edge_color_pos, "Negative (-)" = settings$edge_color_neg),
          name   = "Cross-Domain Link: "
        ) +
        ggplot2::scale_linewidth_continuous(
          range = settings$edge_width_range,
          name  = "Absolute Correlation: "
        )
      }

      # Domain background hulls or banners if bipolar
      if (settings$layout != "concentric") {
        domain_headers <- data.frame(
          x = c(-3.4, 3.4),
          y = c(4.3, 4.3),
          label = c(settings$domain_a, settings$domain_b),
          color = c(settings$node_color_a, settings$node_color_b)
        )
        p <- p + ggplot2::geom_text(
          data = domain_headers,
          ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
          fontface = "bold",
          size = 3.6,
          color = domain_headers$color
        )
      }

      # Nodes
      p <- p +
        ggplot2::geom_point(
          data = nodes_placed,
          ggplot2::aes(x = .data$x, y = .data$y, fill = .data$domain, size = .data$degree),
          shape = 21,
          color = "white",
          stroke = 0.5,
          alpha = 0.95
        ) +
        ggplot2::scale_fill_manual(
          values = stats::setNames(c(settings$node_color_a, settings$node_color_b),
                                  c(settings$domain_a, settings$domain_b)),
          name   = "Domain: "
        ) +
        ggplot2::scale_size_continuous(
          range = settings$node_size_range,
          name  = "Degree: "
        )

      # Labels for top hubs
      if (nrow(top_labels) > 0) {
        if (settings$layout == "concentric") {
          top_labels$hjust <- ifelse(top_labels$x >= 0, -0.15, 1.15)
          p <- p + ggplot2::geom_text(
            data = top_labels,
            ggplot2::aes(x = .data$x, y = .data$y, label = .data$node, hjust = .data$hjust),
            size = settings$label_size,
            fontface = "bold",
            color = "#2A2D34"
          )
        } else {
          top_labels$hjust <- ifelse(top_labels$domain == settings$domain_a, 1.15, -0.15)
          p <- p + ggplot2::geom_text(
            data = top_labels,
            ggplot2::aes(x = .data$x, y = .data$y, label = .data$node, hjust = .data$hjust),
            size = settings$label_size,
            fontface = "bold",
            color = "#2A2D34"
          )
        }
      }

      p <- p +
        ggplot2::theme_void() +
        ggplot2::theme(
          legend.position = settings$legend,
          legend.box = "horizontal",
          legend.title = ggplot2::element_text(size = 8, face = "bold"),
          legend.text  = ggplot2::element_text(size = 7.5),
          plot.margin  = ggplot2::margin(10, 20, 10, 20, "pt")
        ) +
        ggplot2::guides(
          fill      = ggplot2::guide_legend(nrow = 1, override.aes = list(size = 4)),
          color     = ggplot2::guide_legend(nrow = 1, override.aes = list(linewidth = 1.2)),
          size      = ggplot2::guide_legend(nrow = 1),
          linewidth = ggplot2::guide_legend(nrow = 1)
        )

      p
    }
  )
}
