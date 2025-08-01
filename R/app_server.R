#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  alt_chain_plot_height <- reactiveVal(1000)
  current.chaintip.hash <- reactiveVal("")

  unrestricted.rpc.url <- "http://127.0.0.1:18081"

  shiny::observe({

    invalidateLater(30 * 1000) # 30 seconds

    new.chaintip.hash <- rpc.req(unrestricted.rpc.url = unrestricted.rpc.url,
      method = "get_last_block_header", params = "")$result$block_header$hash

    draw.new.plot <- isolate( current.chaintip.hash() != new.chaintip.hash )

    # TODO: Cache plot for all users

    if (draw.new.plot) {

      isolate(current.chaintip.hash(new.chaintip.hash))

      result <- alt_chains_graph(unrestricted.rpc.url = "http://127.0.0.1:18081")

      alt_chain_plot_height(result$plot.height)

      layout.raw <- igraph::layout_as_tree(result$igraph.plot.data, flip.y = FALSE)

      output$plot1 <- renderPlot({
        # This is a workaround for incorrect edge plotting (likely, a bug
        # in the package) when the plot area is very tall.
        # First, create the plot area:

        plot(result$igraph.plot.data, layout = layout.raw,
          main = "",
          asp = 0,
          edge.arrow.mode = 0,
          margin = c(-0.15, 0.05, -0.1, 0.1),
          # The amount of empty space below, over, at the left and right of the plot
        )

        # Allow plotted elements to fall outside of plotting region:
        par(xpd = TRUE)
        # Then erase it:
        rect(xleft = -5, ybottom = -5, xright = 5, ytop = 5, col = "white")

        # Then manually plot the edges, which will go below the other plotting
        # elements later.

        layout.rescaled <- layout.raw

        # https://stackoverflow.com/questions/66602071/r-scaling-a-vector-between-1-and-1
        rescale_minMax <- function(x){
          1 - (x - max(x)) / (min(x) - max(x))
        }

        layout.rescaled[, 1] <- rescale_minMax(layout.rescaled[, 1]) * 2 - 1
        layout.rescaled[, 2] <- rescale_minMax(layout.rescaled[, 2]) * 2 - 1
        # Rescale t the plot cordinate area : -1 to 1.

        edgelist <- igraph::as_edgelist(result$igraph.plot.data, names = FALSE)

        for (i in seq_len(nrow(edgelist))) {
          v.1 <- edgelist[i, 1]
          v.2 <- edgelist[i, 2]
          lines(layout.rescaled[c(v.1, v.2), 1], layout.rescaled[c(v.1, v.2), 2],
            lwd = 1, col = "gray")
        }

        # Finally, plot the main plot

        plot(result$igraph.plot.data, layout = layout.raw,
          add = TRUE,
          main = "",
          asp = 0, vertex.shape = "square", vertex.frame.color = NA,
          edge.width = 0,
          edge.arrow.mode = 0,
          margin = c(-0.15, 0.05, -0.1, 0.1),
          vertex.label.family = "monospace",
          vertex.label.color = "black",
          # https://stackoverflow.com/questions/64207220/rendering-plot-in-r-with-mono-spaced-family-font-does-not-display-characters-any
          vertex.color = result$chain.attr$color,
          vertex.label = result$chain.attr$label
        )

        legend(x = -1, y = 2 + grconvertY(2.3, from = "inches", to = "user"),
          legend = c("Known pool", "Unknown pool or\nsolo miner"),
          fill = c("lightgreen", "pink"),
          cex = 1.5,
          bty = "n", horiz = TRUE)


      }, width = 500, height = alt_chain_plot_height()
      )

    }

  })

}
