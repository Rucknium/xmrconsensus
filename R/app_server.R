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
  draw.new.plot <- reactiveVal(FALSE)
  last.update.time <- reactiveVal(as.character(round(Sys.time())))

  unrestricted.rpc.url <- golem::get_golem_options("unrestricted.rpc.url")
  # https://github.com/ColinFay/golemexample#passing-arguments-to-run_app
  # http://127.0.0.1:18081 by default

  pool.chart.begin <- as.POSIXct("2025-08-08 12:00:00", tz = "UTC")


  observe({
    input$n_blocks_display_chaintip
    input$n_blocks_display_after_orphan
    draw.new.plot(TRUE)
    # Trigger draw of new plot if either of these inputs change
  })

  shiny::observe({

    invalidateLater(75 * 1000) # 75 seconds. Good sync with monero-blocks running every 60 seconds.

    new.chaintip.hash <- rpc.req(unrestricted.rpc.url = unrestricted.rpc.url,
      method = "get_last_block_header", params = "")$result$block_header$hash


    # TODO: Cache plot for all users

    if ( draw.new.plot() ) {

      isolate(current.chaintip.hash(new.chaintip.hash))

      result <- alt_chains_graph(unrestricted.rpc.url = unrestricted.rpc.url,
        n.blocks.display.chaintip = input$n_blocks_display_chaintip,
        n.blocks.display.after.orphan = input$n_blocks_display_after_orphan)

      output$orphaned_blocks_last_day <- renderText(result$orphaned.blocks.last.day)
      output$orphaned_blocks_last_day_percent <-
        renderText(round( 100 * result$orphaned.blocks.last.day / 720, digits = 2))

      isolate(alt_chain_plot_height(result$plot.height))

      layout.raw <- igraph::layout_as_tree(result$igraph.plot.data, flip.y = FALSE)

      output$plot1 <- renderPlot({
        # This is a workaround for incorrect edge plotting (likely, a bug
        # in the package) when the plot area is very tall.
        # First, create the plot area:

        if (input$dark_mode == "dark") {
          par(bg = "#1D1F21")
          # "#1D1F21" is "bs-secondary-bg" for bslib
          result$chain.attr[color == "pink", color := "darkred"]
          result$chain.attr[color == "lightgreen", color := "darkgreen"]
          result$chain.attr[color == "yellow", color := "yellow4"]
        } else {
          par(bg = "white")
          result$chain.attr[color == "darkred", color := "pink"]
          result$chain.attr[color == "darkgreen", color := "lightgreen"]
          result$chain.attr[color == "yellow4", color := "yellow"]
          # Need to the "else" so that toggle works between plot refreshes
        }

        plot(result$igraph.plot.data, layout = layout.raw,
          main = "",
          asp = 0,
          edge.arrow.mode = 0,
          margin = c(-0.15, 0.1, -0.1, 0.1),
          # The amount of empty space below, over, at the left and right of the plot
        )

        # Allow plotted elements to fall outside of plotting region:
        par(xpd = TRUE)
        # Then erase it:
        rect(xleft = -5, ybottom = -5, xright = 5, ytop = 5,
          col = ifelse(input$dark_mode == "dark", "#1D1F21", "white"))

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
          asp = 0,
          vertex.shape = result$chain.attr$shape,
          vertex.frame.color = "darkgray",
          vertex.size = 20,
          edge.width = 0,
          edge.arrow.mode = 0,
          margin = c(-0.15, 0.1, -0.1, 0.1),
          vertex.label.family = "monospace",
          vertex.label.color = ifelse(input$dark_mode == "dark", "white", "black"),
          # https://stackoverflow.com/questions/64207220/rendering-plot-in-r-with-mono-spaced-family-font-does-not-display-characters-any
          vertex.color = result$chain.attr$color,
          vertex.label = result$chain.attr$label
        )

        legend(x = -1.25, y = 2 + grconvertY(3, from = "inches", to = "user"),
          legend = c("Known pool", "Unknown pool or\nsolo miner"),
          fill = ifelse(input$dark_mode == rep("dark", 2), c("darkgreen", "darkred"),
            c("lightgreen", "pink")),
          cex = 1.5,
          text.col = ifelse(input$dark_mode == "dark", "white", "black"),
          border = ifelse(input$dark_mode == "dark", "white", "black"),
          bty = "n", horiz = TRUE)

        legend(x = -1.5, y = 2 + grconvertY(2.25, from = "inches", to = "user"),
          legend = c("Block containing zero transactions",
            "Block containing one or more transactions"),
          pch = c(1, 0),
          cex = 1.5,
          pt.cex = 2,
          text.col = ifelse(input$dark_mode == "dark", "white", "black"),
          col = ifelse(input$dark_mode == "dark", "white", "black"),
          bty = "n", horiz = FALSE)


      }, width = 500, height = alt_chain_plot_height()
      )


    }

    isolate( draw.new.plot(current.chaintip.hash() != new.chaintip.hash ))

    isolate(last.update.time(as.character(round(Sys.time()))))
    output$last_update_time <- renderText(last.update.time())

  })






  observe({
    pools <- data.table::fread("data-raw/pools/blocks.csv", fill = TRUE)
    # fill = TRUE because CSV file format changed
    data.table::setnames(pools, tolower) # Applied tolower() function to all column names

    pools <- pools[timestamp / 1000 >= pool.chart.begin, ]
    # Timestamp is in milliseconds
    # Ignoring whether block was added to main chain, for simplicity

    pools.tabulation <- pools[, .(n.blocks = .N), by = "pool"]
    data.table::setorder(pools.tabulation, -n.blocks)
    updateCheckboxGroupInput(session, "which_pools",
      label = NULL,
      choices = c("unknown", pools.tabulation$pool),
      selected = c("unknown", pools.tabulation$pool))

  })


  output$plot2 <- plotly::renderPlotly({


    blocks.pools <- pool_blocks(unrestricted.rpc.url = unrestricted.rpc.url,
      aggregation.hours = input$pool_aggregation_hours, which.pools = input$which_pools,
      pool.chart.begin = pool.chart.begin)

    if (input$pool_percentage_or_number == "percentage") {
      blocks.pools[, display.data := round(share.blocks * 100, digits = 2)]
    }
    if (input$pool_percentage_or_number == "number") {
      blocks.pools[, display.data := n.blocks]
    }

    plotly::plot_ly(x = ~ blocks.pools$timestamp.binned, line = list(width = 5)) |>
      plotly::add_lines(y = ~ blocks.pools$display.data,
        color = ~ factor(blocks.pools$pool)
      ) |>
      plotly::layout(
        title = list(text = paste0("Blocks mined by Monero mining pools: ",
          input$pool_aggregation_hours, "-hour intervals")),
        margin = list(t = 100, l = 0, r = 0),
        hovermode = "x",
        plot_bgcolor= ifelse(input$dark_mode == "dark", "#1D1F21", "#fff"),
        paper_bgcolor = ifelse(input$dark_mode == "dark", "#1D1F21", "#fff"),
        font = list(color = ifelse(input$dark_mode == "dark", "#fff", "#444"),
          size = 18),
        xaxis = list(
          title = "Time in UTC time zone",
          gridcolor = ifelse(input$dark_mode == "dark", "#444", "lightgray")),
        yaxis = list(
          title = ifelse(input$pool_percentage_or_number == "percentage",
            "Percentage of blocks mined during interval", "Number of blocks mined during interval"),
          gridcolor = ifelse(input$dark_mode == "dark", "#444", "lightgray"),
          rangemode = "tozero",
          zerolinecolor = ifelse(input$dark_mode == "dark", "#fff", "#1D1F21"),
          zerolinewidth = 2,
          side = "right",
          ticksuffix = ifelse(input$pool_percentage_or_number == "percentage", "%", "")),
        # https://stackoverflow.com/questions/44638590/format-axis-tick-labels-to-percentage-in-plotly
        legend = list(orientation = "h", xanchor = "center", x = 0.5, yanchor = "top", y = 1.05)
      ) |>
      plotly::config(displayModeBar = FALSE)




  })







}
