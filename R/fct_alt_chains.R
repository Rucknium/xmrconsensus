



#' alt_chains_graph
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
alt_chains_graph <- function(unrestricted.rpc.url,
  n.blocks.display.chaintip = 10, n.blocks.display.after.orphan = 1,
  mining.pool.data.available = TRUE) {

  handle <- RCurl::getCurlHandle()

  # https://docs.getmonero.org/rpc-library/monerod-rpc/#get_alternate_chains
  alt_chains <- rpc.req(unrestricted.rpc.url, method = "get_alternate_chains", params = "", handle = handle)

  # https://docs.getmonero.org/rpc-library/monerod-rpc/#get_last_block_header
  last_block_header <- rpc.req(unrestricted.rpc.url, method = "get_last_block_header", params = "", handle = handle)

  # TODO: Error handling if RPC fails
  chaintip.height <- last_block_header$result$block_header$height

  # https://docs.getmonero.org/rpc-library/monerod-rpc/#get_block_headers_range
  block_headers <- rpc.req(unrestricted.rpc.url, method = "get_block_headers_range",
    params = list(start_height = chaintip.height - 720 * 7, end_height = chaintip.height),
    # Get data from about a week ago to today
    handle = handle)

  if (length(alt_chains$result$chains) == 0 ) {
    stop(paste0("Monero node is not aware of any alt chains or orphaned blocks.\n",
      "This can occur if your node was restarted recently.\n",
      "You can check if you have any alt chains by going to the monerod console\n",
      "(the terminal window where monerod is running) and inputting 'alt_chain_info'.\n",
      "Since there are no alt chains, the app has nothing interesting to display and has exited."))
  }


  alt_chains <- lapply(alt_chains$result$chains, function(x) {
    chain.length <- length(x$block_hashes)
    base.starting.height <- x$height - chain.length + 1
    # Docs seem wrong "height - unsigned int; the block height of the first diverging block of this alternative chain."
    # (Docs are being fixed.)
    # Seems that height is the height of the _last_ diverging block of the chain.
    result <- data.table(prev_hash = x$main_chain_parent_block,
      hash = x$block_hashes[chain.length],
      height = base.starting.height, main_chain_parent_block = x$main_chain_parent_block)
    if (length(x$block_hashes) == 1) { return(result) }
    result <- rbind(result,
      data.table(
        prev_hash = x$block_hashes[2:chain.length],
        hash = x$block_hashes[1:(chain.length - 1)],
        height = base.starting.height + rev(seq_len(chain.length - 1)),
        main_chain_parent_block = x$main_chain_parent_block))
    return(result)
  })


  block_headers <- data.table::rbindlist(block_headers$result$headers, fill = TRUE)

  alt_chains <- data.table::rbindlist(alt_chains)

  # https://stackoverflow.com/questions/58109098/cumulative-sum-based-on-a-condition-but-reset-after-condition-ends
  block_headers[, not.alt := ! (hash %in% alt_chains$prev_hash | height %in% alt_chains$height)]
  # prev_hash gets the main chain block that the chain diverges from. height gets
  # any additional blocks if the alt chain is longer than one block. height
  # also gets the first block after the divergence when there is a single orphan block

  block_headers[, rearrange.index := if(data.table::first(not.alt)) cumsum(not.alt) else 0,
    data.table::rleid(not.alt)]

  block_headers <- block_headers[rearrange.index %in% 0:(1 + n.blocks.display.after.orphan) |
      height >= (max(height) - n.blocks.display.chaintip), ]

  block_headers$prev_hash[2:nrow(block_headers)] <- block_headers$hash[-nrow(block_headers)]
  # This makes sure that the false link that bridges over omitted blocks is mended

  max.blocks.displayed <- 150

  if (nrow(block_headers) > max.blocks.displayed) {
    block_headers <- block_headers[(nrow(block_headers) - (max.blocks.displayed - 1)):nrow(block_headers), ]
  }


  block_headers.graph <- block_headers[, .(prev_hash, hash)]

  block_headers.attr <- block_headers[, .(hash, height, num_txes, rearrange.index)]

  block_headers.attr <- block_headers.attr[, blocks.omitted := c(diff(height), 0)]

  if (mining.pool.data.available) {
    pools <- data.table::fread("data-raw/pools/blocks.csv", fill = TRUE)
    # fill = TRUE because CSV file format changed
  } else {
    pools <- structure(list(Height = integer(0), Id = character(0),
      Timestamp = structure(numeric(0), class = "integer64"),
      Reward = character(0), Pool = character(0), Valid = logical(0),
      Miner = character(0)), row.names = c(NA, 0L), class = c("data.table",
        "data.frame"))
  }

  data.table::setnames(pools, c("Id", "Pool"), c("hash", "pool"))

  block_headers.attr <- merge(block_headers.attr, pools, all.x = TRUE)
  block_headers.attr[is.na(pool), pool := "unknown"]

  # Testing
  # block_headers.attr[, pool := rep(LETTERS, 10)[seq_len(nrow(block_headers.attr))] ]
  # block_headers.attr[1, pool := "unknown" ]

  orphaned.blocks.last.day <- alt_chains[height >= chaintip.height - 720, .N]
  # 720 is approximately one day of blocks
  # Get orphaned.blocks.last.day before alt_chains is trimmed to
  # max.blocks.displayed (150) main chain blocks by the next line

  alt_chains <- alt_chains[main_chain_parent_block %in% unlist(block_headers), ]

  data.table::setorder(alt_chains, height)
  # Important to set order like this so that orphan blocks alternate sides

  alt_chains.graph <- alt_chains[, .(prev_hash, hash, main_chain_parent_block)]

  alt_chains.attr <- alt_chains[, .(hash, height)]
  alt_chains.attr[, blocks.omitted := 0]

  alt_chains.attr <- merge(alt_chains.attr, pools, all.x = TRUE)
  alt_chains.attr[is.na(pool), pool := "unknown"]

  # Testing
  # alt_chains.attr[, pool := letters[seq_len(nrow(alt_chains.attr))] ]

  alt_chains.attr[, rearrange.index := 0]

  if (nrow(alt_chains.graph) == 0) {
    chain.graph <- block_headers.graph
    alt_chains.graph.chunks <- NULL
  } else {
    alt_chains.graph.chunks <- split(alt_chains.graph, by = "main_chain_parent_block")
  }

  if (length(alt_chains.graph.chunks) == 1) {
    chain.graph <- rbind(block_headers.graph, alt_chains.graph, fill = TRUE)
    chain.graph[, main_chain_parent_block := NULL]
    # Don't need this anymore
  }

  if (length(alt_chains.graph.chunks) > 1) {

    alt_chains.graph.top.seq <- seq(1, length(alt_chains.graph.chunks), by = 2)

    alt_chains.graph.end.seq <- seq(2, length(alt_chains.graph.chunks), by = 2)

    chain.graph <- rbind(
      data.table::rbindlist(alt_chains.graph.chunks[alt_chains.graph.top.seq]),
      block_headers.graph,
      data.table::rbindlist(alt_chains.graph.chunks[alt_chains.graph.end.seq]),
      fill = TRUE)
    # Need to do this so that the orphaned alt chains
    # alternate between left and right of the main chain
    # in the plot
    chain.graph[, main_chain_parent_block := NULL]
    # Don't need this anymore
  }

  alt_chains.attr[, num_txes := 1]
  # TODO: get number of txs of alt chain blocks. Assuming at least one
  # tx in each alt block for now.

  chain.attr <- rbind(block_headers.attr, alt_chains.attr)

  chain.attr[, label := paste0(
    "Height: ", prettyNum(height, big.mark = ","), "\n",
    "Hash: ", substr(hash, 1, 8), "...", "\n",
    "Pool: " , pool
  )]

  chain.attr[rearrange.index == (1 + n.blocks.display.after.orphan) & height < (max(height) - n.blocks.display.chaintip),
    label := paste0("[", prettyNum(blocks.omitted, big.mark = ","), " blocks of\nuncontested chain omitted]")]
  # Don't display "blocks omitted" if the orphan block is in the chaintip

  chain.attr[, color := ifelse(pool == "unknown", "pink", "lightgreen")]
  chain.attr[rearrange.index == (1 + n.blocks.display.after.orphan) & height < (max(height) - n.blocks.display.chaintip),
    color := "yellow"]

  vertex.order <- unique(c(t(as.matrix(chain.graph))))
  # This is how things are arranged in the plot

  chain.attr <- chain.attr[match(vertex.order, hash), ]
  chain.attr[is.na(color), color := "yellow"]
  chain.attr[is.na(label), label := paste0("[", prettyNum(blocks.omitted, big.mark = ","), " blocks of\nuncontested chain omitted]")]
  # "Missing" because this vertex only appears in 'prev_hash', not 'hash'

  chain.attr[, shape := ifelse( (num_txes == 0 | is.na(num_txes)) & color != "yellow",
    "circle", "square")]

  chain.attr[, is.alt.block := hash %in% alt_chains$hash]

  igraph.plot.data <- igraph::graph_from_edgelist(as.matrix(chain.graph), directed = TRUE)

  plot.height <- nrow(block_headers.graph) * 60

  list(igraph.plot.data = igraph.plot.data, chain.attr = chain.attr,
    plot.height = plot.height, orphaned.blocks.last.day = orphaned.blocks.last.day)

}
