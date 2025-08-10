#' pool_blocks
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
pool_blocks <- function(unrestricted.rpc.url, aggregation.hours, which.pools, pool.chart.begin) {

  aggregation.hours <- as.numeric(aggregation.hours)

  handle <- RCurl::getCurlHandle()

  # https://docs.getmonero.org/rpc-library/monerod-rpc/#get_last_block_header
  last_block_header <- rpc.req(unrestricted.rpc.url, method = "get_last_block_header", params = "", handle = handle)

  # TODO: Error handling if RPC fails
  chaintip.height <- last_block_header$result$block_header$height

  # https://docs.getmonero.org/rpc-library/monerod-rpc/#get_block_headers_range
  block_headers <- rpc.req(unrestricted.rpc.url, method = "get_block_headers_range",
    params = list(start_height = 3473380, end_height = chaintip.height),
    # 3473380 is about at pool.chart.begin (will trim it to exactly
    # pool.chart.begin later)
    handle = handle)

  block_headers <- data.table::rbindlist(block_headers$result$headers, fill = TRUE)

  pools <- data.table::fread("data-raw/pools/blocks.csv", fill = TRUE)
  # fill = TRUE because CSV file format changed
  data.table::setnames(pools, c("Id", "Pool"), c("hash", "pool"))

  block_headers.attr <- block_headers[timestamp >= pool.chart.begin, .(hash, height, timestamp)]

  block_headers.attr <- merge(block_headers.attr, pools, all.x = TRUE)
  block_headers.attr[is.na(pool), pool := "unknown"]


  block_headers.attr[, timestamp := as.POSIXct(timestamp)]

  block_headers.attr[, timestamp.binned := timestamp - as.numeric(timestamp) %%
      (60 * 60 * aggregation.hours) + (60 * 60 * aggregation.hours)]
  # Add aggregation.hours so that it the binned value is the end of the period

  block_headers.attr <- block_headers.attr[timestamp.binned <= Sys.time() , ]
  # Remove rows that are in the current time interval. We want complete intervals.

  block_headers.attr[, total.in.timestamp.bin := .N, by = "timestamp.binned"]

  blocks.pools <- block_headers.attr[, .(n.blocks = .N,
    share.blocks = .N / unique(total.in.timestamp.bin)),
    by = c("timestamp.binned", "pool")]

  blocks.pools <- blocks.pools[pool %in% which.pools, ]

  if (data.table::uniqueN(blocks.pools$pool) > 5) {

    aggregate.these.pools <- blocks.pools[, .(share.blocks.sum = sum(share.blocks)), by = "pool"]

    data.table::setorder(aggregate.these.pools, -share.blocks.sum)

    aggregate.these.pools <- aggregate.these.pools$pool[-(1:5)]

    aggregate.these.pools <- setdiff(aggregate.these.pools, "unknown")
    # Do not aggregate the unknown miners with "other known pools"

    aggregated.pools <- blocks.pools[pool %in% aggregate.these.pools, .(
      pool = "other known pools",
      n.blocks = sum(n.blocks),
      share.blocks = sum(share.blocks)), by = "timestamp.binned"]

    blocks.pools <- rbind(blocks.pools[ ! pool %in% aggregate.these.pools, ], aggregated.pools)

  }

  data.table::setorder(blocks.pools, timestamp.binned)

  blocks.pools

}

