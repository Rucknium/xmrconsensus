
#' Collect claimed blocks from mining pools' APIs
#'
#' @description
#'
#' Uses the `monero-blocks` tool to continuously collect data from mining
#' pools' APIs about which Monero blocks they have claimed to mine. Data is
#' written to a CSV file named `blocks.csv`.
#'
#' The `monero-blocks` Go program must be built in a 'monero-blocks'
#' local working directory. Available at
#' \url{https://git.gammaspectra.live/WeebDataHoarder/monero-blocks}
#'
#' This function is an infinite loop. Input `ctrl + c` to interrupt.
#'
#'
#' @param unrestricted.rpc.url URL and port of the `monerod` unrestricted RPC.
#' Default is `⁠http://127.0.0.1:18081`
#' @param init.blocks Number of recent blocks to collect on initiation.
#' @param recent.blocks Number of recent blocks to collect while running
#' continuously.
#' @param poll.time Time interval, in seconds, between each API request.
#'
#' @returns
#' NULL (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' pools.collect()
#' }
pools.collect <- function(unrestricted.rpc.url = "http://127.0.0.1:18081",
  init.blocks = 720 * 7, recent.blocks = 720, poll.time = 60) {

  handle <- RCurl::getCurlHandle()

  last_block_header <- tryCatch(
    rpc.req(unrestricted.rpc.url, method = "get_last_block_header",
      handle = handle), error = function(e) {NULL})

  if (length(last_block_header) == 0) {
    stop(paste0("Invalid RPC response from monerod. Is the Monero node's unrestricted RPC available at ",
      unrestricted.rpc.url, "?"))
  }

  if ( ! file.exists("monero-blocks/monero-blocks")) {
    stop("Cannot find the 'monero-blocks' program in the 'monero-blocks' local working directory.\nDid you remember to build it?")
  }

  system(paste0("monero-blocks/monero-blocks --height ", chaintip.height - init.blocks))
  # Initial data download

  # Infinite loop. Input ctrl + c to stop.
  while(TRUE) {

    last_block_header <- tryCatch(
      rpc.req(unrestricted.rpc.url, method = "get_last_block_header",
        handle = handle), error = function(e) {NULL})

    if (length(last_block_header) == 0) {
      message("Invalid RPC response from monerod.")
      Sys.sleep(poll.time)
      next
    }

    chaintip.height <- last_block_header$result$block_header$height
    system(paste0("monero-blocks/monero-blocks --height ", chaintip.height - recent.blocks))
    Sys.sleep(poll.time)
  }

  return(invisible(NULL))

}
