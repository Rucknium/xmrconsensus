#' rpc
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
rpc.req <- function(unrestricted.rpc.url, method, params = "", handle = RCurl::getCurlHandle()) {

  json.post <- RJSONIO::toJSON(
    list(
      jsonrpc = "2.0",
      id = "0",
      method = method,
      params = params
    )
  )

  RJSONIO::fromJSON(
    RCurl::postForm(paste0(unrestricted.rpc.url, "/json_rpc"),
      .opts = list(
        userpwd = "",
        postfields = json.post,
        httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
      ),
      curl = handle
    ), asText = TRUE
  )

}
