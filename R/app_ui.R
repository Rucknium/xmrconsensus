#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_fluid(
      titlePanel("Monero Consensus Status"),
      shiny::h5(shiny::strong(shiny::HTML('The open source code for this web app is available <a href="https://github.com/Rucknium/xmrconsensus">here</a>. If website is unstable, <a href="https://github.com/Rucknium/xmrconsensus?tab=readme-ov-file#running-xmrconsensus-on-your-own-computer">run it on your own computer</a>.'))),
      shiny::br(),
      shiny::tabsetPanel(
        shiny::tabPanel("⛓ Orphaned blocks",
          shiny::br(),
          shiny::h5(shiny::HTML('This web app displays a visualization of recent <a href="https://monero.stackexchange.com/questions/3311/what-are-orphaned-blocks">orphaned blocks</a> and alternative chains of the Monero blockchain.')),
          shiny::br(),
          shiny::h5(shiny::HTML('Occasional orphaned blocks are normal. They occur naturally when two miners mine different valid blocks almost simultaneously. A high rate of orphaned blocks can indicate a problem in network-wide connection latency or even malicious behavior by one or more entities with a large hashpower share.')),
          shiny::br(),
          shiny::h5(shiny::HTML('If a malicious entity with a high share of network hashpower attempted a selfish mining strategy to raise its share of block rewards, the rate of orphaned blocks could increase. The malicious entity would cause the blocks of other pools to become orphaned. This evidence would appear in the visualization below.')),
          shiny::br(),
          shiny::h5(shiny::HTML('A malicious entity with 50 percent or more of total network hashpower could attempt deep chain re-organizations by mining an alternative chain. Alternative chain are like orphaned blocks, but are more than one block in length. Alternative chains should appear in the visualization below.')),
          shiny::br(),
          shiny::h5(shiny::HTML('This web app is new and untested.')),
          shiny::br(),
          shiny::h5('Click to toggle dark mode: '),
          bslib::input_dark_mode(id = "dark_mode"),
          shiny::sliderInput("n_blocks_display_chaintip",
            "Number of chaintip blocks to display (most recent blocks):",
            min = 5, max = 30, value = 10, step = 5, width = "500px"),
          shiny::sliderInput("n_blocks_display_after_orphan",
            "Number of blocks to display after each orphan/altchain:",
            min = 0, max = 10, value = 1, step = 1, width = "500px"),
          shiny::h5('Note: Total displayed chain length will not exceed 150.'),
          shiny::br(),
          shiny::br(),
          shiny::h5(shiny::strong(shiny::HTML(paste0(
            shiny::textOutput("orphaned_blocks_last_day", inline = TRUE),
            " (", shiny::textOutput("orphaned_blocks_last_day_percent", inline = TRUE), "%)",
            ' block(s) orphaned in last 720 blocks (about 24 hours).')))),
          shiny::h5(shiny::HTML(paste0("Last checked for new block: ",
            shiny::textOutput("last_update_time", inline = TRUE), " UTC"))),
          shiny::br(),
          # golem::golem_welcome_page() # Remove this line to start building your UI
          plotOutput("plot1", inline = TRUE),
          # inline = TRUE means that it displays the plot with dimensions specified on the server side
        ),
        shiny::tabPanel("📈 Blocks mined by mining pools",
          shiny::br(),
          shiny::h5(shiny::HTML('Below is a line chart of the percentage of blocks mined by each mining pool. Over long intervals, the percentage of blocks mined by each mining pool can help assess certain risks in the Monero ecosystem. A malicious mining pool that acquires a majority of mining hashpower for a period of time can cause problems for ordinary users and other miners, depending on its choices:')),
          shiny::br(),
          shiny::h3(shiny::HTML('Double-spend attack')),
          shiny::br(),
          shiny::h5(shiny::HTML('Arguably the most harmful behavior of a majority-hashpower miner, often known simply as a "51% attack", is a double-spend attack. Satoshi Nakamoto, the creator of bitcoin, described it in <a href="https://nakamotoinstitute.org/library/bitcoin/#calculations">section 11 of his 2008 bitcoin white paper</a>:')),
          shiny::h5(shiny::HTML('<blockquote>We consider the scenario of an attacker trying to generate an alternate chain faster than the honest chain. Even if this is accomplished, it does not throw the system open to arbitrary changes, such as creating value out of thin air or taking money that never belonged to the attacker. Nodes are not going to accept an invalid transaction as payment, and honest nodes will never accept a block containing them. An attacker can only try to change one of his own transactions to take back money he recently spent.</blockquote>')),
          shiny::br(),
          shiny::h5(shiny::HTML('To execute the double-spend attack, a miner would have to send Monero coins to a victim, such as a merchant or centralized exchange. The victim would wait a certain number of block confirmations, depending on the victim\'s preference, and then send an irretrievable item to the miner, such as a physical good shipped in the mail or coins of another type of cryptocurrency.')),
          shiny::br(),
          shiny::h5(shiny::HTML('Meanwhile, the miner would be mining an alternative chain in secret that contains a transaction that contradicts the "payment" to the victim. Once the malicious miner\'s chain was long enough and enough blocks on the honest public chain are confirmed, the miner would broadcast the secret chain, which causes a "blockchain reorganization" and reverses the payment to the victim.')),
          shiny::br(),
          shiny::h5(shiny::HTML('Such an event would be observable to everyone running a Monero node. The "Orphaned blocks" visualization on the previous page would show a long alternative chain of many blocks being orphaned by the network. Also, one or more transactions present in the alternative chain would not be present in the main chain, and vice versa. The majority hashpower miner cannot reveal private information about Monero transactions nor force honest Monero nodes to accept blocks that do not comply with the requirements of the consensus of Monero\'s blockchain protocol.')),
          shiny::br(),
          shiny::h3(shiny::HTML('Selfish mining all blocks')),
          shiny::br(),
          shiny::h5(shiny::HTML('A miner with majority hashpower could decide to reject all blocks of other miners and instead only build on top of the malicious miner\'s own blocks. This behavior would deprive all other miners of block reward revenue. There would be little direct disruption to ordinary users in this situation.')),
          shiny::br(),
          shiny::h3(shiny::HTML('Mining empty blocks')),
          shiny::br(),
          shiny::h5(shiny::HTML('If a majority-hashpower miner mines all blocks, it can choose to include zero transactions in all blocks. That would stop all new transactions from being confirmed on the blockchain. This behavior would cause a major disruption to all ordinary users until the miner is unwilling or unable to mine all empty blocks.')),
          shiny::br(),
          shiny::h3(shiny::HTML('Mining only certain transactions')),
          shiny::br(),
          shiny::h5(shiny::HTML('Instead of mining blocks with zero transactions, a miner could include only certain transactions in blocks. Monero transactions are mostly indistinguishable from one another, so it is not clear on what basis a miner would reject/accept transactions. However, the miner could demand a higher fee per transaction, at least.')),
          shiny::br(),
          shiny::h3(shiny::HTML('Chart')),
          shiny::br(),
          shiny::h5(shiny::HTML('The below chart aggregates the percentage of blocks each mining pool has mined in 6, 12, or 24 hour intervals. Mining blocks is a random process, but just as large sample sizes can provide reliable statistical estimates, large aggregation intervals can give a picture of actual hashpower of each mining pool. The Monero protocol aims to confirm a block about every two minutes on average. Therefore, about 180, 360, and 720 blocks should be produced in each 6, 12, and 24 hour interval, respectively, in normal circumstances.')),
          shiny::br(),
          shiny::h5(shiny::HTML('The time indicated on the horizontal axis is the time of the end of the measured interval, e.g. when the aggregation interval is set to 6 hours, the value at 18:00 UTC is the aggregate value between 12:00 UTC and 18:00 UTC. The data starts at August 8, 2025 at 12:00 UTC because almost all mining pools\' APIs had been added to the <a href="https://git.gammaspectra.live/WeebDataHoarder/monero-blocks">pool data collection tool</a> by that time. Any known pools that are not in the top 5 pools are included in the "other known pools" category.')),
          shiny::br(),
          shiny::h5(shiny::HTML('Glide your mouse over the chart\'s lines to view exact numbers.')),
          plotly::plotlyOutput("plot2", height = "800px"),
        shiny::radioButtons("pool_aggregation_hours", "Number of hours in aggregation interval:",
            choices = c(6, 12, 24), selected = 12, inline = TRUE, width = "50%"),
          shiny::radioButtons("pool_percentage_or_number", "Display percentages or number of blocks:",
            choiceNames = c("Percentage of all blocks mined", "Number of blocks"),
            choiceValues = c("percentage", "number"),
            selected = "percentage", inline = FALSE, width = "50%"),
          shiny::checkboxGroupInput("which_pools", "Display these mining pools:", inline = TRUE),
          shiny::h5('Click to toggle dark mode: '),
          bslib::input_dark_mode(id = "dark_mode"),
        )
      ),
      shiny::hr(),
      shiny::h5(shiny::HTML('Created by <a href="https://github.com/Rucknium">Rucknium</a> at the <a href="https://github.com/monero-project/research-lab">Monero Research Lab</a>')),
      shiny::h6(shiny::HTML('Pool mining data collected by <a href="https://git.gammaspectra.live/WeebDataHoarder/monero-blocks">monero-blocks</a>, developed by DataHoarder.'))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "xmrconsensus"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
