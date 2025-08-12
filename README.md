
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `xmrconsensus`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`xmrconsensus` is an alpha project. Expect breaking changes in future
versions.

## Live deployment

An accessible live deployment of this app is available at
[moneroconsensus.info](https://moneroconsensus.info).

## Running `xmrconsensus` on your own computer

Deployment has only been tested on Linux.

### Run a Monero node

You must have a Monero node with an unrestricted RPC port available to
run the app. Normally, the only way to have access to an unrestricted
RPC port is to run a Monero node on your own machine. The quickest way
to start running a Monero node is to download the [“Monero CLI Wallet”
at the getmonero.org website](https://www.getmonero.org/downloads/#cli)
for your operating system and start the `monerod` node software on your
command line. A pruned node should be fine for the purposes of this app.
You will need to wait a while, up to a few days, to download and verify
the blockchain. Only sync on an internal SSD. Do not use an HDD or USB
drive. As of August 2025, an unpruned node will occupy about 260GB of
storage. A pruned node will occupy about 100GB of storage.

If you already have a Monero node running, just keep it running. If you
are starting up a completely new node or re-starting a node after some
time of it being turned off, you can add the `--keep-alt-blocks` flag to
the `monerod` startup arguments to [preserve the data on alternative
chains between
restarts](https://docs.getmonero.org/interacting/monerod-reference/#testing-monero-itself).
If this flag is not enabled, the node will delete the alternative chain
data the next time it shuts down.

### Download and install `xmrconsensus`

Install [R](https://www.r-project.org/). Linux users should install the
`r-base` and `r-base-dev` system packages.

Clone this repo into a directory on your local machine with:

``` bash
git clone https://github.com/Rucknium/xmrconsensus.git
```

Go into the `xmrconsensus` directory by inputting `cd xmrconsensus`.
Then start an R session by inputting `R` into the terminal. Install the
package:

``` r
install.packages("devtools")
devtools::install(upgrade = FALSE, Ncpus = 4)
```

If you have greater or fewer than 4 threads available on your machine,
you can adjust the `Ncpus` argument. If you are on Linux, compilation of
all the package dependencies may take some time. close the R session
after it is finished installing by inputting `quit(save = "no")`.

### Download and build `monero-blocks`

`moneroconsensus` needs a separate tool to fetch data on which mining
pools claim each block. The tool is `monero-blocks`, written in Go by
DataHorader and available
[here](https://git.gammaspectra.live/WeebDataHoarder/monero-blocks). You
should have [a recent version of Go](https://go.dev/doc/install) on your
machine.

To set up the tool, start in the `xmrconsensus`. Then input into your
console:

``` bash
cd data-raw/pools
git clone https://git.gammaspectra.live/WeebDataHoarder/monero-blocks.git
cd monero-blocks
go build
```

This will create the `monero-blocks` binary executable program in the
`data-raw/pools/monero-blocks` directory.

### Run `monero-blocks`

Next, you need to run `monero-blocks` in a loop to create the
`blocks.csv` file in the `data-raw/pools` directory and collect mined
block data as pools post it in their API. The easiest way to do this is
navigate back to the `data-raw/pools`, start an R session, and input

``` r
xmrconsensus::pools.collect()
```

If your Monero node’s unrestricted RPC is not at the default
`http://127.0.0.1:18081`, then use
`xmrconsensus::pools.collect(unrestricted.rpc.url = "http://127.0.0.1:18081")`
instead, replacing `http://127.0.0.1:18081` with the full URL and port
of your node’s unrestricted RPC.

You will need to leave the process running in its own terminal window.

### Run `xmrconsensus`

Go to the `xmrconsensus` directory in a terminal. Open an R session and
input

``` r
xmrconsensus::run_app()
```

By default, R will open your default web browser to the Shiny app. If
you prefer to open your web browser yourself, use this instead:

``` r
xmrconsensus::run_app(options = list(launch.browser = FALSE))
```

The full local URL, with port number, will be printed to the console.
Paste the URL into your internet browser.

If your Monero node’s unrestricted RPC is not at the default
`http://127.0.0.1:18081`, then use
`xmrconsensus::run_app(unrestricted.rpc.url = "http://127.0.0.1:18081")`
instead, replacing `http://127.0.0.1:18081` with the full URL and port
of your node’s unrestricted RPC.

### Update `xmrconsensus`

To get the latest version of `xmrconsensus`, pull the latest git repo
and install:

``` bash
cd xmrconsensus
git pull
R -e "devtools::install(upgrade = FALSE)"
```

And restart `xmrconsensus::run_app()`.
