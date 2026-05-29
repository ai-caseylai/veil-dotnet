#!/usr/bin/env Rscript
# usage: [script] --path "http://127.0.0.1:8888" --bu 106 --port 16479

library(methods)
library(optparse)
library(shiny)

optionList <- list(make_option(c('--path'), default = ""),
                   make_option(c('--port'), default = 16479),
                   make_option(c('--bu'), default = 106),
                   make_option(c('--logpath'), default = "./log/"))
parser <- OptionParser(usage = "%prog [options]", option_list=optionList);
cmdOptions <- parse_args(parser)

# Global variable to server.R
.GlobalEnv$bu <- cmdOptions$bu
.GlobalEnv$path <- cmdOptions$path
.GlobalEnv$logpath <- cmdOptions$logpath

shiny::runApp(host = "0.0.0.0", port = cmdOptions$port)
