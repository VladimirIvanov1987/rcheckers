# app.R
library(shiny)
library(pkgload)


pkgload::load_all(".")


rcheckers::run_app()
