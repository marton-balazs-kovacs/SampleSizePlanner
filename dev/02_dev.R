# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step is optional. 
# 

# 2. All along your project

## 2.1 Add modules

golem::add_module(name = "about")
golem::add_module(name = "home")
golem::add_module(name = "tost")
golem::add_module(name = "traditional_power")
golem::add_module(name = "traditional_power_anova")
golem::add_module(name = "power_curve")
golem::add_module(name = "apriori_precision")
golem::add_module(name = "rope")
golem::add_module(name = "interval_equiv_bf")
golem::add_module(name = "bf_threshold")
golem::add_module(name = "AIPE")
golem::add_module(name = "BFDA")
golem::add_module(name = "download")
golem::add_module(name = "justification")
golem::add_module(name = "code")

## 2.2 Add dependencies

usethis::use_package("waiter")
usethis::use_package("plotly")
usethis::use_package("ggplot2")
usethis::use_package("DiagrammeR")
usethis::use_package("tibble")
usethis::use_package("shinyMatrix")
usethis::use_package("shinyWidgets")
usethis::use_package("shinyjs")
usethis::use_package("shinyBS")
usethis::use_package("BayesFactor")
usethis::use_package("assertthat")
usethis::use_package("MBESS")
usethis::use_package("clipr")
usethis::use_package("papaja")
usethis::use_package("formatR")
usethis::use_package("config")
usethis::use_package("dplyr")
usethis::use_package("rlang")
usethis::use_package("purrr")
usethis::use_package("rclipboard")
usethis::use_package("stats")
usethis::use_package("shinyMatrix")
usethis::use_package("htmlwidgets")
usethis::use_package("magick")
usethis::use_package("base64enc")
usethis::use_package("bayestestR")

## 2.3 Add tests

usethis::use_test("app")

## 2.4 Add a browser button

golem::browser_button()

## 2.5 Add external files

golem::add_js_file("script")
golem::add_js_handler("btn_disable")
golem::add_css_file("custom")

# 3. Documentation

## 3.1 Vignette
usethis::use_vignette("SampleSizePlanner")
devtools::build_vignettes()

## 3.2 Code coverage
## You'll need GitHub there
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! 
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

usethis::use_pipe()
devtools::document()
devtools::load_all()
