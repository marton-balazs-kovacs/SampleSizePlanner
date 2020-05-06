# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step is optional. 
# 

# 2. All along your project

## 2.1 Add modules
### TODO: Change names everywhere
golem::add_module( name = "question" )
golem::add_module( name = "article" )
golem::add_module( name = "flowchart" )
golem::add_module( name = "about" )
golem::add_module( name = "tost" )
golem::add_module( name = "menu" )
golem::add_module( name = "traditional_power" )
golem::add_module( name = "power_curve" )
golem::add_module( name = "apriori_precision" )
golem::add_module( name = "rope" )
golem::add_module( name = "interval_equiv_bf" )
golem::add_module( name = "bf_threshold" )

## 2.2 Add dependencies

usethis::use_package( "waiter" )
usethis::use_package( "DiagrammeR" )
usethis::use_package( "shinyglide" )
usethis::use_package( "tibble" )
usethis::use_package( "shinyWidgets" )
usethis::use_package( "shinyjs" )
usethis::use_package( "shinyBS" )
usethis::use_package( "BayesFactor" )

## 2.3 Add tests

usethis::use_test( "app" )

## 2.4 Add a browser button

golem::browser_button()

## 2.5 Add external files

golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

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
