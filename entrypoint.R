library(plumber)
pr("api_range.R") %>% pr_run(host = "0.0.0.0", port = as.integer(Sys.getenv("PORT", 8001)))
