# Before running anything, please run this:
cfg <- 'options(
  repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/noble/latest"),
  HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(),
    paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"]))
)'
writeLines(cfg, "~/.Rprofile")   # future sessions
eval(parse(text = cfg))          # this session, right now
