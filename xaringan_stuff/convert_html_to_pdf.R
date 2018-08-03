browseURL("https://github.com/yihui/xaringan/wiki/Export-Slides-to-PDF")
browseURL("https://github.com/astefanutti/decktape")
browseURL("https://github.com/astefanutti/decktape/releases/tag/v1.0.0")

# 1. download and unzip source code zip
# 2. download phantomjs-msvc2013-x86, rename to phantomjs and copy it into the
#    extracted (source code) directory
# 3. execute ./phantomjs decktape.js -h


dir_deck = "pres/rmd/decktape-1.0.0"
files = dir("pres/rmd", pattern = ".html$")
for (i in files) {
  in_file = file.path("pres/rmd", i)
  out_file = file.path("pres/pdf", gsub(".html", ".pdf", i))
  cmd = paste(file.path(dir_deck, "phantomjs"), 
              file.path(dir_deck, "decktape.js"),
              in_file, out_file)
  system(cmd)
}

