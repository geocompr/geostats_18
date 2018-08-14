browseURL("https://github.com/yihui/xaringan/wiki/Export-Slides-to-PDF")
browseURL("https://github.com/astefanutti/decktape")
browseURL("https://github.com/astefanutti/decktape/releases/tag/v1.0.0")

# 1. download and unzip source code zip
# 2. download phantomjs-msvc2013-x86, rename to phantomjs and copy it into the
#    extracted (source code) directory
# 3. execute ./phantomjs decktape.js -h


files = dir("pres/", recursive = TRUE, pattern = ".html$")
files = files[1]
for (i in files) {
  in_file = file.path("pres", i)
  out_file = file.path("pres", gsub(".html", ".pdf", i))
  webshot(in_file, out_file)

}

# Windows style
# dir_deck = "xaringan_stuff/decktape-1.0.0"
# files = dir("pres/", recursive = TRUE, pattern = ".html$")
# for (i in files) {
#   in_file = file.path("pres", i)
#   out_file = file.path("pres", gsub(".html", ".pdf", i))
#   cmd = paste(file.path(dir_deck, "phantomjs"),
#               file.path(dir_deck, "decktape.js"),
#               in_file, out_file)
#   system(cmd)
# }

