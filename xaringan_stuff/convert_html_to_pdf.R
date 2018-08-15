browseURL("https://github.com/yihui/xaringan/wiki/Export-Slides-to-PDF")
browseURL("https://github.com/astefanutti/decktape")
browseURL("https://github.com/astefanutti/decktape/releases/tag/v1.0.0")

# 1. download and unzip source code zip
# 2. download phantomjs-msvc2013-x86, rename to phantomjs and copy it into the
#    extracted (source code) directory
# 3. execute ./phantomjs decktape.j

cmd = paste("~/bin/phantomjs /usr/bin/decktape", in_file, out_file)
system(cmd)

file_name <- paste0("file://", normalizePath("my_xaringan.html"))
files = dir("pres/", recursive = TRUE, pattern = ".html$")
files = files[1]
in_file = normalizePath(file.path("pres", files[1]))
out_file = gsub(".html", ".pdf", in_file)
# webshot does not work, I suspect because I have not installed Chrome
for (i in files) {
  in_file = i
  out_file = gsub(".html", ".pdf", in_file)
  webshot(files, "~/uni/test.pdf")
}

# ok, doing it without Chrome
cmd = paste("decktape", in_file, out_file, "--no-sandbox")
system(cmd)

cmd = paste("decktape remark", in_file, out_file)
system(cmd)

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





