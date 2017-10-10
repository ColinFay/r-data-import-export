library(tidyverse)
library(stringr)
library(glue)
library(magrittr)

file.create("ga")
write("<!-- Global Site Tag (gtag.js) - Google Analytics -->\n<script async src=\"https://www.googletagmanager.com/gtag/js?id=UA-65307055-3\"></script>\n<script>\n  window.dataLayer = window.dataLayer || [];\n  function gtag(){dataLayer.push(arguments);}\n  gtag('js', new Date());\n\n  gtag('config', 'UA-65307055-3');\n</script>", file="ga",append=TRUE)
# Download epub

download.file("https://cran.r-project.org/doc/manuals/r-release/R-data.epub", destfile = "importexport.epub")
unzip(zipfile = "importexport.epub")
file.remove(c("toc.ncx","titlepage.xhtml", "stylesheet.css"))

# Rename file and keep a track of file change
rename_file <- function(name){
  new_file <- gsub("(R-data)_split_([0-9]*)", "\\2-\\1", name)
  new_file <- gsub("^0", "", new_file)
  file.rename(from = name, to = new_file)
  return(data.frame(orig = name, new = new_file))
}

file_names_change <- purrr::map_df(list.files(pattern = "R-data"), rename_file)

html_converter <- function(file){
  file_name <- gsub("\\.html", "", file)
  system(command = glue("pandoc {file_name}.html -o {file_name}.Rmd"))
}

purrr::walk(list.files(pattern = "R-data"), html_converter)

purrr::walk(list.files(pattern = "\\.html"), file.remove)

clean_html_rmd <- function(file){
  
  a <- readLines( file )
  
  a %<>% str_replace_all("<h1 .*>([A-Za-z0-9])", "# \\1") %>%
    str_replace_all("</h1>", "")%>% 
    str_replace_all("<h2 .*>([A-Za-z0-9])", "# \\1") %>%
    str_replace_all("</h2>", "") %>%
    str_replace_all("# [0-9]+", "# ")
  write(a, file = file)
}

purrr::walk(list.files(pattern = "Rmd"), clean_html_rmd)

clean_auto_ref <- function(file){
  
  a <- readLines( file )
  
  a %<>% str_replace_all("(R-data)_split_([0-9]*)", "\\2-\\1") %>%
    str_replace_all("^0", "")
  write(a, file = file)
  
}

purrr::walk(list.files(pattern = "Rmd"), clean_auto_ref)

build_url_ref <- function(file){
  a <- readLines( file )
  url <- tolower(a[1]) %>%
    str_replace_all("# *", "") %>%
    str_replace_all(" ", "-")
  return(glue("{url}.html"))
}

file_names_change$url <- map(list.files(pattern = "data.Rmd"), build_url_ref)

# Manual changes to index.Rmd

# Append some files 

file.append("index.Rmd", "02-R-data.Rmd")
file.append("index.Rmd", "03-R-data.Rmd")

# Remove some useless files

file.remove(c("01-data.Rmd","02-literature.Rmd","03-method.Rmd",
              "04-application.Rmd", "05-summary.Rmd","06-references.Rmd"))


file.remove(c("00-R-data.Rmd","01-R-data.Rmd", "02-R-data.Rmd", "03-R-data.Rmd"))

# Manually replace url 

clean_url <- function(file){
  a <- readLines( file )
  
  a %<>% str_replace_all("004-R-data.html", "introduction.html") %>%
    str_replace_all("005-R-data.html", "spreadsheet-like-data.html") %>%
    str_replace_all("006-R-data.html", "importing-from-other-statistical-systems.html") %>%
    str_replace_all("007-R-data.html", "relational-databases.html") %>%
    str_replace_all("008-R-data.html", "binary-files.html") %>%
    str_replace_all("009-R-data.html", "image-files.html") %>%
    str_replace_all("010-R-data.html", "connections.html") %>%
    str_replace_all("011-R-data.html", "network-interfaces.html") %>%
    str_replace_all("012-R-data.html", "reading-excel-spreadsheets.html") %>%
    str_replace_all("013-R-data.html", "appendix-a-references.html") %>%
    str_replace_all("014-R-data.html", "function-and-variable-index.html") %>%
    str_replace_all("015-R-data.html", "concept-index.html")  
  write(a, file = file)
}

purrr::walk(list.files(pattern = "Rmd"), clean_url)

replace_in_file <- function(file, pattern, replacement){
  a <- read_file(file) 
  a <- str_replace_all(a, pattern, replacement)
  write(a, file)
}

purrr::map(list.files(pattern = "Rmd"), replace_in_file,  pattern = "\\\\", replacement = "&#92;")


# Build \o/

bookdown::render_book("index.Rmd", "bookdown::gitbook")
