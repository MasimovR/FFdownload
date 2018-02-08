#' Downloads tables
#'
#'
#' Downloads tables
#'
#'
#' @param output_file Name of the .Rdata file
#'
#'
#'






download_tables <- function(output_file = "data.Rdata") {

  URL <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html"
  pg <- read_html(URL)
  Flinks <- html_attr(html_nodes(pg, "a"), "href")
  Findex <- grep("CSV.zip",Flinks)

  temp <- tempdir()
  for (i in 1:length(Findex)){
    Fdest <- gsub("ftp/","",Flinks[Findex[i]])
    download.file(paste0("http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/",Flinks[Findex[i]]), paste0(temp,"/", Fdest))
  }

  zip_files <- list.files(temp, full.names = TRUE, pattern = "\\.zip$", ignore.case = TRUE)

  lapply(zip_files, function (x) unzip(zipfile = x, exdir = temp))

  csv_files <- list.files(temp, full.names = TRUE, pattern = "\\.csv$", ignore.case = TRUE)

  vars <- paste0("x_", gsub(paste0(temp, "/(.*)\\..*"), "\\1", csv_files))

  returns <- new.env()

  mapply(function(x, y) assign(x, converter(y), envir = returns), vars,  csv_files)

  returns <- as.list(returns)
  save(returns, file = output_file)
}




