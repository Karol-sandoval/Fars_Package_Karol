#' @title Fars_read function
#'
#' @description 
#' This is a function that allows you to load all types of 
#' documents with .csv extension. You can load these documents
#' using the filename argument followed by the .csv extension 
#' inside double quotes and you will get a data frame in tibble 
#' format to work in the console.
#'
#' @param filename of excel file whose extension is .csv.
#' 
#' @return This function returns a data frame of class tbd_df, 
#' which is a special case of the base data.frame. This tibble 
#' format has the advantage of printing only the first 10 rows 
#' and columns of the file that fit on the screen. 
#' 
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' 
#' @note  You must make sure you have the file where the R 
#' directory is located or otherwise modify the directory, 
#' as this could generate an error; you must also write the 
#' file name correctly without omitting any characters and 
#' put the name between double quotes, otherwise it will 
#' generate an error and do not forget  to load the 
#' external packages.
#' 
#' @examples
#' fars_read("directory/filename.csv")
#' fars_read("accident_2013.csv")
#' fars_read("accident_2013.csv.bz2")
#' fars_read("accident_2014.csv.bz2")
#' fars_read("accident_2015.csv.bz2")
#' 
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}



#' @title Make_filename function
#'
#' @description 
#' this function allows you to generate character objects 
#' containing a formatted combination of input values.
#' 
#' @param year year found in the file name
#' 
#' @return this function returns a formatted combination of 
#' input values, where the year is the input and the result 
#' is the string "accident_year.csv.bz2".
#' 
#' @importFrom base sprintf
#' 
#' @note  you must make sure to put a numeric object as input,
#' because if you put a text string you will get an error and 
#' a warning will come up if you put the text string between 
#' double quotes.
#' 
#' @examples
#' make_filename(year)
#' make_filename(2013)
#' make_filename(2014)
#' 
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}





#' @title Fars_read_years function
#'
#' @description 
#' function that takes a set of years to generate file names 
#' and thus extract the month and year corresponding to each 
#' record in the dataset.
#' 
#' @param Years a vector of numerical objects (years).
#' 
#' @return this function returns a list where each component 
#' of the list contains the month and year of each record 
#' belonging to the exported dataset.
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' 
#' @note The function will create the name of a file 
#' depending on the set of input years, if the year 
#' does not exist, the function will throw an error. 
#' 
#' @examples
#' fars_read_years(c(2013,2014))
#' 
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>% 
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}




#' @title Fars_summarize_years function
#'
#' @description 
#' function that takes a set of years and calls the 
#' function fars_read_years, then joins them together 
#' and at the end you get a summary of the data.
#' 
#' @param Years a vector of numerical objects (years).
#' 
#' @return this function returns a summarized data frame 
#' where the first column corresponds to the month and the 
#' following columns correspond to the count performed in 
#' the selected years.
#' 
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' 
#' @note As this function depends on the function 
#' fars_read_years the error for the non-existence 
#' of the year will also be generated.
#' 
#' @examples
#' fars_summarize_years(c(2013,2014))
#' 
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}




#' @title Fars_map_state function
#'
#' @description 
#' this function takes the state number and plots 
#' the accidents per selected year.
#' 
#' @param state.num number of the state to plot the crashes
#' @param year year of choice for plotting the accidents occurred

#' 
#' @return this function returns the graph of the selected 
#' state from the year of interest.

#' 
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' 
#' @note as this function depends on make_filename 
#' and fars_read it is important to write the name 
#' of the file correctly and type the year properly, 
#' otherwise the function will return an error. 
#' In case there is no data for the selected state(integer) 
#' the function will stop and will throw an error, 
#' in case there are no accidents the function will 
#' notify through a message.
#' 
#' @examples
#' fars_map_state(1,2013)
#' 
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}

