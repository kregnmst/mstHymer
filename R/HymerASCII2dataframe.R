read_hymerASCII <- function(Hymer_ASCII){

  library(readr)
  library(dplyr)
  library(stringr)
  library(lubridate)


  # Indlæs filen som en vektor af linjer
  lines <- read_lines(Hymer_ASCII)#("your_file.TXT")#

  # Find linjerne med [SERIE]
  start_serie_lines <- grep("\\[SERIE\\]", lines)

  start_data_lines <- grep("\\[DATA\\]", lines)

  all_header_lines <- lines[start_serie_lines[1]:(start_data_lines[1]-1)]
  all_data_lines <- lines[start_data_lines[1]:length(lines)]

  start_serie_lines <- grep("\\[SERIE\\]", all_header_lines)

  start_data_lines <- grep("\\[DATA\\]", all_data_lines)

  alleserier <- data.frame()

  # Løb gennem hver [SERIE]
  for (i in seq_along(start_serie_lines)) {
    ############
    # Header dataframe
    ############

    # Find slutningen af den i'te [SERIE], HEADER
    serie_end <- ifelse(i < length(start_serie_lines), start_serie_lines[i+1] - 1, length(all_header_lines))
    # Uddrag linjerne for denne [SERIE], HEADER
    serie_linjer <- all_header_lines[(start_serie_lines[i]+1):serie_end]
    # # Trimm white space, Split linjerne på "=" vælge første del
    serie_header_names <- str_trim(str_split_i(serie_linjer, "=",1))
    headerkolonnenavne <- unique(serie_header_names)
    # Opret en tom dataframe til at gemme station information
    headerdf <- data.frame(matrix(ncol = length(headerkolonnenavne), nrow = 0))
    colnames(headerdf) <- headerkolonnenavne
    # Trimm white space, Split linjerne på "=" vælge anden del
    trimhead <- t(str_trim(str_split_i(serie_linjer, "=",2)))
    headervalues <- as.data.frame(trimhead)
    colnames(headervalues) <- headerkolonnenavne
    # færdig header i'te serie df
    headerdf <- rbind(headerdf,headervalues) #%>% mutate(dataserie = i)


    ############
    # Data dataframe
    ############



    data_serie_end <- ifelse(i < length(start_data_lines), start_data_lines[i+1] - 1, length(all_data_lines))
    serie_linjer <- all_data_lines[(start_data_lines[i]+1):data_serie_end]
    dato=ymd_hms(str_split_i(serie_linjer," ",1))
    værdi=as.numeric(str_split_i(serie_linjer," ",3))
    data_df <- data.frame(dato,værdi)

    serie_df <- cbind(do.call("rbind", replicate(
      length(serie_linjer), headerdf, simplify = FALSE)),data_df)

    alleserier <- rbind(alleserier,serie_df)
  }
  return(alleserier)
}

