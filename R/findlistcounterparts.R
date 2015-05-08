# find_list_counterparts()
# This is an example function named find_list_counterparts
# When a data frame (parameter data_frame_name) has multiple values in one column (parameter column_to_search_keyword)
# if these values are separated by a special splitter character sequence such as "%%%" (parameter splitter),
# then the column values can be searched for special keywords suhc as "Industry" (parameter search_keyword)
# if another column (parameter column_to_return_keyword) has a different info in the same sequence
# of values splitted by splitter, then this function returns the relevant counterpart keywords
# in the column_to_return_keyword
#
# an example use case is as follows:
#in the following data frame carowners,
#findlistcounterparts(carowners,"cars_made_in","cars_brand","France","%%") would return alist as
#Citroen Renault
#1       2

#data frame for above sample
#owner_name|cars_brand|cars_made_in
#John|Citroen%%Volkswagen|France%%Germany
#Bob|Chevrolet%%Renault|US%%France
#Alice|Mercedes%%Renault|Germany%%France
#James||
#Mary|Nissan|Japan


findlistcounterparts <- function(data_frame_name, column_to_search_keyword, column_to_return_keyword, search_keyword, splitter="%%%") {
  characterized_search_columns <- as.character(data_frame_name[, column_to_search_keyword])
  search_keywords_as_list      <- strsplit(characterized_search_columns, splitter, fixed = TRUE)
  unlisted_search_keywords     <- unlist(search_keywords_as_list)
  indices_of_returned          <- which(unlisted_search_keywords == search_keyword)
  
  characterized_return_columns <- as.character(data_frame_name[, column_to_return_keyword])
  return_keywords_as_list      <- strsplit(characterized_return_columns, splitter, fixed = TRUE)
  unlisted_return_keywords     <- unlist(return_keywords_as_list)
  
  searchedKeywords <-  unlisted_return_keywords [indices_of_returned]
  return(table(searchedKeywords))
  #return(indices_of_returned)
}

