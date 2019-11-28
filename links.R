# create a PubPsych search string with term boosting
####################################################

# https://stackoverflow.com/questions/28117556/clickable-links-in-shiny-datatable
# https://lucene.apache.org/core/2_9_4/queryparsersyntax.html#Boosting%20a%20Term


# boosting by beta probabilities
################################

# PubPsych.eu search terms are boosted according to the relations of beta probabilites
# Factors were computed by dividing the beta probabilites of Terms 1-4 by beta of Term 5


createLink <- function(val, boost, topicnum) {
  list <- list()
  for (i in 1:length(val)){
    list[[i]] <- unlist(strsplit(val[i], ", ", fixed = TRUE))
    for (j in 1:4){
      list[[i]][j] <- paste0('"', list[[i]][j], '"%5E', boost[j, topicnum[i]]) # add boost factors for first 4 terms
      # list[[i]][j] <- paste0('"', list[[i]][j], '"%5E', boost[[i]][j]) STM version
    }
    list[[i]][5] <- paste0('"', list[[i]][5], '"') # Term 5 is reference, so no boosting
    list[[i]] <- paste0(list[[i]], collapse="+OR+")
    list[[i]] <- gsub("'", "%27", list[[i]])
  }
  val <- unlist(list)
  paste0("<a href='https://pubpsych.zpid.de/pubpsych/Search.action?q=%28CT%3D%28", 
         val,"%29%29+DB%3DPSYNDEX&stats=TOP' target='_blank' class='btn btn-primary'>Search PSYNDEX</a>")
}
