library(tm)
library(wordcloud)
library(memoise)
library(qdap)

# The list of valid party
parties <<- list("Liberal Party" = "liberal",
              "Conservative Party" = "conservative",
              "New Democratic Party" = "ndp",
              "Green Party" = "green")

stopwordslist <- tolower(Top100Words)

# Using "memoise" to automatically cache the results
getTextReady <- memoise(function(party) {
  
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(party %in% parties))
    stop("Unknown party")
  
  mytext <- readLines(sprintf("./%s.txt", party),
                      encoding="UTF-8", skipNul = TRUE)
  
  load("special3.RData")
  
  for(ii in 3:10){
    mytext = gsub(vec3[ii], "", mytext, fixed = TRUE)
  }

  # Convert the text to lower case
  mytext <- tolower(mytext)
  
  # Remove numbers with text attached to it
  mytext = gsub("[0-9]st", "", mytext, fixed = TRUE)
  
  mytext = gsub("[0-9]s", "", mytext, fixed = TRUE)
  
  # Remove numbers
  mytext <- removeNumbers(mytext)
  
  # Eliminate extra white spaces
  mytext <- stripWhitespace(mytext)
  
  mytext <- bracketX(mytext)  #removes brackets (non dialogue)
  
  mytext <- scrubber(gsub(vec3[1], " ", mytext)) #removes dashes
  mytext <- scrubber(gsub(vec3[2],"",mytext))
  
  mytext <- removePunctuation(mytext)
  mytext <- scrubber(gsub("_", " ", mytext))
  
  mytext <- mytext[mytext != ""]
  mytext <- mytext[mytext != " "]
  
  mytext <- mytext[mytext != "q"]
  mytext <- mytext[mytext != "c"]
  mytext <- mytext[mytext != "s"]
  
  
  mytext <- scrubber(gsub(" c ","", mytext))

  mytext <- scrubber(gsub("ei ", "", mytext))
  mytext <- scrubber(gsub("chapter", "", mytext))
  mytext <- scrubber(gsub(as.character(party), "", mytext))
  
  # Eliminate extra white spaces
  mytext <- stripWhitespace(mytext)
  
  mytext = gsub(" stephen harpers ", "stephen harper", mytext)
  mytext = gsub(" tom mulcairs ", "tom mulcair", mytext)
  mytext = gsub(" justin trudeaus ", "justin trudeau", mytext)
  mytext = gsub(" justins ", "justin", mytext)
  mytext = gsub(" harpers ", "stephen harper", mytext)
  mytext = gsub(" harper ", "stephen harper", mytext)
  mytext = gsub(" stephen ", "stephen harper", mytext)
  mytext = gsub(" elizabeth mays ", "elizabeth may", mytext)
  
  mytext = gsub("canadians", "", mytext)
  mytext = gsub("canadian", "", mytext)
  mytext = gsub("canada", "", mytext)
  
  mytext = gsub("our", "", mytext)
  
  mytext = gsub(" f ", "", mytext)
  
  mytext = gsub("governments", "government", mytext)
  mytext = gsub("investments", "investment", mytext)
  mytext = gsub("investing", "investment", mytext)
  mytext = gsub("benefits", "benefit", mytext)
  mytext = gsub("taxes", "tax", mytext)
  
  mytext <- mgsub(c("stephen harper", "tom mulcair", "justin trudeau", "elizabeth may", "middle class", "health care", " g "), 
                  c("stephen~harper", "tom~mulcair", "justin~trudeau", "elizabeth~may", "middle~class", "health~care", " g~7 "), 
                  mytext)
})

# Using "memoise" to automatically cache the results
getMatrix <- memoise(function(mytext) {
  # Load the data as a corpus
  docs <- Corpus(VectorSource(mytext))
  
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwordslist)
  
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  
  d <- data.frame(word = names(v),freq=v)
})
