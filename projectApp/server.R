# Platform downloaded from:
# http://www.cbc.ca/news/politics/canada-election-2015-party-platforms-1.3264887

function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing text...")
        getTextReady(input$selection)
      })
    })
  })
  
  mname <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      toupper(as.character(input$selection))
    })
  })
  
  
  # Make the wordcloud drawing predictable during a session
  trans_cloud_rep <- repeatable(trans_cloud)

  output$plot <- renderPlot({
    mytext <- terms()
    myname <- mname()
    trans_cloud_rep(text.var = mytext, proportional = TRUE, min.freq = input$freq, stem=FALSE,
                    target.words = list(health=c("health", "insurance", "medic", "hospital"), 
                                        economic = c("econom", "jobs", "job", "unemploy", "business", "banks", 
                                                     "budget", "market", "paycheck", "work", "tax", "sustainable",
                                                     "companies", "income", "trade", "g7"),
                                        foreign = c("war ", "terror", "foreign", "fear", "military", "international",
                                                    "nations"),
                                        class = c("middleclass", "poor", "rich"),
                                        candidate = c("tommulcair", "stephenharper","justintrudeau"),
                                        environment = c("environmental", "green", "climate")),
                    cloud.colors = c("red", "blue", "black", "orange", "purple", "light green", "gray45"),
                    legend = c("health", "economic", "foreign", "class", "candidate", "environment"),
                    stopwords=stopwordslist, char2space = "~", title.names = myname,
                    min.word.size = 0.9)
  })
  
  output$hist<- renderPlot({
    mytext <- terms()
    d <- getMatrix(mytext)
    # Determine matrix with freq min
    d <- d[which(d$freq > input$freq),]
    ## Increase bottom margin to make room for rotated labels
    par(mar = c(7, 4, 4, 2) + 0.1)
    ## Create plot with no x axis and no x axis label
    #plot(1 : 8, xaxt = "n",  xlab = "")
    bp <- barplot(d$freq[1:input$max], las=2, names.arg = d$word[1:input$max],
                  col ="red", main ="Most frequent words",
                  ylab = "Word frequencies", xaxt = "n",  xlab = "")
    ## Set up x axis with tick marks alone
    a = axis(1, at=bp, labels = FALSE, tick = FALSE)
    ## Create some text labels
    labels <- d[1:input$max,]$word
    labels <- sub("~", " ", labels)
    ## Plot x axis labels at default tick marks
    text(as.vector(a), par("usr")[3] - 10.0, srt = 45, adj = 1,
         labels = labels, xpd = TRUE)
  
  })
}
