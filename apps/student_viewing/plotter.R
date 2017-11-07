

parse_time <- function(column) {
    return(strptime(as.character(column),"%Y-%m-%dT%H:%M"))
}

# mpid level data showing intervals
# results from elastic::fetch_histogram_by_mpid
plot_mpid_by_interval_df <- function(d, title, guide=FALSE, vline=-1, offset=0, colors=T) {
    data <- with(d, d[order(interval, huid),])
    # can use hms() instead
    if (colors) {
        g = ggplot(data=data, aes_string(x= "interval+offset", y="viewcount", fill="factor(huid)" ))
    }
    else {
        g = ggplot(data=data, aes_string(x= "interval+offset", y="viewcount"))
    }
    g = g + geom_bar(stat = "identity")
    g = g + labs(title= title)
    g = g + ylab("Number of views (each color represents 1 student)")
    g = g + theme(axis.text.x = element_text(size=11), #,angle=90),
        axis.text.y = element_text(size=10),
        legend.text=element_text(size=7),
        legend.key.size = unit(.4, "cm"),
        plot.title=element_text(size=12, vjust=3),
        plot.margin = unit(c(1,0.9,1,1), "cm"),
        axis.text=element_text(size=11),
        axis.title = element_text(size=11),
        axis.title.y=element_text(margin=margin(r = 10)),
        axis.title.x=element_text(margin = margin(t = 10))
        #axis.title.x=element_blank()
        )
    g = g + scale_x_time(position="bottom",breaks=c(0,1800,3600,5400,7200))
    if (!guide) {
        g = g + guides(fill=FALSE)
    }
    if (vline > 0) {
        g = g + geom_vline(aes(xintercept= vline))  # vertical line
    }
    return (g)
}

# mpid level data with student views
# results from elastic::fetch_histogram_by_mpid
plot_mpid_by_student_df <- function(viewtable,title="", offset=0) {
    d <- with(viewtable, viewtable[order(huid, interval),])
    g = ggplot(data=d, aes(x=interval+offset, y=huid, fill=factor(huid) )) + scale_x_time(position="bottom")
    g = g + geom_tile(data=d, height=.5)
    g = g + labs(title=title)+ guides(fill=FALSE)
    g = g + ylab("Students)")
    g = g + xlab("Time Interval)")
    g = g + theme(axis.text.x = element_text(size=11), #,angle=90),
            axis.text.y = element_text(size=10),
            legend.text=element_text(size=10),
            legend.key.size = unit(1, "cm"),
        plot.title=element_text(size=10, vjust=3),
        plot.margin = unit(c(1,0.9,1,1), "cm"),
        axis.text=element_text(size=12),
        axis.title = element_text(size=12),
        axis.title.y=element_text(margin=margin(r = 10)),
        axis.title.x=element_text(margin = margin(t = 10)))
    return(g)
}

plot_mpid_by_studentname_df <- function(viewtable,title="",offset=150) {
    d <- with(viewtable, viewtable[order(Student, interval),])
    g = ggplot(data=d, aes(x=interval, y=Student, fill=factor(huid) )) + scale_x_time(position="bottom")
    g = g + geom_tile(data=d,height=.5)
    #g = g + scale_y_discrete(expand=c(.1, 1))
    g = g + labs(title=title)+ guides(fill=FALSE)
    g = g + ylab("Viewers")
    g = g + xlab("Elapsed time into video")
    g = g + theme_minimal()
    g = g + theme(axis.text.x = element_text(size=10), #,angle=90),
            axis.text.y = element_text(size=7),
            legend.text=element_text(size=10),
            legend.key.size = unit(1, "cm"),
        plot.title=element_text(size=10, vjust=3),
        plot.margin = unit(c(1,0.9,1,1), "cm"),
        axis.text=element_text(size=12),
        axis.title = element_text(size=12),
        axis.title.y=element_text(margin=margin(r = 10)),
        axis.title.x=element_text(margin = margin(t = 11)))
    return(g)
}

# using numstudents_by_series_df(series)
plot_student_view_boxplot_by_series <- function(data, title="Overall Student Video Views (ordered by average views of each video)") {
  h$huid <- with(data, reorder(huid, views, mean))
  g = ggplot(h,aes(x=huid,y=log(views),col=status,fill=reglevel))+ geom_boxplot()
  g = g + theme(axis.text.x = element_text(size=8,angle=90),
        axis.text.y = element_text(size=8),
        legend.text=element_text(size=10),
        legend.key.size = unit(1, "cm"),
        plot.title=element_text(size=10, vjust=3)) + coord_flip()
  g = g + ggtitle(title)
  return(g)
}

# using numstudents_by_series_df(series)
plot_mpid_view_boxplot_by_series <- function(data, title="Amount of viewing by video") {
  # h$huid <- with(data, reorder(mpid, views, mean))
  g = ggplot(data,aes(x=mpid,y=log(views)))+ geom_boxplot()
  g = g + theme(axis.text.x = element_text(size=8,angle=90),
        axis.text.y = element_text(size=8),
        legend.text=element_text(size=10),
        legend.key.size = unit(1, "cm"),
        plot.title=element_text(size=10, vjust=3)) + coord_flip()
  g = g + ggtitle(title)
  return(g)
}

# numstudents_by_series_df(series)
plot_mpid_view_student_histo_by_series <- function(data, title="Number of student viewers") {
  h$mpid <- with(data, reorder(mpid, huid, length))
  g = ggplot(h,aes(x=mpid,fill=2))+ geom_histogram(stat="count")+guides(fill=F)
  g = g + theme(axis.text.x = element_text(size=8,angle=90),
        axis.text.y = element_text(size=8),
        legend.text=element_text(size=10),

        legend.key.size = unit(1, "cm"),
        plot.title=element_text(size=10, vjust=3)) + coord_flip()
  g = g + ggtitle(title)
  return(g)
}

# numstudents_by_series_df(series)
plot_mpid_view_student_sessions_histo_by_series <- function(data,title="Number of student view sessions") {
  #h$mpid <- with(data, reorder(mpid, views, length))
  hhh = with(h, table(mpid,huid))
  g = ggplot(as.data.frame(hhh),aes(x=mpid,y=Freq, fill=huid))+ geom_bar(stat="Identity")
  g = g + guides(fill=F)
  g = g + xlab("Number of student viewing sessions")
  g = g + theme(axis.text.x = element_text(size=8,angle=90),
        axis.text.y = element_text(size=8),
        legend.text=element_text(size=10),
        legend.key.size = unit(1, "cm"),
        plot.title=element_text(size=10, vjust=3)) + coord_flip()

  g = g + ggtitle(title)
  return(g)
}

# numstudents_by_series_df(series)
heatmap_mpid_view_student_histo_by_series <- function(data, title="Heatmap of student viewing") {
  h$huid = with(h, reorder(huid, views, length))
  hhh_df = as.data.frame( with(h, table(mpid,huid))) # change to table
  hhh_df$Freq = as.factor(hhh_df$Freq)
  colnames(hhh_df) <- c("mpid","huid","Viewed")
  g = ggplot(hhh_df,aes(x=mpid,y=huid,fill=Viewed))+ geom_tile(stat="Identity")
  g = g + scale_fill_manual(values=c("lightgrey","red"),labels=c("","Viewed"),name="Student views")
  g = g + theme(axis.text.x = element_text(size=8,angle=90),
        axis.text.y = element_text(size=8),
        legend.text=element_text(size=10),
        legend.key.size = unit(1, "cm"),
        plot.title=element_text(size=10, vjust=3)) + coord_flip()

  return(g)
}


# histo = results from elastic::fetch_histogram_by_mpid
plot_histogram_by_mpid <- function(histo) {
    htotal = histo$hits$total
    course = histo$hits$hits[[1]]$`_source`$episode$course
    series = histo$hits$hits[[1]]$`_source`$episode$series
    title = histo$hits$hits[[1]]$`_source`$episode$title
    interv = histo$aggregations$intervals$buckets
    viewtable <- NULL
    for (m in 1:length(interv)) { # per interval
      d = interv[[m]]$users$buckets  # per user
      numstudents = length(d)
      tally = 0
      for (i in 1:numstudents) {
        #user $doc_count = heartbeats - same session can have multiple
        times_total = length(d[[i]]$period$buckets)  # Each session counts as one
        tally = tally + times_total
        viewtable <- rbind(viewtable, data.frame( interval = interv[[m]]$key, huid=as.factor(d[[i]]$key) , viewcount= times_total, tally=tally ))
      }
    }

    d <- with(viewtable, viewtable[order(huid, interval),])
    g = ggplot(data=d, aes(x=interval, y=huid, fill=factor(huid) )) + scale_x_time(position="bottom")
    g = g + geom_tile(data=d,aes(height=viewcount))
    g = g + labs(title=paste0(series," ",course," ",title))+ guides(fill=FALSE)
    g = g + ylab("Students)")
    g = g + xlab("Time Interval)")
    g = g + theme(axis.text.x = element_text(size=11),
            axis.text.y = element_text(size=10),
            legend.text=element_text(size=10),
            legend.key.size = unit(1, "cm"),
        plot.title=element_text(size=10, vjust=3),
        plot.margin = unit(c(1,0.9,1,1), "cm"),
        axis.text=element_text(size=12),
        axis.title = element_text(size=12),
        axis.title.y=element_text(margin=margin(r = 10)),
        axis.title.x=element_text(margin = margin(t = 10)))
    return(g)
}

# copied and modified from https://github.com/mkfs/misc-text-mining/blob/master/R/wordcloud.R
fix.contractions <- function(doc) {
    # "won't" is a special case as it does not expand to "wo not"
    doc <- gsub("won't", "will not", doc)
    doc <- gsub("wouldn't", "will not", doc)
    doc <- gsub("couldn't", "will not", doc)
    doc <- gsub("don't", "do not", doc)
    doc <- gsub("n't", " not", doc)
    doc <- gsub("'ll", " will", doc)
    doc <- gsub("'re", " are", doc)
    doc <- gsub("'ve", " have", doc)
    doc <- gsub("'m", " am", doc)
    # 's could be 'is' or could be possessive: it has no expansion
    doc <- gsub("'s", "", doc)
    return(doc)
}

# function to combine single and plural variants in a term-frequency vector
aggregate.plurals <- function (v) {
    aggr_fn <- function(v, singular, plural) {
        if (! is.na(v[plural])) {
            v[singular] <- v[singular] + v[plural]
            v <- v[-which(names(v) == plural)]
        }
        return(v)
    }
    for (n in names(v)) {
        n_pl <- paste(n, 's', sep='')
        v <- aggr_fn(v, n, n_pl)
        n_pl <- paste(n, 'es', sep='')
        v <- aggr_fn(v, n, n_pl)
    }
    return(v)
}


gen_wordcloud <- function(textsrc,minwordlen=3, minfreq=3) {
captions = Corpus(VectorSource(textsrc))
captions <- tm_map(captions, stripWhitespace)
captions <- tm_map(captions, removeWords, c("HESITATION"))
captions <- tm_map(captions, tolower)
captions <- tm_map(captions, removeNumbers)
captions <-
  tm_map(
    captions,
    removeWords,
    c(
      stopwords("english"),
      "one","two","three","four","five","six","seven","eight","nine","ten",
      "me","you","they","your","mine", "our","gonna","going","can","say",
      "look","see","now","get","got","take","going","also","say","said",
      "need","make","want","like","know","well","just"
    )
  )
captions <- tm_map(captions, removePunctuation)
#corpus.copy <- captions
#captions <- tm_map(captions, stemDocument, language = "english")
#captions <- tm_map(captions, stemCompletion, dictionary = corpus.copy)
tdm <-
  TermDocumentMatrix(captions, control = list(removePunctuation = TRUE, stopwords = TRUE,minWordLength = minwordlen))
#dtm = DocumentTermMatrix(captions) #turns the corpus into a document term matrix
#notSparse = removeSparseTerms(tdm, 0.99) # extracts most frequently occuring words
notSparse = tdm

matrix = as.matrix(notSparse)
freq <- sort (rowSums (matrix), decreasing=TRUE)  # word + frequency data
freq <- aggregate.plurals(freq)

tmdata <- data.frame (words = names(freq), freq)
wordcloud (
  tmdata$words,
  tmdata$freq,
  max.words = 300,
  random.order = FALSE,
  scale = c(4, 0.3),
  colors = brewer.pal(8, "Dark2"),
  min.freq = minfreq
)
}



plot_mpid_totalviews <- function(data, title, guide=FALSE, vline=-1) {
    g = ggplot(data=data, aes(x=interval, y=viewcount, fill=factor(huid) ))
    g = g + geom_bar(stat = "identity")
    #g = g + scale_x_time(position="bottom")
    g = g + labs(title= title)
    g = g + ylab("Number of views (each color represents 1 student)")
    g = g + theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=10),
        legend.text=element_text(size=7),
        legend.key.size = unit(.4, "cm"),
        plot.title=element_text(size=10, vjust=3),
        plot.margin = unit(c(1,0.9,1,1), "cm"),
        axis.text=element_text(size=12),
        axis.title = element_text(size=12),
        axis.title.y=element_text(margin=margin(r = 10)),
        axis.title.x=element_text(margin = margin(t = 10)))
    if (!guide) {
        g = g + guides(fill=FALSE)
    }
    if (vline > 0) {
        g = g + geom_vline(aes(xintercept= vline))  # vertical line
    }
    return (g)
}

