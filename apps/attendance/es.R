
es <- connect(
  es_host = es.host,
  es_port = es.port,
  es_transport_schema = es.transport,
  es_path = es.path,
  es_user = es.user,
  es_pwd = es.pwd
  )

tryCatch({
  es.info <- elastic::info()
  print("Connected to elasticsearch")
}, error = function(ex) {
  print("Unable to communicate with Elasticsearch");
  stop(ex)
})


makeQuery <- function(q) {
  elasticsearchr::query(q)
}

for.everything <- makeQuery('{ "match_all": {} }')

episodesByTerm <- function(year.term) {
  year.term <- str_split(year.term, "-")
  year <- year.term[[1]][1]
  term <- year.term[[1]][2]

  qbody <- str_interp('{
    "query": {
      "bool": {
        "must": [
             { "term": { "year": "${year}" }},
             { "term": { "term": "${term}" }}
        ]
      }
    }
  }')
  res <- Search(es.episode.index, body = qbody, asdf = TRUE, size = 9999)
  res <- res$hits$hits
  # rewrite the source doc field names to make the df easier to work with
  res %>% setNames(gsub("_source\\.", "", names(.)))
}

