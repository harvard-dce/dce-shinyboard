
makeQuery <- function(q) {
  elasticsearchr::query(q)
}

for.everything <- makeQuery('{ "match_all": {} }')

episodesByTerm <- function(year.term) {
  year.term <- str_split(year.term, "-")
  year <- year.term[[1]][1]
  term <- year.term[[1]][2]
  by.term <- makeQuery(
    str_interp('{
      "bool": {
        "must": [
             { "term": { "year": "${year}" }},
             { "term": { "term": "${term}" }}
        ]
      }
    }')
  )
  elastic(es.host, es.episode.index) %search% by.term
}

