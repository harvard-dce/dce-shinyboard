
es <- elastic::connect(
  es_host = es.host,
  es_port = es.port,
  es_transport_schema = es.transport,
  es_path = es.path,
  es_user = es.user,
  es_pwd = es.pwd,
  force = FALSE,
  errors = "complete",
  headers = NULL
)

tryCatch({
  es.info <- elastic::info()
  print("Connected to elasticsearch")
}, error = function(ex) {
  print("Unable to communicate with Elasticsearch");
  stop(ex)
})

# Usage
# mpid="d24e43dd-9662-49e6-921f-28dc629063c0"
# s <- Search(index = "useractions-*", body=fetchheartbeats_by_mpid(mpid), size = 10)
fetch_heartbeats_by_mpid<- function(mpid) {
return( paste0('{
 "query": {
    "constant_score": {
      "filter": {
        "and": [
          {"match": {"mpid": "', mpid, '" } },
          {
            "match": {
              "action.type": "HEARTBEAT"
            }
          },
          {
            "match": {
              "action.is_playing": true
            }
          },
          {
            "range": {
              "action.inpoint": {
                "from": 1,
                "to": 12000
              }
            }
          }
        ]
      }
    }
  },
  "aggs": {
    "huid": {
      "terms": {
        "field": "huid",
        "size": 0
      },
      "aggs": {
        "viewing": {
          "terms": {
            "field": "action.inpoint",
            "size": 0
          }
        }}}}
}'))
}


fetch_numstudents_by_mpid<- function(mpid) {
return( paste0('{
  "query": {
    "constant_score": {
      "filter": {
        "and": [
          {
            "match": {
              "mpid": "', mpid, '"
            }
          },
          {
            "match": {
              "action.type": "HEARTBEAT"
            }
          },
          {
            "match": {
              "action.is_playing": true
            }
          }
        ]
      }
    }
  },
  "size": 0,
  "aggs": {
    "student_count": {
      "cardinality": {
        "field": "huid"
      }
    }
  }
}'))
}


# For Debug only
fetch_histogram_by_mpid2<- function(mpid, interval_inpoint='300', interval_timestamp='5m', live=NA) {
if (is.na(live)) {
    ll = ""
} else {
    ll = paste0('"{ "match": {
              "is_live": ', live ,'
            }
          },"')
}

return( paste0('{
"query": {
    "constant_score": {
      "filter": {
        "and": [
          {
            "match": {
              "mpid": "', mpid ,'"
            }
          },
          {
            "match": {
              "action.type": "HEARTBEAT"
            }
          },
          {
            "match": {
              "action.is_playing": true
            }
          },', ll,
          '{
            "range": {
              "action.inpoint": {
                "from": 1,
                "to": 10800
              }
            }
          }
        ]
      }
    }
  },
  "size": 0,
  "aggs": {
        "users": {
          "terms": {
            "field": "huid"
          }
        },
      "aggs": {
        "intervals": {
          "histogram": {
            "field": "action.inpoint",
            "interval": ', interval_inpoint,
          '}
      }
    }
  }
}'))
}

# live=1 or 0 (vod only) or NA (both)
fetch_histogram_by_mpid<- function(mpid, interval_inpoint='300', interval_timestamp='5m', duration=10800, live=NA) {
if (is.na(live)) {
    ll = ""
} else {
    ll = paste0('"{ "match": {
              "is_live": ', live ,'
            }
          },"')
}

return( paste0('{
"query": {
    "constant_score": {
      "filter": {
        "and": [
          {
            "match": {
              "mpid": "', mpid ,'"
            }
          },
          {
            "match": {
              "action.type": "HEARTBEAT"
            }
          },
          {
            "match": {
              "action.is_playing": true
            }
          },', ll,
          '{
            "range": {
              "action.inpoint": {
                "from": 1,
                "to": ', duration ,'
              }
            }
          }
        ]
      }
    }
  },
  "size": 0,
  "aggs": {
    "intervals": {
      "histogram": {
        "field": "action.inpoint",
        "interval": ', interval_inpoint,
      '},
      "aggs": {
        "users": {
          "terms": {
            "field": "huid"
          },
          "aggs": {
            "period": {
              "date_histogram": {
                "field": "@timestamp",
                "format": "yyyy-MM-dd HH:mm:ss",
                "interval": "', interval_timestamp,'",
                "min_doc_count": 1
              }
            }
          }
        }
      }
    }
  }
}'))
}


histogram_by_mpid_df <- function( mpid, interval='60', ts='1m', live=NA, duration=10800, longtitle=T) {
    p = floor(as.numeric(interval)/60)
    ts = paste0(p,"m")
    interval = p * 60
    rm(p)

    histo = Search(index = "useractions-*", body=fetch_histogram_by_mpid(mpid, interval_inpoint=interval, interval_timestamp=ts, duration=duration, live=live),size=1)
    if (histo$hits$total == 0)
        return( list( series=NA, title="No Data", duration="", data=NA ))
    course = histo$hits$hits[[1]]$`_source`$episode$course
    series = histo$hits$hits[[1]]$`_source`$episode$series
    title  = histo$hits$hits[[1]]$`_source`$episode$title
    duration = histo$hits$hits[[1]]$`_source`$episode$duration
    interv = histo$aggregations$intervals$buckets
    viewtable = NULL
    int_duration = as.numeric(duration)/1000
    if (length(interv) > 0) {
     for (m in 1:length(interv)) { # per interval
        # Filter out junk using episode duration
      if (interv[[m]]$key < int_duration) {
          d = interv[[m]]$users$buckets  # per user
          numstudents = length(d)
          tally = 0
          if (numstudents > 0) {
              for (i in 1:numstudents) {
                    #user $doc_count = heartbeats - same session can have multiple
                times_total = length(d[[i]]$period$buckets)  # Each session counts as one
                tally = tally + times_total
                viewtable= rbind(viewtable, data.frame(interval =interv[[m]]$key, huid=as.factor(d[[i]]$key), viewcount=times_total, tally=tally ))
              }}
    }}}
    #viewtable= viewtable[viewtable$interval <= as.numeric(duration)/1000,] # + as.numeric(interval),]
    if (longtitle) title=paste0(course," ",title)
    return( list( series=series, title=title, duration=duration, data=viewtable ))
}

find_series <- function(series_id) {
return( paste0(
'{
"_source":["title","mpid","type","start","duration"],
"query": {
    "constant_score": {
      "filter": {
            "match": {
              "series": "', series_id ,'"
            }
      }
    }
}
,"size": 100
}'))
}

series_df <- function(series_id) {
    histo = Search(index = "episodes", body=find_series(series_id))
    l=NULL
    if ( length(histo$hits$hits) > 0) {
        for (i in 1:length(histo$hits$hits)) {
          l = rbind( l, as.data.frame(histo$hits$hits[[i]]$`_source`))
        }
    }
    #s = sapply(histo$hits$hits, as.data.frame( function(x) { as.data.frame( x$`_source`) }) )
    return(l)
}


find_transcript<- function(mpid) {
interval="300"
return( paste0(
'{
"size": 10000,
"_source":[ "confidence","inpoint","outpoint","length","word_count","text"],
"query": {
    "constant_score": {
      "filter": {
            "match": {
              "mpid": "', mpid ,'"
            } } } }
}'))
}
#, "text", "hesitations","hesitation_length" ],
#,"aggs": { "intervals": { "histogram": { "field": "inpoint", "interval": "', interval,'" }}}

transcript_df <- function(mpid) {
    histo = Search(index = "transcripts", body=find_transcript(mpid))
    l <- NULL
    if ( length(histo$hits$hits) > 0) {
        for (i in 1:length(histo$hits$hits)) {
          l = rbind( l, as.data.frame(histo$hits$hits[[i]]$`_source`))
        }
    }
    #s = sapply(histo$hits$hits, as.data.frame( function(x) { as.data.frame( x$`source`) }) )
    return(l)
}



  #"_source":[ "mpid","huid"],
fetch_numstudents_by_series<- function(series) {
return( paste0('{
  "query": {
    "constant_score": {
      "filter": {
        "and": [
          {
            "match": {
              "episode.series": "', series, '"
            }
          },
          {
            "match": {
              "action.type": "HEARTBEAT"
            }
          },
          {
            "match": {
              "action.is_playing": true
            }
          },
          {
            "range": {
              "action.inpoint": {
                "from": 1,
                "to": 10800
              }
            }
          }
        ]
      }
    }
  },
  "size": 0,
  "aggs": {
    "mpid": {
      "terms": {
        "field": "mpid",
         "size":0
       },
      "aggs": {
          "student": {
              "terms": {
                "field": "huid",
                "size":0
               }
          }
       }
     }
  }
}'))
}


# Total number of students viewing a series
numstudents_by_series_df <- function(series) {
    histo = Search(index = "useractions-*", body=fetch_numstudents_by_series(series), size = 1)
    if (histo$hits$total < 1)  return(NA)
    ll <- NULL
    l = length(histo$aggregations$mpid$buckets)
    if ( l > 0) {
        for (i in 1:l) {  # R will enter the loop with 1:0
          k = histo$aggregations$mpid$buckets[[i]]$key
          if (length(histo$aggregations$mpid$buckets[[i]]$student$buckets) > 1) {
            views = sapply(histo$aggregations$mpid$buckets[[i]]$student$buckets,
                function(x) {cbind(x$key,x$doc_count)} )
            ll = rbind(ll, as.data.frame( cbind(k,t(views)) )) # bind to transpose
          }
        }
    }
    colnames(ll)<- c("mpid","huid","views")
    ll$views <- as.numeric(ll$views)
    return(ll)
}


  #"_source":[ "mpid","huid"],
fetch_numstudents_by_allseries<- function() {
return( paste0('{
  "query": {
    "constant_score": {
      "filter": {
        "and": [
          {
            "match": {
              "is_live": 0
            }
          },
          {
            "match": {
              "action.type": "HEARTBEAT"
            }
          },
          {
            "match": {
              "action.is_playing": true
            }
          }
        ]
      }
    }
  },
  "size": 0,
  "aggs": {
    "series":{
      "terms": {
            "field":"episode.series",
             "size":0
       }
       ,"aggs": {
          "student": {
              "terms": {
                "field": "huid",
                "size":0
               }
          }
       }
    }
   }
}'))
}


# Total number of students viewing a series
numstudents_by_allseries_df <- function(series) {
    histo = Search(index = "useractions-*", body=fetch_numstudents_by_allseries(), size = 1)
    if (histo$hits$total < 1)  return(NA)
    ll <- NULL
    l = length(histo$aggregations$series$buckets)
    if ( l > 0) {
        for (i in 1:l) {  # R will enter the loop with 1:0
          k = histo$aggregations$series$buckets[[i]]$key

          #if (length(histo$aggregations$series$buckets[[i]]$student$buckets) > 1) {
            #views = sapply(histo$aggregations$series$buckets[[i]]$student$buckets,
                #function(x) {cbind(x$key,x$doc_count)} )
            #ll = rbind(ll, as.data.frame( cbind(k,t(views)) )) # bind to transpose
          #}
          ll = rbind(ll, as.data.frame( cbind(k, length(histo$aggregations$series$buckets[[i]]$student$buckets)) )) # bind to transpose
        }
    }
    colnames(ll)<- c("series","views")
    ll$views <- as.numeric(ll$views)
    return(ll)
}


  #"_source":[ "mpid","huid"],
fetch_numstudents_group_by_series <- function(
     from="2017-01-10",to="2017-05-25") {
return( paste0('{
  "query": {
    "bool": {
      "filter": [
          { "range": {
              "@timestamp": {
                  "gt": "', from, 'T00:00:00.000Z",
                  "lt": "', to ,'T00:00:00.000Z"
                }
            }
          },
          {
            "term": {
              "is_live": 1
            }
          },
          {
            "term": {
              "action.type": "HEARTBEAT"
            }
          },
          {
            "term": {
              "action.is_playing": true
            }
          }
        ]
      }
  },
  "size": 1,
  "aggs": {
    "series":{
      "terms": {
            "field":"episode.series",
            "size": 0
       }
    }
   }
}'))
}

# Total number of students viewing live by series
numstudents_group_by_series_df <- function(series) {
    histo = Search(index = "useractions-*", body=fetch_numstudents_group_by_series(), size = 1)
    if (histo$hits$total < 1)  return(NA)
    ll <- NULL
    l = length(histo$aggregations$series$buckets)
    if ( l > 0) {
        for (i in 1:l) {  # R will enter the loop with 1:0
          k = histo$aggregations$series$buckets[[i]]$key
          ll = rbind(ll, as.data.frame( cbind(k, histo$aggregations$series$buckets[[i]]$doc_count))) # bind to transpose
        }
    }
    colnames(ll)<- c("series","views")
    ll$views <- as.numeric(as.character(ll$views))
    return(ll)
}

# OLD: just like histogram_by_mpid_df
mpid_totalviews <- function(mpid) {
  histo = Search(index = "useractions-*", body=fetch_histogram_by_mpid(mpid),size=1)
  if (histo$hits$total==0) return (None)

  course = histo$hits$hits[[1]]$`_source`$episode$course
  series = histo$hits$hits[[1]]$`_source`$episode$series
  title = histo$hits$hits[[1]]$`_source`$episode$title
  duration = histo$hits$hits[[1]]$`_source`$episode$duration

  interv = histo$aggregations$intervals$buckets
  viewtable <- NULL
  for (m in 1:length(interv)) { # per interval
    d = interv[[m]]$users$buckets  # per user
    numstudents = length(d)
    tally = 0
    if (numstudents > 0) {
    for (i in 1:numstudents) {
      #user $doc_count = heartbeats - same session can have multiple
      times_total = length(d[[i]]$period$buckets)  # Each session counts as one
      tally = tally + times_total
      viewtable <- rbind(viewtable, data.frame( interval = interv[[m]]$key, huid=as.factor(d[[i]]$key) , viewcount= times_total, tally=tally ))
    }
  }}
  d <- with(viewtable, viewtable[order(interval, huid),])
  # list will keep data as data.frame, c() will make them into vectors
  return( list( series=series, title=paste0(course," ",title), duration=duration, data=d ))
}

