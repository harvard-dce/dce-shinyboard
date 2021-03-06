
dotenv::load_dot_env(file = ".env")

env <- function(key, default=NA) {
  val <- Sys.getenv(key)
  if (val == "" && !is.na(default)) {
    val <- default
  }
  val
}

es.host <- env("ES_HOST", 'localhost')
es.port <- env("ES_PORT", '9200')
es.transport <- env("ES_TRANSPORT", 'http')
es.path <- env("ES_PATH", "")
es.user <- env("ES_USER", NA)
es.pwd <- env("ES_PWD", NA)
es.episode.index <- env("ES_EPISODE_INDEX", 'episodes')
default.term <- env("DEFAULT_TERM", "2017-03")
default.tz <- "America/New_York"
data.dir <- env("DATA_DIR", "./")

term.options <- c(
  "Fall 2017" = "2017-03",
  "Spring 2017" = "2017-02",
  "Fall 2016" = "2017-01"
)

lecture.fields <- c("title", "available", "duration", "attendance")
student.fields <- c("name", "huid", "status", "reg_level", "attendance")
