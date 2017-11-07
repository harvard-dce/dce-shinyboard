
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
