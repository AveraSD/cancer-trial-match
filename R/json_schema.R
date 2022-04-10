library(jsonvalidate)


# note: tidyjson::json_schema cannot produce a usable schema; designing schema manually
# schema <- read_json("data/trials/01.auto.ndjson", format = "json") %>% json_schema()

schema <- "template/schema.json"
validator <- json_validator(schema)
json_validate("data/trials/01.json", schema, verbose = TRUE)


json_validate("data/trials/test.ndjson", "data/trials/test.schema.json", verbose = T)



trialspath <- here("data", "trials")
fullfiles <- dir(path = trialspath, pattern = "[0-9].full.json", full.names = T)
writeLines(fullfiles[1] %>% fromJSON() %>% toJSON(pretty = F), "data/trials/test.ndjson")
