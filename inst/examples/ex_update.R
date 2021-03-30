ex <- list(gameId = 987796L,
           gameDate = as.POSIXct(strptime("2019-01-01T12:30:00Z", "%Y-%m-%dT%H:%M:%SZ", 'UTC')),
           Date = as.Date("2019-01-01"),
           recordedDate = Sys.time(),
           AT_pred_xG_attempts = 0.538225581177464,
           HT_pred_xG_attempts = 1.18599238298089,
           model_name = "xG_attempts_gaussian_8")

up_query <- buildUpdateQuery(names(ex), ex)

keys <- list(gameId = 987796L)
qr <- buildQuery(names(keys), keys)

m <- mongo("HH_values", "InternalData", url="mongodb://192.168.0.101")
m$update(qr, up_query, upsert = TRUE)
