prod_idb_config <- list(
    "url" =  "mongodb-a.test.internal.kickdex.com",
    "replicaset" = NA_character_,
    "port" =  27017,
    "db_name" = "InternalDataBookie",
    "user" = "ModellingBookieUser",
    "password" = "d344987jhkdfFDGD4322dh"
)

nc <- test_mongo_con("skellamSI", prod_idb_config)
print(nc)
