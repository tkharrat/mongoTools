linMod <- lm(dist ~ speed, data = cars)

## save the object in tmp location
tmp_file <- file.path("~/Downloads", "test_model.RDS")
saveRDS(linMod, tmp_file)

## insert the model in the database
remoteName <- "linModel_test"
status <- insert_object_inDB(tmp_file, remoteName, '{"This is" : "just a test"}')

## read the model from db
linMod2 <- read_object_fromDB(remoteName)

## you can compare linMod and linMod2 to make sure they are the same

## when done, we trash the GridFS
attr(status, "att")$fs[[1]]$drop()
