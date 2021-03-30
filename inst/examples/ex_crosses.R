## In this document we show how we extra information about crosses
## using the Events and GameEvents collection

library(mongoTools) ## make sure you have version >= 1.3.5
library(dplyr)
library(pbapply)

## mongo connections
mEvents <- mongo("Events", "opta")
mGameEvents <- mongo("GameEvents", "opta")
PASS_TYPE_ID <- 1
SHOTS_TYPE_ID <- c("Miss" = 13, "Post" = 14, "Attempt Saved" = 15, "Goal" = 16)
WINDOW_LEN_SEC <- 5 ## length of the window to select following events

## first filter limit to a set of season and competitionIds
## You can find all the competition names in Fixture collection
keys_base <- list(competitionId = 8, ## EPL, Championship: 10
                  seasonId = 2017)

## count all crosses
keys_cross <- keys_base
keys_cross$typeId <- PASS_TYPE_ID
keys_cross[["qualifiers.2"]] <- TRUE
qr_cross <- buildQuery(names(keys_cross),
                       keys_cross,
                       c(rep("equality", 3),
                         "exists"))

message(paste("total number of crosses:", mEvents$count(qr_cross)))

## find all successful crosses in a given box
X_BOX <- c("X_MIN" = 60, "X_MAX" = 80)
Y_BOX <- c("Y_MIN" = 0, "Y_MAX" = 40) 
keys_cross_box_succ <- keys_cross
keys_cross_box_succ$outcome <- 1
keys_cross_box_succ$x <- X_BOX
keys_cross_box_succ$y <- Y_BOX
qr_cross_box_succ <- buildQuery(names(keys_cross_box_succ),
                                keys_cross_box_succ,
                                c(rep("equality", 3), ## competitionId, seasonId, typeId
                                  "exists", ## qualifiers.2 for crosses
                                  "equality", ## outcome
                                  rep("intervals", 2)) ## x,y box
                                )

## here we will extract the gameId and eventId
out_cross_box_succ <- list(eventId = 1, gameId = 1, "_id" = 0)
qo_cross_box_succ <- buildQuery(names(out_cross_box_succ), out_cross_box_succ)
cross_box_succ_info <- mEvents$find(qr_cross_box_succ, qo_cross_box_succ)
message(paste("total number of successful crosses in",
              "(", X_BOX["X_MIN"], "<= x <= ", X_BOX["X_MAX"], ")", "x",
              "(", Y_BOX["Y_MIN"], "<= y <= ", Y_BOX["Y_MAX"], "):",
              nrow(cross_box_succ_info))
        )

.check_next_event <- function(eventId_, eve) {
    ## extract event for the desired window
    ind_start <- which(eve$eventId == eventId_)
    ind_end <- which(eve$time <= eve$time[ind_start] + WINDOW_LEN_SEC)
    ind_end <- ind_end[ind_end >= ind_start]
    eve_win <- eve[ind_end, ]

    data.frame(eventId = eventId_,
               n_shots = sum(eve_win$typeId %in% SHOTS_TYPE_ID),
               n_goals = sum(eve_win$typeId %in% SHOTS_TYPE_ID["Goal"])
               )
}

.compute_next_event_by_game <- function(gameId_) {
    ## extract events from database
    k <- list(gameId = gameId_)
    qr <- buildQuery(names(k), k)
    out <- list(events = 1, "_id" = 0)
    qo <- buildQuery(names(out), out)
    events <- mGameEvents$find(qr, qo)$events[[1]]

    ## compute time in sec
    events$time <- events$min * 60 + events$sec

    ## extract info for the desired gameId
    info <- filter(cross_box_succ_info, gameId == gameId_)

    ## add extra info
    bind_rows(lapply(info$eventId, .check_next_event, eve = events))
}

## find the list of gameIds
gameIds <- unique(cross_box_succ_info$gameId)
next_event_info <- bind_rows(pblapply(gameIds, .compute_next_event_by_game))
