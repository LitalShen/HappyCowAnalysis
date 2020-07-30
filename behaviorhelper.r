
# Tag numbers from the RTLS system and last position (x, y)
TAGS <- list(
    13 = c(0,0),
    14 = c(0,0),
    15 = c(0,0),
    16 = c(0,0)
)

# Arena size (meters)
ARENA_WIDTH <- 14.4
ARENA_LENGTH <- 7.4

# Threshold for subject movment
MOVE_THRESH <- 0.2

# Arena objects designated area (name: (x1, x2, y1, y2))
ARENA_OBJECTS <- list(
    'daily_meal' = c(0, 7.7, 6.88, 7.38),
    'daily_hay' = c(7.7, 14.4, 6.88, 7.380),
    'water' = c(11.0, 13.0, 0.0, 0.4),
    'play_ball' = c(6.9, 7.4, 0.945, 1.445),
    'grooming_brush' = c(3.5, 4.5, 0.5, 1.5),
    'exit' = c(4.572, 6.225, 0.0, 0.5),
    'liking_stone' = c(0, 0, 0, 0)
)

# Arena zone devision (zone: (x1, x2, y1, y2))
ARENA_ZONES <- list(
    'zone1' = c(0, 4.8, 0.0, 3.69),
    'zone2' = c(4.8, 9.6, 0.0, 3.69),
    'zone3' = c(9.6, 14.4, 0.0, 3.69),
    'zone4' = c(0, 4.8, 3.69, 7.38),
    'zone5' = c(4.8, 9.6, 3.69, 7.38),
    'zone6' = c(9.6, 14.4, 3.69,  7.38),
    'out_borders' = c(14.4, 20, 7.38,  10)
)


# Social interactions parameters - nested vector with functions 
# If condition is met return the interaction
INTERACTIONS <- c(
    function(dist) if (0 <= dist & dist <= 1) return('close'),
    function(dist) if (1 < dist & dist <= 4) return('intermediate'),
    function(dist) if (4 < dist) return('far')
)


which_zones <- function(x, y) {
    #' Check which zone
    #'
    #' @description Provides the zone from the input position.
    #' @param x numeric. x coordinate.
    #' @param y numeric. y coordinate.
    #' @return The specific zone as a character string.
    for (zone in names(ARENA_ZONES)) {
        if (
            ARENA_ZONES[[zone]][1] <= x &
            x <= ARENA_ZONES[[zone]][2] &
            ARENA_ZONES[[zone]][3] <= y &
            y <= ARENA_ZONES[[zone]][4]
        ) {
            return(zone)
        }
    }
}

which_objects <- function(x, y) {
    #' Check which object
    #'
    #' @description Provides the object from the input position.
    #' @param x numeric. x coordinate.
    #' @param y numeric. y coordinate.
    #' @return The specific object as a character string.
    for (obj in names(ARENA_OBJECTS)) {
        if (
            ARENA_OBJECTS[[obj]][1] <= x &
            x <= ARENA_OBJECTS[[obj]][2] &
            ARENA_OBJECTS[[obj]][3] <= y &
            y <= ARENA_OBJECTS[[obj]][4]
        ) {
            return(zone)
        }
    }
    return(NULL)
}

social_dist <- function(dist) {
    #' Gives the interaction type
    #'
    #' @description Provides the interaction range from the input distance.
    #' @param dist numeric. Distance between subjects.
    #' @return Interaction distance as a character string.
    for (sol_check in INTERACTIONS) {
        if (!is.null(sol_check(dist))) {
            return(sol_check(dist))
        }
    }   
}

dist_movement <- function(x1, y1, tag) {
    #' Gives the distance
    #'
    #' @description Provides the distance between two points.
    #' @param x1 numeric. x coordinate.
    #' @param y1 numeric. y coordinate.
    #' @param tag character string. Tag name of subject.
    #' @return Interaction distance as a character string.
    x2 <- TAGS[[tag]][1]; y2 <- TAGS[[tag]][2]
    dist <- sqrt(sum((x1 - x2) ^ 2 ,(y1 - y2) ^ 2))
    TAGS[[tag]] <- c(x1, y1)
    # Check if the distance is above theshold
    if (dist >= MOVE_THRESH) {
        return(dist)
    } else {
        return(0)
    }
}
















