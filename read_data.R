library("reshape2")
library("plyr")
library("ggplot2")

files <- list.files("data", pattern = "-final.csv")

read_esc <- function(x) {
  xp <- paste0("data/", x)
  d_wide <- read.csv(xp, sep = "\t", check.names = FALSE, stringsAsFactors = FALSE)
  d_wide[is.na(d_wide)] <- 0
  names(d_wide) <- gsub("&| |\\.", "", names(d_wide))
  d_wide$Participant <- gsub("&| |\\.", "", d_wide$Participant)
  # ranking <- d_wide[, c("Participant", "Points", "Place")]
  
  d_wide$Points <- NULL
  d_wide$Place <- NULL
  
  d <- melt(d_wide, id.vars = "Participant")
  names(d) <- c("to", "from", "points")
  # attr(d, "ranking") <- ranking
  
  d$year <- as.numeric(gsub("-final.csv", "", x))
  d
}


esc <- ldply(files, read_esc)  
points_alltime <- ddply(esc, .(to), function(x) sum(x$points))
points_alltime <- rbind(points_alltime,
                        data.frame(to = levels(esc$from)[!(levels(esc$from) %in% 
                                                             points_alltime$to)],
                                   V1 = 0))
esc$to <- factor(esc$to, levels = points_alltime$to[order(points_alltime$V1, decreasing = TRUE)])
esc$from <- factor(esc$from, levels = points_alltime$to[order(points_alltime$V1, decreasing = TRUE)])

ggplot(esc[esc$year == 2003, ], aes(x = from, y = to)) +
  geom_text(aes(label = points))


ggplot(esc[esc$year == 2003, ], aes(x = to, y = from)) + geom_tile(aes(fill = points)) +
  theme(axis.text.x = element_text(angle = 330, hjust = 0)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  facet_grid(year ~ .) +
  geom_text(aes(label = points))

ggplot(esc, aes(x = to, y = from, fill = points)) + geom_tile() +
  theme(axis.text.x = element_text(angle = 330, hjust = 0)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  facet_grid(year ~ .)


ggplot(esc, aes(x = from, y = to, fill = points)) + geom_tile() +
  theme(axis.text.x = element_text(angle = 330, hjust = 0)) +
  scale_fill_gradient(low = "yellow", high = "steelblue", na.value = "black") +
  facet_grid(year ~ .) 



library("ggmap")
# https://opendata.socrata.com/dataset/Country-List-ISO-3166-Codes-Latitude-Longitude/mnkm-8ram
lonlat <- read.csv("data/Country_List_ISO_3166_Codes_Latitude_Longitude.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)
names(lonlat) <- gsub(" |\\(average\\)|-", "", names(lonlat))
lonlat <- lonlat[ ,c( "Country", "Alpha3code", "Latitude", "Longitude")]


get_lonlat <- function(x, from = TRUE, to = TRUE) {
  if(from) {
    w_from <- lonlat$Country == x$from
    if(any(w_from)) {
      from <- lonlat[w_from, ]
    } else {
      # from <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(lonlat)))
      cdist <- adist(lonlat$Country, x$from, costs = list(deletions = 0))
      from <- lonlat[which.min(cdist), ]
    }
    names(from) <- paste0(names(lonlat), "_from")
  }
  
  if(to) {
    w_to <- lonlat$Country == x$to
    if(any(w_to)) {
      to <- lonlat[w_to, ]
    } else {
      # to <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(lonlat)))
      cdist <- adist(lonlat$Country, x$to, costs = list(deletions = 0))
      to <- lonlat[which.min(cdist), ]
    }
    names(to) <- paste0(names(lonlat), "_to")
  }
  
  cbind(to, from)
}

escd <- adply(esc, 1, get_lonlat)

# travel_data <- data.frame(lon = c(102, 103, 103.1), lat = c(18, 15, 16), person = c("A", "B", "B"))
# europe <- get_map(location = "Europe", zoom = 4, source="stamen", maptype="toner")
# ggmap(europe) +
#   geom_point(data = travel_data, aes(x = lon, y = lat, color = person)) +
#   geom_line(data = travel_data, aes(x = lon, y = lat, color = person))


esc_mean_ft <- ddply(escd, .(to, from), function(x) data.frame(mean_points = mean(x$points),
                                                               n = nrow(x)))
esc_mean_ft <- adply(esc_mean_ft, 1, get_lonlat)
esc_mean_t <- ddply(escd, .(to), function(x) data.frame(mean_points = mean(x$points),
                                                        n = nrow(x)))
esc_mean_t <- adply(esc_mean_t, 1, get_lonlat, from = FALSE)

ggplot() +
  geom_segment(data = esc_mean_ft, aes(x = Longitude_from, xend = Longitude_to,
                                       y = Latitude_from, yend = Latitude_to,
                                       #alpha = mean_points, 
                                       color = mean_points), 
               alpha = 0.2) + 
  # geom_point(data = esc_mean_t, aes(x = Longitude_to, y = Latitude_to, size = mean_points, alpha = n)) +
  geom_text(data = esc_mean_t, aes(label = Alpha3code_to, x = Longitude_to, y = Latitude_to,
                                   size = mean_points, alpha = n)) +
  scale_color_gradient(low = "yellow", high = "navyblue") +
  scale_size_area() +
  theme_bw()
