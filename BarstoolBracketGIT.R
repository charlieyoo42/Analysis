##This program determines in real time who the winner of the "Best College Bar" Challenge is, 
#Miami University's Brick Street vs California State University's Riley's Bar

pacman:: p_load(magrittr, lubridate, tidyverse, forecast, zoo, tm, rtweet, mongolite, sqldf)
appname = "charlieyoo4"
api_key = "###"
api_secret = "###"
access_token = "######"
access_token_secret = "###"
token = create_token(
  app = appname,
  consumer_key = api_key,
  consumer_secret = api_secret,
  access_token = access_token,
  access_secret = access_token_secret)
x = search_tweets(q = '#BestBarBrickSt',
                     n = 3000,
                     token = token)

xx = x ## in order to reset tweet df back in case needed

y = subset(xx, and(xx$created_at>= "2021-3-27 15:00:00", xx$created_at<="2021-3-29 15:00:00")) ##time the competition started
z = levels(as.factor(y$screen_name)) %>% data.frame()
brickvotes = nrow(z)

a = search_tweets(q = '#BestBarRileys',
                  n = 3000,
                  token = token)

aa = a
b = subset(aa, and(aa$created_at>= "2021-3-27 15:00:00", aa$created_at<="2021-3-29 15:00:00")) ##time competition started
c = levels(as.factor(b$screen_name)) %>% data.frame()
wheelvotes = nrow(c)

d = c("#BestBarBrickSt", "#BestBarRileys")
e = c(brickvotes, wheelvotes)
Bracket = data.frame("School" = d, "Votes" = e)
view(Bracket)
write.csv(Bracket, file = paste("Bracket", today()), row.names = F)
#######











# coordinates = unlist(aa$coords_coords)
# xandy = rep(c("x", "y"),nrow(aa))
# 
# coordsxy = data.frame("XorY" = xandy, "Coordinates" = coordinates)
# xcoords = subset(coordsxy, coordsxy$XorY == "x")
# ycoords = subset(coordsxy, coordsxy$XorY == "y")
# aa$Xcords = xcoords$Coordinates
# aa$Ycords = ycoords$Coordinates
# aa = select(aa, -"coords_coords")
# aacords = select(aa, c("created_at", "screen_name", "Xcords", "Ycords"))
# aacords %<>% na.omit()
# ?rtweet


