library(tidyverse)
file.tracking <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017090700.csv"
tracking.example <- read_csv(file.tracking)

file.game <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/games.csv"
games.sum <- read_csv(file.game) 

file.plays <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv"
plays.sum <- read_csv(file.plays)


tracking.example.merged <- tracking.example %>% inner_join(games.sum) %>% inner_join(plays.sum) 

example.play <- tracking.example.merged %>% filter(playId == 2756)

THill <- example.play[example.play$displayName == "Tyreek Hill",]
THill_Before_Catch <- THill[1:69,]
max(THill_Before_Catch$s)

THill_After_Catch <- THill[71:length(THill$time),]
max(THill_After_Catch$s)

example.play %>% select(playDescription) %>% slice(1)

library(gganimate)
library(cowplot)
library(gifski)
library(png)

## General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3


## Specific boundaries for a given play
ymin <- max(round(min(example.play$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(example.play$x, na.rm = TRUE) + 10, -1), 120)
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

animate.play <- ggplot() +
  geom_point(data = example.play, aes(x = (xmax-y), y = x, 
                                      colour = team, group = nflId, pch = team, size = team)) + 
  geom_text(data = example.play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
            vjust = 0.36, size = 3.5) + 
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
  scale_shape_manual(values = c(19, 16, 19), guide = FALSE) +
  scale_colour_manual(values = c("#e31837", "#654321", "#002244"), guide = FALSE) + 
  annotate("text", x = df.hash$x[df.hash$x < 55/2], 
           y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df.hash$x[df.hash$x > 55/2], 
           y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
  ylim(ymin, ymax) + 
  coord_fixed() +  
  theme_nothing() + 
  transition_time(frame.id)  +
  ease_aes('linear') + 
  NULL

animate.play
## Ensure timing of play matches 10 frames-per-second
play.length.ex <- length(unique(example.play$frame.id))
animate(animate.play, fps = 10, nframe = play.length.ex)

file.tracking <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017090700.csv"
tracking.example <- read_csv(file.tracking)






# Subsetting the long plays
long.plays <- plays.sum[plays.sum$PlayResult >= 40 & plays.sum$isSTPlay == FALSE
                        & plays.sum$isPenalty == FALSE,]
length(unique(plays.sum$gameId))

# Long plays that are passes with YAC and catch conditions met
long.plays.passes <- long.plays[is.na(long.plays$YardsAfterCatch) == FALSE,]
long.plays.passes_o <- long.plays.passes[long.plays.passes$YardsAfterCatch > 15 & 
                                           (long.plays.passes$PlayResult - 
                                           long.plays.passes$YardsAfterCatch) > 15, ]
long.plays.passes_o

### Summary Statistics ###
games_plays <- long.plays.passes_o %>% inner_join(games.sum)
# Which quearter did the play happen
table(games_plays$quarter)
# 20 teams represented - 37 plays in all
table(games_plays$possessionTeam)
# Which down did the plays happen on
table(games_plays$down)
# Formation
table(games_plays$offenseFormation)
# 28 different games 
length(unique(games_plays$gameId)) 




# Merging the datasets and isolating the player that had the reception and creating
# total dataframe

td_df <- data.frame()
g1 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017090700.csv"
File_g1 <- read_csv(g1)
Game1 <- File_g1 %>% inner_join(games_plays)

p1 <- Game1 %>% filter(playId == 2756)
THill_TD <- p1[p1$displayName == "Tyreek Hill",]
p1_description <- p1 %>% select(playDescription) %>% slice(1)



p2 <- Game1 %>% filter(playId == 3725)
KHunt_TD <- p2[p2$displayName == "Kareem Hunt",]
p2_description <- p2 %>% select(playDescription) %>% slice(1)

g2 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017091003.csv"
File_g2 <- read_csv(g2)
Game2 <- File_g2 %>% inner_join(games_plays)

ABrown_Catch <- Game2[Game2$displayName == "Antonio Brown",]

g3 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017091001.csv"
File_g3 <- read_csv(g3)
Game3 <- File_g3 %>% inner_join(games_plays)

AHooper_TD <- Game3[Game3$displayName == "Austin Hooper",]

g4 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017091008.csv"
File_g4 <- read_csv(g4)
Game4 <- File_g4 %>% inner_join(games_plays)
Game4 %>% select(playDescription) %>% slice(1)

NAgholor_TD <- Game4[Game4$displayName == "Nelson Agholor",]

g5 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017091011.csv"
File_g5 <- read_csv(g5)
Game5 <- File_g5 %>% inner_join(games_plays)
Game5 %>% select(playDescription) %>% slice(1)

RShepard_TD <- Game5[Game5$displayName == "Russell Shepard",]

g6 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017091100.csv"
File_g6 <- read_csv(g6)
Game6 <- File_g6 %>% inner_join(games_plays)
Game6 %>% select(playDescription) %>% slice(1)

AThielen_Catch <- Game6[Game6$displayName == "Adam Thielen",]

g7 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017091704.csv"
File_g7 <- read_csv(g7)
Game7 <- File_g7 %>% inner_join(games_plays)

p5 <- Game7 %>% filter(playId == 145)
p5 %>% select(playDescription) %>% slice(1)
TKelce_Catch <- p5[p5$displayName == "Travis Kelce",]

p6 <- Game7 %>% filter(playId == 1822)
p6 %>% select(playDescription) %>% slice(1)
ZErtz_Catch <- p6[p6$displayName == "Zach Ertz",]

g8 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017091705.csv"
File_g8 <- read_csv(g8)
Game8 <- File_g8 %>% inner_join(games_plays)

p7 <- Game8 %>% filter(playId == 615)
p7 %>% select(playDescription) %>% slice(1)
RGronkowski_TD <- p7[p7$displayName == "Rob Gronkowski",]

p8 <- Game8 %>% filter(playId == 1514)
p8 %>% select(playDescription) %>% slice(1)
BColeman_Catch <- p8[p8$displayName == "Brandon Coleman",]

g9 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017091702.csv"
File_g9 <- read_csv(g9)
Game9 <- File_g9 %>% inner_join(games_plays)
Game9 %>% select(playDescription) %>% slice(1)

IMomah_Catch <- Game9[Game9$displayName == "Ifeanyi Momah",]

g10 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017092408.csv"
File_g10 <- read_csv(g10)
Game10 <- File_g10 %>% inner_join(games_plays)
Game10 %>% select(playDescription) %>% slice(1)

RAnderson_Catch <- Game10[Game10$displayName == "Robby Anderson",]

g11 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017091700.csv"
File_g11 <- read_csv(g11)
Game11 <- File_g11 %>% inner_join(games_plays)
Game11 %>% select(playDescription) %>% slice(1)

SDevalve_Catch <- Game11[Game11$displayName == "Seth DeValve",]

g12 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017091711.csv"
File_g12 <- read_csv(g12)
Game12 <- File_g12 %>% inner_join(games_plays)
Game12 %>% select(playDescription) %>% slice(1)

GEverett_Catch <- Game12[Game12$displayName == "Gerald Everett",]

g13 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017092407.csv"
File_g13 <- read_csv(g13)
Game13 <- File_g13 %>% inner_join(games_plays)
unique(Game13$playId)

p9 <- Game13 %>% filter(playId == 1812)
p9 %>% select(playDescription) %>% slice(1)
CHogan_TD <- p9[p9$displayName == "Chris Hogan",]

p10 <- Game13 %>% filter(playId == 2363)
p10 %>% select(playDescription) %>% slice(1)
BCooks_TD <- p10[p10$displayName == "Brandin Cooks",]

g14 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017100104.csv"
File_g14 <- read_csv(g14)
Game14 <- File_g14 %>% inner_join(games_plays)
Game14 %>% select(playDescription) %>% slice(1)

TGurley_TD <- Game14[Game14$displayName == "Todd Gurley",]

g15 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017092410.csv"
File_g15 <- read_csv(g15)
Game15 <- File_g15 %>% inner_join(games_plays)
Game15 %>% select(playDescription) %>% slice(1)

CProsise_TD <- Game15[Game15$displayName == "C.J. Prosise",]

g16 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017100107.csv"
File_g16 <- read_csv(g16)
Game16 <- File_g16 %>% inner_join(games_plays)
unique(Game16$playId)

p11 <- Game16 %>% filter(playId == 1442)
p11 %>% select(playDescription) %>% slice(1)
RGronkowski_Catch <- p11[p11$displayName == "Rob Gronkowski",]

p12 <- Game16 %>% filter(playId == 1739)
p12 %>% select(playDescription) %>% slice(1)
KBenjamin_Catch <- p12[p12$displayName == "Kelvin Benjamin",]

g17 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017092411.csv"
File_g17 <- read_csv(g17)
Game17 <- File_g17 %>% inner_join(games_plays)
Game17 %>% select(playDescription) %>% slice(1)
LKendricks_Catch <- Game17[Game17$displayName == "Lance Kendricks",]

g18 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017100111.csv"
File_g18 <- read_csv(g18)
Game18 <- File_g18 %>% inner_join(games_plays)
Game18 %>% select(playDescription) %>% slice(1)

OHoward_TD <- Game18[Game18$displayName == "O.J. Howard",]

g19 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017100802.csv"
File_g19 <- read_csv(g19)
Game19 <- File_g19 %>% inner_join(games_plays)
unique(Game19$playId)

p13 <- Game19 %>% filter(playId == 604)
p13 %>% select(playDescription) %>% slice(1)
EDickson_Catch <- p13[p13$displayName == "Ed Dickson",]

p14 <- Game19 %>% filter(playId == 1121)
p14 %>% select(playDescription) %>% slice(1)
EDickson_Catch2 <- p14[p14$displayName == "Ed Dickson",]

g20 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017100112.csv"
File_g20 <- read_csv(g20)
Game20 <- File_g20 %>% inner_join(games_plays)
Game20 %>% select(playDescription) %>% slice(1)

JHolton_TD <- Game20[Game20$displayName == "Johnny Holton",]

g21 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017100805.csv"
File_g21 <- read_csv(g21)
Game21 <- File_g21 %>% inner_join(games_plays)
Game21 %>% select(playDescription) %>% slice(1)

OBeckham_TD <- Game21[Game21$displayName == "Odell Beckham",]

g22 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017100806.csv"
File_g22 <- read_csv(g22)
Game22 <- File_g22 %>% inner_join(games_plays)
unique(Game22$playId)

p15 <- Game22 %>% filter(playId == 856)
p15 %>% select(playDescription) %>% slice(1)
unique(p15$displayName)
TSmith_TD <- p15[p15$displayName == "Torrey Smith",]

p16 <- Game22 %>% filter(playId == 2533)
p16 %>% select(playDescription) %>% slice(1)
NAgholor_TD2 <- p16[p16$displayName == "Nelson Agholor",]

g23 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017100110.csv"
File_g23 <- read_csv(g23)
Game23 <- File_g23 %>% inner_join(games_plays)
Game23 %>% select(playDescription) %>% slice(1)
TWilliams_TD <- Game23[Game23$displayName == "Tyrell Williams",]

g24 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017100800.csv"
File_g24 <- read_csv(g24)
Game24 <- File_g24 %>% inner_join(games_plays)
unique(Game24$playId)

p17 <- Game24 %>% filter(playId == 273)
p17 %>% select(playDescription) %>% slice(1)

AGreen_TD <- p17[p17$displayName == "A.J. Green",]

p18 <- Game24 %>% filter(playId == 3155)
p18 %>% select(playDescription) %>% slice(1)
AGreen_Catch <- p18[p18$displayName == "A.J. Green",]

g25 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017100200.csv"
File_g25 <- read_csv(g25)
Game25 <- File_g25 %>% inner_join(games_plays)
Game25 %>% select(playDescription) %>% slice(1)
VDavis_Catch <- Game25[Game25$displayName == "Vernon Davis",]

g26 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017101509.csv"
File_g26 <- read_csv(g26)
Game26 <- File_g26 %>% inner_join(games_plays)
unique(Game26$playId)

p19 <- Game26 %>% filter(playId == 3022)
p19 %>% select(playDescription) %>% slice(1)
DThomas_TD <- p19[p19$displayName == "De'Anthony Thomas",]

p20 <- Game26 %>% filter(playId == 3161)
p20 %>% select(playDescription) %>% slice(1)
ABrown_TD <- p20[p20$displayName == "Antonio Brown",]

g27 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017101506.csv"
File_g27 <- read_csv(g27)
Game27 <- File_g27 %>% inner_join(games_plays)
Game27 %>% select(playDescription) %>% slice(1)
VDavis_Catch2 <- Game27[Game27$displayName == "Vernon Davis",]

g28 <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017101505.csv"
File_g28 <- read_csv(g28)
Game28 <- File_g28 %>% inner_join(games_plays)
Game28 %>% select(playDescription) %>% slice(1)
JKearse_Catch <- Game28[Game28$displayName == "Jermaine Kearse",]

View(THill_TD)
# Isolating
catch1 <- 71
speed_at_catch1 <- THill_TD[catch1,]$s
max_b_catch1 <- max(THill_TD[1:catch1-1,]$s)
max_a_catch1 <- max(THill_TD[catch1+1:(length(THill_TD$s) - catch1),]$s)

View(KHunt_TD)
catch2 <- 75
speed_at_catch2 <- KHunt_TD[catch2,]$s
max_b_catch2 <- max(KHunt_TD[1:catch2-1,]$s)
max_a_catch2 <- max(KHunt_TD[catch2+1:(length(KHunt_TD$s) - catch2),]$s)

View(ABrown_Catch)
catch3 <- 52
speed_at_catch3 <- ABrown_Catch[catch3,]$s
max_b_catch3 <- max(ABrown_Catch[1:catch3-1,]$s)
max_a_catch3 <- max(ABrown_Catch[catch3+1:(length(ABrown_Catch$s) - catch3),]$s)

View(AHooper_TD)
catch4 <- 81
speed_at_catch4 <- AHooper_TD[catch4,]$s
max_b_catch4 <- max(ABrown_Catch[1:catch4-1,]$s)
max_a_catch4 <- max(ABrown_Catch[catch4+1:(length(ABrown_Catch$s) - catch4),]$s)

View(NAgholor_TD)
catch5 <- 129
speed_at_catch5 <- NAgholor_TD[catch5,]$s
max_b_catch5 <- max(NAgholor_TD[1:catch5-1,]$s)
max_a_catch5 <- max(NAgholor_TD[catch5+1:(length(NAgholor_TD$s) - catch5),]$s)

View(RShepard_TD)
catch6 <- 65
speed_at_catch6 <- RShepard_TD[catch6,]$s
max_b_catch6 <- max(RShepard_TD[1:catch6-1,]$s)
max_a_catch6 <- max(RShepard_TD[catch6+1:(length(RShepard_TD$s) - catch6),]$s)

View(AThielen_Catch)
catch7 <- 54
speed_at_catch7 <- AThielen_Catch[catch7,]$s
max_b_catch7 <- max(AThielen_Catch[1:catch7-1,]$s)
max_a_catch7 <- max(AThielen_Catch[catch7+1:(length(AThielen_Catch$s) - catch7),]$s)

View(TKelce_Catch)
catch8 <- 58
speed_at_catch8 <- TKelce_Catch[catch8,]$s
max_b_catch8 <- max(TKelce_Catch[1:catch8-1,]$s)
max_a_catch8 <- max(TKelce_Catch[catch8+1:(length(TKelce_Catch$s) - catch8),]$s)

View(ZErtz_Catch)
catch9 <- 76
speed_at_catch9 <- ZErtz_Catch[catch9,]$s
max_b_catch9 <- max(ZErtz_Catch[1:catch9-1,]$s)
max_a_catch9 <- max(ZErtz_Catch[catch9+1:(length(ZErtz_Catch$s) - catch9),]$s)

View(RGronkowski_TD)
catch10 <- 70
speed_at_catch10 <- RGronkowski_TD[catch10,]$s
max_b_catch10 <- max(RGronkowski_TD[1:catch10-1,]$s)
max_a_catch10 <- max(RGronkowski_TD[catch10+1:(length(RGronkowski_TD$s) - catch10),]$s)

View(BColeman_Catch)
catch11 <- 62
speed_at_catch11 <- BColeman_Catch[catch11,]$s
max_b_catch11 <- max(BColeman_Catch[1:catch11-1,]$s)
max_a_catch11 <- max(BColeman_Catch[catch11+1:(length(BColeman_Catch$s) - catch11),]$s)

View(IMomah_Catch)
catch12 <- 64
speed_at_catch12 <- IMomah_Catch[catch12,]$s
max_b_catch12 <- max(IMomah_Catch[1:catch12-1,]$s)
max_a_catch12 <- max(IMomah_Catch[catch12+1:(length(IMomah_Catch$s) - catch12),]$s)

View(RAnderson_Catch)
catch13 <- 65
speed_at_catch13 <- RAnderson_Catch[catch13,]$s
max_b_catch13 <- max(RAnderson_Catch[1:catch13-1,]$s)
max_a_catch13 <- max(RAnderson_Catch[catch13+1:(length(RAnderson_Catch$s) - catch13),]$s)

View(SDevalve_Catch)
catch14 <- 63
speed_at_catch14 <- SDevalve_Catch[catch14,]$s
max_b_catch14 <- max(SDevalve_Catch[1:catch14-1,]$s)
max_a_catch14 <- max(SDevalve_Catch[catch14+1:(length(SDevalve_Catch$s) - catch14),]$s)

View(GEverett_Catch)
catch15 <- 86
speed_at_catch15 <- GEverett_Catch[catch15,]$s
max_b_catch15 <- max(GEverett_Catch[1:catch15-1,]$s)
max_a_catch15 <- max(GEverett_Catch[catch15+1:(length(GEverett_Catch$s) - catch15),]$s)

View(CHogan_TD)
catch16 <- 67
speed_at_catch16 <- CHogan_TD[catch16,]$s
max_b_catch16 <- max(CHogan_TD[1:catch16-1,]$s)
max_a_catch16 <- max(CHogan_TD[catch16+1:(length(CHogan_TD$s) - catch16),]$s)

View(BCooks_TD)
catch17 <- 61
speed_at_catch17 <- BCooks_TD[catch17,]$s
max_b_catch17 <- max(BCooks_TD[1:catch17-1,]$s)
max_a_catch17 <- max(BCooks_TD[catch17+1:(length(BCooks_TD$s) - catch17),]$s)

View(TGurley_TD)
catch18 <- 54
speed_at_catch18 <- TGurley_TD[catch18,]$s
max_b_catch18 <- max(TGurley_TD[1:catch18-1,]$s)
max_a_catch18 <- max(TGurley_TD[catch18+1:(length(TGurley_TD$s) - catch18),]$s)

View(CProsise_TD)
catch19 <- 54
speed_at_catch19 <- CProsise_TD[catch19,]$s
max_b_catch19 <- max(CProsise_TD[1:catch19-1,]$s)
max_a_catch19 <- max(CProsise_TD[catch19+1:(length(CProsise_TD$s) - catch19),]$s)

View(RGronkowski_Catch)
catch20 <- 52
speed_at_catch20 <- RGronkowski_Catch[catch20,]$s
max_b_catch20 <- max(RGronkowski_Catch[1:catch20-1,]$s)
max_a_catch20 <- max(RGronkowski_Catch[catch20+1:(length(RGronkowski_Catch$s) - catch20),]$s)

View(KBenjamin_Catch)
catch21 <- 57
speed_at_catch21 <- KBenjamin_Catch[catch21,]$s
max_b_catch21 <- max(KBenjamin_Catch[1:catch21-1,]$s)
max_a_catch21 <- max(KBenjamin_Catch[catch21+1:(length(KBenjamin_Catch$s) - catch21),]$s)

View(LKendricks_Catch)
catch22 <- 89
speed_at_catch22 <- LKendricks_Catch[catch22,]$s
max_b_catch22 <- max(LKendricks_Catch[1:catch22-1,]$s)
max_a_catch22 <- max(LKendricks_Catch[catch22+1:(length(LKendricks_Catch$s) - catch22),]$s)

View(OHoward_TD)
catch23 <- 70
speed_at_catch23 <- OHoward_TD[catch23,]$s
max_b_catch23 <- max(OHoward_TD[1:catch23-1,]$s)
max_a_catch23 <- max(OHoward_TD[catch23+1:(length(OHoward_TD$s) - catch23),]$s)

View(EDickson_Catch)
catch24 <- 54
speed_at_catch24 <- EDickson_Catch[catch24,]$s
max_b_catch24 <- max(EDickson_Catch[1:catch24-1,]$s)
max_a_catch24 <- max(EDickson_Catch[catch24+1:(length(EDickson_Catch$s) - catch24),]$s)

View(EDickson_Catch2)
catch25 <- 65
speed_at_catch25 <- EDickson_Catch2[catch25,]$s
max_b_catch25 <- max(EDickson_Catch2[1:catch25-1,]$s)
max_a_catch25 <- max(EDickson_Catch2[catch25+1:(length(EDickson_Catch2$s) - catch25),]$s)

View(JHolton_TD)
catch26 <- 75
speed_at_catch26 <- JHolton_TD[catch26,]$s
max_b_catch26 <- max(JHolton_TD[1:catch26-1,]$s)
max_a_catch26 <- max(JHolton_TD[catch26+1:(length(JHolton_TD$s) - catch26),]$s)

View(OBeckham_TD)
catch27 <- 63
speed_at_catch27 <- OBeckham_TD[catch27,]$s
max_b_catch27 <- max(OBeckham_TD[1:catch27-1,]$s)
max_a_catch27 <- max(OBeckham_TD[catch27+1:(length(OBeckham_TD$s) - catch27),]$s)

View(TSmith_TD)
catch28 <- 70
speed_at_catch28 <- TSmith_TD[catch28,]$s
max_b_catch28 <- max(TSmith_TD[1:catch28-1,]$s)
max_a_catch28 <- max(TSmith_TD[catch28+1:(length(TSmith_TD$s) - catch28),]$s)

View(NAgholor_TD2)
catch29 <- 75
speed_at_catch29 <- NAgholor_TD2[catch29,]$s
max_b_catch29 <- max(NAgholor_TD2[1:catch29-1,]$s)
max_a_catch29 <- max(NAgholor_TD2[catch29+1:(length(NAgholor_TD2$s) - catch29),]$s)

View(TWilliams_TD)
catch30 <- 80
speed_at_catch30 <- TWilliams_TD[catch30,]$s
max_b_catch30 <- max(TWilliams_TD[1:catch30-1,]$s)
max_a_catch30 <- max(TWilliams_TD[catch30+1:(length(TWilliams_TD$s) - catch30),]$s)

View(AGreen_TD)
catch31 <- 69
speed_at_catch31 <- AGreen_TD[catch31,]$s
max_b_catch31 <- max(AGreen_TD[1:catch31-1,]$s)
max_a_catch31 <- max(AGreen_TD[catch31+1:(length(AGreen_TD$s) - catch31),]$s)

View(AGreen_Catch)
catch32 <- 59
speed_at_catch32 <- AGreen_Catch[catch32,]$s
max_b_catch32 <- max(AGreen_Catch[1:catch32-1,]$s)
max_a_catch32 <- max(AGreen_Catch[catch32+1:(length(AGreen_Catch$s) - catch32),]$s)

View(VDavis_Catch)
catch33 <- 61
speed_at_catch33 <- VDavis_Catch[catch33,]$s
max_b_catch33 <- max(VDavis_Catch[1:catch33-1,]$s)
max_a_catch33 <- max(VDavis_Catch[catch33+1:(length(VDavis_Catch$s) - catch33),]$s)

View(DThomas_TD)
catch34 <- 89
speed_at_catch34 <- DThomas_TD[catch34,]$s
max_b_catch34 <- max(DThomas_TD[1:catch34-1,]$s)
max_a_catch34 <- max(DThomas_TD[catch34+1:(length(DThomas_TD$s) - catch34),]$s)

View(ABrown_TD)
catch35 <- 64
speed_at_catch35 <- ABrown_TD[catch35,]$s
max_b_catch35 <- max(ABrown_TD[1:catch35-1,]$s)
max_a_catch35 <- max(ABrown_TD[catch35+1:(length(ABrown_TD$s) - catch35),]$s)

View(VDavis_Catch2)
catch36 <- 55
speed_at_catch36 <- VDavis_Catch2[catch36,]$s
max_b_catch36 <- max(VDavis_Catch2[1:catch36-1,]$s)
max_a_catch36 <- max(VDavis_Catch2[catch36+1:(length(VDavis_Catch2$s) - catch36),]$s)

View(JKearse_Catch)
catch37 <- 85
speed_at_catch37 <- JKearse_Catch[catch37,]$s
max_b_catch37 <- max(JKearse_Catch[1:catch37-1,]$s)
max_a_catch37 <- max(JKearse_Catch[catch37+1:(length(JKearse_Catch$s) - catch37),]$s)

Speed_At_Catch <- c(speed_at_catch1, speed_at_catch2, speed_at_catch3,speed_at_catch4,
                    speed_at_catch5, speed_at_catch6, speed_at_catch7,
                    speed_at_catch8, speed_at_catch9, speed_at_catch10,
                    speed_at_catch11, speed_at_catch12, speed_at_catch13, 
                    speed_at_catch14, speed_at_catch15, 
                    speed_at_catch18, speed_at_catch19,
                    speed_at_catch20,speed_at_catch21, speed_at_catch22,
                    speed_at_catch23, speed_at_catch24, speed_at_catch25,
                    speed_at_catch26, speed_at_catch27, speed_at_catch28,
                    speed_at_catch29, speed_at_catch30, speed_at_catch31,
                    speed_at_catch32, speed_at_catch33, speed_at_catch34,
                    speed_at_catch35, speed_at_catch36, speed_at_catch37)

Max_Speed_Before_Catch <- c(max_b_catch1, max_b_catch2, max_b_catch3,
                            max_b_catch4, max_b_catch5, max_b_catch6,
                            max_b_catch7, max_b_catch8, max_b_catch9,
                            max_b_catch10, max_b_catch11, max_b_catch12,
                            max_b_catch13, max_b_catch14, max_b_catch15,
                            max_b_catch18,
                            max_b_catch19, max_b_catch20, max_b_catch21, 
                            max_b_catch22, max_b_catch23, max_b_catch24, 
                            max_b_catch25, max_b_catch26, max_b_catch27,
                            max_b_catch28, max_b_catch29, max_b_catch30,
                            max_b_catch31, max_b_catch32, max_b_catch33,
                            max_b_catch34, max_b_catch35, max_b_catch36,
                            max_b_catch37)

Max_Speed_After_Catch <- c(max_a_catch1, max_a_catch2, max_a_catch3, max_a_catch4,
                           max_a_catch5, max_a_catch6, max_a_catch7, max_a_catch8,
                           max_a_catch9, max_a_catch10, max_a_catch11, max_a_catch12,
                           max_a_catch13, max_a_catch14, max_a_catch15,
                          max_a_catch18, max_a_catch19, max_a_catch20,
                           max_a_catch21, max_a_catch22, max_a_catch23, max_a_catch24,
                           max_a_catch25, max_a_catch26, max_a_catch27, max_a_catch28,
                           max_a_catch29, max_a_catch30, max_a_catch31, max_a_catch32,
                           max_a_catch33, max_a_catch34, max_a_catch35, max_a_catch36,
                           max_a_catch37)

plays <- long.plays.passes_o %>% inner_join(games_plays)
plays$YardsBeforeCatch <- plays$PlayResult - plays$YardsAfterCatch
plays_df <- plays[-c(16, 17), ]
plays_df$Speed_at_Catch <- Speed_At_Catch
plays_df$Max_Speed_Before_Catch <- Max_Speed_Before_Catch
plays_df$Max_Speed_After_Catch <- Max_Speed_After_Catch

# There were initially 37 plays. We had to eliminate two plays, the Chris Hogan
# and Brandin Cooks Touchdown because there was no speed data for these two games
plays_df 
pt <- plays_df[,c("playDescription", "PlayResult", "YardsBeforeCatch", "YardsAfterCatch",
               "Max_Speed_Before_Catch", "Speed_at_Catch", "Max_Speed_After_Catch")]
write.csv(plays_df, "plays_df.csv")
write.csv(pt, "plays_df_speed.csv")
