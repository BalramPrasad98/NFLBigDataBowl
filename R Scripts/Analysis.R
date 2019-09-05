plays_df_speed <- read.csv("plays_df_speed.csv")
plays_df <- read.csv("plays_df.csv")
library(ggplot2)
library(ggpubr)
library(scales)


length(unique(plays_df$possessionTeam))
d <- table(plays_df$down)
d <- as.data.frame(d)
q <- table(plays_df$quarter)
q <- as.data.frame(q)
table(plays_df$possessionTeam)
table(plays_df$offenseFormation)

ggplot(plays_df, aes(offenseFormation)) + 
  geom_bar(fill = "lightblue") + labs(x = "Offensive Formation", y = "Count")

bp <- ggplot(d, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")
pie <- bp + coord_polar("y", start=0)

library(scales)
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

down_pie <- pie + scale_fill_brewer(palette="Blues") + theme(axis.text.x=element_blank()) + blank_theme + labs(title = "Down")
bp1 <- ggplot(q, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")
pie1 <- bp1 + coord_polar("y", start=0)
quarter_pie <- pie1 + scale_fill_brewer(palette="Blues") + theme(axis.text.x=element_blank()) + blank_theme
table(plays_df$StadiumType)
table(plays_df$Turf)

Max_Speed_diff <- plays_df_speed$Max_Speed_After_Catch -
  plays_df_speed$Max_Speed_Before_Catch
plays_df_speed$Max_Speed_diff <- Max_Speed_diff

Yards_diff <- plays_df_speed$YardsAfterCatch - plays_df_speed$YardsBeforeCatch
plays_df_speed$YardsDiff <- Yards_diff

ggscatter(plays_df_speed, x = "YardsDiff", y = "Max_Speed_diff",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Difference in Yards After Catch and Yards Before Catch", 
          ylab = "Difference in Speed After Catch and Speed Before Catch") 

plays_df_speed[11,]

Graphic_1 <- ggscatter(plays_df_speed, x = "YardsDiff", y = "Max_Speed_diff",
          xlab = "Difference in Yards After Catch and Yards Before Catch", 
          ylab = "Difference in Speed After Catch and Speed Before Catch") + 
  geom_vline(xintercept = 0, linetype="dashed") + 
  geom_hline(yintercept=0, linetype="dashed") + xlim(-25, 30) + ylim(-2, 3)

plays_df_speed$Classification <- ifelse((plays_df_speed$Max_Speed_diff > 0 & 
                                        plays_df_speed$YardsDiff > 0) | (
                                        plays_df_speed$Max_Speed_diff < 0 & 
                                        plays_df_speed$YardsDiff < 0
                                          ),
                                        "Correct", "Incorrect")

ggscatter(plays_df_speed, x = "YardsDiff", y = "Max_Speed_diff",
          xlab = "Difference in Yards After Catch and Yards Before Catch", 
          ylab = "Difference in Speed After Catch and Speed Before Catch", 
          color = "Classification") + geom_vline(xintercept = 0, linetype="dashed") + 
  geom_hline(yintercept=0, linetype="dashed") + 
  scale_color_manual(values=c("blue", "red")) + xlim(-25, 30) + ylim(-2, 3) + 
  theme(legend.position="right")

number_incorrect <- sum(plays_df_speed$Classification == "Incorrect")
number_correct <- sum(plays_df_speed$Classification == "Correct")

pos_sd_yd <- sd(plays_df_speed$YardsDiff)
neg_sd_yd <- -sd(plays_df_speed$YardsDiff)
pos_sd_spd <- sd(plays_df_speed$Max_Speed_diff)
neg_sd_spd <- -sd(plays_df_speed$Max_Speed_diff)

ggscatter(plays_df_speed, x = "YardsDiff", y = "Max_Speed_diff",
          xlab = "Difference in Yards After Catch and Yards Before Catch", 
          ylab = "Difference in Speed After Catch and Speed Before Catch", 
          color = "Classification") + geom_vline(xintercept = 0, linetype="dashed") + 
  geom_vline(xintercept = pos_sd_yd, linetype="dashed", color = "grey") +
  geom_vline(xintercept = neg_sd_yd, linetype="dashed", color = "grey") +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_hline(yintercept=pos_sd_spd, linetype="dashed", color = "grey") +
  geom_hline(yintercept=neg_sd_spd, linetype="dashed", color = "grey") +
  scale_color_manual(values=c("blue", "red")) + xlim(-25, 30) + ylim(-2, 3)


plays_df_speed$Outside_one_SD_and_Incorrect <- ifelse((plays_df_speed$Classification == "Incorrect") &  ((plays_df_speed$YardsDiff > 
                                                                                              pos_sd_yd) | plays_df_speed$YardsDiff
                                                                                           < neg_sd_yd | plays_df_speed$Max_Speed_diff
                                                                                           < neg_sd_spd), "Match", "Not Match")

ggscatter(plays_df_speed, x = "YardsDiff", y = "Max_Speed_diff",
          xlab = "Difference in Yards After Catch and Yards Before Catch", 
          ylab = "Difference in Speed After Catch and Speed Before Catch", 
          color = "Outside_one_SD_and_Incorrect") + geom_vline(xintercept = 0, linetype="dashed") + 
  geom_vline(xintercept = pos_sd_yd, linetype="dashed", color = "red") +
  geom_vline(xintercept = neg_sd_yd, linetype="dashed", color = "red") +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_hline(yintercept=pos_sd_spd, linetype="dashed", color = "red") +
  geom_hline(yintercept=neg_sd_spd, linetype="dashed", color = "red") +
  scale_color_manual(values=c("red", "grey")) + xlim(-25, 30)+ylim(-2, 3) + 
  theme(legend.position="none")

plays_df_speed[plays_df_speed$Classification == "Incorrect",]
plays_df_speed[31,]
plays_df_speed[11,]
plays_df_speed[17,]
plays_df_speed

plays_df_speed$Inside_One_SD <- ifelse((plays_df_speed$YardsDiff < pos_sd_yd & 
                                          plays_df_speed$YardsDiff > neg_sd_yd & 
                                          plays_df_speed$Max_Speed_diff < pos_sd_spd & 
                                          plays_df_speed$Max_Speed_diff > neg_sd_spd),"Match", "Not Match")

ggscatter(plays_df_speed[plays_df_speed$Inside_One_SD == "Match",], x = "YardsDiff", y = "Max_Speed_diff",
          xlab = "Difference in Yards After Catch and Yards Before Catch", 
          ylab = "Difference in Speed After Catch and Speed Before Catch", 
          color = "Classification") + geom_vline(xintercept = 0, linetype="dashed") + 
  geom_vline(xintercept = pos_sd_yd, linetype="dashed", color = "red") +
  geom_vline(xintercept = neg_sd_yd, linetype="dashed", color = "red") +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_hline(yintercept=pos_sd_spd, linetype="dashed", color = "red") +
  geom_hline(yintercept=neg_sd_spd, linetype="dashed", color = "red") +
  scale_color_manual(values=c("blue", "red")) + xlim(-25, 30) + ylim(-2, 3) +
  theme(legend.position="right")

plays_df_positive <- plays_df_speed[plays_df_speed$YardsDiff > 0 & 
                                      plays_df_speed$Max_Speed_diff > 0,]

plays_df_negative <- plays_df_speed[plays_df_speed$YardsDiff < 0 & 
                                                           plays_df_speed$Max_Speed_diff < 0,]

ggscatter(plays_df_positive, x = "YardsDiff", y = "Max_Speed_diff",
          xlab = "Difference in Yards After Catch and Yards Before Catch", 
          ylab = "Difference in Speed After Catch and Speed Before Catch", 
          color = "blue") + 
  geom_vline(xintercept = 0, linetype="dashed") + 
  geom_hline(yintercept=0, linetype="dashed") +
  xlim(-25, 30) + ylim(-2, 3)
cor(plays_df_positive$YardsDiff, plays_df_positive$Max_Speed_diff)


ggscatter(plays_df_negative, x = "YardsDiff", y = "Max_Speed_diff",
          xlab = "Difference in Yards After Catch and Yards Before Catch", 
          ylab = "Difference in Speed After Catch and Speed Before Catch", 
          color = "blue") + 
  geom_vline(xintercept = 0, linetype="dashed") + 
  geom_hline(yintercept=0, linetype="dashed") +
  xlim(-25, 30) + ylim(-2, 3)
cor(plays_df_negative$YardsDiff, plays_df_negative$Max_Speed_diff)

plays_df_negative$Percentage <- 100 * ((plays_df_negative$Max_Speed_Before_Catch - 
                                   plays_df_negative$Max_Speed_After_Catch)/plays_df_negative$Max_Speed_After_Catch)
plays_df_positive$Percentage <- 100 * ((plays_df_positive$Max_Speed_After_Catch - 
                                   plays_df_positive$Max_Speed_Before_Catch)/plays_df_positive$Max_Speed_Before_Catch)

desired <- rbind(plays_df_negative, plays_df_positive)
group <- c(rep("negative",length(plays_df_negative$X)), 
rep("positive", length(plays_df_positive$X)))
desired$group <- group
desired$abs_YardsDiff <- abs(desired$YardsDiff)
desired$abs_Max_Speed_diff <- abs(desired$Max_Speed_diff)

ggscatter(desired, x = "abs_YardsDiff", y = "abs_Max_Speed_diff",
          color = "group") + scale_color_manual(values=c("red", "blue"))  

ggplot(desired, aes(x = abs_YardsDiff, y = Percentage, color = group)) + 
  geom_point() + geom_smooth(method = lm, se = FALSE)

# Data shows no linear relationship. Cannot make an observations about the 
# percentage increase as yardage increases
p <- desired[desired$group == "positive",]
n <- desired[desired$group == "negative",]

positive_lm <- lm(formula = p$Percentage ~ p$abs_YardsDiff)
summary(positive_lm)

negative_lm <- lm(formula = n$Percentage ~ n$abs_YardsDiff)
summary(negative_lm)

ggplot(desired, aes(x = abs_YardsDiff, y = Percentage, color = group)) + 
  geom_point() 

# Bad Correlation. Can therefore just look at the graphic
cor(desired$abs_YardsDiff, desired$Percentage)

mean(p$Percentage)
sd(p$Percentage)

mean(n$Percentage)
sd(n$Percentage)

desired$key <- desired[desired]

ggplot(desired, aes(x = abs_YardsDiff, y = Percentage, color = group)) + 
  geom_point() + geom_vline(xintercept = 20, linetype="dashed") + 
  geom_hline(yintercept=10, linetype="dashed") + 
  labs(x = "Difference in Yards", y = "Percentage Increase in Speed")
