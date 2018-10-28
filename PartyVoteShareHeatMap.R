library(ggplot2)
library(ggmap)
library(maps)
library(data.table)
library(ggthemes)
library(vidiris)


res <- read.csv("./Data/PartyVoteShare.csv")
res1 <- res
res1$Party <- factor(res1$Party,levels = unique(sort(res1$Party)))
View(res1)
ggplot(data=res1,aes(x=State,y=Party)) +
geom_tile(aes(fill=VoteShare)) +
scale_fill_viridis(option = "plasma") +
theme_solarized_2() +
xlab("State") +
ylab("Party") + guides(fill = guide_legend(title="Vote Share"))
