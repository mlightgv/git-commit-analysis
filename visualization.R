ggplot(dataFrame7, aes(factor(week), fill = factor(Category))) + geom_bar() + guides(fill=guide_legend(reverse=TRUE)) + facet_grid(. ~ Repository + Branch)
ggplot(dataFrame7, aes(week, sumChanges,colour= factor(Category),group=Category)) +  geom_line() +  geom_point() + facet_grid(. ~ Repository + Branch)

ggplot(dataFrame7, aes(week, cumChanges,colour= factor(Category),group=Category)) +  geom_line() +  geom_point() + facet_grid(. ~ Repository + Branch) 
ggplot(dataFrame7, aes(x = week, y = cumChanges, group =Category, fill=Category)) + geom_area(position="identity",alpha=.5) + facet_grid(. ~ Repository + Branch)

ggplot(commits, aes(week, NumCommits,colour= factor(Category),group=Category)) +  geom_line() +  geom_point() + facet_grid(. ~ Repository + Branch)
ggplot(commits2, aes(week, CumCommits,colour= factor(Category),group=Category)) +  geom_line() +  geom_point() + facet_grid(. ~ Repository + Branch)
ggplot(commits2, aes(x = week, y = CumCommits, group =Category, fill=Category)) + geom_area(position="identity",alpha=.5)  + facet_grid(. ~ Repository + Branch)


ggplot(commits2, aes(x = week, y = CumCommits, group =Category, fill=Category)) + geom_area() + facet_grid(. ~ Repository + Branch)+  scale_fill_discrete() 

repositories <- ddply(commit_extraction, c("Repository"), summarize,  N=length(Category))

plot3d(dataFrame7$sumChanges, dataFrame7$week, dataFrame7$cumChanges, type="s", size=0.75, lit=FALSE)

