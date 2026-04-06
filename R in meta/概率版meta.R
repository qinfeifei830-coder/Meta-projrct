library(netmeta)
library(dmetar)
data(TherapyFormats) 
#CBT for depression抑郁症的认知行为疗法（CBT）

head(TherapyFormats[1:5])
as.matrix(table(TherapyFormats$author))


# fit the model
m.netmeta <- netmeta(TE = TE,
                     seTE = seTE,
                     treat1 = treat1,
                     treat2 = treat2,
                     studlab = author,
                     data = TherapyFormats,
                     sm = "SMD", #type of effect size
                     common =  TRUE,
                     random = FALSE,
                     reference.group = "cau",
                     details.chkmultiarm = TRUE,
                     sep.trts = " vs ") #分隔符
summary(m.netmeta)

decomp.design(m.netmeta)


# Show treatment order (shortened labels)
m.netmeta$trts

# Replace with full name (see treat1.long and treat2.long)
long.labels <- c("Care As Usual", "Group", 
                 "Guided Self-Help", 
                 "Individual", "Telephone", 
                 "Unguided Self-Help", 
                 "Waitlist")

netgraph(m.netmeta, 
         labels = long.labels)


library(rgl)
netgraph(m.netmeta, dim = "3d")

library(dmetar)

d.evidence <- direct.evidence.plot(m.netmeta)
plot(d.evidence)

result.matrix <- m.netmeta$TE.fixed
result.matrix <- round(result.matrix, 2)

result.matrix[lower.tri(result.matrix, diag = FALSE)] <- NA

result.matrix

# Produce effect table
netleague <- netleague(m.netmeta, 
                       bracket = "(", # use round brackets
                       digits=2)      # round to two digits

# Save results (here: the ones of the fixed-effect model)
write.csv(netleague$fixed, "netleague.csv")

netrank(m.netmeta, small.values = "good")

forest(m.netmeta, 
       reference.group = "cau",
       sortvar = TE,
       xlim = c(-1.3, 0.5),
       smlab = paste("Therapy Formats vs. Care As Usual \n",
                     "(Depressive Symptoms)"),
       drop.reference.group = TRUE,
       label.left = "Favors Intervention",
       label.right = "Favors Care As Usual",
       labels = long.labels)

netheat(m.netmeta)

netheat(m.netmeta, random = TRUE)

netsplit(m.netmeta)

netsplit(m.netmeta) %>% forest()

funnel(m.netmeta, 
       order = c("wlc", "cau", "ind", "grp", # from old to new
                 "tel", "ush", "gsh"), 
       pch = c(1:4, 5, 6, 8, 15:19, 21:24), 
       col = c("blue", "red", "purple", "forestgreen", "grey", 
               "green", "black", "brown", "orange", "pink", 
               "khaki", "plum", "aquamarine", "sandybrown", 
               "coral", "gold4"), 
       linreg = TRUE)







