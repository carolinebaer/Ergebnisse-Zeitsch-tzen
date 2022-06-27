#install.packages("xlsx")

library(xlsx)
library(agricolae)

plan <- design.ab(4, trt = c(2,3,2), seed = 12322)
Blockplan <- plan$book

Blockplan$A[which(Blockplan$A == 1)] <- "Eisen"
Blockplan$A[which(Blockplan$A == 2)] <- "Stahl"
Blockplan$B[which(Blockplan$B == 1)] <- "mo-mu"
Blockplan$B[which(Blockplan$B == 2)] <- "lo-ru"
Blockplan$B[which(Blockplan$B == 3)] <- "ro-lu"
Blockplan$C[which(Blockplan$C == 1)] <- "rechts"
Blockplan$C[which(Blockplan$C == 2)] <- "links"

Blockplan$plots <- c(rep("Tag 1", 24), rep("Tag 2", 24))

names(Blockplan) <- c("Tag", "Block", "Drahtsorte", "Biegerichtung", "Händigkeit")

str(Blockplan)
write.xlsx(Blockplan, "Versuchsplan.xlsx")

