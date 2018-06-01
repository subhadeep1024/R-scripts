library("fpc")
library("dbscan")
library("factoextra")
library("rgl")

tmp <- rbind(PCA_comb1_w,PCA_comb1_r)

set.seed(123)
db <- fpc::dbscan(PCA_comb1_w_r[,1:4], eps = 1.5, MinPts = 100)
fviz_cluster(db, data =PCA_comb1_w_r[,1:4], stand = FALSE,
             ellipse = TRUE, show.clust.cent = FALSE,
             geom = "point", palette = "jco", ggtheme = theme_classic(),
             main = "cluster plot of WETs and RETs, eps = 1.5, MinPts = 100")

db_cluster <- as.data.frame(cbind(PCA_comb1_w_r[,1:4], db$cluster))
db_cluster_2 <- db_cluster[db_cluster$`db$cluster` !=0,]
table(db_cluster_2$`db$cluster`)
plot3d(db_cluster_2[,1:3],col=db_cluster_2$`db$cluster`,
       main = "Scatter plot after Density based clustering of WETs and RETs over first 3 PCs (without noise points)",
       xlab = "PC1", ylab ="PC2", zlab = "PC3")

plot3d(PCA_comb1_w_r[,1:3], main = "Scatter plot of WETs and RETs over first 3 PCs",
       xlab = "PC1", ylab ="PC2", zlab = "PC3")

PCA_comb1_w_r <- transform(PCA_comb1_w_r, cluster=db$cluster)

PCA_comb1_w_r_C1 <- PCA_comb1_w_r[PCA_comb1_w_r$cluster == 1,]
PCA_comb1_w_r_C2 <- PCA_comb1_w_r[PCA_comb1_w_r$cluster == 2,]
PCA_comb1_w_r_C0 <- PCA_comb1_w_r[PCA_comb1_w_r$cluster == 0,]

cluster_stat <- rbind(table(PCA_comb1_w_r_C0$exp_type), 
                      table(PCA_comb1_w_r_C1$exp_type),
                      table(PCA_comb1_w_r_C2$exp_type))
colnames(cluster_stat) <- c("RETs","WETs")
row.names(cluster_stat) <- c("Cluster 0","Cluster 1", "Cluster 2")


####################################### write tables #######################################
write.table(row.names(PCA_comb1_w_r_C0), file="C0_names", sep="\t")
names(C0_names) <- "transcript_id"
C0_transcript_details <- merge(C0_names,transcript_details, by = "transcript_id")

write.table(row.names(PCA_comb1_w_r_C1), file="C1_names", sep="\t")
names(C1_names) <- "transcript_id"
C1_transcript_details <- merge(C1_names,transcript_details, by = "transcript_id")

write.table(row.names(PCA_comb1_w_r_C2), file="C2_names", sep="\t")
names(C2_names) <- "transcript_id"
C2_transcript_details <- merge(C2_names,transcript_details, by = "transcript_id")

######################################## analysis of common genes ##########################
unq_C0 <- C0_transcript_details[!duplicated(C0_transcript_details$gene_name),"gene_name"]
unq_C1 <- C1_transcript_details[!duplicated(C1_transcript_details$gene_name),"gene_name"]
unq_C2 <- C2_transcript_details[!duplicated(C2_transcript_details$gene_name),"gene_name"]

unq <- list(C0 = unq_C0, C1 = unq_C1, C2 = unq_C2)
tmp1 <- intersect(intersect(unq_C0,unq_C1),unq_C2)
common_transcript <- transcript_details[transcript_details$gene_name %in% tmp1,]

common_transcript_C0 <- merge(C0_names, common_transcript, by = "transcript_id")
common_transcript_C1 <- merge(C1_names, common_transcript, by = "transcript_id")
common_transcript_C2 <- merge(C2_names, common_transcript, by = "transcript_id")


tmp2 <- intersect(unq_C1,unq_C2)
common_transcript_C1_C2 <- transcript_details[transcript_details$gene_name %in% tmp2,]
common_transcript_C1_C2_in_C1 <- merge(C1_names, common_transcript_C1_C2, by = "transcript_id")
common_transcript_C1_C2_in_C2 <- merge(C2_names, common_transcript_C1_C2, by = "transcript_id")



########################################### count analysis ########################################
count_table <- cbind(CpG_methylation_Cell_wise[,ncol(CpG_methylation_Cell_wise)],
                     H2az_Cell_wise[,ncol(H2az_Cell_wise)], 
                     H3k9ac_Cell_wise[,ncol(H3k9ac_Cell_wise)],
                     H3k9me3_Cell_wise[,ncol(H3k9me3_Cell_wise)],
                     H3k4me1_Cell_wise[,ncol(H3k4me1_Cell_wise)],
                     H3k4me2_Cell_wise[,ncol(H3k4me2_Cell_wise)],
                     H3k4me3_Cell_wise[,ncol(H3k4me3_Cell_wise)], 
                     H3k27ac_Cell_wise[,ncol(H3k27ac_Cell_wise)],
                     H3k27me3_Cell_wise[,ncol(H3k27me3_Cell_wise)],
                     H3k79me2_Cell_wise[,ncol(H3k79me2_Cell_wise)],
                     H4k20me1_Cell_wise[,ncol(H4k20me1_Cell_wise)],
                     DHS_len_Cell_wise[,ncol(DHS_len_Cell_wise)],
                     CAGE_Cell_wise_v2[,ncol(CAGE_Cell_wise_v2)])

percentage_table <- data.frame(
  CpG=(count_table[,1]/(ncol(CpG_methylation_Cell_wise) -1))*100,
  H2az = (count_table[,2]/(ncol(H2az_Cell_wise) -1))*100,
  H3k9ac = (count_table[,3]/(ncol(H3k9ac_Cell_wise) -1))*100,
  H3k9me3 = (count_table[,4]/(ncol(H3k9me3_Cell_wise) -1))*100,
  H3k4me1 = (count_table[,5]/(ncol(H3k4me1_Cell_wise) -1))*100,
  H3k4me2 = (count_table[,6]/(ncol(H3k4me2_Cell_wise) -1))*100,
  H3k4me3 = (count_table[,7]/(ncol(H3k4me3_Cell_wise) -1))*100,
  H3k27ac = (count_table[,8]/(ncol(H3k27ac_Cell_wise) -1))*100,
  H3k27me3 = (count_table[,9]/(ncol(H3k27me3_Cell_wise) -1))*100,
  H3k79me2 = (count_table[,10]/(ncol(H3k79me2_Cell_wise) -1))*100,
  H4k20me1 = (count_table[,11]/(ncol(H4k20me1_Cell_wise) -1))*100,
  DHS_len = (count_table[,12]/(ncol(DHS_len_Cell_wise) -1))*100,
  CAGE = (count_table[,13]/(ncol(CAGE_Cell_wise_v2) -1))*100)

row.names(percentage_table) <- row.names(H2az_Cell_wise)
expression_groups <- rbind(x,y,w,int)
expression_groups <- transform(expression_groups,
                               expression_group = c(rep("Widely expressed",nrow(x)),
                                                    rep("Rarely expressed",nrow(y)),
                                                    rep("Intermediately expressed",nrow(int)),
                                                    rep("Not expressed",nrow(w))
                               )
)

percentage_table_exp <-  merge(percentage_table,expression_groups, by = "row.names")[,c(1:14,46)]
row.names(percentage_table_exp) <- percentage_table_exp[,1]
percentage_table_exp <- percentage_table_exp[,-1]
percentage_table <- transform(percentage_table_exp, no_of_factors = rowSums(percentage_table_exp[,1:12] != 0))
write.table(percentage_table, file="percentage_table.tsv", sep="\t")
percentage_table_NETs <- percentage_table[percentage_table$expression_group == "Not expressed",]
write.table(percentage_table_NETs, file = "percentage_table_NETs.tsv", sep="\t")
percentage_table_nonzero_factors <- percentage_table[percentage_table$no_of_factors != 0,] 

percentage_table_w_r <- merge(percentage_table,PCA_comb1_w_r, by = "row.names")[,c(1:8,102,103)]
row.names(percentage_table_w_r) <- percentage_table_w_r[,1]
percentage_table_w_r <- percentage_table_w_r[,-1]

percentage_table_w_r_C1 <- percentage_table_w_r[percentage_table_w_r$cluster == 1,]
percentage_table_w_r_C2 <- percentage_table_w_r[percentage_table_w_r$cluster == 2,]
percentage_table_w_r_C0 <- percentage_table_w_r[percentage_table_w_r$cluster == 0,]

PCA_percentage_table_w_r_C1 <- princomp(percentage_table_w_r_C1[,1:7])
PCA_percentage_table_w_r_C2 <- princomp(percentage_table_w_r_C2[,1:7])
PCA_percentage_table_w_r_C0 <- princomp(percentage_table_w_r_C0[,1:7])

plot3d(PCA_percentage_table_w_r_C2$scores[,1:3])
plot(PCA_percentage_table_w_r_C2$scores[,c(1,2)])

db1 <- fpc::dbscan(PCA_percentage_table_w_r_C2$scores[,1:3], eps = 8, MinPts =  )
#km <- kmeans(PCA_percentage_table_w_r_C2$scores[,1:3],25)
db1_cluster <- as.data.frame(cbind(PCA_percentage_table_w_r_C2$scores[,1:3], db1$cluster))
table(db1_cluster$V4)

db1_cluster_2 <- db1_cluster[db1_cluster$V4 %in% 1:5,]
db1_cluster_2 <- db1_cluster[db1_cluster$V4 != 0,]
table(db1_cluster_2$V4)
                    
#plot3d(db1_cluster_2[,1:3],col=c("black","green","blue","yellow")[db1_cluster_2$V4])
plot3d(db1_cluster_2[,1:3],col=db1_cluster_2$V4)

PCA_comb1_w_r <- transform(tmp, exp_type =c(rep("w",nrow(PCA_comb1_w)), rep("r",nrow(PCA_comb1_r))))
