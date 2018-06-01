H2az_r <- H2az_Cell_wise[row.names(H2az_Cell_wise) %in% row.names(y),-6]
H2az_w <- H2az_Cell_wise[row.names(H2az_Cell_wise) %in% row.names(x),-6]
H2az_n <- H2az_Cell_wise[row.names(H2az_Cell_wise) %in% row.names(w),-6]
H2az_int <- H2az_Cell_wise[row.names(H2az_Cell_wise) %in% row.names(int),-6]

H3k9ac_r <- H3k9ac_Cell_wise[row.names(H3k9ac_Cell_wise) %in% row.names(y),-14]
H3k9ac_w <- H3k9ac_Cell_wise[row.names(H3k9ac_Cell_wise) %in% row.names(x),-14]
H3k9ac_n <- H3k9ac_Cell_wise[row.names(H3k9ac_Cell_wise) %in% row.names(w),-14]
H3k9ac_int <- H3k9ac_Cell_wise[row.names(H3k9ac_Cell_wise) %in% row.names(int),-14]

H3k9me3_r <- H3k9me3_Cell_wise[row.names(H3k9me3_Cell_wise) %in% row.names(y),-7]
H3k9me3_w <- H3k9me3_Cell_wise[row.names(H3k9me3_Cell_wise) %in% row.names(x),-7]
H3k9me3_n <- H3k9me3_Cell_wise[row.names(H3k9me3_Cell_wise) %in% row.names(w),-7]
H3k9me3_int <- H3k9me3_Cell_wise[row.names(H3k9me3_Cell_wise) %in% row.names(int),-7]

H3k4me1_r <- H3k4me1_Cell_wise[row.names(H3k4me1_Cell_wise) %in% row.names(y),-13]
H3k4me1_w <- H3k4me1_Cell_wise[row.names(H3k4me1_Cell_wise) %in% row.names(x),-13]
H3k4me1_n <- H3k4me1_Cell_wise[row.names(H3k4me1_Cell_wise) %in% row.names(w),-13]
H3k4me1_int <- H3k4me1_Cell_wise[row.names(H3k4me1_Cell_wise) %in% row.names(int),-13]

H3k4me2_r <- H3k4me2_Cell_wise[row.names(H3k4me2_Cell_wise) %in% row.names(y),-14]
H3k4me2_w <- H3k4me2_Cell_wise[row.names(H3k4me2_Cell_wise) %in% row.names(x),-14]
H3k4me2_n <- H3k4me2_Cell_wise[row.names(H3k4me2_Cell_wise) %in% row.names(w),-14]
H3k4me2_int <- H3k4me2_Cell_wise[row.names(H3k4me2_Cell_wise) %in% row.names(int),-14]

H3k4me3_r <- H3k4me3_Cell_wise[row.names(H3k4me3_Cell_wise) %in% row.names(y),-ncol(H3k4me3_Cell_wise)]
H3k4me3_w <- H3k4me3_Cell_wise[row.names(H3k4me3_Cell_wise) %in% row.names(x),-ncol(H3k4me3_Cell_wise)]
H3k4me3_n <- H3k4me3_Cell_wise[row.names(H3k4me3_Cell_wise) %in% row.names(w),-ncol(H3k4me3_Cell_wise)]
H3k4me3_int <- H3k4me3_Cell_wise[row.names(H3k4me3_Cell_wise) %in% row.names(int),-ncol(H3k4me3_Cell_wise)]

H3k36me3_r <- H3k36me3_Cell_wise[row.names(H3k36me3_Cell_wise) %in% row.names(y),-ncol(H3k36me3_Cell_wise)]
H3k36me3_w <- H3k36me3_Cell_wise[row.names(H3k36me3_Cell_wise) %in% row.names(x),-ncol(H3k36me3_Cell_wise)]
H3k36me3_n <- H3k36me3_Cell_wise[row.names(H3k36me3_Cell_wise) %in% row.names(w),-ncol(H3k36me3_Cell_wise)]
H3k36me3_int <- H3k36me3_Cell_wise[row.names(H3k36me3_Cell_wise) %in% row.names(int),-ncol(H3k36me3_Cell_wise)]

H3k27ac_r <- H3k27ac_Cell_wise[row.names(H3k27ac_Cell_wise) %in% row.names(y),-ncol(H3k27ac_Cell_wise)]
H3k27ac_w <- H3k27ac_Cell_wise[row.names(H3k27ac_Cell_wise) %in% row.names(x),-ncol(H3k27ac_Cell_wise)]
H3k27ac_n <- H3k27ac_Cell_wise[row.names(H3k27ac_Cell_wise) %in% row.names(w),-ncol(H3k27ac_Cell_wise)]
H3k27ac_int <- H3k27ac_Cell_wise[row.names(H3k27ac_Cell_wise) %in% row.names(int),-ncol(H3k27ac_Cell_wise)]

H3k27me3_r <- H3k27me3_Cell_wise[row.names(H3k27me3_Cell_wise) %in% row.names(y),-ncol(H3k27me3_Cell_wise)]
H3k27me3_w <- H3k27me3_Cell_wise[row.names(H3k27me3_Cell_wise) %in% row.names(x),-ncol(H3k27me3_Cell_wise)]
H3k27me3_n <- H3k27me3_Cell_wise[row.names(H3k27me3_Cell_wise) %in% row.names(w),-ncol(H3k27me3_Cell_wise)]
H3k27me3_int <- H3k27me3_Cell_wise[row.names(H3k27me3_Cell_wise) %in% row.names(int),-ncol(H3k27me3_Cell_wise)]

H3k79me2_r <- H3k79me2_Cell_wise[row.names(H3k79me2_Cell_wise) %in% row.names(y),-ncol(H3k79me2_Cell_wise)]
H3k79me2_w <- H3k79me2_Cell_wise[row.names(H3k79me2_Cell_wise) %in% row.names(x),-ncol(H3k79me2_Cell_wise)]
H3k79me2_n <- H3k79me2_Cell_wise[row.names(H3k79me2_Cell_wise) %in% row.names(w),-ncol(H3k79me2_Cell_wise)]
H3k79me2_int <- H3k79me2_Cell_wise[row.names(H3k79me2_Cell_wise) %in% row.names(int),-ncol(H3k79me2_Cell_wise)]

H4k20me1_r <- H4k20me1_Cell_wise[row.names(H4k20me1_Cell_wise) %in% row.names(y),-ncol(H4k20me1_Cell_wise)]
H4k20me1_w <- H4k20me1_Cell_wise[row.names(H4k20me1_Cell_wise) %in% row.names(x),-ncol(H4k20me1_Cell_wise)]
H4k20me1_n <- H4k20me1_Cell_wise[row.names(H4k20me1_Cell_wise) %in% row.names(w),-ncol(H4k20me1_Cell_wise)]
H4k20me1_int <- H4k20me1_Cell_wise[row.names(H4k20me1_Cell_wise) %in% row.names(int),-ncol(H4k20me1_Cell_wise)]

DHS_len_r <- DHS_len_Cell_wise[row.names(DHS_len_Cell_wise) %in% row.names(y),-ncol(DHS_len_Cell_wise)]
DHS_len_w <- DHS_len_Cell_wise[row.names(DHS_len_Cell_wise) %in% row.names(x),-ncol(DHS_len_Cell_wise)]
DHS_len_n <- DHS_len_Cell_wise[row.names(DHS_len_Cell_wise) %in% row.names(w),-ncol(DHS_len_Cell_wise)]
DHS_len_int <- DHS_len_Cell_wise[row.names(DHS_len_Cell_wise) %in% row.names(int),-ncol(DHS_len_Cell_wise)]

CpG_r <- CpG_methylation_Cell_wise[row.names(CpG_methylation_Cell_wise) %in% row.names(y),-ncol(CpG_methylation_Cell_wise)]
CpG_w <- CpG_methylation_Cell_wise[row.names(CpG_methylation_Cell_wise) %in% row.names(x),-ncol(CpG_methylation_Cell_wise)]
CpG_n <- CpG_methylation_Cell_wise[row.names(CpG_methylation_Cell_wise) %in% row.names(w),-ncol(CpG_methylation_Cell_wise)]
CpG_int <- CpG_methylation_Cell_wise[row.names(CpG_methylation_Cell_wise) %in% row.names(int),-ncol(CpG_methylation_Cell_wise)]
############################################################################################################################################

comb1 <- cbind(H2az_Cell_wise[,-ncol(H2az_Cell_wise)], H3k9ac_Cell_wise[,-ncol(H3k9ac_Cell_wise)], 
               H3k4me2_Cell_wise[,-ncol(H3k4me2_Cell_wise)], H3k4me3_Cell_wise[,-ncol(H3k4me3_Cell_wise)], 
               H3k27ac_Cell_wise[,-ncol(H3k27ac_Cell_wise)],H3k79me2_Cell_wise[,-ncol(H3k79me2_Cell_wise)],
               DHS_len_Cell_wise[,-col(DHS_len_Cell_wise)])

comb1_r <- comb1[row.names(comb1) %in% row.names(y),]
comb1_w <- comb1[row.names(comb1) %in% row.names(x),]
comb1_n <- comb1[row.names(comb1) %in% row.names(w),]
comb1_int <- comb1[row.names(comb1) %in% row.names(int),]
##################################################################################################################
######################################### PCA all ##############################################################

PCA_H2az <- princomp(H2az_Cell_wise[,-ncol(H2az_Cell_wise)])
PCA_H2az_r <- PCA_H2az$scores[row.names(PCA_H2az$scores) %in% row.names(H2az_r),]
PCA_H2az_w <- PCA_H2az$scores[row.names(PCA_H2az$scores) %in% row.names(H2az_w),]
PCA_H2az_n <- PCA_H2az$scores[row.names(PCA_H2az$scores) %in% row.names(H2az_n),]
PCA_H2az_int <- PCA_H2az$scores[row.names(PCA_H2az$scores) %in% row.names(H2az_int),]

PCA_H3k9ac <- princomp(H3k9ac_Cell_wise[,-ncol(H3k9ac_Cell_wise)])
PCA_H3k9ac_r <- PCA_H3k9ac$scores[row.names(PCA_H3k9ac$scores) %in% row.names(H3k9ac_r),]
PCA_H3k9ac_w <- PCA_H3k9ac$scores[row.names(PCA_H3k9ac$scores) %in% row.names(H3k9ac_w),]
PCA_H3k9ac_n <- PCA_H3k9ac$scores[row.names(PCA_H3k9ac$scores) %in% row.names(H3k9ac_n),]
PCA_H3k9ac_int <- PCA_H3k9ac$scores[row.names(PCA_H3k9ac$scores) %in% row.names(H3k9ac_int),]

PCA_H3k9me3 <- princomp(H3k9me3_Cell_wise[,-ncol(H3k9me3_Cell_wise)])
PCA_H3k9me3_r <- PCA_H3k9me3$scores[row.names(PCA_H3k9me3$scores) %in% row.names(H3k9me3_r),]
PCA_H3k9me3_w <- PCA_H3k9me3$scores[row.names(PCA_H3k9me3$scores) %in% row.names(H3k9me3_w),]
PCA_H3k9me3_n <- PCA_H3k9me3$scores[row.names(PCA_H3k9me3$scores) %in% row.names(H3k9me3_n),]
PCA_H3k9me3_int <- PCA_H3k9me3$scores[row.names(PCA_H3k9me3$scores) %in% row.names(H3k9me3_int),]

PCA_H3k4me1 <- princomp(H3k4me1_Cell_wise[,-ncol(H3k4me1_Cell_wise)])
PCA_H3k4me1_r <- PCA_H3k4me1$scores[row.names(PCA_H3k4me1$scores) %in% row.names(H3k4me1_r),]
PCA_H3k4me1_w <- PCA_H3k4me1$scores[row.names(PCA_H3k4me1$scores) %in% row.names(H3k4me1_w),]
PCA_H3k4me1_n <- PCA_H3k4me1$scores[row.names(PCA_H3k4me1$scores) %in% row.names(H3k4me1_n),]
PCA_H3k4me1_int <- PCA_H3k4me1$scores[row.names(PCA_H3k4me1$scores) %in% row.names(H3k4me1_int),]

PCA_H3k4me2 <- princomp(H3k4me2_Cell_wise[,-ncol(H3k4me2_Cell_wise)])
PCA_H3k4me2_r <- PCA_H3k4me2$scores[row.names(PCA_H3k4me2$scores) %in% row.names(H3k4me2_r),]
PCA_H3k4me2_w <- PCA_H3k4me2$scores[row.names(PCA_H3k4me2$scores) %in% row.names(H3k4me2_w),]
PCA_H3k4me2_n <- PCA_H3k4me2$scores[row.names(PCA_H3k4me2$scores) %in% row.names(H3k4me2_n),]
PCA_H3k4me2_int <- PCA_H3k4me2$scores[row.names(PCA_H3k4me2$scores) %in% row.names(H3k4me2_int),]


PCA_H3k4me3 <- princomp(H3k4me3_Cell_wise[,-ncol(H3k4me3_Cell_wise)])
PCA_H3k4me3_r <- PCA_H3k4me3$scores[row.names(PCA_H3k4me3$scores) %in% row.names(H3k4me3_r),]
PCA_H3k4me3_w <- PCA_H3k4me3$scores[row.names(PCA_H3k4me3$scores) %in% row.names(H3k4me3_w),]
PCA_H3k4me3_n <- PCA_H3k4me3$scores[row.names(PCA_H3k4me3$scores) %in% row.names(H3k4me3_n),]
PCA_H3k4me3_int <- PCA_H3k4me3$scores[row.names(PCA_H3k4me3$scores) %in% row.names(H3k4me3_int),]

PCA_H3k36me3 <- princomp(H3k36me3_Cell_wise[,-ncol(H3k36me3_Cell_wise)])
PCA_H3k36me3_r <- PCA_H3k36me3$scores[row.names(PCA_H3k36me3$scores) %in% row.names(H3k36me3_r),]
PCA_H3k36me3_w <- PCA_H3k36me3$scores[row.names(PCA_H3k36me3$scores) %in% row.names(H3k36me3_w),]
PCA_H3k36me3_n <- PCA_H3k36me3$scores[row.names(PCA_H3k36me3$scores) %in% row.names(H3k36me3_n),]
PCA_H3k36me3_int <- PCA_H3k36me3$scores[row.names(PCA_H3k36me3$scores) %in% row.names(H3k36me3_int),]

PCA_H3k27ac <- princomp(H3k27ac_Cell_wise[,-ncol(H3k27ac_Cell_wise)])
PCA_H3k27ac_r <- PCA_H3k27ac$scores[row.names(PCA_H3k27ac$scores) %in% row.names(H3k27ac_r),]
PCA_H3k27ac_w <- PCA_H3k27ac$scores[row.names(PCA_H3k27ac$scores) %in% row.names(H3k27ac_w),]
PCA_H3k27ac_n <- PCA_H3k27ac$scores[row.names(PCA_H3k27ac$scores) %in% row.names(H3k27ac_n),]
PCA_H3k27ac_int <- PCA_H3k27ac$scores[row.names(PCA_H3k27ac$scores) %in% row.names(H3k27ac_int),]

PCA_H3k27me3 <- princomp(H3k27me3_Cell_wise[,-ncol(H3k27me3_Cell_wise)])
PCA_H3k27me3_r <- PCA_H3k27me3$scores[row.names(PCA_H3k27me3$scores) %in% row.names(H3k27me3_r),]
PCA_H3k27me3_w <- PCA_H3k27me3$scores[row.names(PCA_H3k27me3$scores) %in% row.names(H3k27me3_w),]
PCA_H3k27me3_n <- PCA_H3k27me3$scores[row.names(PCA_H3k27me3$scores) %in% row.names(H3k27me3_n),]
PCA_H3k27me3_int <- PCA_H3k27me3$scores[row.names(PCA_H3k27me3$scores) %in% row.names(H3k27me3_int),]

PCA_H3k79me2 <- princomp(H3k79me2_Cell_wise[,-ncol(H3k79me2_Cell_wise)])
PCA_H3k79me2_r <- PCA_H3k79me2$scores[row.names(PCA_H3k79me2$scores) %in% row.names(H3k79me2_r),]
PCA_H3k79me2_w <- PCA_H3k79me2$scores[row.names(PCA_H3k79me2$scores) %in% row.names(H3k79me2_w),]
PCA_H3k79me2_n <- PCA_H3k79me2$scores[row.names(PCA_H3k79me2$scores) %in% row.names(H3k79me2_n),]
PCA_H3k79me2_int <- PCA_H3k79me2$scores[row.names(PCA_H3k79me2$scores) %in% row.names(H3k79me2_int),]

PCA_H4k20me1 <- princomp(H4k20me1_Cell_wise[,-ncol(H4k20me1_Cell_wise)])
PCA_H4k20me1_r <- PCA_H4k20me1$scores[row.names(PCA_H4k20me1$scores) %in% row.names(H4k20me1_r),]
PCA_H4k20me1_w <- PCA_H4k20me1$scores[row.names(PCA_H4k20me1$scores) %in% row.names(H4k20me1_w),]
PCA_H4k20me1_n <- PCA_H4k20me1$scores[row.names(PCA_H4k20me1$scores) %in% row.names(H4k20me1_n),]
PCA_H4k20me1_int <- PCA_H4k20me1$scores[row.names(PCA_H4k20me1$scores) %in% row.names(H4k20me1_int),]

PCA_DHS_len <- princomp(DHS_len_Cell_wise[,-ncol(DHS_len_Cell_wise)])
PCA_DHS_len_r <- PCA_DHS_len$scores[row.names(PCA_DHS_len$scores) %in% row.names(DHS_len_r),]
PCA_DHS_len_w <- PCA_DHS_len$scores[row.names(PCA_DHS_len$scores) %in% row.names(DHS_len_w),]
PCA_DHS_len_n <- PCA_DHS_len$scores[row.names(PCA_DHS_len$scores) %in% row.names(DHS_len_n),]
PCA_DHS_len_int <- PCA_DHS_len$scores[row.names(PCA_DHS_len$scores) %in% row.names(DHS_len_int),]

PCA_CpG <- princomp(CpG_methylation_Cell_wise[,-ncol(CpG_methylation_Cell_wise)])
PCA_CpG_r <- PCA_CpG$scores[row.names(PCA_CpG$scores) %in% row.names(CpG_r),]
PCA_CpG_w <- PCA_CpG$scores[row.names(PCA_CpG$scores) %in% row.names(CpG_w),]
PCA_CpG_n <- PCA_CpG$scores[row.names(PCA_CpG$scores) %in% row.names(CpG_n),]
PCA_CpG_int <- PCA_CpG$scores[row.names(PCA_CpG$scores) %in% row.names(CpG_int),]
###combination##
########## Caution: all variables are not in same scale, use correlation instead of covariance##################
PCA_comb1 <- princomp(scale(comb1))
PCA_comb1_r <- PCA_comb1$scores[row.names(PCA_comb1$scores) %in% row.names(comb1_r),]
PCA_comb1_w <- PCA_comb1$scores[row.names(PCA_comb1$scores) %in% row.names(comb1_w),]
PCA_comb1_n <- PCA_comb1$scores[row.names(PCA_comb1$scores) %in% row.names(comb1_n),]
PCA_comb1_int <- PCA_comb1$scores[row.names(PCA_comb1$scores) %in% row.names(comb1_int),]
