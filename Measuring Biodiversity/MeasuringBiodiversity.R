MeasuringBiodiversity <- read.csv("MeasuringBiodiversity.csv")
str(MeasuringBiodiversity)
colnames(MeasuringBiodiversity[, (1:5)])

#Question 1
gammadiversity <- sum(apply(MeasuringBiodiversity[, -(1:5)], 2,
                            function(col)any(col >= 1)))
print(gammadiversity) 

#Question 2
LPReference<-subset(MeasuringBiodiversity,
                    Location=="LPNWA"&HabitatType=="Reference")
species_matrixLPReference <- (LPReference[, -(1:5)])

LPTreated<-subset(MeasuringBiodiversity,
                    Location=="LPNWA"&HabitatType=="Treated")
species_matrixLPTreated <- (LPTreated[, -(1:5)])

LPPhragmites<-subset(MeasuringBiodiversity,
                    Location=="LPNWA"&HabitatType=="Phragmites")
species_matrixLPPhragmites <- (LPPhragmites[, -(1:5)])

BCReference<-subset(MeasuringBiodiversity,
                    Location=="BCNWA"&HabitatType=="Reference")
species_matrixBCReference <- (BCReference[, -(1:5)])

BCTreated<-subset(MeasuringBiodiversity,
                    Location=="BCNWA"&HabitatType=="Treated")
species_matrixBCTreated <- (BCTreated[, -(1:5)])

BCPhragmites<-subset(MeasuringBiodiversity,
                    Location=="BCNWA"&HabitatType=="Phragmites")
species_matrixBCPhragmites <- (BCPhragmites[, -(1:5)])


species_accum_LPReference <- specaccum(species_matrixLPReference,
                                       method="random")
species_accum_LPTreated <- specaccum(species_matrixLPTreated,
                                       method="random")
species_accum_LPPhragmites <- specaccum(species_matrixLPPhragmites,
                                       method="random")
species_accum_BCReference <- specaccum(species_matrixBCReference,
                                       method="random")
species_accum_BCTreated <- specaccum(species_matrixBCTreated,
                                       method="random")
species_accum_BCPhragmites <- specaccum(species_matrixBCPhragmites,
                                       method="random")


plot(species_accum_LPReference, ci.type="poly", col="blue", lwd=2, ci =.95,
     ci.col="lightblue", xlab="Number of Transects", ylab="Number of
Species")+boxplot(species_accum_LPReference, col="yellow", add=TRUE)

plot(species_accum_LPTreated, ci.type="poly", col="blue", lwd=2, ci =.95,
     ci.col="lightblue", xlab="Number of Transects", ylab="Number of
Species")+boxplot(species_accum_LPTreated, col="yellow", add=TRUE)

plot(species_accum_LPPhragmites, ci.type="poly", col="blue", lwd=2, ci =.95,
     ci.col="lightblue", xlab="Number of Transects", ylab="Number of
Species")+boxplot(species_accum_LPPhragmites, col="yellow", add=TRUE)

plot(species_accum_BCReference, ci.type="poly", col="blue", lwd=2, ci =.95,
     ci.col="lightblue", xlab="Number of Transects", ylab="Number of
Species")+boxplot(species_accum_BCReference, col="yellow", add=TRUE)

plot(species_accum_BCTreated, ci.type="poly", col="blue", lwd=2, ci =.95,
     ci.col="lightblue", xlab="Number of Transects", ylab="Number of
Species")+boxplot(species_accum_BCTreated, col="yellow", add=TRUE)

plot(species_accum_BCPhragmites, ci.type="poly", col="blue", lwd=2, ci =.95,
     ci.col="lightblue", xlab="Number of Transects", ylab="Number of
Species")+boxplot(species_accum_BCPhragmites, col="yellow", add=TRUE)

#Question 4
species_matrix<-(MeasuringBiodiversity[, -(1:5)])

singleton_counts <- apply(species_matrix, 1, function(row) sum(row == 1))
uniques <- vapply(species_matrix, \(x) sum(x !=0) == 1, logical(1L))
rowcol_uniques <- which(species_matrix[uniques] != 0, arr.ind = TRUE)
rownames(rowcol_uniques) <- names(uniques[uniques])
anyuniques <- merge(data.frame(row = seq_len(nrow(species_matrix))),
                    as.data.frame(table(row = rowcol_uniques[,1])),all.x = TRUE)
anyuniques[is.na(anyuniques)] <- 0
Results <- data.frame(SiteID = MeasuringBiodiversity$SiteID, Location =
                        MeasuringBiodiversity$Location,
                      HabitatType=MeasuringBiodiversity$HabitatType,
                      Singletons=singleton_counts, Uniques=anyuniques$Freq)
View(Results)

ggplot(Results, aes(x = Location, y = Singletons, fill = Location)) +
  geom_boxplot() +
  labs(
    x = "Location",
    y = "Singletons") +
  scale_fill_manual(values = c("red", "blue", "green"))
ggsave("Boxplot_of_Singletons_by_Location.png")

ggplot(Results, aes(x = HabitatType, y = Singletons, fill = HabitatType)) +
  geom_boxplot() +
  labs(
    x = "Habitat Type",
    y = "Singletons") +
  scale_fill_manual(values = c("red", "blue", "green"))
ggsave("Boxplot_of_Singletons_by_HabitatType.png")

uniques_LPNWA <- sum(Results$Uniques[Results$Location == "LPNWA"])
print(uniques_LPNWA)
uniques_BCNWA <- sum(Results$Uniques[Results$Location == "BCNWA"])
print(uniques_BCNWA)

#Question 6
pool_results_LPReference<-(poolaccum(species_matrixLPReference))
pool_results_LPTreated<-(poolaccum(species_matrixLPTreated))
pool_results_LPPhragmites<-(poolaccum(species_matrixLPPhragmites))
pool_results_BCReference<-(poolaccum(species_matrixBCReference))
pool_results_BCTreated<-(poolaccum(species_matrixBCTreated))
pool_results_BCPhragmites<-(poolaccum(species_matrixBCPhragmites))

print(pool_results_LPReference)
print(pool_results_LPTreated)
print(pool_results_LPPhragmites)
print(pool_results_BCReference)
print(pool_results_BCTreated)
print(pool_results_BCPhragmites)

plot(pool_results_LPReference)
plot(pool_results_LPTreated)
plot(pool_results_LPPhragmites)
plot(pool_results_BCReference)
plot(pool_results_BCTreated)
plot(pool_results_BCPhragmites)

#Question 10
InvSimpson <- diversity(species_matrix, index = "invsimpson")

InvSimpsonResults <- data.frame(SiteID = MeasuringBiodiversity$SiteID,
                                Location = MeasuringBiodiversity$Location,
                                HabitatType=MeasuringBiodiversity$HabitatType, InvSimpson=InvSimpson)

mean_table <- InvSimpsonResults %>%
  group_by(Location, HabitatType) %>%
  summarise(mean_invsimpson = mean(InvSimpson))

View(mean_table)

#Question 11
MeasuringBiodiversity1 <- (MeasuringBiodiversity)
rownames(MeasuringBiodiversity1) <- MeasuringBiodiversity1$SiteID
matrix_with_rownames<-subset(MeasuringBiodiversity1[,-(1:5)])

Sorenson<-as.matrix(vegdist(matrix_with_rownames, method="bray",
                            binary=TRUE))
head(Sorenson[, 1:5], n = 5)
Sorenson_df <- reshape2::melt(Sorenson)
colnames(Sorenson_df) <- c("SiteID_Column", "SiteID_Row", "Sorenson")

Bray<-as.matrix(vegdist(matrix_with_rownames, method="bray", binary=FALSE))
Bray_df <- reshape2::melt(Bray)
colnames(Bray_df) <- c("SiteID_Column", "SiteID_Row", "BrayCurtis")

Jaccard<-as.matrix(vegdist(matrix_with_rownames, method="jaccard",
                           binary=TRUE))
Jaccard_df <- reshape2::melt(Jaccard)
colnames(Jaccard_df) <- c("SiteID_Column", "SiteID_Row", "Jaccard")

merged_df <- merge(Sorenson_df, Bray_df, by = c("SiteID_Column",
                                                "SiteID_Row"), all = TRUE)
merged_df <- merge(merged_df, Jaccard_df, by = c("SiteID_Column",
                                                 "SiteID_Row"), all = TRUE)

merged_df <- merge(merged_df, MeasuringBiodiversity[, c("SiteID",
                                                        "HabitatType")], by.x = "SiteID_Row", by.y = "SiteID", all.x = TRUE)
colnames(merged_df)[colnames(merged_df) == "HabitatType"] <- "HabitatType_Row"

merged_df <- merge(merged_df, MeasuringBiodiversity[, c("SiteID",
                                                        "HabitatType")], by.x = "SiteID_Column", by.y = "SiteID", all.x = TRUE)
colnames(merged_df)[colnames(merged_df) == "HabitatType"] <- "HabitatType_Column"

colnames(merged_df)

summary_table <- merged_df %>%
  group_by(HabitatType_Column, HabitatType_Row) %>%
  summarise(
    Mean_BrayCurtis = mean(BrayCurtis), Mean_Sorenson=mean(Sorenson),
    Mean_Jaccard=mean(Jaccard)
  )

View(summary_table)

#Question 12
ggplot(merged_df, aes(x = Sorenson, y = Jaccard)) +
  geom_point() +
  labs(x = "Sorenson", y = "Jaccard") +
  theme_minimal()

#Question 13
ggplot(merged_df, aes(x = Sorenson, y = BrayCurtis)) +
  geom_point() +
  labs(x = "Sorenson", y = "Bray-Curtis") +
  theme_minimal()