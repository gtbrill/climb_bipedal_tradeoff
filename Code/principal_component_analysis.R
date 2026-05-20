library(dplyr)
library(here)

data <- read.csv(here("CSV","anthropometric_data.csv"))
data <- data[1:304,1:39]

male <- subset(data,data$Sex=="Male")
female <- subset(data,data$Sex=="Female")

# Create data frame
pca_data <- data %>%
       dplyr::select(all_of(c("Stature", "SitHeight", "ArmSpan", "Tibia", "ForearmL", "HandLength", "FootLength","Finger", "Biacrom", "Biiliac"))) %>%
       dplyr::mutate(across(everything(), as.numeric)) %>%
       tidyr::drop_na()

# Run PCA
pca <- prcomp(pca_data, center = TRUE, scale. = TRUE, retx = TRUE)

# View PCA
summary(pca)
round(pca$rotation, 2)
loadings <- pca$rotation[, 1:4] # Communalities for first 4 PCs
communalities <- rowSums(loadings^2)
round(communalities, 2)

# Add PCs to original dataset
pc_scores <- as.data.frame(pca$x[, 1:4]) # First 4 PCs
PC1 <- bind_cols(data, pc_scores)

# Add PC1 to main data
data$PC1 <- pca$x[, 1]

write.csv(data, here("CSV", "anthropometric_data_PC1.csv"), row.names = FALSE)