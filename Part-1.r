# DATA UNDERSTANDING & DESCRIPTIVE STATISTICS
setwd("/Users/druvitha/Desktop/Bussiness Analytics and Decision science Course Work/Part-1")
orders <- read.csv("order_july25.csv", header = TRUE)

# BASIC DATA INSPECTION
dim(orders)
colnames(orders)
head(orders, 10)
str(orders)
summary(orders)

# CONVERT CATEGORICAL VARIABLES
cat("\nCONVERTING CATEGORICAL VARIABLES\n")
orders$ad_channel <- as.factor(orders$ad_channel)
orders$voucher <- as.factor(orders$voucher)

# DESCRIPTIVE STATISTICS - NUMERICAL
cat("\n --REVENUE-- \n")
cat("Mean:", mean(orders$revenue, na.rm = TRUE), "\n")
cat("Median:", median(orders$revenue, na.rm = TRUE), "\n")
cat("SD:", sd(orders$revenue, na.rm = TRUE), "\n")
print(quantile(orders$revenue, na.rm = TRUE))

cat("\n --NUMBER OF PAST ORDERS-- \n")
cat("Mean:", mean(orders$number_past_order, na.rm = TRUE), "\n")
cat("Median:", median(orders$number_past_order, na.rm = TRUE), "\n")
print(quantile(orders$number_past_order, na.rm = TRUE))

cat("\n --PAST SPEND-- \n")
cat("Mean:", mean(orders$past_spend, na.rm = TRUE), "\n")
cat("Median:", median(orders$past_spend, na.rm = TRUE), "\n")
print(quantile(orders$past_spend, na.rm = TRUE))

cat("\n --TIME ON WEBSITE-- \n")
cat("Mean:", mean(orders$time_web, na.rm = TRUE), "\n")
cat("Median:", median(orders$time_web, na.rm = TRUE), "\n")
print(quantile(orders$time_web, na.rm = TRUE))

# DESCRIPTIVE STATISTICS - CATEGORICAL
cat("\n --AD CHANNEL-- \n")
print(table(orders$ad_channel))
print(round(100 * prop.table(table(orders$ad_channel)), 2))

cat("\n --VOUCHER-- \n")
print(table(orders$voucher))
print(round(100 * prop.table(table(orders$voucher)), 2))

# MISSING DATA
cat("\n --MISSING DATA-- \n")
cat("Complete:", sum(complete.cases(orders)), "\n")
cat("Incomplete:", sum(!complete.cases(orders)), "\n")
print(colSums(is.na(orders)))



# ========================================
# VISUALIZATIONS (4 FIGURES)
# ========================================

cat("=== CREATING 4 VISUALIZATIONS ===\n\n")

graphics.off()

# FIGURE 1
par(family = "sans", col.axis = "grey30", col.lab = "grey30", 
    col.main = "grey10", cex.main = 1.2)

hist(orders$revenue,
     main = "Figure 1: Distribution of Order Revenue",
     xlab = "Revenue (£ GBP)",
     ylab = "Number of Orders",
     col = "#6BAED6",
     border = "white",
     breaks = 30,
     las = 1)

abline(v = mean(orders$revenue, na.rm = TRUE), col = "#D62728", lwd = 2.5, lty = 2)
abline(v = median(orders$revenue, na.rm = TRUE), col = "#2CA02C", lwd = 2.5, lty = 2)

legend("topright", 
       legend = c(paste0("Average Order: £", round(mean(orders$revenue, na.rm = TRUE), 2)),
                  paste0("Typical Order: £", round(median(orders$revenue, na.rm = TRUE), 2)),
                  paste0("Price Range: £ ", round(max(orders$revenue, na.rm = TRUE), 2))), 
       col = c("#D62728", "#2CA02C","#6BAED6"), lty = c(2,2,1), lwd = 2.5, bty = "n", inset = 0.01 )

cat("✓ Figure 1\n")

# FIGURE 2
par(family = "sans", col.axis = "grey30", col.lab = "grey30", 
    col.main = "grey10", cex.main = 1.2)

hist(orders$past_spend,
     main = "Figure 2: Distribution of Historical Customer Spending",
     xlab = "Past Spend (£ GBP)",
     ylab = "Number of Orders",
     col = "#98DF8A",
     border = "white",
     breaks = 30,
     las = 1)

abline(v = median(orders$past_spend, na.rm = TRUE), col = "#D62728", lwd = 2.5, lty = 2)

legend("topright",
       legend = paste0("Typical Customer: £", round(median(orders$past_spend, na.rm = TRUE), 2)),
       col = "#D62728", lty = 2, lwd = 2.5, bty = "n")

cat("✓ Figure 2\n")

# FIGURE 3
par(family = "sans", col.axis = "grey30", col.lab = "grey30", 
    col.main = "grey10", cex.main = 1.2, mar = c(7, 5, 4, 2))

# Create the table
ad_counts <- table(orders$ad_channel)

# Assign custom names
names(ad_counts) <- c("No Ads", "Paid Search", "Social Media", "Email")

# Create barplot
bp <- barplot(ad_counts,
              main = "Figure 3: Order Distribution by Advertisement Channel",
              xlab = "",
              ylab = "Number of Orders",
              col = c("#BCBD22", "#17BECF", "#FF7F0E", "#E377C2"),
              border = "white",
              las = 2,
              ylim = c(0, max(ad_counts) * 1.15))

mtext("Advertisement Channel", side = 1, line = 5.5)

text(x = bp, y = ad_counts + 150, labels = ad_counts, cex = 0.9)

pct <- round(100 * prop.table(ad_counts), 1)
text(x = bp, y = ad_counts / 2, labels = paste0(pct, "%"), 
     cex = 0.9, col = "white", font = 2)

cat("✓ Figure 3\n")


# FIGURE 4
par(family = "sans", col.axis = "grey30", col.lab = "grey30", 
    col.main = "grey10", cex.main = 1.2, mar = c(5, 5, 4, 2))

vouch_counts <- table(orders$voucher)

# Rename voucher categories
names(vouch_counts) <- c("No Voucher", "Voucher Used")

bp2 <- barplot(vouch_counts,
               main = "Figure 4: Distribution of Discount Voucher Usage",
               xlab = "Voucher Status",
               ylab = "Number of Orders",
               col = c("#1F77B4", "#FF7F0E"),
               border = "white",
               las = 1,
               ylim = c(0, max(vouch_counts) * 1.15))

text(x = bp2, y = vouch_counts + 350, labels = vouch_counts, cex = 1.0)

vouch_pct <- round(100 * prop.table(vouch_counts), 1)
text(x = bp2, y = vouch_counts / 2, labels = paste0(vouch_pct, "%"), 
     cex = 1.1, col = "white", font = 2)

cat("✓ Figure 4\n")

cat("\n=== ALL 4 PLOTS COMPLETE ===\n")
cat("Use ◀ ▶ arrows in Plots pane to navigate!\n")

# ============================================================================
# DATA PREPARATION: MISSING DATA ANALYSIS + ROW DELETION
# ============================================================================

library(tidyverse)

# Assume 'orders' already loaded:
# orders <- read.csv("order_july25.csv", header = TRUE)

# 1. MISSING DATA SUMMARY (ORIGINAL DATA)
cat("========== MISSING DATA SUMMARY (ORIGINAL) ==========\n\n")

missing_summary <- data.frame(
  Variable = names(orders),
  Missing_Count = colSums(is.na(orders)),
  Missing_Percentage = round(colSums(is.na(orders)) / nrow(orders) * 100, 2)
)

print(missing_summary)
cat("\nTotal missing values:", sum(is.na(orders)), "\n\n")

# 2. REMOVE ROWS WITH ANY MISSING VALUES

cat("========== REMOVING ROWS WITH MISSING VALUES ==========\n\n")

orders_clean <- na.omit(orders)   # drop all rows containing at least one NA

cat("Rows before cleaning: ", nrow(orders), "\n")
cat("Rows after cleaning:  ", nrow(orders_clean), "\n")
cat("Total missing values after cleaning:",
    sum(is.na(orders_clean)), "\n\n")

# 3. VALIDATION SUMMARY (CLEAN DATA)

cat("========== VALIDATION CHECK (CLEAN DATA) ==========\n\n")
order <- orders_clean

cat("Final Dataset Summary:\n")
cat("  Total rows: ", nrow(order), "\n")
cat("  Total columns: ", ncol(order), "\n")
cat("  Missing values: ", sum(is.na(order)), "\n\n")

cat("Revenue Statistics (clean data):\n")
cat("  Mean:   £", round(mean(order$revenue), 2), "\n")
cat("  Median: £", round(median(order$revenue), 2), "\n")
cat("  SD:     £", round(sd(order$revenue), 2), "\n\n")

cat("✓ Dataset with complete cases ready for modelling\n\n")

# 4. SAVE CLEANED DATA
write.csv(order, "orders_cleaned_complete_cases.csv", row.names = FALSE)
cat("✓ Saved: orders_cleaned_complete_cases.csv\n")

## ============================================
## PHASE 4: MODELLING + EVALUATION GRAPHS
## ============================================

# 1. Use cleaned data
orders_clean <- read.csv("orders_cleaned.csv", header = TRUE)
orders_clean$ad_channel <- as.factor(orders_clean$ad_channel)
orders_clean$voucher    <- as.factor(orders_clean$voucher)

## 2. Build full model
model_full <- lm(revenue ~ ad_channel + number_past_order +
                   past_spend + time_web + voucher,
                 data = orders_clean)
summary(model_full)

## 3. Stepwise selection (optional but you already used it)
model_step <- step(model_full, direction = "backward", trace = 0)
summary(model_step)

# Use stepwise model as final
final_model <- model_step

## 4. Predictions on training data
orders_clean$pred_revenue <- predict(final_model, newdata = orders_clean)
orders_clean$residuals    <- orders_clean$revenue - orders_clean$pred_revenue

## 5. Key metrics for report
r2   <- summary(final_model)$r.squared
adjr2<- summary(final_model)$adj.r.squared
rmse <- sqrt(mean(orders_clean$residuals^2))
mae  <- mean(abs(orders_clean$residuals))

cat("R² =", round(r2, 4), "\n")
cat("Adjusted R² =", round(adjr2, 4), "\n")
cat("RMSE = £", round(rmse, 2), "\n")
cat("MAE  = £", round(mae, 2), "\n")

## 6. Graph 1: Actual vs Predicted Revenue
graphics.off()
par(family = "sans", col.axis = "grey30", col.lab = "grey30",
    col.main = "grey10", cex.main = 1.1)

plot(orders_clean$revenue, orders_clean$pred_revenue,
     main = "Figure X: Actual vs Predicted Order Revenue",
     xlab = "Actual Revenue (£)",
     ylab = "Predicted Revenue (£)",
     pch  = 16,
     col  = rgb(0, 0, 1, 0.25))
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)
legend("topleft",
       legend = c("Orders", "Perfect prediction line"),
       pch = c(16, NA),
       lty = c(NA, 2),
       col = c(rgb(0,0,1,0.6), "red"),
       bty = "n", cex = 0.9)

## 7. Graph 2: Residuals vs Fitted
par(family = "sans", col.axis = "grey30", col.lab = "grey30",
    col.main = "grey10", cex.main = 1.1)
plot(orders_clean$pred_revenue, orders_clean$residuals,
     main = "Figure Y: Residuals vs Fitted Values",
     xlab = "Predicted Revenue (£)",
     ylab = "Residuals (£)",
     pch  = 16,
     col  = rgb(0, 0.5, 0, 0.25))
abline(h = 0, col = "red", lwd = 2, lty = 2)

## 8. Graph 3: Histogram of Residuals
par(family = "sans", col.axis = "grey30", col.lab = "grey30",
    col.main = "grey10", cex.main = 1.1)
hist(orders_clean$residuals,
     main = "Figure Z: Distribution of Model Residuals",
     xlab = "Residuals (£)",
     ylab = "Number of Orders",
     col  = "#6BAED6",
     border = "white",
     breaks = 40)
abline(v = 0, col = "red", lwd = 2, lty = 2)



