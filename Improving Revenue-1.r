# =========================================================
# Part 1: Business Analytics: Improving Revenue 
#STUDENT ID : 202003543
# =========================================================
setwd("order_july25.csv")


# =========================================================
# STEP 1: DATA UNDERSTANDING & VISUALS
# =========================================================

# Load raw data
orders <- read.csv("order_july25.csv", header = TRUE)

cat("=== BASIC STRUCTURE ===\n")
dim(orders)
colnames(orders)
str(orders)
summary(orders)

# Convert categorical variables
orders$ad_channel <- as.factor(orders$ad_channel)
orders$voucher    <- as.factor(orders$voucher)

cat("\n=== STRUCTURE AFTER FACTOR CONVERSION ===\n")
str(orders)

# Descriptive statistics
cat("\n=== NUMERIC DESCRIPTIVES ===\n")

cat("\n-- REVENUE --\n")
cat("Mean:",   mean(orders$revenue, na.rm = TRUE), "\n")
cat("Median:", median(orders$revenue, na.rm = TRUE), "\n")
cat("SD:",     sd(orders$revenue, na.rm = TRUE), "\n")

cat("\n-- PAST SPEND --\n")
cat("Mean:",   mean(orders$past_spend, na.rm = TRUE), "\n")
cat("Median:", median(orders$past_spend, na.rm = TRUE), "\n")

cat("\n-- TIME ON WEBSITE --\n")
cat("Mean:",   mean(orders$time_web, na.rm = TRUE), "\n")
cat("Median:", median(orders$time_web, na.rm = TRUE), "\n")

cat("\n=== CATEGORICAL FREQUENCIES ===\n")
cat("\n-- AD CHANNEL --\n")
print(table(orders$ad_channel))
print(round(100 * prop.table(table(orders$ad_channel)), 2))

cat("\n-- VOUCHER --\n")
print(table(orders$voucher))
print(round(100 * prop.table(table(orders$voucher)), 2))

# Missing data check
cat("\n=== MISSING DATA SUMMARY ===\n")
cat("Complete rows:",   sum(complete.cases(orders)), "\n")
cat("Incomplete rows:", sum(!complete.cases(orders)), "\n")
miss_by_var <- colSums(is.na(orders))
print(miss_by_var)
cat("\nMissing percentage by variable (%):\n")
print(round(miss_by_var / nrow(orders) * 100, 3))

# Set purple color palette with different shades
colors <- list(
  purple_dark = "#7B68BE",
  purple_main = "#9B7EBD",
  purple_light = "#B89FD8",
  purple_lighter = "#D4C4E8",
  purple_accent = "#8E7CC3"
)

# Recode variables
orders$ad_channel_label <- factor(orders$ad_channel,
                                  levels = 1:4,
                                  labels = c("No Advertisement",
                                             "Paid Search",
                                             "SEO (Organic)",
                                             "Online Display Ads"))

orders$voucher_label <- factor(orders$voucher,
                               levels = c(0, 1),
                               labels = c("No Voucher", "Voucher Applied"))

# ============================================================================
# Figure 1: Revenue Distribution (Matching Reference Style)
# ============================================================================
par(mar = c(5, 5, 4, 2), family = "sans", bg = "white")

# Create histogram with cleaner appearance
hist(orders$revenue,
     main = "Distribution of order-level revenue",
     xlab = "Revenue per Order (£)",
     ylab = "Number of Orders",
     col = colors$purple_light,
     border = colors$purple_main,
     breaks = 30,
     las = 1,
     cex.lab = 1.1,
     cex.axis = 1.0,
     cex.main = 1.3,
     col.main = "#2C3E50",
     xlim = c(0, max(orders$revenue, na.rm = TRUE)))

# Add grid lines for readability (subtle)
grid(nx = NULL, ny = NULL, col = "gray95", lty = 1)

# Redraw histogram on top of grid
hist(orders$revenue,
     col = colors$purple_light,
     border = colors$purple_main,
     breaks = 30,
     add = TRUE)

# ---- ADDED: mean & median lines + legend ----
rev_mean   <- mean(orders$revenue, na.rm = TRUE)
rev_median <- median(orders$revenue, na.rm = TRUE)

abline(v = rev_mean,   col = "red",   lwd = 3, lty = 2)
abline(v = rev_median, col = "darkgreen", lwd = 3, lty = 2)

legend("topright",
       legend = c(
         paste0("Average order value: £", round(rev_mean, 2)),
         paste0("Typical order: £", round(rev_median, 2))
       ),
       col = c("red", "darkgreen"),
       lty = 2,
       lwd = 3,
       bty = "n")


# ============================================================================
# Figure 2: Average Revenue by Advertisement Channel
# ============================================================================
# Assuming orders$ad_channel_label already exists as in your code

# Counts by advertising channel
ad_counts <- table(orders$ad_channel_label)

# Percentages for labels
ad_pct    <- round(100 * ad_counts / sum(ad_counts), 1)
pie_labels <- paste0(names(ad_counts), "\n", ad_pct, "%")

par(mar = c(5, 5, 4, 5), family = "sans", bg = "white")

# Pie chart
pie(ad_counts,
    labels = pie_labels,
    col = c(colors$purple_dark,
            colors$purple_main,
            colors$purple_light,
            colors$purple_lighter),
    border = "white",
    main = "Figure 2: Distribution of Advertising Channels")


# ============================================================================
# Figure 3: Average Revenue by Voucher
# ============================================================================
# counts
voucher_counts <- table(orders$voucher_label)
total_n <- sum(voucher_counts)
pct <- round(100 * voucher_counts / total_n, 1)

par(mar = c(5, 5, 4, 2), family = "sans", bg = "white")

bp <- barplot(voucher_counts,
              col = c("#B89FD8", "#D4C4E8"),
              border = NA,
              ylim = c(0, max(voucher_counts) * 1.25),
              ylab = "Number of Orders",
              xlab = "Voucher Status",
              main = "Figure 3: Distribution of Discount Voucher Usage",
              las = 1,
              cex.main = 1.3)

# count labels on top
text(x = bp,
     y = voucher_counts + max(voucher_counts) * 0.03,
     labels = format(voucher_counts, big.mark = ","),
     cex = 0.9)

# percentage labels inside bars
text(x = bp,
     y = voucher_counts * 0.5,
     labels = paste0(pct, "%"),
     col = "white",
     font = 2,
     cex = 1.2)
# ============================================================
# Figure 4: Distribution of Number of Past Orders
# ============================================================

par(family = "sans", col.axis = "grey30", col.lab = "grey30", 
    col.main = "grey10", cex.main = 1.2)

hist(orders$past_spend,
     main = "Figure 4: Distribution of Historical Customer Spending",
     xlab = "Past Spend (£ GBP)",
     ylab = "Number of Orders",
     col = "#B89FD8",
     border = "white",
     breaks = 30,
     las = 1)

abline(v = median(orders$past_spend, na.rm = TRUE), col = "#D62728", lwd = 2.5, lty = 2)

legend("topright",
       legend = paste0("Typical Customer: £", round(median(orders$past_spend, na.rm = TRUE), 2)),
       col = "#D62728", lty = 2, lwd = 2.5, bty = "n")

cat("✓ Figure 2\n")




# =========================================================
# STEP 2: DATA PREPARATION (CLEANING)
# =========================================================

cat("Rows before cleaning:", nrow(orders), "\n")

# Listwise deletion
orders_clean <- na.omit(orders)

cat("Rows after cleaning:", nrow(orders_clean), "\n")
cat("Total missing values after cleaning:", sum(is.na(orders_clean)), "\n")

summary(orders_clean)

# Save cleaned data
write.csv(orders_clean, "orders_cleaned.csv", row.names = FALSE)
cat("\nSaved cleaned training data as 'orders_cleaned.csv'\n")

# ================================
# Figure 5: Average Revenue vs Input factors
# ================================

# 1) Prepare summary values you can explain in business terms
avg_rev <- c(
  mean(orders_clean$revenue),                                   # overall revenue
  tapply(orders_clean$revenue, orders_clean$ad_channel_label, mean)[1],  # ad_channel example (No Ad)
  mean(orders_clean$revenue[orders_clean$number_past_order == 0]),
  mean(orders_clean$revenue[orders_clean$past_spend > 0]),
  mean(orders_clean$revenue[orders_clean$time_web > 120]),
  mean(orders_clean$revenue[orders_clean$voucher_label == "Voucher Applied"])
)

factor_names <- c("All Orders",
                  "No Advertisement",
                  "0 Past Orders",
                  "Has Past Spend",
                  ">120s on Website",
                  "Voucher Applied")

par(mar = c(9, 5, 4, 2), family = "sans", bg = "white")

x_vals <- 1:length(factor_names)

plot(x_vals, avg_rev,
     type = "n",
     xaxt = "n",
     xlab = "",
     ylab = "Average Revenue (£)",
     main = "Figure 5: How Customer Factors Relate to Average Revenue",
     ylim = c(min(avg_rev) * 0.9, max(avg_rev) * 1.1),
     cex.lab = 1.1,
     cex.axis = 1.0,
     cex.main = 1.3,
     col.main = "#2C3E50")

grid(nx = NA, ny = NULL, col = "gray95", lty = 1)

lines(x_vals, avg_rev,
      col = colors$purple_main,
      lwd = 2.5)
points(x_vals, avg_rev,
       pch = 16,
       cex = 1.4,
       col = colors$purple_dark)

# Value labels
text(x_vals,
     avg_rev + 1,
     labels = paste0("£", round(avg_rev, 1)),
     cex = 0.9,
     col = "#2C3E50")

axis(side = 1,
     at = x_vals,
     labels = FALSE)

text(x_vals,
     par("usr")[3] - 1,
     labels = factor_names,
     srt = 35,
     adj = 1,
     xpd = TRUE,
     cex = 0.9)

mtext("Customer & Marketing Factors", side = 1, line = 7, cex = 1.1, col = "#2C3E50")

legend("topleft",
       legend = "Line shows how average order revenue changes across factors",
       lty = 1,
       lwd = 2.5,
       col = colors$purple_main,
       bty = "n")

# =========================================================
# STEP 3: MODELLING & EVALUATION
# =========================================================

orders_clean <- read.csv("orders_cleaned.csv", header = TRUE)
orders_clean$ad_channel <- as.factor(orders_clean$ad_channel)
orders_clean$voucher    <- as.factor(orders_clean$voucher)

str(orders_clean)

# 3.1 Fit regression model
model_full <- lm(revenue ~ ad_channel +
                   number_past_order +
                   past_spend +
                   time_web +
                   voucher,
                 data = orders_clean)

summary(model_full)

# 3.2 Evaluation
orders_clean$pred_revenue <- predict(model_full, newdata = orders_clean)
orders_clean$residuals    <- orders_clean$revenue - orders_clean$pred_revenue

r2    <- summary(model_full)$r.squared
adjr2 <- summary(model_full)$adj.r.squared
rmse  <- sqrt(mean(orders_clean$residuals^2))
mae   <- mean(abs(orders_clean$residuals))

cat("R² =", round(r2, 4), "\n")
cat("Adjusted R² =", round(adjr2, 4), "\n")
cat("RMSE =", round(rmse, 2), "\n")
cat("MAE =", round(mae, 2), "\n")

# Set purple colours
col_point <- colors$purple_main
col_line  <- colors$purple_dark

# 1) Actual vs Predicted Order Revenue (purple)

plot(orders_clean$revenue, orders_clean$pred_revenue,
     main = "Actual vs Predicted Order Revenue",
     xlab = "Actual revenue (£)",
     ylab = "Predicted revenue (£)",
     pch  = 16,
     col  = adjustcolor(col_point, alpha.f = 0.25),
     cex  = 0.7)

abline(a = 0, b = 1,
       col = col_line,
       lwd = 2,
       lty  = 2)

legend("topleft",
       legend = c("Orders", "Perfect prediction (y = x)"),
       pch    = c(16, NA),
       lty    = c(NA, 2),
       lwd    = c(NA, 2),
       col    = c(adjustcolor(col_point, alpha.f = 0.8), col_line),
       bty    = "n")


# 2) Residuals vs Fitted Values (purple)

plot(orders_clean$pred_revenue, orders_clean$residuals,
     main = "Residuals vs Fitted Values",
     xlab = "Predicted revenue (£)",
     ylab = "Residuals (£)",
     pch  = 16,
     col  = adjustcolor(col_point, alpha.f = 0.25),
     cex  = 0.7)

abline(h = 0,
       col = col_line,
       lwd = 2,
       lty  = 2)

legend("topleft",
       legend = c("Residuals", "Zero‑residual reference"),
       pch    = c(16, NA),
       lty    = c(NA, 2),
       lwd    = c(NA, 2),
       col    = c(adjustcolor(colors$purple_main, alpha.f = 0.8),
                  colors$purple_dark),
       bty    = "n")


# =========================================================
# STEP 4: PREDICTIONS FOR NEW CUSTOMERS (20)
# =========================================================

newcust <- read.csv("new_customer25.csv", header = TRUE)
newcust$ad_channel <- as.factor(newcust$ad_channel)
newcust$voucher    <- as.factor(newcust$voucher)

newcust$prediction <- predict(model_full, newdata = newcust)

prediction_table <- data.frame(
  order      = newcust$order,
  prediction = round(newcust$prediction, 2)
)

prediction_table

write.csv(prediction_table, "prediction_table_20_orders.csv", row.names = FALSE)

