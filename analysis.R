
# ==========================================
# PROJECT: ONLINE SHOPPING BEHAVIOR
# AND CONSUMER PREFERENCES
# STEP 3: DATA ANALYSIS
# ==========================================

# Load dataset
shopping_data <- read.csv(
  "online_shopping_data.csv",
  stringsAsFactors = FALSE
)

# Load required package
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

library(dplyr)

# ==========================================
# 1. GENERAL INFORMATION
# ==========================================

cat("\n===== DATASET INFORMATION =====\n")

cat("Total Customers:",
    n_distinct(shopping_data$CustomerID), "\n")

cat("Total Transactions:",
    nrow(shopping_data), "\n")

cat("Total Sales:",
    sum(shopping_data$PurchaseAmount), "\n")

cat("Average Purchase Amount:",
    mean(shopping_data$PurchaseAmount), "\n")

cat("Average Customer Rating:",
    mean(shopping_data$CustomerRating), "\n")


# ==========================================
# 2. SALES BY PRODUCT CATEGORY
# ==========================================

category_summary <- shopping_data %>%
  group_by(ProductCategory) %>%
  summarise(
    Total_Sales = sum(PurchaseAmount),
    Total_Items = sum(ItemsPurchased),
    Transactions = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Sales))

cat("\n===== SALES BY CATEGORY =====\n")
print(category_summary)


# ==========================================
# 3. MONTHLY SHOPPING TRENDS
# ==========================================

shopping_data$PurchaseMonth <- factor(
  shopping_data$PurchaseMonth,
  levels = month.name
)

monthly_summary <- shopping_data %>%
  group_by(PurchaseMonth) %>%
  summarise(
    Total_Sales = sum(PurchaseAmount),
    Transactions = n(),
    .groups = "drop"
  ) %>%
  arrange(PurchaseMonth)

cat("\n===== MONTHLY SHOPPING TRENDS =====\n")
print(monthly_summary)


# ==========================================
# 4. PAYMENT METHOD PREFERENCES
# ==========================================

payment_summary <- shopping_data %>%
  group_by(PaymentMethod) %>%
  summarise(
    Total_Transactions = n(),
    Total_Sales = sum(PurchaseAmount),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Transactions))

cat("\n===== PAYMENT PREFERENCES =====\n")
print(payment_summary)


# ==========================================
# 5. SHOPPING PLATFORM ANALYSIS
# ==========================================

platform_summary <- shopping_data %>%
  group_by(ShoppingPlatform) %>%
  summarise(
    Total_Transactions = n(),
    Total_Sales = sum(PurchaseAmount),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Transactions))

cat("\n===== SHOPPING PLATFORM =====\n")
print(platform_summary)


# ==========================================
# 6. CUSTOMER SATISFACTION ANALYSIS
# ==========================================

rating_summary <- shopping_data %>%
  group_by(CustomerRating) %>%
  summarise(
    Total_Customers = n(),
    .groups = "drop"
  ) %>%
  arrange(CustomerRating)

cat("\n===== CUSTOMER RATINGS =====\n")
print(rating_summary)


# ==========================================
# 7. CUSTOMER AGE ANALYSIS
# ==========================================

age_summary <- shopping_data %>%
  mutate(
    AgeGroup = case_when(
      Age <= 25 ~ "18-25",
      Age <= 35 ~ "26-35",
      Age <= 45 ~ "36-45",
      TRUE ~ "46-60"
    )
  ) %>%
  group_by(AgeGroup) %>%
  summarise(
    Total_Customers = n(),
    Average_Spending = mean(PurchaseAmount),
    .groups = "drop"
  )

cat("\n===== CUSTOMER AGE ANALYSIS =====\n")
print(age_summary)


# ==========================================
# 8. TOP PRODUCT CATEGORY
# ==========================================

top_category <- category_summary %>%
  slice_max(Total_Sales, n = 1)

cat("\n===== TOP PRODUCT CATEGORY =====\n")
print(top_category)


# ==========================================
# 9. TOP PAYMENT METHOD
# ==========================================

top_payment <- payment_summary %>%
  slice_max(Total_Transactions, n = 1)

cat("\n===== MOST PREFERRED PAYMENT =====\n")
print(top_payment)


# ==========================================
# 10. EXPORT ANALYSIS RESULTS
# ==========================================

dir.create("analysis_output", showWarnings = FALSE)

write.csv(category_summary,
          "analysis_output/category_summary.csv",
          row.names = FALSE)

write.csv(monthly_summary,
          "analysis_output/monthly_summary.csv",
          row.names = FALSE)

write.csv(payment_summary,
          "analysis_output/payment_summary.csv",
          row.names = FALSE)

write.csv(platform_summary,
          "analysis_output/platform_summary.csv",
          row.names = FALSE)

write.csv(rating_summary,
          "analysis_output/rating_summary.csv",
          row.names = FALSE)

write.csv(age_summary,
          "analysis_output/age_summary.csv",
          row.names = FALSE)

cat("\n====================================\n")
cat("ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat("All summary files exported!\n")
cat("====================================\n")

