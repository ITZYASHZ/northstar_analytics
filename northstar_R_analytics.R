# NorthStar Urban Mobility - R Analytics
# Basic statistical analysis and charts

library(ggplot2)
library(dplyr)

setwd("E:/Database/northstar_dataset")

# Load data
orders     <- read.csv("orders.csv")
deliveries <- read.csv("deliveries.csv")
customers  <- read.csv("customers.csv")
drivers    <- read.csv("drivers.csv")
vehicles   <- read.csv("vehicles.csv")
complaints <- read.csv("complaints.csv")

cat("Data loaded!\n")

# ── ANALYSIS 1: Basic summary of deliveries ──
cat("\n=== Delivery Summary ===\n")
summary(deliveries$customer_rating_post_delivery)
summary(deliveries$route_distance_km)

# ── ANALYSIS 2: Delivery status bar chart ──
status_count <- deliveries %>%
  group_by(delivery_status) %>%
  summarise(count = n())

ggplot(status_count, aes(x=delivery_status, y=count, fill=delivery_status)) +
  geom_bar(stat="identity") +
  labs(title="Delivery Status Overview",
       x="Status", y="Count") +
  theme_minimal()

# ── ANALYSIS 3: Complaints by severity ──
severity_count <- complaints %>%
  group_by(severity) %>%
  summarise(count = n())

print(severity_count)

ggplot(severity_count, aes(x=severity, y=count, fill=severity)) +
  geom_bar(stat="identity") +
  labs(title="Complaints by Severity",
       x="Severity", y="Number of Complaints") +
  theme_minimal()

# ── ANALYSIS 4: Vehicle battery health histogram ──
ggplot(vehicles, aes(x=battery_health_pct)) +
  geom_histogram(binwidth=5, fill="steelblue", color="white") +
  geom_vline(xintercept=60, color="red", linetype="dashed") +
  labs(title="Vehicle Battery Health Distribution",
       subtitle="Red line = 60% risk threshold",
       x="Battery Health (%)", y="Number of Vehicles") +
  theme_minimal()

# ── ANALYSIS 5: Orders by service type ──
service_count <- orders %>%
  group_by(service_type) %>%
  summarise(count = n())

ggplot(service_count, aes(x=service_type, y=count, fill=service_type)) +
  geom_bar(stat="identity") +
  labs(title="Orders by Service Type",
       x="Service Type", y="Total Orders") +
  theme_minimal() +
  theme(legend.position="none")

# ── ANALYSIS 6: Driver rating distribution ──
ggplot(drivers, aes(x=driver_rating)) +
  geom_histogram(binwidth=0.5, fill="coral", color="white") +
  labs(title="Driver Rating Distribution",
       x="Driver Rating", y="Number of Drivers") +
  theme_minimal()

cat("\nR Analytics complete!\n")