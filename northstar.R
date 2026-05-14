# NorthStar Urban Mobility - SQL in R
# Using sqldf to run SQL queries on CSV data

library(sqldf)
library(ggplot2)

# Set working directory and load data
setwd("E:/Database/northstar_dataset")

orders     <- read.csv("orders.csv")
deliveries <- read.csv("deliveries.csv")
customers  <- read.csv("customers.csv")
drivers    <- read.csv("drivers.csv")
vehicles   <- read.csv("vehicles.csv")
complaints <- read.csv("complaints.csv")
hubs       <- read.csv("hubs.csv")

cat("Data loaded successfully\n")

# ── QUERY 1: Count deliveries by status ──
q1 <- sqldf("
  SELECT delivery_status, COUNT(*) AS total
  FROM deliveries
  GROUP BY delivery_status
  ORDER BY total DESC
")
print(q1)

# Chart for Query 1
ggplot(q1, aes(x=delivery_status, y=total, fill=delivery_status)) +
  geom_bar(stat="identity") +
  labs(title="Delivery Status Count", x="Status", y="Total") +
  theme_minimal()

# ── QUERY 2: Number of orders per service type ──
q2 <- sqldf("
  SELECT service_type, COUNT(*) AS total_orders
  FROM orders
  GROUP BY service_type
  ORDER BY total_orders DESC
")
print(q2)

# Chart for Query 2
ggplot(q2, aes(x=service_type, y=total_orders, fill=service_type)) +
  geom_bar(stat="identity") +
  labs(title="Orders by Service Type", x="Service Type", y="Total Orders") +
  theme_minimal()

# ── QUERY 3: Join deliveries and orders to see status by zone ──
q3 <- sqldf("
  SELECT o.pickup_zone, d.delivery_status, COUNT(*) AS total
  FROM deliveries d
  JOIN orders o ON d.order_id = o.order_id
  GROUP BY o.pickup_zone, d.delivery_status
  ORDER BY o.pickup_zone
")
print(q3)

# ── QUERY 4: Average customer rating per hub ──
q4 <- sqldf("
  SELECT hub_id, 
         COUNT(*) AS total_deliveries,
         AVG(customer_rating_post_delivery) AS avg_rating
  FROM deliveries
  GROUP BY hub_id
  ORDER BY avg_rating DESC
")
print(q4)

# ── QUERY 5: Complaints count by type ──
q5 <- sqldf("
  SELECT complaint_type, COUNT(*) AS total
  FROM complaints
  GROUP BY complaint_type
  ORDER BY total DESC
")
print(q5)

# Chart for Query 5
ggplot(q5, aes(x=reorder(complaint_type, -total), y=total, fill=complaint_type)) +
  geom_bar(stat="identity") +
  labs(title="Complaints by Type", x="Complaint Type", y="Count") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=30, hjust=1),
        legend.position="none")