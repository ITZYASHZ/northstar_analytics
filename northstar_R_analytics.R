# NORTHSTAR URBAN MOBILITY - R Analytics
# Part 2: Statistical Analysis & Visualisation


library(ggplot2)
library(dplyr)
library(corrplot)

# Load datasets (working directory already set)
orders      <- read.csv("orders.csv",      stringsAsFactors=FALSE)
deliveries  <- read.csv("deliveries.csv",  stringsAsFactors=FALSE)
customers   <- read.csv("customers.csv",   stringsAsFactors=FALSE)
drivers     <- read.csv("drivers.csv",     stringsAsFactors=FALSE)
vehicles    <- read.csv("vehicles.csv",    stringsAsFactors=FALSE)
incidents   <- read.csv("incidents.csv",   stringsAsFactors=FALSE)
complaints  <- read.csv("complaints.csv",  stringsAsFactors=FALSE)
hubs        <- read.csv("hubs.csv",        stringsAsFactors=FALSE)

cat("All files loaded!\n")



# ANALYSIS 1: Summary Statistics Overview

cat("\n=== DELIVERY STATUS BREAKDOWN ===\n")
print(table(deliveries$delivery_status))

cat("\n=== ORDER SERVICE TYPE BREAKDOWN ===\n")
print(table(orders$service_type))

cat("\n=== COMPLAINT SEVERITY BREAKDOWN ===\n")
print(table(complaints$severity))

cat("\n=== VEHICLE MAINTENANCE STATUS ===\n")
print(table(vehicles$maintenance_status))

cat("\n=== DELIVERY SUMMARY STATS ===\n")
summary(deliveries[, c("route_distance_km", "fuel_or_charge_cost", 
                       "customer_rating_post_delivery", 
                       "manual_route_override_count")])



# ANALYSIS 2: Delivery Status Distribution

status_counts <- deliveries %>%
  group_by(delivery_status) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count / sum(count) * 100, 1))

ggplot(status_counts, aes(x="", y=count, fill=delivery_status)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y") +
  geom_text(aes(label=paste0(pct, "%")), 
            position=position_stack(vjust=0.5), size=4) +
  labs(
    title="Delivery Status Distribution",
    subtitle="NorthStar Operations Overview",
    fill="Status"
  ) +
  theme_void() +
  theme(plot.title=element_text(face="bold", hjust=0.5))



# ANALYSIS 3: Driver Rating vs Delivery Success

driver_perf <- deliveries %>%
  left_join(drivers, by="driver_id") %>%
  group_by(driver_id, driver_rating, employment_type) %>%
  summarise(
    total_jobs       = n(),
    success_rate     = round(mean(delivery_status == "Delivered") * 100, 1),
    avg_cust_rating  = round(mean(customer_rating_post_delivery, na.rm=TRUE), 2),
    .groups="drop"
  )

ggplot(driver_perf, aes(x=driver_rating, y=success_rate, 
                        color=employment_type)) +
  geom_point(aes(size=total_jobs), alpha=0.7) +
  geom_smooth(method="lm", se=TRUE, linetype="dashed") +
  labs(
    title="Driver Rating vs Delivery Success Rate",
    subtitle="Bubble size = number of jobs completed",
    x="Driver Rating (Internal)", y="Success Rate (%)",
    color="Employment Type", size="Total Jobs"
  ) +
  theme_minimal() +
  theme(plot.title=element_text(face="bold"))



# ANALYSIS 4: Complaint Severity by Zone

complaints_zone <- complaints %>%
  left_join(customers, by="customer_id") %>%
  group_by(home_zone, severity) %>%
  summarise(count=n(), .groups="drop")

ggplot(complaints_zone, aes(x=home_zone, y=count, fill=severity)) +
  geom_bar(stat="identity", position="dodge") +
  labs(
    title="Complaint Severity by Customer Zone",
    subtitle="Identifies which zones have the most serious service failures",
    x="Zone", y="Number of Complaints", fill="Severity"
  ) +
  theme_minimal() +
  theme(plot.title=element_text(face="bold"),
        axis.text.x=element_text(angle=30, hjust=1))




# ANALYSIS 5: Fleet Battery Health Distribution

ggplot(vehicles, aes(x=battery_health_pct, fill=vehicle_type)) +
  geom_histogram(binwidth=5, color="white", alpha=0.8) +
  geom_vline(xintercept=60, color="red", linetype="dashed", size=1) +
  annotate("text", x=58, y=Inf, label="Risk threshold", 
           vjust=2, hjust=1.1, color="red", size=3.5) +
  labs(
    title="Fleet Battery Health Distribution",
    subtitle="Vehicles below 60% are at operational risk",
    x="Battery Health (%)", y="Number of Vehicles", fill="Vehicle Type"
  ) +
  theme_minimal() +
  theme(plot.title=element_text(face="bold"))




# ANALYSIS 6: Correlation Between Key Metrics

corr_data <- deliveries %>%
  select(route_distance_km, fuel_or_charge_cost, 
         manual_route_override_count, 
         customer_rating_post_delivery) %>%
  filter(complete.cases(.))

corr_matrix <- cor(corr_data)

corrplot(corr_matrix, 
         method="color",
         type="upper",
         addCoef.col="black",
         tl.col="black",
         tl.srt=45,
         title="Correlation: Key Delivery Metrics",
         mar=c(0,0,2,0))




# ANALYSIS 7: Incident Patterns

incident_summary <- incidents %>%
  left_join(deliveries, by="delivery_id") %>%
  left_join(orders, by="order_id") %>%
  group_by(incident_type, pickup_zone) %>%
  summarise(count=n(), .groups="drop")

ggplot(incident_summary, aes(x=pickup_zone, y=count, fill=incident_type)) +
  geom_bar(stat="identity") +
  labs(
    title="Incident Types by Zone",
    subtitle="Reveals which zones have the highest operational risk",
    x="Zone", y="Number of Incidents", fill="Incident Type"
  ) +
  theme_minimal() +
  theme(plot.title=element_text(face="bold"),
        axis.text.x=element_text(angle=30, hjust=1))




# ANALYSIS 8: Order Value by Service Type

orders_joined <- orders %>%
  left_join(deliveries, by="order_id") %>%
  filter(!is.na(order_value) & !is.na(delivery_status))

ggplot(orders_joined, aes(x=service_type, y=order_value, fill=delivery_status)) +
  geom_boxplot(alpha=0.8) +
  labs(
    title="Order Value by Service Type and Delivery Status",
    subtitle="Do failed deliveries cluster around certain order values?",
    x="Service Type", y="Order Value (£)", fill="Delivery Status"
  ) +
  theme_minimal() +
  theme(plot.title=element_text(face="bold"),
        axis.text.x=element_text(angle=30, hjust=1))