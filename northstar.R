# NORTHSTAR URBAN MOBILITY - SQL in R Analysis
# Part 1: SQL Queries using sqldf
library(sqldf)
library(ggplot2)
library(dplyr)

# Load all datasets
orders      <- read.csv("orders.csv",      stringsAsFactors=FALSE)
deliveries  <- read.csv("deliveries.csv",  stringsAsFactors=FALSE)
customers   <- read.csv("customers.csv",   stringsAsFactors=FALSE)
drivers     <- read.csv("drivers.csv",     stringsAsFactors=FALSE)
vehicles    <- read.csv("vehicles.csv",    stringsAsFactors=FALSE)
incidents   <- read.csv("incidents.csv",   stringsAsFactors=FALSE)
complaints  <- read.csv("complaints.csv",  stringsAsFactors=FALSE)
hubs        <- read.csv("hubs.csv",        stringsAsFactors=FALSE)
app_events  <- read.csv("app_events.csv",  stringsAsFactors=FALSE)

cat("=== Data Loaded Successfully ===\n")
cat("Orders:", nrow(orders), "\n")
cat("Deliveries:", nrow(deliveries), "\n")
cat("Customers:", nrow(customers), "\n")
cat("Drivers:", nrow(drivers), "\n")
cat("Vehicles:", nrow(vehicles), "\n")
cat("Incidents:", nrow(incidents), "\n")
cat("Complaints:", nrow(complaints), "\n")
cat("Hubs:", nrow(hubs), "\n")
cat("App Events:", nrow(app_events), "\n")



# SQL QUERY 1: Delivery Failure Rate by Zone
# Which zones are performing worst?


q1_zone_failures <- sqldf("
  SELECT 
    o.pickup_zone,
    COUNT(*) AS total_deliveries,
    SUM(CASE WHEN d.delivery_status = 'Failed' THEN 1 ELSE 0 END) AS failed_count,
    ROUND(
      SUM(CASE WHEN d.delivery_status = 'Failed' THEN 1 ELSE 0 END) * 100.0 / COUNT(*), 2
    ) AS failure_rate_pct
  FROM deliveries d
  JOIN orders o ON d.order_id = o.order_id
  GROUP BY o.pickup_zone
  ORDER BY failure_rate_pct DESC
")

cat("\n=== QUERY 1: Delivery Failure Rate by Zone ===\n")
print(q1_zone_failures)

# Chart
ggplot(q1_zone_failures, aes(x=reorder(pickup_zone, -failure_rate_pct),
                             y=failure_rate_pct, fill=pickup_zone)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(failure_rate_pct, "%")), vjust=-0.5, size=3.5) +
  labs(
    title="Delivery Failure Rate by Pickup Zone",
    subtitle="NorthStar Operations Analysis",
    x="Zone", y="Failure Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position="none",
        axis.text.x=element_text(angle=30, hjust=1),
        plot.title = lement_text(face="bold"))




# SQL QUERY 2: Driver Route Override Analysis
# Are certain drivers avoiding targets?


q2_driver_overrides <- sqldf("
  SELECT 
    d.driver_id,
    dr.base_zone,
    dr.driver_rating,
    dr.employment_type,
    COUNT(*) AS total_jobs,
    SUM(d.manual_route_override_count) AS total_overrides,
    ROUND(AVG(d.customer_rating_post_delivery), 2) AS avg_customer_rating,
    SUM(CASE WHEN d.delivery_status = 'Failed' THEN 1 ELSE 0 END) AS failures
  FROM deliveries d
  JOIN drivers dr ON d.driver_id = dr.driver_id
  GROUP BY d.driver_id, dr.base_zone, dr.driver_rating, dr.employment_type
  HAVING total_jobs >= 3
  ORDER BY total_overrides DESC
  LIMIT 15
")

cat("\n=== QUERY 2: Top 15 Drivers by Manual Route Overrides ===\n")
print(q2_driver_overrides)

# Chart
ggplot(q2_driver_overrides, aes(x=reorder(driver_id, -total_overrides),
                                y=total_overrides, fill=base_zone)) +
  geom_bar(stat="identity") +
  labs(
    title="Top 15 Drivers by Manual Route Override Count",
    subtitle="High overrides may indicate route planning issues or target avoidance",
    x="Driver ID", y="Total Overrides", fill="Base Zone"
  ) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1),
        plot.title=element_text(face="bold"))





# SQL QUERY 2: Driver Route Override Analysis
# Are certain drivers avoiding targets?

q2_driver_overrides <- sqldf("
  SELECT 
    d.driver_id,
    dr.base_zone,
    dr.driver_rating,
    dr.employment_type,
    COUNT(*) AS total_jobs,
    SUM(d.manual_route_override_count) AS total_overrides,
    ROUND(AVG(d.customer_rating_post_delivery), 2) AS avg_customer_rating,
    SUM(CASE WHEN d.delivery_status = 'Failed' THEN 1 ELSE 0 END) AS failures
  FROM deliveries d
  JOIN drivers dr ON d.driver_id = dr.driver_id
  GROUP BY d.driver_id, dr.base_zone, dr.driver_rating, dr.employment_type
  HAVING total_jobs >= 3
  ORDER BY total_overrides DESC
  LIMIT 15
")

cat("\n=== QUERY 2: Top 15 Drivers by Manual Route Overrides ===\n")
print(q2_driver_overrides)

# Chart
ggplot(q2_driver_overrides, aes(x=reorder(driver_id, -total_overrides),
                                y=total_overrides, fill=base_zone)) +
  geom_bar(stat="identity") +
  labs(
    title="Top 15 Drivers by Manual Route Override Count",
    subtitle="High overrides may indicate route planning issues or target avoidance",
    x="Driver ID", y="Total Overrides", fill="Base Zone"
  ) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1),
        plot.title=element_text(face="bold"))




# SQL QUERY 3: Hub Performance Comparison
# Which hubs are most inefficient?

q3_hub_performance <- sqldf("
  SELECT 
    h.hub_name,
    h.zone,
    h.hub_type,
    COUNT(*) AS deliveries_handled,
    ROUND(AVG(d.fuel_or_charge_cost), 2) AS avg_cost,
    SUM(CASE WHEN d.delivery_status = 'Failed' THEN 1 ELSE 0 END) AS total_failures,
    ROUND(
      SUM(CASE WHEN d.delivery_status = 'Failed' THEN 1 ELSE 0 END) * 100.0 / COUNT(*), 2
    ) AS failure_rate_pct,
    ROUND(AVG(d.route_distance_km), 2) AS avg_distance_km
  FROM deliveries d
  JOIN hubs h ON d.hub_id = h.hub_id
  GROUP BY h.hub_name, h.zone, h.hub_type
  ORDER BY failure_rate_pct DESC
")

cat("\n=== QUERY 3: Hub Performance Summary ===\n")
print(q3_hub_performance)

# Chart - failures vs cost per hub
ggplot(q3_hub_performance, aes(x=avg_cost, y=failure_rate_pct, 
                               label=hub_name, color=zone)) +
  geom_point(size=4) +
  geom_text(vjust=-0.8, size=3.2) +
  labs(
    title="Hub Performance: Cost vs Failure Rate",
    subtitle="Top-right quadrant = high cost AND high failure (priority for review)",
    x="Average Cost per Delivery (£)", y="Failure Rate (%)", color="Zone"
  ) +
  theme_minimal() +
  theme(plot.title=element_text(face="bold"))



# SQL QUERY 4: Complaints by Customer Segment
# Who is complaining most and costing most in compensation?


q4_complaints <- sqldf("
  SELECT 
    c.customer_type,
    c.home_zone,
    COUNT(DISTINCT c.customer_id) AS num_customers,
    COUNT(comp.complaint_id) AS total_complaints,
    ROUND(AVG(comp.resolution_days), 2) AS avg_resolution_days,
    ROUND(SUM(comp.compensation_amount), 2) AS total_compensation,
    ROUND(AVG(c.loyalty_score), 2) AS avg_loyalty_score
  FROM customers c
  LEFT JOIN complaints comp ON c.customer_id = comp.customer_id
  GROUP BY c.customer_type, c.home_zone
  ORDER BY total_complaints DESC
  LIMIT 12
")

cat("\n=== QUERY 4: Complaints by Customer Segment ===\n")
print(q4_complaints)

# Chart
ggplot(q4_complaints, aes(x=reorder(paste(customer_type, home_zone, sep="\n"), 
                                    -total_complaints),
                          y=total_complaints, fill=customer_type)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0("£", total_compensation)), 
            vjust=-0.5, size=3) +
  labs(
    title="Total Complaints by Customer Type and Zone",
    subtitle="Numbers above bars = total compensation paid (£)",
    x="Customer Segment", y="Number of Complaints", fill="Type"
  ) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=30, hjust=1),
        plot.title=element_text(face="bold"))



# SQL QUERY 5: Vehicle Maintenance Risk
# Which vehicles are likely to cause future incidents?

q5_vehicle_risk <- sqldf("
  SELECT 
    v.vehicle_id,
    v.vehicle_type,
    v.assigned_zone,
    v.battery_health_pct,
    v.odometer_km,
    v.maintenance_status,
    COUNT(DISTINCT d.delivery_id) AS total_deliveries,
    COUNT(i.incident_id) AS incident_count
  FROM vehicles v
  LEFT JOIN deliveries d ON v.vehicle_id = d.vehicle_id
  LEFT JOIN incidents i ON d.delivery_id = i.delivery_id
  GROUP BY v.vehicle_id, v.vehicle_type, v.assigned_zone,
           v.battery_health_pct, v.odometer_km, v.maintenance_status
  HAVING v.battery_health_pct < 65 OR v.maintenance_status != 'OK'
  ORDER BY incident_count DESC, v.battery_health_pct ASC
  LIMIT 15
")

cat("\n=== QUERY 5: High-Risk Vehicles ===\n")
print(q5_vehicle_risk)

# Chart
ggplot(q5_vehicle_risk, aes(x=battery_health_pct, y=incident_count,
                            color=maintenance_status, shape=vehicle_type)) +
  geom_point(size=4, alpha=0.8) +
  geom_vline(xintercept=60, linetype="dashed", color="red", alpha=0.6) +
  labs(
    title="Vehicle Risk: Battery Health vs Incident Count",
    subtitle="Red dashed line = critical battery threshold (60%)",
    x="Battery Health (%)", y="Number of Incidents",
    color="Maintenance Status", shape="Vehicle Type"
  ) +
  theme_minimal() +
  theme(plot.title=element_text(face="bold"))