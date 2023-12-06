library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)
library(plotly)
library(tidyr)
library(tidyverse)

Superstore <- read.csv("D:/Hard Drive E/Downloads/preprocessed_SampleSuperstore.csv", header = TRUE)

#Total Sales
sales_by_category <- Superstore %>%
  group_by(category) %>%
  summarise(total_sales = sum(sales))

total_sales <- Superstore %>% 
  summarise(total_sales = sum(sales))

formatted_sales <- dollar_format(suffix = "M", big.mark = ",", prefix = "$")(total_sales$total_sales/1000000)

# Average Delivery Days
delivery_days <- Superstore %>%
  mutate(delivery_days = as.numeric(difftime(Ship.Date, Order.Date, units = "days"))) %>%
  summarise(avg_delivery_days = round(mean(delivery_days, na.rm = TRUE)))

# Returned Orders
returned_orders <- Superstore %>%
  filter(profit < 0) %>%
  nrow()

# Profit Ratio
profit_ratio <- Superstore %>%
  group_by(category) %>%
  summarise(sales = sum(sales),
            profit = sum(profit)) %>%
  mutate(total_profit = profit / sales) %>%
  summarise(avg_profit_ratio = round(mean(total_profit, na.rm = TRUE), 2))

profit_ratio_value <- paste0(round(profit_ratio$avg_profit_ratio * 100, 2), "%")

#  Average Order Value
avg_order_value <- Superstore %>%
  group_by(Order.Date) %>%
  summarise(total_sales = sum(sales)) %>%
  summarise(avg_order_value = round(mean(total_sales), 2)) %>%
  pull(avg_order_value)

formatted_avg_order_value <- comma_format()(round(avg_order_value/1000, 1)) %>% paste0("K")

formatted_avg_order_value <- sprintf("%.2f", round(avg_order_value/1000, 2)) %>% paste0("K")

# Average Item Value
aiv <- Superstore %>%
  summarise(aiv = round(sum(sales)/sum(quantity), 2)) %>%
  pull(aiv)

aiv_vb <- valueBox(
  paste0("$", aiv),
  icon = "fa-briefcase"
)

# Total Quantity
total_quantity <- Superstore %>%
  summarise(total_quantity = sum(quantity)) %>%
  pull(total_quantity)

# Profit Per Order
profit_per_order <- Superstore %>%
  group_by(Order.ID) %>%
  summarise(total_profit = sum(profit)) %>%
  summarise(avg_profit_per_order = round(mean(total_profit), 2))

formatted_profit_per_order <- dollar_format(prefix = "$")(pull(profit_per_order, avg_profit_per_order))

# Item Per Customer
items_per_customer <- Superstore %>%
  group_by(Customer.Name) %>%
  summarise(total_items = sum(quantity)) %>%
  summarise(avg_items_per_customer = round(mean(total_items), 2)) %>%
  pull(avg_items_per_customer)

# Profit Per Customer
profit_per_customer <- Superstore %>%
  group_by(Customer.ID) %>%
  summarise(total_sales = sum(sales),
            total_profit = sum(profit)) %>%
  mutate(profit_per_customer = total_profit / n_distinct(Customer.ID)) %>%
  summarise(avg_profit_per_customer = round(mean(profit_per_customer), 2)) %>%
  pull()

# Sales by Category and Sub-Category
df_summary <- Superstore %>%
  group_by(category) %>%
  summarize(Sales = sum(sales)) %>%
  ungroup() %>%
  mutate(Sales_Millions = paste0("$", format(round(Sales/1000000, 2), nsmall = 2), "M"))

# Create sub-category data
subcat_data <- Superstore %>%
  group_by(category, sub_category) %>%
  summarize(Sales = sum(sales)) %>%
  ungroup() %>%
  mutate(percent_sales = Sales/sum(Sales))

# Create donut chart
donut_chart <- plot_ly(df_summary, labels = ~category, values = ~Sales, type = "pie",
                       hole = 0.6, text = ~paste0("$", round(Sales/1000, 2), "K"), textposition = "inside",
                       hovertemplate = paste("Category: %{label}<br>Sales: %{value}<br>"),
                       marker = list(colors = c("#99d8c9", "#fc8d62", "#8da0cb"), line = list(color = "#FFFFFF", width = 1))) %>%
  add_trace(labels = subcat_data$sub_category, values = subcat_data$Sales, text = ~paste0(round(subcat_data$percent_sales*100,2), "%"), 
            domain = list(x = c(0.15, 0.85), y = c(0.15, 0.85)), hovertemplate = paste("Sub-category: %{label}<br>Sales: %{value}<br>Percent of Sub-category Sales: %{text}<br>"), 
            name = NULL) %>%
  layout(title = "Sales by Category and Sub-Category",
         showlegend = TRUE,
         margin = list(l = 10, r = 10, t = 50, b = 10),
         annotations = list(text = "<b>Sales</b>", x = 0.5, y = 0.5, font = list(size = 20), showarrow = FALSE),
         customdata = df_summary[c("category", "Sales")],
         legend = list(x = 1.1, y = 0.5),
         uniformtext = list(minsize = 12, mode = "hide"))

# Chloropleth
df_profit_by_state <- Superstore %>%
  group_by(state) %>%
  summarize(total_profit = sum(profit)) %>%
  na.omit() # remove rows with missing values

state_abbreviations <- data.frame(state.name, state.abb)
colnames(state_abbreviations) <- c("state", "state_code")

df_profit_by_state <- df_profit_by_state %>%
  left_join(state_abbreviations, by = c("state" = "state"))

plot_map <- plot_ly(df_profit_by_state, type = "choropleth", locations = ~state_code, z = ~total_profit,
                    locationmode = "USA-states",
                    colorscale = "Heat",
                    text = ~paste("State: ", state, "<br>Total Profit: $", total_profit)) %>%
  colorbar(title = "Total Profit") %>%
  layout(title = "Total Profit by State",
         geo = list(scope = "usa", projection = list(type = "albers usa")),
         margin = list(l = 10, r = 10, t = 50, b = 10))

# Top 10 Products
# Group the data by Product Name and calculate the total Sales
product_summary <- Superstore %>%
  separate(Product.Name, into = c("Product", "Name"), sep = " ", remove = FALSE) %>%
  group_by(Product, Name) %>%
  summarize(Sales = sum(sales)) %>%
  ungroup() %>%
  arrange(desc(Sales)) %>%
  top_n(10, Sales)

# Create horizontal bar chart
ggplot(product_summary, aes(x = reorder(paste(Product, Name), Sales), y = Sales, fill = Sales)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Top 10 Products by Sales", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_blank(),
        axis.title = element_blank(), plot.title = element_text(size = 18, face = "bold")) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format(scale = 0.001, prefix = "$"), expand = c(0, 0.05)) +
  guides(fill = FALSE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Sales and Profit Correlation
sales_profit_correlation <- plot_ly(
  data = Superstore,
  x = ~sales,
  y = ~profit,
  type = "scatter",
  mode = "markers",
  color = ~category,
  text = ~paste("Category: ", category, "<br>Sub-Category: ", sub_category, "<br>Sales: ", sales, "<br>Profit: ", profit),
  hoverinfo = "text"
) %>%
  layout(
    title = "Sales and Profit Correlation",
    xaxis = list(title = "Sales", showgrid = FALSE),
    yaxis = list(title = "Profit", showgrid = FALSE),
    orientation = "h", y = -0.2, x = 0.2,
    scene = list(xaxis = list(gridcolor = "#f5f5f5"), yaxis = list(gridcolor = "#f5f5f5"))
  )

# Monthly Profitability

superstore <- read.csv("D:/Hard Drive E/Downloads/preprocessed_SampleSuperstore.csv", header = TRUE)

# calculate total profit by month
profit_by_month <- superstore %>%
  mutate(month = as.Date(paste(year(Order.Date), month(Order.Date), "01", sep = "-"))) %>%
  group_by(month) %>%
  summarise(total_profit = sum(profit))

# plot the interactive line chart
monthly_profitability <- plot_ly(profit_by_month, x = ~month, y = ~total_profit, type = "scatter", mode = "lines", 
        hoverinfo = "text", text = ~paste("Total Profit: $", total_profit), line = list(color = "darkgreen")) %>%
  layout(title = list(text = "Monthly Profitability", font = list(size=16, face="bold", family="Comic Sans MS")), 
         xaxis = list(title = list(text = "Month", font = list(size=10)), showgrid = FALSE), 
         yaxis = list(title = list(text = "Total Profit", font = list(size=10)), showgrid = FALSE))


# Top 10 Customers
# Group the data by Customer Name and calculate the total Profit
customer_summary <- superstore %>%
  group_by(Customer.Name) %>%
  summarize(Profit = sum(profit)) %>%
  ungroup() %>%
  arrange(desc(Profit)) %>%
  top_n(10, Profit)

# Create horizontal bar chart
customers_top_ten <- ggplot(customer_summary, aes(x = reorder(Customer.Name, Profit), y = Profit, fill = Profit)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#DDA0DD", high = "#4B0082") +
  labs(title = "Top 10 Customers by Profit", x = "", y = "") +
  ggtitle("Top 10 Customers by Profit") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9), axis.text.x = element_blank(),
        axis.title = element_blank(), plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format(scale = 0.001, prefix = "$"), expand = c(0, 0.05)) +
  guides(fill = FALSE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# Total Sales by Segment
# Group the data by segment and calculate the sum of sales
segment_sales <- superstore %>% 
  group_by(segment) %>% 
  summarise(total_sales = sum(sales)) %>% 
  arrange(desc(total_sales))



# Create the interactive bar chart using plotly
total_sales_segment <- plot_ly(segment_sales, x = ~segment, y = ~total_sales, type = "bar", 
                               marker = list(color = c('#003f5c','#2f4b7c','#665191'))) %>% 
  layout(title = "Purchases by Segment", 
         xaxis = list(title = "Segment"), 
         yaxis = list(title = "Total Sales"))



# Purchases by Region
# Calculate purchases by region
purchases_by_region <- superstore %>%
  group_by(region) %>%
  summarise(Total_Purchases = n())

# Create the interactive bar chart
regional_purchase <- plot_ly(purchases_by_region, x = ~region, y = ~Total_Purchases, type = "bar", 
                             marker = list(color = c('#d9f0ff', '#90e0ef', '#00bfff', '#1e90ff'))) %>%
  layout(title = "Purchases by Region", xaxis = list(title = "Region"), yaxis = list(title = "Total Purchases"))

# Total Sales by Ship Mode
# Group data by ship mode and calculate total sales
ship_mode_sales <- superstore %>%
  group_by(shipping_mode) %>%
  summarise(total_sales = sum(sales))

# Create plotly bar chart
shipmode_sales <- plot_ly(ship_mode_sales, x = ~shipping_mode, y = ~total_sales, type = 'bar', 
                          marker = list(color = c("#D62728", "#F45B5B", "#FF8C00", "#FFA07A"))) %>%
  layout(title = "Purchases by Ship Mode")


# Annual Profit by Category
# convert Order Date column to Date format
superstore$Order.Date <- as.Date(superstore$Order.Date)

# create new column for year
superstore$year <- year(superstore$Order.Date)

# calculate total profit by year and category
profit_by_year_cat <- superstore %>%
  group_by(category, year) %>%
  summarise(total_profit = sum(profit), .groups = "drop")

# define function to format y-axis labels in thousands of dollars
format_thousands <- function(x) {
  paste0("$", x/1000, "K")
}

# plot the line chart for annual profit by category with formatted y-axis labels
# define function to format y-axis labels in thousands of dollars
format_thousands <- function(x) {
  paste0("$", x/1000, "K")
}

# plot the line chart for annual profit by category with formatted y-axis labels and centered, bolded title
annual_profit <- ggplot(profit_by_year_cat, aes(x = year, y = total_profit, color = category)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = format_thousands) +
  ggtitle("Annual Profit by Category") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 9, margin = margin(t = 10)),
        axis.title.y = element_text(size = 9, margin = margin(r = 10))) +
  labs(x = "Year", y = "Total Profit")

# Unique Customer
# Read in data
Superstore1 <- read.csv("D:/Hard Drive E/Downloads/preprocessed_SampleSuperstore_new.csv", header = TRUE)

# Calculate the number of unique customers by year
unique_customers_by_year <- Superstore1 %>%
  group_by(year) %>%
  summarize(unique_customers = n_distinct(Customer.ID))

# Plot the interactive line graph
unique_customer_plot <- plot_ly(unique_customers_by_year, x = ~year, y = ~unique_customers, type = "scatter", mode = "lines",
                                hoverinfo = "text", text = ~paste("Unique Customers: ", unique_customers)) %>%
  layout(title = "Annual Change in Unique Customers",
         xaxis = list(title = "Year"), yaxis = list(title = "Number of Unique Customers"))


# Technology
# Filter the dataset to only technology products
df_tech <- Superstore %>% 
  filter(category == "Technology")

# Group the data by state and calculate the total profit
df_profit_by_state <- df_tech %>%
  group_by(state) %>%
  summarize(total_profit = sum(profit)) %>%
  na.omit() # remove rows with missing values

# Get state names and their 2-letter abbreviations
state_abbreviations <- data.frame(state.name, state.abb)
colnames(state_abbreviations) <- c("state", "state_code")

# Merge the profit data with state abbreviations
df_profit_by_state <- df_profit_by_state %>%
  left_join(state_abbreviations, by = c("state" = "state"))

# Create choropleth map
plot_geo_tech <- plot_ly(df_profit_by_state, type = "choropleth", locations = ~state_code, z = ~total_profit,
                         locationmode = "USA-states",
                         colorscale = "Greens",
                         text = ~paste("State: ", state, "<br>Total Profit: $", total_profit)) %>%
  colorbar(title = "Total Profit") %>%
  layout(title = "Technology Products Profit by State",
         geo = list(scope = "usa", projection = list(type = "albers usa")),
         margin = list(l = 10, r = 10, t = 50, b = 10))

# Filter the dataset to only technology products
df_office <- Superstore %>% 
  filter(category == "Office Supplies")

# Group the data by state and calculate the total profit
df_profit_by_state_off <- df_office %>%
  group_by(state) %>%
  summarize(total_profit = sum(profit)) %>%
  na.omit() # remove rows with missing values

# Get state names and their 2-letter abbreviations
state_abbreviations <- data.frame(state.name, state.abb)
colnames(state_abbreviations) <- c("state", "state_code")

# Merge the profit data with state abbreviations
df_profit_by_state_off <- df_profit_by_state_off %>%
  left_join(state_abbreviations, by = c("state" = "state"))

# Create choropleth map
plot_geo_office <- plot_ly(df_profit_by_state_off, type = "choropleth", locations = ~state_code, z = ~total_profit,
                           locationmode = "USA-states",
                           colorscale = "Cividis",
                           text = ~paste("State: ", state, "<br>Total Profit: $", total_profit)) %>%
  colorbar(title = "Total Profit") %>%
  layout(title = "Office Supply Products Profit by State",
         geo = list(scope = "usa", projection = list(type = "albers usa")),
         margin = list(l = 10, r = 10, t = 50, b = 10))

# Furniture
# Filter the dataset to only technology products
df_Furniture <- Superstore %>% 
  filter(category == "Office Supplies")

# Group the data by state and calculate the total profit
df_profit_by_state_Fur <- df_Furniture %>%
  group_by(state) %>%
  summarize(total_profit = sum(profit)) %>%
  na.omit() # remove rows with missing values

# Get state names and their 2-letter abbreviations
state_abbreviations <- data.frame(state.name, state.abb)
colnames(state_abbreviations) <- c("state", "state_code")

# Merge the profit data with state abbreviations
df_profit_by_state_Fur <- df_profit_by_state_Fur %>%
  left_join(state_abbreviations, by = c("state" = "state"))

# Create choropleth map
plot_geo_fur <- plot_ly(df_profit_by_state_Fur, type = "choropleth", locations = ~state_code, z = ~total_profit,
                        locationmode = "USA-states",
                        colorscale = "Reds",
                        text = ~paste("State: ", state, "<br>Total Profit: $", total_profit)) %>%
  colorbar(title = "Total Profit") %>%
  layout(title = "Furniture Products Profit by State",
         geo = list(scope = "usa", projection = list(type = "albers usa")),
         margin = list(l = 10, r = 10, t = 50, b = 10))

# Profit by Shipping Mode
df_summary_profit <- Superstore %>% 
  group_by(shipping_mode) %>% 
  summarize(profit = sum(profit)) %>%
  mutate(percent_profit = round(profit / sum(profit) * 100, 2))

pie_chart_profit <- plot_ly(df_summary_profit, labels = ~shipping_mode, values = ~profit, type = "pie", 
                            marker = list(colors = c("yellow", "blue", "#48C9B0", "#A83DAD")),
                            hovertemplate = paste("Shipping mode: %{label}<br>Quantity: %{value}<br>Percentage: %{percent}<br>")) %>%
  layout(title = "Profit by Shipping Mode", 
         showlegend = TRUE, 
         margin = list(l = 10, r = 10, t = 50, b = 10),
         textinfo = "percent+label")

# Sales by Shipping Mode
library(colorspace)

df_summary_sales <- Superstore %>% 
  group_by(shipping_mode) %>% 
  summarize(sales = sum(sales)) %>%
  mutate(percent_sales = round(sales / sum(sales) * 100, 2))

colorway <- rev(colorspace::sequential_hcl(length(unique(df_summary_sales$shipping_mode)), palette = "inferno"))


pie_chart_sales <- plot_ly(df_summary_sales, labels = ~shipping_mode, values = ~sales, type = "pie", 
                           hovertemplate = paste("Shipping mode: %{label}<br>Quantity: %{value}<br>Percentage: %{percent}<br>"),
                           marker = list(colors = colorway)) %>%
  layout(title = "Sales by Shipping Mode", 
         showlegend = TRUE, 
         margin = list(l = 10, r = 10, t = 50, b = 10),
         textinfo = "percent+label")


# Quantity by Shipping Mode
df_summary_quantity <- Superstore %>% 
  group_by(shipping_mode) %>% 
  summarize(quantity = sum(quantity)) %>%
  mutate(percent_quantity = round(quantity / sum(quantity) * 100, 2))

pie_chart_quantity <- plot_ly(df_summary_quantity, labels = ~shipping_mode, values = ~quantity, type = "pie", 
                              colorway = "inferno",
                              hovertemplate = paste("Shipping mode: %{label}<br>Quantity: %{value}<br>Percentage: %{percent}<br>")) %>%
  layout(title = "Quantity by Shipping Mode", 
         showlegend = TRUE, 
         margin = list(l = 10, r = 10, t = 50, b = 10),
         textinfo = "percent+label")

# Customer Segment
df_scatter <- c('segment')

for (col in df_scatter) {
  p <- ggplot(Superstore, aes(x = sales, y = profit, size = 10, color = .data[[col]])) +
    geom_point(alpha = 0.7) +
    scale_size(range = c(3, 8)) +
    labs(title = paste("Sales and Profit Across", col),
         x = "Sales",
         y = "Profit",
         size = "Sales",
         color = col) +
    theme_bw() +
    theme(plot.title = element_text(size = 12, color = "lightseagreen"))
  scatter_custSegment <- ggplotly(p, tooltip = col)
  print(scatter_custSegment)
}


# Region

df_scatter <- c('region')

for (col in df_scatter) {
  p <- ggplot(Superstore, aes(x = sales, y = profit, size = 10, color = .data[[col]])) +
    geom_point(alpha = 0.7) +
    scale_size(range = c(3, 8)) +
    labs(title = paste("Sales and Profit Across", col),
         x = "Sales",
         y = "Profit",
         size = "Sales",
         color = col) +
    theme_bw() +
    theme(plot.title = element_text(size = 12, color = "lightseagreen"))
  scatter_region <- ggplotly(p, tooltip = col)
  print(scatter_region)
}

# Shipping Mode
df_scatter <- c('shipping_mode')

for (col in df_scatter) {
  p <- ggplot(Superstore, aes(x = sales, y = profit, size = sales, color = .data[[col]])) +
    geom_point(alpha = 0.7) +
    scale_size(range = c(3, 8)) +
    labs(title = paste("Sales and Profit Across", col),
         x = "Sales",
         y = "Profit",
         size = "Sales",
         color = col) +
    theme_bw() +
    theme(plot.title = element_text(size = 12, color = "lightseagreen"))
  scatter_shipping <- ggplotly(p, tooltip = col)
  print(scatter_shipping)
}
