library(lubridate)
library(ggplot2)


olist_customers_dataset <- read.csv("data_input/olist_customers_dataset.csv")
olist_order_items_dataset<- read.csv("data_input/olist_order_items_dataset.csv")
olist_order_payments_dataset <- read.csv("data_input/olist_order_payments_dataset.csv")
olist_order_reviews_dataset <- read.csv("data_input/olist_order_reviews_dataset.csv")
olist_orders_dataset <- read.csv("data_input/olist_orders_dataset.csv")
olist_products_dataset <- read.csv("data_input/olist_products_dataset.csv")
olist_sellers_dataset <- read.csv("data_input/olist_sellers_dataset.csv")
product_category_name_translation <- read.csv("data_input/product_category_name_translation.csv")


# manipulate_customers_data 
# str(olist_customers_dataset)
olist_customers_dataset$customer_id <- as.character(olist_customers_dataset$customer_id)
olist_customers_dataset$customer_unique_id <- as.character(olist_customers_dataset$customer_unique_id)
olist_customers_dataset$customer_zip_code_prefix <- as.factor(olist_customers_dataset$customer_zip_code_prefix)


# manipulate_products_data 

# check product information
str(olist_products_dataset)

# check product category name translation information
summary(olist_products_dataset)

# cek_product_null <- olist_products_dataset[olist_products_dataset$product_category_name=="",]

# check product category name translation information
str(product_category_name_translation)

# adjust product dataset
olist_products_dataset$product_id <- as.character(olist_products_dataset$product_id)
olist_products_dataset$product_category_name <- as.character(olist_products_dataset$product_category_name)

# adjust product category data type
product_category_name_translation$product_category_name <- as.character(product_category_name_translation$product_category_name)

# merge product data with name translation
olist_products_dataset<-merge(x=olist_products_dataset,y=product_category_name_translation, by.x ="product_category_name", by.y="product_category_name", all.x = T)



# manipulate_sellers_data
str(olist_sellers_dataset)
olist_sellers_dataset$seller_id <- as.character(olist_sellers_dataset$seller_id)
olist_sellers_dataset$seller_zip_code_prefix <- as.factor(olist_sellers_dataset$seller_zip_code_prefix)


# manipulate_payments_data
str(olist_order_payments_dataset)
olist_order_payments_dataset$order_id <- as.character(olist_order_payments_dataset$order_id)

# manipulate_order_items_data
str(olist_order_items_dataset)
olist_order_items_dataset$order_id <- as.character(olist_order_items_dataset$order_id)
olist_order_items_dataset$order_item_id <- as.character(olist_order_items_dataset$order_item_id)
olist_order_items_dataset$product_id <- as.character(olist_order_items_dataset$product_id)
olist_order_items_dataset$seller_id <- as.character(olist_order_items_dataset$seller_id)
olist_order_items_dataset$shipping_limit_date <- ymd_hms(olist_order_items_dataset$shipping_limit_date)

# manipulate_order_reviews
str(olist_order_reviews_dataset)
olist_order_reviews_dataset$review_id <- as.character(olist_order_reviews_dataset$review_id)
olist_order_reviews_dataset$order_id <- as.character(olist_order_reviews_dataset$order_id)
olist_order_reviews_dataset$review_comment_title <- as.character(olist_order_reviews_dataset$review_comment_title)
olist_order_reviews_dataset$review_comment_message <- as.character(olist_order_reviews_dataset$review_comment_message)
olist_order_reviews_dataset$review_creation_date <- ymd_hms(olist_order_reviews_dataset$review_creation_date)
olist_order_reviews_dataset$review_answer_timestamp <- ymd_hms(olist_order_reviews_dataset$review_answer_timestamp)
summary(olist_order_reviews_dataset)



# manipulate_order_data ----

str(olist_orders_dataset)
olist_orders_dataset$order_id <- as.character(olist_orders_dataset$order_id)
olist_orders_dataset$customer_id <- as.character(olist_orders_dataset$customer_id)
olist_orders_dataset$order_purchase_timestamp <- ymd_hms(olist_orders_dataset$order_purchase_timestamp)
olist_orders_dataset$order_approved_at <- ymd_hms(olist_orders_dataset$order_approved_at)
olist_orders_dataset$order_delivered_carrier_date <- ymd_hms(olist_orders_dataset$order_delivered_carrier_date)
olist_orders_dataset$order_delivered_customer_date <- ymd_hms(olist_orders_dataset$order_delivered_customer_date)
olist_orders_dataset$order_estimated_delivery_date <- ymd_hms(olist_orders_dataset$order_estimated_delivery_date)


#--------------------------------------

summary(olist_order_reviews_dataset)


olist_order <- merge(x=olist_order_items_dataset,y=olist_orders_dataset, by.x ="order_id", by.y="order_id")
olist_order <- merge(x=olist_order,y=olist_products_dataset, by.x ="product_id", by.y="product_id")
olist_order <- merge(x=olist_order,y=olist_sellers_dataset, by.x ="seller_id", by.y="seller_id")
olist_order <- merge(x=olist_order,y=olist_order_payments_dataset, by.x ="order_id", by.y="order_id")
olist_order <- merge(x=olist_order,y=olist_customers_dataset, by.x ="customer_id", by.y="customer_id")
olist_order <- merge(x=olist_order,y=olist_order_reviews_dataset, by.x ="order_id", by.y="order_id")
# View(olist_order)


order_review_payment <- merge(x=olist_orders_dataset,y=olist_order_reviews_dataset, by.x ="order_id", by.y="order_id")
order_review_payment <- merge(x=order_review_payment,y=olist_order_payments_dataset, by.x ="order_id", by.y="order_id")


# -------------------

# str(olist_order)
# head(olist_order)
# summary(olist_order$order_status)

olist_order_delivered <- olist_order[olist_order$order_status=="delivered",]



summ_product <- as.data.frame(table(product_category_name_english = olist_order_delivered$product_category_name_english))
summ_product <- summ_product[order(summ_product$Freq, decreasing = T), ]
summ_top20_product <- summ_product[1:20,]
summary(summ_product)

ggplot(summ_top20_product, aes(x=reorder(product_category_name_english, Freq), y=Freq))+
  geom_col()+
  coord_flip()+
  labs(title = "Top 20 product",
       x = "Product", 
       y = "Freq") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(angle = 90),
        legend.text.align = 1) +
  geom_text(aes(label = Freq), hjust = -0.1, size = 2) + 
  theme_minimal()


# Relationship between photo qty and order frequency within each product category

mean_product_photo <- aggregate.data.frame(
  list(photo_qty = olist_order_delivered$product_photos_qty), 
  by = list(product_category_name_english = olist_order_delivered$product_category_name_english), 
  mean)

order_frequency <- aggregate(order_id~product_category_name_english, olist_order_delivered, length)

order_photo_freq <- merge(x=mean_product_photo, y=order_frequency)

order_photo_freq <- order_photo_freq[order(order_photo_freq$order_id,decreasing = T),]


ggplot(order_photo_freq, aes(photo_qty, order_id)) + 
  geom_boxplot() +
  geom_jitter(aes(col = order_photo_freq$order_id)) +
  labs(title = "Boxplot Each category",
       x = "Mean Photo Quantity",
       y = "Order Frequency",
       col = "Order Frequency")

# Test ----

olist_order_delivered$purchase_month <- month(olist_order_delivered$order_purchase_timestamp, label = T, abbr = F)

str(olist_order_delivered$purchase_month)

summary(olist_order_delivered$product_category_name_english)




# Relationship between products purchase with review score for bed_bath_table product category ----

olist_order

order_review_payment$time_purchased_delivered <- as.numeric(date(order_review_payment$order_delivered_customer_date) - date(order_review_payment$order_purchase_timestamp))
order_review_payment$time_estimated_delivered <- as.numeric(date(order_review_payment$order_estimated_delivery_date) - date(order_review_payment$order_delivered_customer_date))

plot(order_review_payment$time_purchased_delivered, order_review_payment$time_estimated_delivered)

ggplot(order_review_payment, aes(time_purchased_delivered, time_estimated_delivered)) +
  geom_col(fill="darkblue") +
  facet_wrap(~review_score) +
  theme(strip.text = element_text(size = 7))


