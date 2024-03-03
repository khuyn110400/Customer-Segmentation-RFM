library(dplyr)
library(ggplot2)
library(tidyr)
library(rfm)

# Di chuyển cột không cần thiết, chỉ giữ lại các cột InvoiceNo, Quantity, InvoiceDate,
# UnitPrice, CustomerID, Revenue
ecom <- mutate(data[,c(-2,-3,-8)],Revenue = Quantity*UnitPrice,
               InvoiceDate =as.Date(InvoiceDate,format='%m/%d/%Y'))
ecom %>% head() 
summary(ecom)
ecom %>% ggplot(aes(Quantity,UnitPrice)) + 
  geom_point(alpha=0.3) + 
  geom_jitter() +
  labs(title = "Comparing UnitPrice against Quantity",
       tag="Figure 1") +
  theme_minimal()

# Loại bỏ các giá trị (-) trong UnitPricce và Quantity
ecom %>% filter(UnitPrice <= 0 | Quantity <= 0) %>% arrange(UnitPrice) %>%
  head(10)

ecom_clean <- ecom %>% drop_na(CustomerID) %>% 
  filter(Quantity > 0, 
         UnitPrice > 0)

ecom_clean %>% ggplot(aes(Quantity,UnitPrice)) +
  geom_point(alpha=0.3,aes()) +
  geom_jitter() + 
  labs(title="Comparing UnitPrice against Quantity",
       subtitle = "There are negatives for both Quantity and UnitPrice",
       tag="Figure 2") +
  theme_light() + 
  scale_x_log10() + 
  scale_y_log10()

ecom_clean %>% arrange(desc(Revenue)) %>%  head(10)


# Phân nhóm RFM
rfm_result <- rfm_table_order(data = ecom_clean,
                              customer_id = CustomerID,
                              revenue = Revenue,
                              order_date = InvoiceDate, 
                              analysis_date = as.Date("2012/01/01"))

print(ecom)
print(rfm_result)
rfm_result_dt <- data.frame(rfm_result)

# Từ biểu đồ heatmap ta thấy: Phần lớn giá trị tiền tệ tập trung ở những người mua sắm
# có frequncy thường xuyên, bên cạnh đó, giá trị tiền tệ còn tập trung ở những người mua mới 
# có recency cao nhưng frequency thấp. Công ty cần lưu ý đến đối tượng này, đảm bảo rằng biến họ trở thành nhóm có 
# frequency thường xuyên 
rfm_heatmap(rfm_result) 

# Như đã phân tích từ heatmap, ta thấy cần quan tâm đến nhóm có recency cao nhưng 
# frequency thấp
rfm_bar_chart(rfm_result)  

# Phân tích Rencency và Monetary: Phần lớn khách hàng tập trung mua hàng dưới 100 ngày,
# và một số khách hàng có Monetary cao không mua sắm trong hơn 200 ngày.
rfm_rm_plot(rfm_result) 

# Phân tích Frequency và Monetary: Có mối liên hệ mạnh mẽ giữa frequency và monetary. 
# Tỉ lệ Frequency (low)/ Monetary (high) (Công ty nên cô lập khách hàng có tỉ lệ ngày để tiếp cận)
rfm_fm_plot(rfm_result)


# Phân chia phân khúc khách hàng theo RFM
segment_names <- c("Champions", "Loyal Customers", "Potential Loyalist",
                   "New Customers", "Promising", "Need Attention", "About To Sleep",
                   "At Risk", "Can't Lose Them", "Lost")

recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)

segment <- rfm_segment(rfm_result,
                       segment_names,
                       recency_lower,
                       recency_upper,
                       frequency_lower, 
                       frequency_upper, 
                       monetary_lower,
                       monetary_upper)
# Biểu đồ giữa recency và segment: Ta thấy,thời điểm mua hàng gần nhất của nhóm
# Lost và At Risk là gân nhất với trung bình trên 250 ngày, còn Champions và Potential và Loyalist
# chiếm phần nhỏ của recency 
rfm_plot_median_recency(segment)

# Biểu đồ giữa Monetary và segment: Phân khúc Champions có Monetary trung bình lớn nhất 
# sự chênh lệch giữa nhóm Champions và Loyal Customers gần nhất, với Champions có giá trị
# lớn gấp 2.5 lần so với nhóm Loyal Customers.
# Nhóm At Risk có Monetary trung bình tót và đây là một trong những nhóm lớn
rfm_plot_median_monetary(segment)

# KẾT LUẬN: Bên cạnh có chiến dịch phù hợp cho các Phân khúc đã ổn định như Champions và 
# Loyal Customers, công ty cần quan tâm nhóm At Risk

segment %>% ggplot(aes(x=segment,y=amount,fill=segment)) +geom_bar(stat = "identity")

print(rfm_result)
