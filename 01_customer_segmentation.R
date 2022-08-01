#################################################################################
#### 01_customer_segmentation.R
################################################################################

library(tidyverse)

retails_dat <- read.csv(file = "00_data/OnlineRetail.csv")

retails_dat %>% 
    glimpse()

# create new data type
retails_dat$created_at <- lubridate::mdy_hm(retails_dat$InvoiceDate)

# convert to date type
retails_dat$created_at <- as.Date(retails_dat$created_at)


retails_dat <- retails_dat[complete.cases(retails_dat$InvoiceDate), ]

retails_dat %>% 
    glimpse()



# 2.0 Análise Exploratória de Dados ---------------------------------------

p <- ggplot(data = top50_produto_categoria_tbl, aes(x = reorder(Description, Total_Amount), y = Total_Amount)) +
    geom_col(fill="steelblue") +
    # Outside bars
    # geom_text(aes(label=percent), vjust=-0.3, size=3.5) +
    geom_text(aes(label = scales::comma(Total_Amount, accuracy = 1)), vjust = 0.5, hjust = 1.2, color = "white", size = 3) +
    labs( x = "Categoria de Produtos", 
          y = "Montante em Libras (GBP)",
          title = "Produtos com maior Rentabilidade",
          subtitle = "online retail company based in the UK") +
    theme_minimal() +
    scale_y_continuous(labels = scales::label_comma())

# Horizontal bar plot
p + coord_flip()


range(retails_dat$created_at)

retails_dat %>% 
    count(created_at) %>% 
    tail(10)

# date of analysis
retails_dat$date_of_analysis <- as.Date('2011-12-10')

# calculate the difference between date_of_analysis and created event
retails_dat$days_since <-
    as.numeric(
        retails_dat$date_of_analysis - retails_dat$created_at
    ) 

# analysis
customer_analysis_tbl <-
    retails_dat %>%
    group_by(CustomerID) %>%
    summarise(
        recency = min(days_since),
        first_purchase = max(days_since),
        frequency = n(),
        monetary = sum(Quantity*UnitPrice)
    ) %>%
    filter(monetary > 0) %>% 
    ungroup()


customer_analysis_tbl %>% 
    glimpse()

# summary statistics
customer_analysis_tbl %>% 
    summary()


# Histogram Recency
ggplot(data = customer_analysis_tbl, aes(x = recency)) +
    geom_histogram(bins = 30, color = "#000000", fill = "#0099F8") +
    labs(
        title = "Histograma sobre compras online mais Recentes em relação à data de analise 2011-12-10",
        subtitle = "online retail company based in the UK",
        caption = "Source: onlineRetails UK dataset",
        x = "Recente (número de dias)",
        y = "Numero de Clientes"
    ) +
    theme_classic() +
    theme(
        plot.title = element_text(color = "#0099F8", size = 16, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "bold"),
        plot.caption = element_text(face = "italic")
    )



# Histogram Frequency
ggplot(data = customer_analysis_tbl, aes(x = frequency)) +
    geom_histogram(bins = 15, color = "#000000", fill = "#0099F8") +
    labs(
        title = "Histograma sobre frequências de compras online em relação à data de analise 2011-12-10",
        subtitle = "online retail company based in the UK",
        caption = "Source: onlineRetails UK dataset",
        x = "Frequência",
        y = "Número de Clientes"
    ) +
    scale_x_continuous(labels = scales::comma) +
    # scale_x_log10() +
    theme_classic() +
    theme(
        plot.title = element_text(color = "#0099F8", size = 16, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "bold"),
        plot.caption = element_text(face = "italic")
    )



# Histogram Monetary
ggplot(data = customer_analysis_tbl, aes(x = monetary)) +
    geom_histogram(bins = 15, color = "#000000", fill = "#0099F8") +
    labs(
        title = "Histograma sobre frequências de compras online em relação à data de analise 2011-12-10",
        subtitle = "online retail company based in the UK",
        caption = "Source: onlineRetails UK dataset",
        x = "Monetary",
        y = "Número de Clientes"
    ) +
    scale_x_continuous(labels = scales::comma) +
    # scale_x_log10() +
    theme_classic() +
    theme(
        plot.title = element_text(color = "#0099F8", size = 16, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "bold"),
        plot.caption = element_text(face = "italic")
    )

# Determinar Percentil ----------------------------------------------------

quantile(customer_analysis_tbl$monetary, probs = seq(0, 1, 0.20))


quantile(customer_analysis_tbl$frequency, probs = seq(0, 1, 0.20))

quantile(customer_analysis_tbl$recency, probs = seq(0, 1, 0.20))



# Determinar RFM score ----------------------------------------------------

customer_analysis_tbl <-
    customer_analysis_tbl %>%
    mutate(
        R = ntile(desc(recency), 5),
        F = ntile(frequency, 5),
        M = ntile(monetary, 5)
    )

customer_analysis_tbl$RFM <-
    customer_analysis_tbl$R * 100 + customer_analysis_tbl$F * 10 + customer_analysis_tbl$M

customer_analysis_tbl %>%
    head()


# Definir a segmentação dos clientes --------------------------------------

# Define Customer Segmentation Groups
customer_analysis_tbl$SEGMENT <- NA

customer_analysis_tbl$SEGMENT[which(customer_analysis_tbl$RFM >=111 & customer_analysis_tbl$RFM <= 225)] <- 'Lost'

customer_analysis_tbl$SEGMENT[which(customer_analysis_tbl$RFM >= 231 & customer_analysis_tbl$RFM <= 325)] <- 'Hibernating'

customer_analysis_tbl$SEGMENT[which(customer_analysis_tbl$RFM >= 331 & customer_analysis_tbl$RFM <= 425)] <- 'About to Sleep'

customer_analysis_tbl$SEGMENT[which(customer_analysis_tbl$RFM >= 431 & customer_analysis_tbl$RFM <= 525)] <- 'Potential Loyalists'

customer_analysis_tbl$SEGMENT[which(customer_analysis_tbl$RFM >= 531 & customer_analysis_tbl$RFM <= 554)] <- 'Loyal Customers'

customer_analysis_tbl$SEGMENT[customer_analysis_tbl$RFM == 555] <- 'Champions'

customer_analysis_tbl$SEGMENT[which(customer_analysis_tbl$first_purchase <= 30)] <-
    'New Customers'


# ou de forma elegante:
    
# Define Customer Segmentation Groups
customer_analysis_tbl$SEGMENT <- NA

customer_analysis_tbl$SEGMENT[between(customer_analysis_tbl$RFM, 111, 225)] <- 'Lost'

customer_analysis_tbl$SEGMENT[between(customer_analysis_tbl$RFM, 231, 325)] <- 'Hibernating'

customer_analysis_tbl$SEGMENT[between(customer_analysis_tbl$RFM, 331, 425)] <- 'About to Sleep'

customer_analysis_tbl$SEGMENT[between(customer_analysis_tbl$RFM, 431, 525)] <- 'Potential Loyalists'

customer_analysis_tbl$SEGMENT[between(customer_analysis_tbl$RFM, 531, 554)] <- 'Loyal Customers'

customer_analysis_tbl$SEGMENT[customer_analysis_tbl$RFM == 555] <- 'Champions'

customer_analysis_tbl$SEGMENT[which(customer_analysis_tbl$first_purchase <= 30)] <-
    'New Customers'

# re-order segments in factor in a way that makes sense 
customer_analysis_tbl$SEGMENT <-
    factor(
        x = customer_analysis_tbl$SEGMENT,
        levels = c(
            'Lost',
            'Hibernating',
            'About to Sleep',
            'New Customers',
            'Potential Loyalists',
            'Loyal Customers',
            'Champions'
        )
    )


customer_analysis_tbl %>% 
    glimpse()


customer_analysis_tbl %>% 
    head()


# 2.4.4  Distribuição dos Clientes por Segmentos --------------------------

customer_analysis_tbl %>% 
    count(SEGMENT)


aggregate(x = customer_analysis_tbl[, c("recency", "frequency", "monetary")],
          by = list(customer_analysis_tbl$SEGMENT),
          mean)

# Visualizacao de Dados

customer_analysis_tbl %>% 
    count(SEGMENT) %>% 
    apexcharter::apex(aes(x=SEGMENT, y = n), type = "column") %>% 
    # Add data labels
    apexcharter::ax_dataLabels(enabled = TRUE)

# Add data labels
apex(
    data = diamonds,
    mapping = aes(x = cut)
) %>%
    ax_dataLabels(enabled = TRUE)

customer_analysis_tbl %>% 
    count(SEGMENT) %>% 
    apexcharter::apex(aes(x = SEGMENT, y=n), "treemap")
    