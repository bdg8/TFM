#columnas repetidas
# Telco_customer_churn$CustomerID
# Telco_customer_churn$Count
# Telco_customer_churn$Country
# Telco_customer_churn$State
# Telco_customer_churn$City
# Telco_customer_churn$`Zip Code`
# Telco_customer_churn$`Lat Long`
# Telco_customer_churn$Latitude
# Telco_customer_churn$Longitude
# Telco_customer_churn$Gender
# Telco_customer_churn$`Senior Citizen`
# Telco_customer_churn$Partner
#   Telco_customer_churn$Dependents
#   Telco_customer_churn$`Tenure Months`
#   Telco_customer_churn$`Phone Service`
#   Telco_customer_churn$`Multiple Lines`
#   Telco_customer_churn$`Internet Service`
#   Telco_customer_churn$`Online Security`
#   Telco_customer_churn$`Online Backup`
#   Telco_customer_churn$`Device Protection`
#   Telco_customer_churn$`Tech Support`
#   Telco_customer_churn$`Streaming TV`
#   Telco_customer_churn$`Streaming Movies`
#   Telco_customer_churn$Contract
#   Telco_customer_churn$`Paperless Billing`
#   Telco_customer_churn$`Payment Method`
#   Telco_customer_churn$`Monthly Charges`
#   Telco_customer_churn$`Total Charges`
#   Telco_customer_churn$`Churn Label`
#   Telco_customer_churn$`Churn Value`
#   Telco_customer_churn$`Churn Score`
#   Telco_customer_churn$CLTV
#   Telco_customer_churn$`Churn Reason`
#   Telco_customer_churn_demographics$Age
#   Telco_customer_churn_demographics$`Under 30`
#   Telco_customer_churn_demographics$Married
#   Telco_customer_churn_demographics$`Number of Dependents`
#   Telco_customer_churn_services$Quarter
#   Telco_customer_churn_services$`Referred a Friend`
#   Telco_customer_churn_services$`Number of Referrals`
#   Telco_customer_churn_services$`Tenure in Months`
#   Telco_customer_churn_services$Offer
#   Telco_customer_churn_services$`Avg Monthly Long Distance Charges`
#   Telco_customer_churn_services$`Internet Type`
#   Telco_customer_churn_services$`Avg Monthly GB Download`
#   Telco_customer_churn_services$`Device Protection Plan`
#   Telco_customer_churn_services$`Premium Tech Support`
#   Telco_customer_churn_services$`Streaming Music`
#   Telco_customer_churn_services$`Unlimited Data`
#   Telco_customer_churn_services$`Total Refunds`
#   Telco_customer_churn_services$`Total Extra Data Charges`
#   Telco_customer_churn_services$`Total Long Distance Charges`
#   Telco_customer_churn_services$`Total Revenue`
#  Telco_customer_churn_status$`Satisfaction Score`
#  Telco_customer_churn_status$`Customer Status`
#  Telco_customer_churn_status$`Churn Category`
library(readxl)
Telco_customer_churn_demographics <- read_excel("Documents/UNIR_BI/TFM/tfm/data/Telco_customer_churn_demographics.xlsx");
Telco_customer_churn_services <- read_excel("Documents/UNIR_BI/TFM/tfm/data/Telco_customer_churn_services.xlsx");
Telco_customer_churn_status <- read_excel("Documents/UNIR_BI/TFM/tfm/data/Telco_customer_churn_status.xlsx");
Telco_customer_churn <- read_excel("Documents/UNIR_BI/TFM/tfm/data/Telco_customer_churn.xlsx");

 
full_data1 = cbind(Telco_customer_churn$CustomerID, Telco_customer_churn$Count, Telco_customer_churn$Country, 
                   Telco_customer_churn$State, Telco_customer_churn$City, Telco_customer_churn$`Zip Code`,
                   Telco_customer_churn$`Lat Long`, Telco_customer_churn$Latitude, Telco_customer_churn$Longitude,
                   Telco_customer_churn$Gender, Telco_customer_churn$`Senior Citizen`, Telco_customer_churn$Partner,
                   Telco_customer_churn$Dependents, Telco_customer_churn$`Tenure Months`, Telco_customer_churn$`Phone Service`,
                   Telco_customer_churn$`Multiple Lines`, Telco_customer_churn$`Internet Service`,
                   Telco_customer_churn$`Online Security`, Telco_customer_churn$`Online Backup`,
                   Telco_customer_churn$`Device Protection`, Telco_customer_churn$`Tech Support`,
                   Telco_customer_churn$`Streaming TV`, Telco_customer_churn$`Streaming Movies`,
                   Telco_customer_churn$Contract, Telco_customer_churn$`Paperless Billing`,
                   Telco_customer_churn$`Payment Method`, Telco_customer_churn$`Monthly Charges);
 
full_data2 = cbind(Telco_customer_churn$`Total Charges`, Telco_customer_churn$`Churn Label`,
                    Telco_customer_churn$`Churn Value`, Telco_customer_churn$`Churn Score`,
                    Telco_customer_churn$CLTV, Telco_customer_churn$`Churn Reason`,
                    Telco_customer_churn_demographics$Age, Telco_customer_churn_demographics$`Under 30`,
                    Telco_customer_churn_demographics$Married, Telco_customer_churn_demographics$`Number of Dependents`,
                    Telco_customer_churn_services$Quarter, Telco_customer_churn_services$`Referred a Friend`,
                    Telco_customer_churn_services$`Number of Referrals`, Telco_customer_churn_services$`Tenure in Months`);
 
full_data3 = cbind(Telco_customer_churn_services$Offer, Telco_customer_churn_services$`Avg Monthly Long Distance Charges`,
                    Telco_customer_churn_services$`Internet Type`, Telco_customer_churn_services$`Avg Monthly GB Download`,
                    Telco_customer_churn_services$`Device Protection Plan`, Telco_customer_churn_services$`Premium Tech Support`,
                    Telco_customer_churn_services$`Streaming Music`, Telco_customer_churn_services$`Unlimited Data`,
                    Telco_customer_churn_services$`Total Refunds`, Telco_customer_churn_services$`Total Extra Data Charges`);
 
full_data4 = cbind(Telco_customer_churn_services$`Total Long Distance Charges`, Telco_customer_churn_services$`Total Revenue`,
                    Telco_customer_churn_status$`Satisfaction Score`, Telco_customer_churn_status$`Customer Status`,
                    Telco_customer_churn_status$`Churn Category`);
full_data = cbind(full_data1, full_data2, full_data3, full_data4);

# renombramos cada variable del full_data con sus nombres reales 
colnames(full_data) <- c("CustomerID", "etc", "etc",...)