month_name_to_num <- function(text, sale_month = month_of_sale){
  return(str_replace_all(string = text,
                         pattern = sale_month,
                         replacement = case_when(
                           any(sale_month == c("january", "jan")) ~ "1",
                           any(sale_month == c("february", "feb")) ~ "2",
                           any(sale_month == c("march", "mar")) ~ "3",
                           any(sale_month == c("april", "apr"))~ "4",
                           sale_month == "may" ~ "5",
                           any(sale_month == c("june", "jun")) ~ "6",
                           any(sale_month == c("july", "jul")) ~ "7",
                           any(sale_month == c("august", "aug")) ~ "8",
                           any(sale_month == c("september", "sept")) ~ "9",
                           any(sale_month == c("october", "oct")) ~ "10",
                           any(sale_month == c("november", "nov")) ~ "11",
                           any(sale_month == c("december", "dec")) ~ "12",
                           
                           TRUE ~ sale_month
                         ))
  )
}