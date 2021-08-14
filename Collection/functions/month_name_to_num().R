month_name_to_num <- function(text, month_of_sale = month_of_sale){
  return(str_replace_all(string = text,
                         pattern = month_of_sale,
                         replacement = case_when(
                           any(month_of_sale == c("january", "jan")) ~ "1",
                           any(month_of_sale == c("february", "feb")) ~ "2",
                           any(month_of_sale == c("march", "mar")) ~ "3",
                           any(month_of_sale == c("april", "apr"))~ "4",
                           month_of_sale == "may" ~ "5",
                           any(month_of_sale == c("june", "jun")) ~ "6",
                           any(month_of_sale == c("july", "jul")) ~ "7",
                           any(month_of_sale == c("august", "aug")) ~ "8",
                           any(month_of_sale == c("september", "sept")) ~ "9",
                           any(month_of_sale == c("october", "oct")) ~ "10",
                           any(month_of_sale == c("november", "nov")) ~ "11",
                           any(month_of_sale == c("december", "dec")) ~ "12",
                           
                           TRUE ~ month_of_sale
                         ))
         )
}