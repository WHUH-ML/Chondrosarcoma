library(survival)
library(magrittr)
library(gtsummary)
library(forestplot)
library(data.table)
library(flextable)

source('preprocessing.R')
data <- get_data() %>% data.frame(check.names = T) 

#### Overall distrubution ####
overall <- tbl_summary(data,statistic = list(all_continuous() ~ "{mean} ({sd})"))

##### multi cox ####
data_cox = data %>% mutate(Status= Status %>% as.numeric() %>% -1) 
fml = as.formula(Surv(Survival.months, Status) ~ .)
multi_cox <- coxph(fml, data = data_cox) %>%
  tbl_regression(exponentiate=T,
                 pvalue_fun = ~style_pvalue(.x, digits = 2),
                 hide_n = T
  )%>%
  add_global_p() %>%
  bold_p(t = 0.050) %>%
  bold_labels() %>%
  italicize_levels()
#### Univariate survival analysis ####
unicox <- data_cox %>% 
  tbl_uvregression(
    method = coxph,
    y = Surv(Survival.months,Status),
    exponentiate = TRUE,
    pvalue_fun = function(x) style_pvalue(x, digits = 2),
    hide_n = T
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.050) %>%
  bold_labels() %>%
  italicize_levels()
#### Merge cox survival analysis ####
tbl_merge(
  tbls = list(overall,unicox, multi_cox),
  tab_spanner = c("**Overall**", "**Univariate**", "**Multivariate**")
) %>% 
  bold_labels() %>%
  italicize_levels()%>%
  as_flex_table() %>%
  autofit()%>%
  bold(bold = TRUE, part = "header")%>%
  flextable::save_as_docx(path = 'Tables/Merge_Unicox_Multicox.docx')

include_cols <-c("Age", "Gender", "Histological.type", "Primary.site",#"Stage", 
                 "Grade", "Surgery", "Tumor.size", 
                 "Tumor.extension", "Distant.metastasis", 
                 "Survival.months", "Status")
include_cols <- colnames(data)
exclude_cols <- c('Year',"Radiotherapy", "Chemotherapy",'Number.of.tumors')
#### impute data ####
library(missForest)
data_imputed <- missForest(data[setdiff(colnames(data),exclude_cols)])[["ximp"]]

#### Feature Selecting -- Correction plot ------------------------------------
library(corrplot)
corr_data <- data_imputed
corr_data <- as.data.frame(lapply(corr_data,as.numeric),check.names = F)
colnames(corr_data) <- lapply(colnames(corr_data), function(x){gsub('\\.',' ',x)})
corMatMy <- cor(corr_data%>%.[,setdiff(names(.),c('Status','Survival months'))])
col <- colorRampPalette(c("darkorange", "white", "steelblue"))(20)
corrplot(corMatMy, type = "upper", order = "hclust", col = col,tl.col="black", tl.cex=0.8, tl.srt=70)
tiff(paste('Figures','correation plot.tiff',sep = '/'),width=2000,height = 2000,res=300)
corrplot(corMatMy, type = "upper", order = "hclust", col = col,tl.col="black", tl.cex=0.8, tl.srt=70)
dev.off()
exclude_cols %<>% append(c('Stage')) 
#### save data ####
data_imputed %>% .[setdiff(names(.),exclude_cols)] %>% 
  mutate_at(.,sapply(.,function(x) length(levels(x))) %>% .[.<5] %>% names,~as.numeric(.)-1) %>%
  data.frame() %>% 
  write.csv('data/data_surv.csv',row.names = F)

#### Baseline table ####
library(tableone)
library(flextable)
base_data <- data %>% .[setdiff(names(.),exclude_cols)] #%>% subset(select=-c(Year,Histologic.type))
#### over all ####
dput(names(base_data))
my_vars <- base_data %>% colnames 
cat_vars <- my_vars %>% sapply(function(x){is.character(levels(base_data[[x]]))}) %>% .[which(.)] %>% names
my_tab <- CreateTableOne(vars = my_vars, data = base_data, factorVars = cat_vars,addOverall = T,includeNA=T)
tabMat <- print(my_tab, quote = FALSE, noSpaces = TRUE, printToggle = T,showAllLevels = TRUE)
tabMat %>% rownames()%>% lapply(function(x){gsub('\\.',' ',x)}) %>% as.character() %>% cbind(tabMat) %>% as.data.frame() %>% 
  flextable() %>%
  bold(j = '.', bold = TRUE, part = "body")%>%
  set_header_labels('.'='','level'='Level','p'='P-value') %>%
  autofit()%>%
  save_as_docx(path=paste('Tables','baseline_overall.docx',sep = '/'))
