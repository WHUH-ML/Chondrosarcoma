library(magrittr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(survival)
library(magrittr)
library(gtsummary)
library(forestplot)
library(data.table)
library(flextable)
get_data <- function(){
  origin_data <- readxl::read_xlsx('../data/Chondrosarcoma AYA site 4.2.xlsx')
  data <- origin_data %>% 
    filter(grepl('9220|9243',`ICD-O-3 Hist/behav`)) %>%
    filter(grepl('C40|C41',`Primary Site - labeled`)) %>%
    filter(`First malignant primary indicator`== 'Yes') %>%
    filter(`Survival months` != 0 & `Survival months` != 'Unknown') %>%
    mutate(`Age recode with single ages and 100+`= gsub('years|\\+','',`Age recode with single ages and 100+`))%>%
    subset(select = c(
      `Year of diagnosis`,
      `Age recode with single ages and 100+`,
      `Sex`,
      `ICD-O-3 Hist/behav`,
      `Primary Site - labeled`,
      `Derived AJCC Stage Group, 6th ed (2004-2015)`,
      `Grade (thru 2017)`,
      `RX Summ--Surg Prim Site (1998+)`,
      `RX Summ--Surg/Rad Seq`,
      `Chemotherapy recode (yes, no/unk)`,
      `CS tumor size (2004-2015)`,
      `Total number of in situ/malignant tumors for patient`,
      `CS extension (2004-2015)`,
      `CS mets at dx (2004-2015)`,
      `Survival months`,
      `Vital status recode (study cutoff used)`
    )) %>%
    setNames(
      c(
        'Year','Age','Gender','Histological type','Primary site','Stage','Grade','Surgery',
        'Radiotherapy','Chemotherapy','Tumor size','Number of tumors','Tumor extension','Distant metastasis',
        'Survival months','Status'
      )
    ) %>%
    mutate(
      `Tumor size` = replace(`Tumor size`,`Tumor size` %in% append(seq(989,999,1),'Blank(s)'),NA),
      Stage = replace(Stage,grepl('UNK|Blank',Stage),NA),
      Grade = replace(Grade,grepl('Unknown|Blank',Grade),NA),
      Surgery = replace(Surgery,grepl('90|99',Surgery),NA) ,
      `Tumor extension` = replace(`Tumor extension`,grepl('999|Blanks',`Tumor extension`),NA) ,
      `Distant metastasis` = replace(`Distant metastasis`,grepl('99|Blank',`Distant metastasis`),NA) 
    ) %>% 
    mutate(
      Year = Year %>% cut(breaks=c(-Inf, 2010, Inf),labels=c('2004-2010','2011-2015')),
      Age = as.numeric(Age), #%>% cut(breaks=c(-Inf,40,50,64,Inf),labels=c('≤ 40','> 40, ≤ 52','> 52, ≤ 64,','> 64')),
      Gender = factor(Gender,levels = c('Female', 'Male')),
      `Histological type` = `Histological type` %>% factor(labels=c('Conventional','Dedifferentiated')),
      `Primary site` = `Primary site` %>% factor(
        levels = c("C40.0-Long bones: upper limb, scapula, and associated joints", "C40.1-Short bones of upper limb and associated joints",
                   "C40.2-Long bones of lower limb and associated joints", "C40.3-Short bones of lower limb and associated joints", 
                   "C40.8-Overlap of bones, joints, and art. cartilage of limbs", "C40.9-Bone of limb, NOS", 
                   "C41.1-Mandible", "C41.2-Vertebral column", "C41.4-Pelvic bones, sacrum, coccyx and associated joints", 
                   "C41.8-Overlap bones, joints, and art. cartilage", 
                   "C41.9-Bone, NOS", "C41.3-Rib, sternum, clavicle and associated joints", "C41.0-Bones of skull and face and associated joints"),
        labels = c("Extremity", "Extremity",
                   "Extremity", "Extremity", 
                   "Extremity", "Extremity", 
                   "Axial skeleton", "Axial skeleton", "Axial skeleton",
                   "Other",  "Other", "Other", "Other")
      ),
      Stage = Stage %>% factor(
        levels = c("IA", "IB","IIA", "IIB","III",  "IVB",  "IVA", "IVNOS"),
        labels = c('I','I','II','II','III','IV','IV','IV')
      ),
      Grade = Grade %>% factor(
        levels = c("Well differentiated; Grade I","Moderately differentiated; Grade II",  
                   "Poorly differentiated; Grade III", "Undifferentiated; anaplastic; Grade IV"
        ),
        labels = c("Well differentiated","Moderately differentiated",  
                   "Poorly differentiated", "Undifferentiated")
      ),#c( 0, 15, 19, 25, 26, 30, 40, 41, 42, 50, 51, 52, 53, 54)) 
      Surgery = Surgery %>% factor(
        levels = c(0, 
                   15, 19, 25, 26,
                   30, 
                   40, 41, 42, 50, 51, 52, 53, 54
        ),
        labels = c('None',
                   rep('Local treatment',4),
                   'Radical excision with limb salvage',
                   rep('Amputation',8)
        )
      ),
      Radiotherapy = grepl('^No',Radiotherapy) %>% `!` %>% factor(labels = c('Not','Yes')),
      Chemotherapy = grepl('^Yes',Chemotherapy) %>% factor(labels = c('Not','Yes')),
      `Tumor size` = `Tumor size` %>% as.numeric(), #%>% cut(breaks=c(-Inf,50,100,Inf),labels=c('≤ 50 mm','> 50, ≤ 100 mm','> 100 mm')),
      `Number of tumors` = `Number of tumors` %>% as.numeric() %>% cut(breaks=c(-Inf,1,Inf),labels=c('1','> 1')),
      `Tumor extension` = `Tumor extension` %>% factor(
        levels = c("100", "200",
                   "600", "400",  "300", "350", "310",
                   "700", "800", "820", "850"
        ),
        labels = c(
          rep('No break in periosteum',2),
          rep('Extension beyond periosteum',5),
          rep('Further extension',4)
        )
      ),
      `Distant metastasis` = `Distant metastasis` %>% factor(
        levels = c("0",
                   "30","35", "40", "50",'53','60'
        ),
        labels = c(
          'Not',
          rep('Yes',6)
        )
      ),
      `Survival months` = `Survival months` %>% as.numeric,
      Status = Status %>% factor(levels = c('Alive','Dead'))
    )
  
  plot_distribution <- function(data_){
    plot_list <- list()
    for (colname in colnames(data_)){
      p <- ggplot(data_,aes(x=.data[[colname]])) + geom_bar() + 
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
        theme(axis.text.x = element_text(angle = 40,hjust=1,size = 5))
      plot_list <- append(plot_list,list(p))
    }
    grid.arrange(grobs = plot_list, ncol = 4)
  }
  # data %>% plot_distribution()
  data
  
}

data <- get_data() %>% data.frame(check.names = T)

#### Overall distrubution ####
overall <- tbl_summary(data,statistic = list(all_continuous() ~ "{mean} ({sd})"))


#### Univariate Cox regression analysis ####
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

##### Multivariate Cox regression analysis ####
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

#### Merge tables ####
tbl_merge(
  tbls = list(overall,unicox, multi_cox),
  tab_spanner = c("**Overall**", "**Univariate Cox**", "**Multivariate Cox**")
) %>% 
  bold_labels() %>%
  italicize_levels()%>%
  as_flex_table() %>%
  autofit()%>%
  bold(bold = TRUE, part = "header")%>%
  flextable::save_as_docx(path = 'Tables/Merge_Unicox_Multicox.docx')

include_cols <- colnames(data)
exclude_cols <- c('Year',"Radiotherapy", "Chemotherapy",'Number.of.tumors')

#### Impute data ####
library(missForest)
data_imputed <- missForest(
  data[colnames(data) %>% setdiff(exclude_cols) %>% setdiff(c('Status','Survival.months'))]
  )[["ximp"]] %>% cbind(
    data[c('Status','Survival.months')]
  )

#### Feature Selecting -- Correlation plot ------------------------------------
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

#### Save data ####
data_imputed %>% .[setdiff(names(.),exclude_cols)] %>% 
  mutate_at(.,sapply(.,function(x) length(levels(x))) %>% .[.<5] %>% names,~as.numeric(.)-1) %>%
  data.frame() %>% 
  write.csv('data/data_surv.csv',row.names = F)


