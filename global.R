# preparation for html href & src -----------------------------------------------
shiny::addResourcePath('www', here::here("www"))

# library -----------------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(DT)
library(shinycssloaders)
library(shinyjs)
library(shinyFeedback)
library(purrr)
library(stringr)
library(shinyscroll)
library(shinyhelper)
library(spsComps)
library(dplyr)
library(readxl)
library(readr)
library(survival)
library(survminer)
#library(survival)
library(reshape)
library(reshape2)
library(tibble)
library(countup)
library(emayili)
library(shinyalert)

# load data ------------------------------------------------------------
load("www/data/genelist.Rdata")#genelist
load("www/data/info_dt.Rdata") #info_agg_dt
load("www/data/finalgene_group.Rdata")#data

# 2.2 creat buttons in box2_KMtable_rna  ---------------------------------------------------------------------
create_btns <- function(x) {
  x %>%
    purrr::map_chr(~
                     paste0(
                       '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-info action_button" id="choosem_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-paintbrush"></i></button>
                   <button class="btn btn-default action-button btn-danger action_button" id="chooseb_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-paintbrush"></i></button></div>'
                     ))
}

# 2.2 load gene DT accordingly in box2_KMtable_rna -----------------------------------------
read_select_dt <- function (x) {
  file_path <- paste0('www/data/GeneDatatable/gene_',final_group$group[which(final_group$gene==x)],'.Rdata')
  load(file = file_path)
  subdt <- subdt[which(subdt$Gene == x),]
  
  # 设置参数以科学计数法输出数值型向量
  #options(scipen = 999)  # 禁止自动转换为小数点格式
  #options(digits = 10)  # 设置输出的最大有效数字位数
  subdt[, c(12, 13, 15, 16, 18, 19, 21, 22)] <- as.data.frame(lapply(subdt[, c(12, 13, 15, 16, 18, 19, 21, 22)], function(x) format(as.numeric(x), scientific = TRUE, digits =3)))
  #subdt[, c(12, 13, 15, 16, 18, 19, 21, 22)] <- as.data.frame(lapply(subdt[, c(12, 13, 15, 16, 18, 19, 21, 22)], function(x) as.numeric(formatC(x,format = "e", digits = 3))))
  subdt
}

# # datatable 科学计数法排序js
# js <- c(
#   "function(row, data, displayNum, index){",
#   "  if(index === 13 || index === 14|| index === 16|| index === 17|| index === 19|| index === 20|| index === 22|| index === 23){",  # Check if it's the specified columns (12, 13, 15, 16, 18, 19, 21, 22)
#   "  var x = data[1];",
#   "  $('td:eq(1)', row).html(x.toExponential(2));",
#   "}"
# )
# 自定义插件以确保科学数字按数值大小排序
# sci_sort_plugin <- JS(
#   "jQuery.extend(jQuery.fn.dataTableExt.oSort, {",
#   "  'scientific-pre': function(a) {",
#   "    return parseFloat(a.replace(/[^0-9eE.\\-]/g, ''));",
#   "  },",
#   "  'scientific-asc': function(a, b) {",
#   "    return a - b;",
#   "  },",
#   "  'scientific-desc': function(a, b) {",
#   "    return b - a;",
#   "  }",
#   "});"
# )
#科学计数法展示
# show_sci <- JS(
#   "function(row, data) {",
#   "for (i = 1; i < data.length; i++) {",
#   "if (data[i]>1000 | data[i]<1){",
#   "$('td:eq('+i+')', row).html(data[i].toExponential(1));",
#   "}",
#   "}",
#   "}")
# # 列索引列表，要格式化的列为第12、13、15、16、18、19、21和22列
# columns_to_format <- c(12, 13, 15, 16, 18, 19, 21, 22)

# # 定义一个 JavaScript 回调函数
# js <- paste0(
#   "function(row, data, displayNum, index){",
#   paste(sapply(columns_to_format, function(col_index) {
#     sprintf(
#       "if (index === %d) { var x = data[%d]; $('td:eq(%d)', row).html(x.toExponential(2)); }",
#       col_index - 1, col_index - 1, col_index - 1
#     )
#   }, simplify = FALSE),
#   "}"
#   )
# )
# 2.4 PLOT in box4_KMplot_methy ----------------------------------------------------------------
methy_km_plot <- function(cancertype, gse, gpl, survival, gene, probe,cutoff, col_1, col_2) {
  tryCatch(
    { 
      exp_file <- paste0("www/data/", cancertype, "/", gse, "/", survival, "_", cutoff, ".Rdata")
      cli_file <- paste0("www/data/", cancertype, "/", gse,"/",survival,"_cli.Rdata")
      sur_index<-c(survival,paste0(survival,'_time'))
      load(exp_file)
      load(cli_file)
      
      cli<-cli[,sur_index]
      cli[,1]<-as.numeric(cli[,1]);cli[,2]<-as.numeric(cli[,2])
      cli<-na.omit(cli)
      methy_mat <- methy_mat[probe,]
      methy_mat <- t(methy_mat)
      methy_mat <- t(methy_mat)
      colnames(methy_mat) <- probe
      
      if(identical(rownames(cli),rownames(methy_mat))){
        rt<-cbind(cli,methy_mat)
      }else{
        both_pt<-intersect(rownames(cli),rownames(methy_mat))
        cli<-cli[both_pt,]
        methy_mat<-methy_mat[both_pt,]
        rt<-cbind(cli,methy_mat)
      }
      
      colnames(rt)[1:2]<-c('status','time')
      my.surv <- Surv(as.numeric(rt$time), as.numeric(rt$status))
      
      final_probe<-data.frame(
        row.names = paste0(paste0('A',c(1:c(ncol(rt)-2)))),
        probe = colnames(rt)[3:ncol(rt)],
        probe1 = paste0(paste0('A',c(1:c(ncol(rt)-2)))))
      
      colnames(rt)[3:ncol(rt)]<-paste0(paste0('A',c(1:c(ncol(rt)-2))))
      
      probe2 <-colnames(rt)[3]
      group = rt[,probe2]
      data = cbind(rt[,1:2],group)
      group <- factor(group, levels = c(0,1))
      fit <- surv_fit(my.surv ~ group, data = data)
      data.survdiff <- survdiff(my.surv ~ group)
      p.val = 1 - pchisq(data.survdiff$chisq, length(data.survdiff$n) - 1)
      HR = (data.survdiff$obs[2]/data.survdiff$exp[2])/(data.survdiff$obs[1]/data.survdiff$exp[1])
      up95 = exp(log(HR) + qnorm(0.975)*sqrt(1/data.survdiff$exp[2]+1/data.survdiff$exp[1]))
      low95 = exp(log(HR) - qnorm(0.975)*sqrt(1/data.survdiff$exp[2]+1/data.survdiff$exp[1]))
      HR <- paste("HR: ", format(HR,scientific= T ,digits = 3), sep = "")
      CI <- paste("95%CI: ", paste(format(low95,scientific= T ,digits = 3), format(up95,scientific= T ,digits = 3),sep = " ~ "), sep = "")
      
      if(nchar(probe)>52){
        probe_re <- paste0(substr(probe,1,52),"\n",substring(probe,53))
      }else{
        probe_re <- probe
      }
      
      ggsurvplot(fit,
                 data = data,
                 # conf.int = TRUE,
                 # conf.int.style = "ribbon",
                 risk.table = "nrisk_cumcensor",#nrisk_cumcensor;abs_pct;nrisk_cumevents
                 tables.theme = theme_cleantable(),
                 tables.height = 0.2,
                 fontsize= 3,
                 tables.y.text=F,
                 surv.median.line='hv',
                 legend=c(0.8,0.8),
                 legend.title = paste0(gene,'_',probe_re),
                 legend.labs = c(paste0("Low"," (",fit$n[1],")"),
                                 paste0("High"," (",fit$n[2],")")),
                 xlab=paste0(sur_index[1]," Time (Months)"),
                 ylab=paste0("Survival Rate for ",cancertype,"\n",gse,'-',gpl),
                 pval = paste(pval = paste("p = ",format(p.val,scientific= T ,digits = 3), sep = ""),
                              HR, CI, sep = "\n"),
                 pval.size=3,
                 palette = c(col_1, col_2),
                 censor=T,
                 censor.shape="|",
                 censor.size=2.5,
                 ggtheme=theme(axis.text.x = element_text(face="bold", color="black", size=8),    #font size
                               axis.text.y = element_text(face="bold",  color="black", size=8),
                               axis.title.x = element_text(face="bold", color="black", size=8),
                               axis.title.y = element_text(face="bold",color="black", size=8),
                               legend.text= element_text(face="bold", color="black", size=8),
                               legend.title = element_text(face="bold", color="black", size=8),
                               axis.line=element_line(color = "black"),
                               panel.background = element_blank(),
                               plot.title=element_text(face="bold", color="black",size=8)))
    },
    error = function(e){
      d <- data.frame(x=1, y=1, lab=c('Dear user, your current analysis encountered an error,\nif the same error is still reported after re-analysis,\nplease send your analysis to the following email\n(shoshanashi@i.smu.edu.cn), we will update the web tool\nand give you feedback as soon as possible.'))
      ggplot(d, aes(x, y)) + 
        geom_text(aes(label=lab), size=6)+
        theme(axis.text.x = element_blank(),    #font size
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.text= element_blank(),
              legend.title =element_blank(),
              axis.line=element_blank(),
              axis.ticks = element_blank(),
              panel.background = element_blank(),
              plot.title=element_blank())
    }
  )
}

# 4 Comment box -----------------------------------------------------------
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}