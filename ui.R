
library(shiny)

navbarPage(
  id = 'path_navbar',
  title = div(
    HTML('<span style="font-size:180%;color:white;font-weight:bold;"> PDMSA</span></a>'),
    tags$style(style = 'position:absolute; right:42px;'),
    tags$style(type="text/css", ".navbar-brand {padding-top: 23px;}"),
    tags$style(HTML("#panel1{font-size: 18px}")),
    tags$style(HTML("#panel2{font-size: 18px}")),
    tags$style(HTML("#panel3{font-size: 18px}")),
    tags$style(HTML("#panel4{font-size: 18px}"))
  ),
  theme = shinytheme('sandstone'),
  fluid = T,#TRUE to use a fluid layout
  windowTitle = "PDMSA",#the browser window title
  ###### 插入依赖项 ######
  header = tagList(
    useShinydashboardPlus(),
    useShinyjs(),
    use_shinyscroll()
  ),
  #----------------tabPanel1.home page----------------------
  
  tabPanel(h4(id = "panel1", "Home"),
           value = "panel1",
           fluidRow(
             column(12,
                    tags$h1("PDMSA"),
                    h4("PDMSA (Pan-cancer DNA Methylation Survival Analysis) is a large-scale interactive web tool dedicated to pan-cancer survival analysis and visualization by using the site of DNA methylation. With PDMSA, users can quickly explore the impact of target DNA methylation on survival outcomes in different tumors, assisting clinicians and researchers in further investigating the mechanism of tumor development and improving clinical decision-making.",
                       br())
             )
           ),
           tags$hr(),
           fluidRow(column(7,
                           align = "center",
                           tags$img(src="www/PANCAN.png", width="90%", alt="Something went wrong...Please refresh page.")),
                    tags$style(HTML(".intro_text {margin-top: 30px;}")),
                    column(5,
                           div(class = "intro_text",
                               tags$h3(strong("How does PDMSA work?")),
                               tags$br(),
                               h4("STEP1: Select Data to Be Processed"),
                               h4("STEP2: View Analysis Results"),
                               h4("STEP3: Costomize and Download Your Plot"),
                               tags$br(),
                               actionButton(inputId = "homebtn", label = h4(strong("Get Started Now >>"))))
                    )
           ),
           tags$hr(),
           fluidRow(column(5,
                           HTML('<h3><b>Updates</b></h3>',
                                '<h4>05/11/24 Version 1.0.0 of PDMSA released.</h4>')),
           
                    column(6,
                           class = 'footer-container',
                           HTML('<div style="text-align: center;"><div style="display:inline-block; width: 50px; height: 50px;"><script type="text/javascript" id="clstr_globe" src="//clustrmaps.com/globe.js?d=duY9lga7PLc62YWaR1lgbE3f_kxEBK6Svn4nDW2BGNc"></script></div> </div>
                             <div style="text-align: center;"><p>Copyright © 2024. All rights reserved.</p></div>'))
                    
           ),
           # fluidRow(column(6,
           #                class = 'footer-container',
           #               HTML('<div style="text-align: right;"><div style="display:inline-block;"><script type="text/javascript" src="//rf.revolvermaps.com/0/0/6.js?i=5s51uibodr2&amp;m=0&amp;c=ff0000&amp;cr1=ffffff&amp;f=times_new_roman&amp;l=1" async="async"></script></div> </div>
           #                   <div style="text-align: center;"><p>Copyright © 2024. All rights reserved.</p></div>'))),
           tags$style(HTML(".footer-container {width: 100%; height: 100%; /*for nothing*/
                             bottom: 0; /*for nothing*/
                             left: 0; /*for nothing*/
                             position: static;
                             padding: 0;/*IMPORTANT! W/0 PADDING*/;
                             }")) 
           
  ),
  
  # 2. tab_panel_2_methy ------------------------------------------------------
  tabPanel(h4(id = "panel2", "PLOT!"),
           value = "panel2",
           # 2.1.1 box1: select & filter input data ----------------------------------------
           fluidRow(
             box(id = "methy_KMinfoselectbox",
                 title = strong("STEP1: Select Data to Be Processed", style = 'font-size:18px;color:white;'),
                 icon = icon("upload"),
                 collapsible = FALSE,
                 collapsed = FALSE,
                 status = "purple", 
                 solidHeader = TRUE, width = 12,
                 fluidRow(
                   column(4, selectizeInput(inputId = "select.gene",
                                            label = "Select a gene symbol",
                                            choices = NULL, 
                                            multiple = FALSE,
                                            options = list(
                                              placeholder = "Type the gene name of your interests",
                                              maxItems = 1
                                            )) %>% helper(type = "inline",
                                                          title = "GENE SELECTION",
                                                          content = c("Please make sure you type the <b>right</b> gene symbol.",
                                                                      "You can check it further in the ABOUT page."))),
                   column(6, 
                          actionButton(inputId = "submitdata_btn", label = "Analyze!", style = 'margin-top:25px'),
                          downloadButton(outputId = "downloaddata_btn", class = "downloadbtn", label = "DOWNLOAD RESULTS", style = 'margin-top:25px')
                   )
                 )
             )
           ),
           # 2.1.2 box2: show results datatable ----------------------------------------
           fluidRow(
             shiny::includeScript("www/script.js"),# js for buttons
             # tags$style(HTML("div.datatables {width: auto; height: auto;}")),
             # datatable 科学计数法排序实现
    #          tags$head(
    #            tags$script(HTML('
    #   $(document).ready(function() {
    #     $.fn.dataTable.ext.type.order["scientific-pre"] = function (data) {
    #       return parseFloat(data.replace(/[,]/g,""));
    #     };
    #   });
    # '))),
    # useShinyjs(),
    # tags$head(
    #   HTML(
    #     "
    #   <script src=\"https://github.com/DataTables/Plugins/tree/master/sorting/scientific.js\"></script>
    #   "
    #   )
    # ), 
             box(id = "methy_tablebox",
                 title = strong("STEP2: View Analysis Results", style = 'font-size:18px;color:white;'),
                 icon = icon("table"),
                 collapsible = TRUE,
                 collapsed = TRUE,
                 status = "navy", 
                 solidHeader = TRUE, width = 12,
                 # fluidRow(column(12, Spinner(id = "test", size = 3, label = "Loading, please wait..."))),
                 # fluidRow(column(12, dataTableOutput("kmresultdt")))
                 fluidRow(column(12, shinycssloaders::withSpinner(dataTableOutput("kmresultdt"), color = "blue")))
             )
           ),
           # 2.1.3 box3: show the plot and download ----------------------------------------
           fluidRow(
             tags$style(HTML(".panel {border: 0;-webkit-box-shadow: 0 0 0;box-shadow: 0 0 0;}
                           .panel-default > .panel-heading {color: blue;background-color: transparent;border-color: transparent;}")),
             box(id = "methy_KMplotbox",
                 title = strong("STEP3: View, Costomize and Download Your Plot", style = 'font-size:18px;color:white;'),
                 icon = icon("square-check"),
                 collapsible = TRUE,
                 collapsed = TRUE,
                 status = "navy", 
                 solidHeader = TRUE, width = 12,
                 fluidRow(column(12,wellPanel(htmlOutput("DTselectinfo")))),
                 fluidRow(column(1, downloadButton(outputId = "download_KMmethy", label = "Download", icon=icon("download")))),
                 br(),
                 fluidRow(column(3, shinyBS::bsCollapsePanel("CUSTOMIZE OPTIONS>>",
                                                             fluidRow(column(6, colorPickr(inputId = "methy_low_color", label = "LOW:", selected = "#c35f50", theme = "monolith", update ="change")),
                                                                      column(6, colorPickr(inputId = "methy_high_color", label = "HIGH:", selected = "#477aae", theme = "monolith", update ="change"))),
                                                             fluidRow(column(12, radioButtons(inputId = "cutoff_KMmethy", label = "Choose a method for the cutpoint", choiceNames = list("Median", "Optimal (maximally selected rank statistics)"), choiceValues = list("median", "bestoff"),inline = FALSE, selected = "m"))),
                                                             fluidRow(column(2, actionButton(inputId = "customize_KMmethybtn", label = "proceed!")),
                                                                      column(2, offset = 2, actionBttn(inputId = "customize_defalutbtn", label = "Default", style = "minimal", color = "primary", size = "sm")))
                 )),
                 column(5, offset = 1, shinycssloaders::withSpinner(plotOutput("KMmethy_plot"), color = "blue")))
             )
           )
  ),
 
# 3. tab_panel_3_DATA -------------------------------------------------
tabPanel(h4(id = "panel3", "Data"),
         value = "panel3",
         fluidRow(
           box(id = "methy_infotbbox",
               title = strong("DATASETS DESCRIPTION", style = 'font-size:18px;color:white;'),
               icon = icon("table"),
               collapsible = FALSE,
               collapsed = FALSE,
               status = "navy", 
               solidHeader = TRUE, width = 12,
               fluidRow(column(12, shinycssloaders::withSpinner(dataTableOutput("kminfodt"), color = "blue")))
           )
         )),
# 4. tab_panel_4_about ----------------------------------------------------
tabPanel(h4(id = "panel4", "About"),
         value = "panel4",
         fluidRow(
           box(id = "contactbox",
               title = strong("Contact", style = 'font-size:18px;color:white;'),
               icon = icon("users"),
               collapsible = FALSE,
               collapsed = FALSE,
               status = "purple", 
               solidHeader = TRUE, width = 12,
               HTML('<p>Should you have any questions, please feel free to contact us.</p>',
                    '<p>Peng Luo: <a href="mailto:luopeng@smu.edu.cn">luopeng@smu.edu.cn</a> </p>',
                    '<p>Ying Shi: <a href="mailto:shoshanashi@i.smu.edu.cn">shoshanashi@i.smu.edu.cn</a></p>',
                    '<p>Weiwei Guo: <a href="mailto:guoweiwei@i.smu.edu.cn">guoweiwei@i.smu.edu.cn</a></p>',
                    '<p>ShanShan Wu: <a href="mailto:wushanshan@i.smu.edu.cn">wushanshan@i.smu.edu.cn</a></p>',
                    '<br/>',
                    '<p>Our lab has a long-standing interest in cancer biomedical research and bioinformatics. We recently developed several other Shiny web tools focusing on solving various scientific questions.</p>',
                    '<p><a href="http://www.camoip.net" target="_blank">CAMOIP</a>: A Web Server for Comprehensive Analysis on Multi-omics of Immunotherapy in Pan-cancer. doi: <a href="https://doi.org/10.1093/bib/bbac129" target="_blank">10.1093/bib/bbac129</a></p>',
                    '<p><a href="https://smuonco.Shinyapps.io/Onlinemeta/" target="_blank">Onlinemeta</a>: A Web Server For Meta-Analysis Based On R-shiny. doi: <a href="https://doi.org/10.1101/2022.04.13.488126" target="_blank">10.1101/2022.04.13.488126[preprint]</a></p>',
                    '<p><a href="https://smuonco.shinyapps.io/PanCanSurvPlot/" target="_blank">PanCanSurvPlot</a>: A Large-scale Pan-cancer Survival Analysis Web Application. doi: <a href="https://doi.org/10.1101/2022.12.25.521884" target="_blank">10.1101/2022.12.25.521884[preprint]</a></p>',
                    '<p><a href="https://smuonco.shinyapps.io/PESSA/" target="_blank">PESSA</a>: A web tool for pathway enrichment score-based survival analysis in cancer. doi: <a href="https://doi.org/10.1371/journal.pcbi.1012024" target="_blank">10.1371/journal.pcbi.1012024</a></p>',
                    '<p><a href="https://smuonco.shinyapps.io/CPADS/" target="_blank">CPADS</a>: A web tool for comprehensive pancancer analysis of drug sensitivity. doi: <a href="https://doi.org/10.1093/bib/bbae237" target="_blank">10.1093/bib/bbae237</a></p>',
               )
           )
         ),
          fluidRow(
            box(id = "tutorialbox",
                title = strong("Tutorial Video", style = 'font-size:18px;color:white;'),
                icon = icon("youtube"),
                collapsible = TRUE,
                collapsed = TRUE,
                status = "navy", 
                solidHeader = TRUE, width = 12,
                fluidRow(column(6, offset=3, HTML('<iframe width="560" height="315" src="https://youtu.be/tI1vPfYlk1k" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))),
                fluidRow(column(6, offset=3, HTML('<p>For users who can not access YouTube, the tutorial video is also available on <a href="https://www.bilibili.com/video/BV1h4qmYnE7Q/" target="_blank">Bilibili</a>.</p>')))
            )
          ),
         fluidRow(
           box(id = "commentbox",
               title = strong("Comment Box", style = 'font-size:18px;color:white;'),
               icon = icon("envelope"),
               collapsible = TRUE,
               collapsed = TRUE,
               status = "navy", 
               solidHeader = TRUE, width = 12,
               useSweetAlert("borderless", ie = F),#"sweetalert2", "minimal", "dark", "bootstrap-4", "material-ui", "bulma","borderless"
               fluidRow(column(8,
                               textInput(inputId = "contact", label = "Name/Email (optional)", width = "60%"),
                               textAreaInput(inputId = "comment", label = labelMandatory("Comment"), placeholder = "Enter your comment here", width = "100%", height = "100px"),
                               actionButton("submit_commentbtn", "Submit Comment")
               ))
           )
         ),
         fluidRow(
           box(id = "updatesbox",
               title = strong("Update History", style = 'font-size:18px;color:white;'),
               icon = icon("file-pen"),
               collapsible = TRUE,
               collapsed = FALSE,
               status = "navy", 
               solidHeader = TRUE, width = 12,
               HTML('<p>05/11/24 Version 1.0.0 of PDMSA released.</p>'
               )
           )
         ),
         fluidRow(
           box(id = "FAQbox",
               title = strong("FAQ", style = 'font-size:18px;color:white;'),
               icon = icon("comments"),
               collapsible = TRUE,
               collapsed = FALSE,
               status = "navy", 
               solidHeader = TRUE, width = 12,
               HTML('<h5> <b>1. Why does the gene symbol I type in return no results?</b> </h5>',
                    '<p>For data included in PDSMA, the methylation microarrays are Infinium HumanMethylation450K BeadChips, and a whole list of methylation probes and gene mappings is available  <a href="www/data/genelist_re.csv" download="genelist.csv">here</a>.</p>',
                    '<h5> <b>2. What are the full forms of the survival outcomes provided?</b> </h5>',
                    '<p>Altogether, there are 4 different survival outcomes available. Their abbreviations and corresponding full forms are shown below.</p>',
                    '<p> DMFS: Distant Metastasis Free Survival</p>',
                    '<p> MFS: Metastasis Free Survival</p>',
                    '<p> OS: Overall Survival</p>',
                    '<p> PFS: Progression Free Survival</p>',
                    '<h5> <b>3. Why is there no median survival line shown in the K-M plot I have made?</b> </h5>',
                    '<p> Because of the specific cutoff point you chose, the median survival has not yet been reached, and more than half of the patients are still alive.</p>',
                    '<h5> <b>4. Details of the CGI and Feature in the results table?</b> </h5>',
                    '<p> CGI: The position of the CpG site in reference to the island: Island, N_Shore or S_Shore (0-2 kb upstream or downstream from CGI), or N_Shelf or S_Shelf (2-4 kbp upstream or downstream from CGI).</p>',
                    '<p> Feature: Description of the regulatory feature referenced in Regulatory_Feature_Name, as provided by the Methylation Consortium.</p>',
                    '<p> Feature including: Gene_Associated; Gene_Associated_Cell_type_specific; NonGene_Associated; Promoter_Associated; Promoter_Associated_Cell_type_specific; Unclassified; Unclassified_Cell_type_specific.</p>'
               )
           )
         )
)
)
