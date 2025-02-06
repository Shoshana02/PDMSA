
library(shiny)


function(input, output, session) {
  # ----------------1 homepage ------------------------------------------------------------------
  observeEvent(input$homebtn, {
    updateNavbarPage(session, "path_navbar", selected = "panel2")
  })
  # 2.1 box1_selectinfo_methy -----------------------------------------------------
  updateSelectizeInput(session, inputId = "select.gene", 
                       choices = genelist$Gene, 
                       selected = "TP53",
                       server = TRUE)
  
  observe_helpers()#Function to show a modal dialog, observing each of the help icons in the app.
  
  ## if all data inputs are not blank, run following codes. 
  submitdata_btn_check <- reactive({
    !input$select.gene == ""
  })
  observe({
    toggleState(id = "submitdata_btn",
                condition = submitdata_btn_check())
  })
  
  ## shinyjs disables all boxes not used yet
  shinyjs::disable(id = "methy_tablebox")
  shinyjs::disable(id = "methy_plotbox")
  shinyjs::disable(id = "downloaddata_btn")
  
  # 2.2 box2_methy_table-----------------------------------------------------
  # after clicking the SUBMIT button in box1 and pass the data check, modify the form displayed according to the input
  dataset <- eventReactive(input$submitdata_btn, {
    subdt <- read_select_dt(input$select.gene) %>% 
      mutate(across(c(Cancer, Dataset, Platform, Survival,CGI,Feature,Strand), factor)) %>% 
      dplyr::arrange(Cancer) %>% #sort according to CA
      mutate(PLOT = create_btns(1:nrow(.))) %>%
      dplyr::select(PLOT, everything()) %>% #put the colulmn PLOT in front of others
      tibble::remove_rownames() %>%
      tibble::column_to_rownames('PLOT')
    
  })
  
  output$downloaddata_btn <- downloadHandler(
    filename = function(){
      paste0(input$select.gene,"_ResultsDatatable.csv")
    },
    content = function(file){
      sep <- ","
      write.table(dataset(), file, sep=sep, row.names = FALSE)
    }
  )
  
  ## after clicking the SUBMIT button in box1, expand box2 wiz Buttons
  observeEvent(input$submitdata_btn,{
    if (input$methy_tablebox$collapsed) {
      updateBox("methy_tablebox", action = "toggle")
    }
    shinyjs::enable(id = "methy_tablebox")
    shinyjs::enable(id = "downloaddata_btn")
    table_frame <- htmltools::withTags(table(class = 'display',
                                             thead(
                                               tr(
                                                 th(rowspan = 2, 'Median|Optimal'),
                                                 th(rowspan = 2, 'ID'),
                                                 th(rowspan = 2, 'Probe'),
                                                 th(rowspan = 2, 'Gene'),
                                                 th(rowspan = 2, 'Cancer'),
                                                 th(rowspan = 2, 'Dataset'),
                                                 th(rowspan = 2, 'Platform'),
                                                 #th(rowspan = 2, 'Treatment'),
                                                 th(rowspan = 2, 'Survival'),
                                                 th(rowspan = 2, 'CHR'),
                                                 th(rowspan = 2, 'Strand'),
                                                 th(rowspan = 2, 'Feature'),
                                                 th(rowspan = 2, 'CGI'),
                                                 th(class = 'dt-center', colspan = 3, 'KM results with median cutpoint'),
                                                 th(class = 'dt-center', colspan = 3, 'KM results with optimal cutpoint'),
                                                 #th(class = 'dt-center', colspan = 3, 'COX results (continuous)'),
                                                 th(class = 'dt-center', colspan = 3, 'COX results with median cutpoint'),
                                                 th(class = 'dt-center', colspan = 3, 'COX results with optimal cutpoint'),
                                                 tr(lapply(rep(
                                                   c('P value','HR', '95% CI' ), 4
                                                 ), th))
                                               )
                                             )))
    output$kmresultdt <- DT::renderDataTable({
      req(input$submitdata_btn)## WIZ OR W/O THIS LINE THE SHINY CAN WORK ANYWAY
      Sys.sleep(0.5) # add a larger number if spinner does not show
      datatable(
        isolate(dataset()),
        container = table_frame,
        escape = FALSE, #don't excape any HTML in the table (i.e., the action button)
        filter = "top",
        # callback=JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Filter" )'),
        rownames = TRUE,
        extensions = "FixedColumns",
        selection = "none", #turn off row selection function otherwise u will select that row when clicking on the action button
        options = list(
          #rowCallback = JS(js),
          #extensions ="scientific",
          #rowCallback = show_sci,
          #initComplete = JS(sci_sort_plugin),  # 初始化时加载自定义排序插件
          #rowCallback = JS(js),
          # processing = FALSE,
          # initComplete = JS("function(setting, json) { $('div.ms-Spinner.root-54').remove(); }"),
          # dom = 'Bfltp', #defines the position of elements around the table
          searchHighlight = TRUE,
          search = list(regex = TRUE, caseInsensitive = TRUE),
          pageLength = 10,
          scrollX = TRUE,
          autoWidth = TRUE,
          fixedColumns = list(leftColumns = 1),
          columnDefs = list(list(className = 'dt-center', targets = "_all"),
                            list(width = '120px', targets = c(0)), ## notice: the first column is c(0)
                            list(width = '60px', targets = c(3)),
                            list(width = '100px', targets = c(4)),## !!!notice: the CA column is c(?)
                            list(width = '60px', targets = c(5)),
                            list(width = '60px', targets = c(6)),
                            list(width = '60px', targets = c(7)),
                            list(width = '60px', targets = c(8)),
                            list(width = '60px', targets = c(9)),
                            list(width = '60px', targets = c(10)),
                            list(width = '60px', targets = c(12)),
                            list(width = '60px', targets = c(15)),
                            list(width = '60px', targets = c(18)),
                            list(width = '60px', targets = c(21)),
                            #list(width = '70px', targets = c(23)),
                            list(searchable = FALSE, targets = c(12:23)), # disable the filter function in columns "HR/CI/P"
                            #list(searchable = TRUE, targets = c(2)),
                            list(visible = FALSE, searchable = FALSE, targets = c(1)))## hide the column "id"
                            #list(targets = c(12, 13, 15, 16, 18, 19, 21, 22), type = "scientific")),  # 对第二列应用自定义的科学数字排序
                            #list(targets = c(12, 13, 15, 16, 18, 19, 21, 22), orderDataType = 'scientific')),\  # 使用自定义的排序类型
          #serverSide = F
          )
      )
    })
  })
  # 2.3 box3_KMcustomize_methy  -----------------------------------------------------
  ## after clicking the button in DT, Upload the number of rows of data selected and the corresponding id to the server for processing
  dt_id_split_re <- eventReactive(input$current_id, {
    d <- dataset()
    dt_row <- which(stringr::str_detect(rownames(d), pattern = paste0("\\b", input$current_id, "\\b")))
    dt_id <- as.character(d[dt_row, 1])
    dt_id_split <- strsplit(dt_id, split = "_")
  })
  # genesymbol_show <- eventReactive(input$current_id, {
  #   d <- dataset()
  #   dt_row <- which(stringr::str_detect(rownames(d), pattern = paste0("\\b", input$current_id, "\\b")))
  #   dt_id <- as.character(d[dt_row, 3])
  #   dt_id
  # })
  shiny::observeEvent(input$current_id, {
    shinyalert(
      title = "Please wait a little while",
      text = "It will take some time to draw, please be patient for a short while.",
      size = "s", 
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
    dt_id_split <- dt_id_split_re()
    if (!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "choosem")) {
      output$DTselectinfo <- renderText(paste("You are interested in <b>", dt_id_split[[1]][1], "(<b>", dt_id_split[[1]][2], "</b>)","</b>expression in <b>", dt_id_split[[1]][3], "</b>.", 
                                              "<br>", "The gene set you have chosen is <b>", dt_id_split[[1]][4], "</b>. Its survival outcome is <b>", dt_id_split[[1]][6], "</b>.",
                                              "<br>", "The cut-off method chosen is <b>Median</b>.",
                                              "<br>",
                                              "<br>", "<b> You can switch to the OPTIMAL cut-off method by clicking the RED button alongside. More colors can be customized below. </b>"))
      methy_plot <- methy_km_plot(cancertype = dt_id_split[[1]][3],
                              gse = dt_id_split[[1]][4],
                              gpl = dt_id_split[[1]][5],
                              survival = dt_id_split[[1]][6],
                              gene = dt_id_split[[1]][1],
                              probe = dt_id_split[[1]][2],
                              #gene_show = genesymbol_show(),
                              cutoff = "median",
                              col_1 = "#c35f50",
                              col_2 = "#477aae")
      output$KMmethy_plot <- renderPlot({methy_plot})
      output$download_KMmethy<- downloadHandler(
        filename = function(){
          paste("KM_methy_",dt_id_split_re()[[1]][1],".pdf",sep="")
        },
        content = function(file) {
          cairo_pdf(file)
          print(methy_plot)
          dev.off()
        }
      )
    }
    if (!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "chooseb")) {
      output$DTselectinfo <- renderText(paste("You are interested in <b>", dt_id_split[[1]][1], "(<b>", dt_id_split[[1]][2], "</b>)","</b>expression in <b>", dt_id_split[[1]][3], "</b>.", 
                                              "<br>", "The gene set you have chosen is <b>", dt_id_split[[1]][4], "</b>. Its survival outcome is <b>", dt_id_split[[1]][6], "</b>.",
                                              "<br>", "The cut-off method chosen is <b>OPTIMAL</b>.",
                                              "<br>",
                                              "<br>", "<b> You can switch to the MEDIAN cut-off method by clicking the BLUE button alongside. More colors can be customized below. </b>"))
      methy_plot <- methy_km_plot(cancertype = dt_id_split[[1]][3],
                                  gse = dt_id_split[[1]][4],
                                  gpl = dt_id_split[[1]][5],
                                  survival = dt_id_split[[1]][6],
                                  gene = dt_id_split[[1]][1],
                                  probe = dt_id_split[[1]][2],
                              #gene_show = genesymbol_show(),
                              cutoff = "bestoff",
                              col_1 = "#c35f50",
                              col_2 = "#477aae")
      output$KMmethy_plot <- renderPlot({methy_plot})
      output$download_KMmethy<- downloadHandler(
        filename = function(){
          paste("KM_methy_",dt_id_split_re()[[1]][1],".pdf",sep="")
        },
        content = function(file) {
          cairo_pdf(file)
          print(methy_plot)
          dev.off()
        }
      )
    }
    if (input$methy_KMplotbox$collapsed) {
      updateBox("methy_KMplotbox", action = "toggle")
    }
    shinyjs::enable(id = "methy_KMplotbox")
    scroll("methy_KMplotbox", block = "end")
    # runjs('
    #   window.scrollTo(0, document.body.scrollHeight);
    # ')
  })
  
  observeEvent(input$customize_KMmethybtn, {
    dt_id_split <- dt_id_split_re()
    methy_plot <- methy_km_plot(cancertype = dt_id_split[[1]][3],
                            gse = dt_id_split[[1]][4],
                            gpl = dt_id_split[[1]][5],
                            survival = dt_id_split[[1]][6],
                            gene = dt_id_split[[1]][1],
                            probe = dt_id_split[[1]][2],
                            #gene_show = genesymbol_show(),
                            cutoff = input$cutoff_KMmethy,
                            col_1 = input$methy_low_color,
                            col_2 = input$methy_high_color)
    output$KMmethy_plot <- renderPlot({methy_plot})
    output$download_KMmethy<- downloadHandler(
      filename = function(){
        paste("KM_methy_",dt_id_split_re()[[1]][1],".pdf",sep="")
      },
      content = function(file) {
        cairo_pdf(file)
        print(methy_plot)
        dev.off()
      }
    )
  })
  
  observeEvent(input$customize_defalutbtn, {
    updateColorPickr(session, inputId = "methy_low_color", value = "#c35f50")
    updateColorPickr(session, inputId = "methy_high_color", value = "#477aae")
    updateRadioButtons(session, inputId = "cutoff_KMmethy", selected = "median")
  })
  
  # 3 box_KMinfodt_box  -----------------------------------------------------
  output$kminfodt <- renderDT({
    Sys.sleep(0.1)
    datatable(
      isolate(info_dt),
      escape = FALSE,#don't excape any HTML in the table (i.e., the action button)
      filter = c("top"),
      callback=JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Search" )'),
      rownames = FALSE,
      extensions = c('Buttons', 'FixedColumns'),
      selection="none",#turn off row selection function otherwise u will select that row when clicking on the action button
      options = list(
        # processing = FALSE,
        dom = 'Bfltp',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        searchHighlight = TRUE,
        search = list(regex = TRUE, caseInsensitive = TRUE),
        pageLength = 20,
        scrollX = TRUE,
        autoWidth = TRUE,
        fixedColumns = list(leftColumns = 1),
        columnDefs = list(list(className = 'dt-center', targets = "_all"),
                          list(width = '150px', targets = c(0)) ## !!!notice: the CA column is c(0)
        )
      )
    )
  })
  
  # 4 Comment box -----------------------------------------------------------
  observeEvent(input$submit_commentbtn, {
    
    req(input$comment)
    
    smtp <- emayili::server(
      host = "smtp.163.com",
      port = 25,
      username = "shiny_luopeng@163.com",
      password = "LFZKTEZQQLXDTOGK"
    )
    
    email <- envelope() %>%
      from("shiny_luopeng@163.com") %>%
      to("shiny_luopeng@163.com")%>%
      cc("shoshanashi@i.smu.edu.cn") %>%
      cc("guoweiwei@i.smu.edu.cn") %>%
      cc("wushanshan@i.smu.edu.cn") %>%
      cc("wushanshan@i.smu.edu.cn") %>%
      cc("smuyanghong@i.smu.edu.cn") %>%
      subject(paste("PDMSA-SHINY_FEEDBACK: ", input$contact))%>%
      text(paste(input$comment, "from:", input$contact))
    
    smtp(email)
    
    show_alert(
      title = "Success",
      text = "Thanks, your response was submitted successfully! We will get back to you as soon as possible.",
      type = "success"
    )
  })
  
}