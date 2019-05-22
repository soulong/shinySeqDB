
# server funciton
server <- function(input, output, session) {

if(file.exists(default_dbPath)) {
    # read existing db
    print("Read existing db")
    db <<- readRDS(default_dbPath)
  } else {
    db <<- tibble()
  }

  string_wrap <- function(stringlist, len=10) {
    re_s <- c()
    for(i in stringlist){
      re <- i %>%
        as.character() %>%
        str_extract_all(paste0(".{", len, "}|.{0,", len-1, "}$")) %>%
        `[[`(1) %>%
        str_c(collapse="\n")
      re_s <- c(re_s, re) }
    return(re_s)
  }


  ################################### summary ###################################
  # summary datasets
  output$summary_table <- renderDT({
    if(dim(db)[1]==0) return(NULL)
    db[, 1:7]
  })

  # summary users profiling
  output$summary_user <- renderPlotly({
    if(dim(db)[1]==0) return(NULL)
   # db_tidy <- gather(db, key=input$summary_x, value="stats")
    ggplot(db, aes(x=input$summary_x, fill=input$summary_color)) +
      geom_bar(stat="count")
     # xlab(input$summary_x)
  })

  ################################### query genes ###################################
  # update dataset UI
  observe({
    if(input$query_gene_dataset_all=="multiple") {
      output$query_gene_dataset_multi <- renderUI({
        selectizeInput("query_gene_dataset_multi", label="Dataset", choices=db$name, multiple=T)
      }) }
  })

  # construct query function
  query_gene_fun <- function(db_list, colname, genelist) {
    re_sum <- tibble()
    re_tidy_sum <- tibble()
    for(i in 1:dim(db_list)[1]) {
      re <- filter(db_list$expr[[i]], toupper(!!as.name(colname)) %in% toupper(genelist))
      if(dim(re)[1]!=0) {
        re$name <- db_list$name[i]
        re <- full_join(db_list[i, 1:7], re, by=c("name"="name"))
        # tidy format and add group column
        re.tidy <- gather(re, key=sample, value=tpm, na.rm=F,
                 -date, -user, -species, -method, -name, -source, -celltype,
                 -ensembl_gene_id, -symbol, -entrezgene)
        re.tidy.group <- match(re.tidy$sample, db_list$group[[i]][[1]]) %>% db_list$group[[i]][[2]][.]
        re.tidy$group <- re.tidy.group
        re_sum <- bind_rows(re_sum, re)
        re_tidy_sum <- bind_rows(re_tidy_sum, re.tidy)
      } else {
        re_sum <- re_sum
        re_tidy_sum <- re_tidy_sum }
    }

    re_tidy_sum$sample <- factor(re_tidy_sum$sample, levels=unique(re_tidy_sum$sample), labels=unique(re_tidy_sum$sample))
    re_tidy_sum$symbol <- factor(re_tidy_sum$symbol, levels=unique(re_tidy_sum$symbol), labels=unique(re_tidy_sum$symbol))
    re_tidy_sum$group <- factor(re_tidy_sum$group, levels=unique(re_tidy_sum$group), labels=unique(re_tidy_sum$group))
    re_tidy_sum$celltype <- factor(re_tidy_sum$celltype, levels=unique(re_tidy_sum$celltype), labels=unique(re_tidy_sum$celltype))
    re_tidy_sum$method <- factor(re_tidy_sum$method, levels=unique(re_tidy_sum$method, labels=re_tidy_sum$method))
    # print(genelist)
    print(re_sum)
    # print(re_tidy_sum)
    # print(re_sum)
    return(list(re_sum, re_tidy_sum))
  }

  # get query result
  query_gene_result <- eventReactive(input$query_gene_submit, {
    genelist <- str_split(input$query_gene_list, "\n", simplify=T) %>% as.character()
    if(input$query_gene_dataset_all=="all") {
      db_select <- db
      result <- query_gene_fun(db_select, input$query_gene_idtype, genelist)
    } else {
      db_select <- filter(db, name %in% input$query_gene_dataset_multi)
      result <- query_gene_fun(db_select, input$query_gene_idtype, genelist)
    }
    return(result)
  })

  query_gene_result_showdata <- reactive({
    if(input$result_data_type=="no") {
      dd <- query_gene_result()[[1]] } else dd <- query_gene_result()[[2]]
    return(dd)
  })

  # table out query result
  output$query_gene_result_table <- renderDT({
    query_gene_result_showdata()
    })

  # plot query result
  output$query_gene_result_plot <- renderPlotly({
    ggplot(query_gene_result()[[2]], aes(x=!!as.name(input$query_gene_plot_x), y=tpm,
          color=!!as.name(input$query_gene_plot_color), shape=!!as.name(input$query_gene_plot_shape))) +
      geom_jitter(width=0.1) + xlab("") + ylab("TPM") + theme(strip.background=element_blank()) +
      facet_wrap(as.formula(paste("~", input$query_gene_plot_facet)), scales=input$query_gene_plot_scale) +
      theme(axis.text.x=element_text(vjust=0.5, hjust=0.5, angle=input$query_gene_plot_x_angle)) +
      theme(legend.position="none") + # remove legend
      scale_x_discrete(labels=function(x) string_wrap(x) ) + # self_define string_wrap in global.R
      theme(plot.margin=unit(c(0, 0, 0, 0), "npc")) # order: t, r, b, l  # to fully display the x and y axis labels
  })

  # download query result
  output$query_gene_download <- downloadHandler(
    filename=function(){"query_gene.csv"},
    content=function(fname){
      write.csv(query_gene_result_showdata(), fname) })



  ################################### upload new dataset ###################################
  # upload new dataset expression
  new_dataset_expr <- reactive({
    if(is.null(input$new_dataset_expr)) return(NULL)
    new_dataset_expr_path <- input$new_dataset_expr$datapath
    new_dataset_expr_data <- read_xlsx(new_dataset_expr_path, sheet=1)
    attr <- c("type", "bio_type", "description", "name")
    discard_column <- which(colnames(new_dataset_expr_data) %in% attr)
    new_dataset_expr_data <- dplyr::select(new_dataset_expr_data, -discard_column)
    colnames(new_dataset_expr_data)[1:3] <- c("ensembl_gene_id", "symbol", "entrezgene")
    return(new_dataset_expr_data)
    })

  # upload new dataset group info
  new_dataset_group <- reactive({
    if(is.null(new_dataset_expr())) return(NULL)
    if(is.null(input$new_dataset_group)) return(NULL)
    new_dataset_group_path <- input$new_dataset_group$datapath
    new_dataset_group_data <- read_xlsx(new_dataset_group_path, sheet=1) %>% na.omit()
    # check if any sample has no group info
    # print(new_dataset_group_data[[1]])
    # print(colnames(new_dataset_expr())[c(-1,-2,-3)])
    # print(all(new_dataset_group_data[[1]] %in% colnames(new_dataset_expr())[c(-1,-2,-3)]))
    if(all(new_dataset_group_data[[1]] %in% colnames(new_dataset_expr())[c(-1,-2)])) {
      return(new_dataset_group_data)
    } else {
      # alert group incomplete info via shinyalert
      shinyalert(title="Group info check invaild", type="error",
                 text="Please check if all samples in expression dataset have group belongs in uploaded group info")
      return(NULL) }
  })

  # show uploaded dataset expression
  output$new_dataset_expr_table <- renderDT({
    if(is.null(new_dataset_expr())) return(NULL)
    new_dataset_expr()
  })

  # show uploaded dataset group info
  output$new_dataset_group_table <- renderDT({
    if(is.null(new_dataset_group())) return(NULL)
    new_dataset_group()
  })

  # enable upload button if conditon is ok
  observe({
    if(all(!is.null(new_dataset_expr()), !is.null(new_dataset_group()), !is.null(input$new_dataset_name),
      !is.null(input$new_dataset_user), !is.null(input$new_dataset_species), !is.null(input$new_dataset_method),
      !is.null(input$new_dataset_source), !is.null(input$new_dataset_celltype))) {
    #  print("Enable upload button")
    #  addClass("new_dataset_upload", "green")
      enable("new_dataset_upload") } # via shinyjs
  })

  # write to db if enabled submitButton pressed
  observeEvent(input$new_dataset_upload, {
   # print("Submit new dataset")
    new_dataset <- tibble(date=Sys.Date(), user=input$new_dataset_user, species=input$new_dataset_species,
                          method=input$new_dataset_method, name=input$new_dataset_name, source=input$new_dataset_source,
                          celltype=input$new_dataset_celltype, expr=list(new_dataset_expr()), group=list(new_dataset_group()))
    db <- bind_rows(db, new_dataset)
    # save new db
    saveRDS(db, default_dbPath, compress=F)
    Sys.sleep(1)
    shinyalert("Upload success, refresh page will see new dataset", type="success")
  })



################################### remove dataset ###################################
  updateSelectizeInput(session, "dataset_tobe_remove", choices=db$name)

  # pass password to remove datasets
  observeEvent(input$submit_dataset_tobe_remove, {
    if(length(input$dataset_tobe_remove)==0) {
      shinyalert("Warning", "No dataset was selected!", type="warning")
    } else {
      shinyalert("Please input admin password to remove selected datasets:", type="input", callbackR=mycallback) }
  })

  # if inputed password is correct
  mycallback <- function(value) {
    Sys.sleep(0.5)
    if(value==admin_password) {
      db_remove <- filter(db, !(name %in% input$dataset_tobe_remove))
      Sys.sleep(0.5)
      saveRDS(db_remove, default_dbPath, compress=F)
      shinyalert("Datasets have beed removed!", type="info")
    } else {
      shinyalert("Incorrect password", type="error")
    }
  }



  output$dbpath <- renderText({dbpath})
}


