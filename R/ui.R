
###########################################################################################
# header
header <- dashboardHeader(title="shinySeqDB")



###########################################################################################
# sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(id="current_tab",
              menuItem("Summary", tabName="summary_page", icon=icon("dashboard"), selected=T),
              # menuItem("DataTable", icon=icon("th"), startExpanded=F,
              #          menuSubItem("Bulk RNAseq", tabName="bulkseq_page"),
              #          menuSubItem("Sing cell RNAseq", tabName="scseq_page")),
              menuItem("Query Genes", tabName="query_genes_page", icon=icon("bar-chart-o")),
              menuItem("Upload Dataset", tabName="upload_page", icon=icon("circle")),
              helpText("Developed by the Hao He",
                       style="padding-left:1em; padding-right:1em;position:absolute; bottom:20em;"),
              helpText("Version: 1.0.0",
                       style="padding-left:1em; padding-right:1em;position:absolute; bottom:18em;"),
              helpText("2019-03-15",
                       style="padding-left:1em; padding-right:1em;position:absolute; bottom:16em;"),
              helpText("Hu lab",
                       style="padding-left:1em; padding-right:1em;position:absolute; bottom:14em;"),
              helpText("haohe90@gmail.com",
                       style="padding-left:1em; padding-right:1em;position:absolute; bottom:12em;"),
              helpText("If you have any problem", br(),"Please contact me",
                style="padding-left:1em; padding-right:1em;position:absolute; bottom:6em;"),
              helpText("See source code at ", a("Github", href="https://github.com/soulong/shinySeqDB"), style="padding-left:1em; padding-right:1em;position:absolute; bottom:4em;")
              )
)



###########################################################################################
# body
body <- dashboardBody(

  useShinyjs(),  # include shinyjs
  useShinyalert(),

  tabItems(
    tabItem("summary_page",
            fluidRow(
              box(width=12, # title="Uploaded expression dataset",
                  DTOutput("summary_table"), style="height:300px; overflow-y: scroll;overflow-x: scroll;"),
              box(width=6, plotlyOutput("summary_user")),
              box(width=3,
                  selectInput("summary_x", "X_label", choices=c("date", "user", "method", "source", "celltype"), selected="user"),
                  selectInput("summary_y", "Y_label", choices=c("date", "user", "method", "source", "celltype"), selected="user"),
                  selectInput("summary_color", "Color", choices=c("date", "user", "method", "source", "celltype"), selected="method"))
              )
            ),

    # tabItem("bulkseq_page",
    #         h2("bulkseq")),
    #
    # tabItem("scseq_page",
    #         h2("scseq")),

    tabItem("query_genes_page",
            fluidRow(
              box(status="danger", solidHeader=T, width=12, height="140px",
                  column(3, textAreaInput("query_gene_list", label=NULL, height="120px")),
                  column(3, selectInput("query_gene_idtype", label="Input type",
                                        choices=c("ensembl_gene_id", "symbol", "entrezgene"), selected="symbol"),
                            actionButton("query_gene_submit", "Query now", icon=icon("refresh")) ),
                  column(3, selectInput("query_gene_celltype", label="Dataset", choices=""),
                         downloadButton("query_gene_download", label="Download result")),
                  column(3, selectInput("query_gene_dataset", label="Dataset", choices=""),
                         radioButtons("result_data_type", "Tidy data", choices=c("yes"="yes", "no"="no"), selected="no", inline=T)) ),
              box(width=12, title="Expression data", status="danger", solidHeader=T, collapsible=T, collapsed=T,
                  DTOutput("query_gene_result_table"), style="height:500px; overflow-y: scroll;overflow-x: scroll;"),
              box(width=2, status="danger", solidHeader=F, collapsible=T, collapsed=F, height="600px",
                         selectInput("query_gene_plot_x", label="X_label",
                                     choices=c("group", "sample", "method", "celltype", "symbol"), selected="group"),
                         selectInput("query_gene_plot_color", label="Color",
                                     choices=c("group", "sample", "method", "celltype", "symbol"), selected="sample"),
                         selectInput("query_gene_plot_shape", label="Shape",
                                     choices=c("group", "sample", "method", "celltype", "symbol"), selected="method"),
                         selectInput("query_gene_plot_facet", label="Facet",
                                     choices=c("group", "sample", "method", "celltype", "symbol"), selected="symbol"),
                         selectInput("query_gene_plot_scale", label="Scale",
                                     choices=c("fixed", "free", "free_x", "free_y"), selected="free")
                         ),
              box(width=10, status="danger", solidHeader=F, collapsible=T, collapsed=F, height="600px",
                  plotlyOutput("query_gene_result_plot") )
              )
            ),

    tabItem("upload_page",
            fluidRow(
              box(title="Upload new expression dataset", status="danger", solidHeader=T, width=3, height="180px",
                  fileInput("new_dataset_expr", "Choose a csv file", accept=c("text/csv",
                    "text/comma-separated-values,text/plain", ".csv")) ),
                  #helpText("Only .csv file recepted")),
              box(title="Upload new dataset group info", status="danger", solidHeader=T, width=3, height="180px",
                  fileInput("new_dataset_group", "Choose a csv file", accept=c("text/csv",
                    "text/comma-separated-values,text/plain", ".csv")) ),
                  #helpText("Only .csv file recepted")),
              box(width=6, height="180px", status="danger", solidHeader=T,
                  column(5, textInput("new_dataset_name", "Dataset name")),
                  column(4, textInput("new_dataset_celltype", "Cell type")),
                  column(3, textInput("new_dataset_user", "Your name")),
                  column(3, textInput("new_dataset_source", "Dataset source", "Hu_lab")),
                  column(3, selectInput("new_dataset_species", "Species",
                                        choices=c("Human", "Mouse"), selected="Human")),
                  column(3, selectInput("new_dataset_method", "Method",
                                        choices=c("Bulk Seq", "Smart Seq", "scRNA Seq"), selected="Bulk Seq")),
                  column(3, br(), # inlineCSS(list(.green = "background: green")),
                         disabled(actionButton("new_dataset_upload","Upload", icon=icon("refresh"))))
                  ),
              box(solidHeader=T, width=9, # title="Uploaded expression dataset",
                  DTOutput("new_dataset_expr_table"), style="height:500px; overflow-y: scroll;overflow-x: scroll;"),
              box(solidHeader=T, width=3, # title="Uploaded expression dataset",
                  DTOutput("new_dataset_group_table"), style="height:500px; overflow-y: scroll;overflow-x: scroll;")
              )
            )
    )
)



###########################################################################################
# main dashboard
ui <- dashboardPage(header, sidebar, body, skin="red")