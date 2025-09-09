#' Landing page
#' 
#' \code{.landing_page} creates the landing page of miaDash, where TreeSE objects
#' can be built and iSEE can be launched.
#'
#' @return The UI is defined by the function. A \code{NULL} value is invisibly
#'   returned.
#'
#' @name landing_page
#' @keywords internal

#' @rdname landing_page
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar
#'   dashboardBody box
#' @importFrom htmltools HTML br tags div tagList
#' @importFrom shinyjs disable
#' @importFrom shinyWidgets addSpinner
#' @importFrom utils data
.landing_page <- function(FUN, input, output, session) {
  
    # nocov start
    mia_datasets <- .import_datasets(-c(2, 5))
  
    output$allPanels <- renderUI({

        dashboardPage(
          
            dashboardHeader(disable = TRUE),
            dashboardSidebar(disable = TRUE),
            dashboardBody(
                
                tags$head(tags$style(HTML(".btn-primary {color: white}"))),
              
                fluidRow(box(id = "import.panel", title = "Import", width = 4,
                    status = "primary", solidHeader = TRUE, collapsible = TRUE,

                    tabsetPanel(id = "format",
                          
                        tabPanel(title = "Dataset", value = "dataset", br(),
                        
                            selectInput(inputId = "data", label = "Dataset:",
                                choices = mia_datasets,
                                selected = mia_datasets[1])),
                          
                        tabPanel(title = "R Object", value = "rds", br(),
                                   
                            fileInput(inputId = "file", label = "RDS:",
                                accept = ".rds", placeholder = "object.rds")),
                          
                        tabPanel(title = "Raw Data", value = "raw", br(),
                                   
                            fileInput(inputId = "assay", label = "Assays:",
                                accept = ".tsv", multiple = TRUE,
                                placeholder = "assay.tsv"),
                            div(style = "margin-top: -20px"),
                                   
                            fileInput(inputId = "coldata", label = "colData:",
                                accept = ".tsv", placeholder = "coldata.tsv"),
                            div(style = "margin-top: -20px"),
                        
                            fileInput(inputId = "rowdata", label = "rowData:",
                                accept = ".tsv", placeholder = "rowdata.tsv"),
                            div(style = "margin-top: -20px"),
                            
                            fileInput(inputId = "row.tree",
                                  label = "rowTree:", placeholder = "row.tree",
                                  accept = c(".tree", ".tre")),
                            div(style = "margin-top: -20px"),
                
                            fileInput(inputId = "col.tree",
                                  label = "colTree:", placeholder = "col.tree",
                                  accept = c(".tree", ".tre")),
                            div(style = "margin-top: -20px"),
                            
                            checkboxInput(inputId = "taxa.from.rownames",
                                label = "Derive rowData from assay rownames")),
                          
                        tabPanel(title = "Foreign", value = "foreign", br(),
                                  
                            radioButtons(inputId = "ftype", label = "Type:",
                                choices = list("biom", "HUMAnN", "MetaPhlAn",
                                "Mothur", "QIIME2"), inline = TRUE),
                             
                            fileInput(inputId = "main.file",
                                label = "Main file:", accept = c(".biom",
                                ".tsv", ".shared", ".QZA", ".txt"),
                                placeholder = "biom, tsv, shared, QZA or txt"),
                            div(style = "margin-top: -20px"),
                            
                            fileInput(inputId = "col.data", label = "colData:",
                                accept = c(".tsv", ".design"),
                                placeholder = "tsv or design"),
                            div(style = "margin-top: -20px"),
                            
                            conditionalPanel(
                                condition = paste0("input.ftype == 'Mothur' | ",
                                    "input.ftype == 'QIIME2'"),
                               
                                fileInput(inputId = "f.rowdata",
                                    label = "rowData:", accept = c(".taxonomy",
                                    ".qza"), placeholder = "taxonomy or qza"),
                                div(style = "margin-top: -20px")),
                                
                            conditionalPanel(
                                condition = paste0("input.ftype == 'biom' | ",
                                    "input.ftype == 'MetaPhlAn'"),
                                
                                fileInput(inputId = "tree.file",
                                    label = "rowTree:", placeholder = "tree.tree",
                                    accept = c(".tree", ".tre", ".qza")),
                                div(style = "margin-top: -20px")),
                            
                            conditionalPanel(
                                condition = paste0("input.ftype == 'biom' | ",
                                    "input.ftype == 'HUMAnN'"),
                            
                                checkboxInput(inputId = "rm.tax.pref",
                                    label = "Remove taxa prefixes")),
                                
                            conditionalPanel(
                                condition = "input.ftype == 'biom'",
                            
                                checkboxInput(inputId = "rank.from.pref",
                                    label = "Derive taxa from prefixes")),
                            
                            conditionalPanel(
                                condition = "input.ftype == 'HUMAnN'",
                            
                                checkboxInput(inputId = "rm.hum.suf",
                                    label = "Remove sample suffix")))),
              
                    actionButton("import", "Upload", class = "btn-primary")),
            
                box(id = "manipulate.panel", title = "Manipulate", width = 4,
                    status = "primary", solidHeader = TRUE, collapsible = TRUE,

                    tabsetPanel(id = "manipulate",
                          
                        tabPanel(title = "Subset", value = "subset", br(),
                      
                            radioButtons(inputId = "subkeep", label = "Keep:",
                                choices = c("prevalent", "rare"),
                                inline = TRUE),
                      
                            selectInput(inputId = "subassay", label = "Assay:",
                                choices = NULL),
                      
                            sliderInput(inputId = "prevalence", value = 0,
                                label = "Prevalence threshold:", step = 0.01,
                                min = 0, max = 1),
                      
                            numericInput(inputId = "detection", value = 0,
                                label = "Detection threshold:", min = 0,
                                step = 1)),
                  
                        tabPanel(title = "Agglomerate", value = "agglomerate",

                            br(),
                  
                            selectInput(inputId = "taxrank",
                                label = "Taxonomic rank:", choices = NULL)),
                  
                        tabPanel(title = "Transform", value = "transform", br(),
                  
                            selectInput(inputId = "assay.type",
                                label = "Assay:", choices = NULL),
              
                            selectInput(inputId = "trans.method",
                                label = "Method:", choices = .transformMethods),
              
                            checkboxInput(inputId = "pseudocount",
                                label = "Pseudocount"),
              
                            textInput(inputId = "assay.name", label = "Name:"),
                      
                            radioButtons(inputId = "margin", label = "Margin:",
                                choices = c("samples", "features"),
                                inline = TRUE))),
              
                    actionButton("apply", "Apply", class = "btn-primary")),
            
                box(id = "estimate.panel", title = "Estimate", width = 4,
                    status = "primary", solidHeader = TRUE, collapsible = TRUE,

                    tabsetPanel(id = "estimate",
                                
                        header = tagList(
                            br(), selectInput(inputId = "estimate.assay",
                                label = "Assay:", choices = NULL)),
                        
                        tabPanel(title = "Quality", value = "quality",
                            
                            checkboxGroupInput(inputId = "quality.metrics",
                                label = "Metrics:",
                                choices = .qualityMetrics)),
                          
                        tabPanel(title = "Alpha", value = "alpha",
                      
                            selectInput(inputId = "alpha.index",
                                label = "Metrics:", multiple = TRUE,
                                choices = .alphaMetrics)),

                        tabPanel(title = "Beta", value = "beta",
                           
                            radioButtons(inputId = "bmethod",
                                label = "Method:", choices = .betaMethods,
                                inline = TRUE),
                      
                            conditionalPanel(
                                condition = paste0("input.bmethod == 'MDS' || ",
                                    "input.bmethod == 'NMDS' || ",
                                    "input.bmethod == 'RDA'"),
                            
                                selectInput(inputId = "beta.index",
                                    label = "Metric:", choices = .betaMetrics)),
                      
                            conditionalPanel(
                                condition = "input.bmethod == 'RDA'",
                            
                                textInput(inputId = "rda.formula",
                                    label = "Formula:",
                                    placeholder = "data ~ var1 + var2 * var3")),
                      
                            numericInput(inputId = "ncomponents", value = 5,
                                label = "Number of components:", min = 1,
                                step = 1)),
                        
                        tabPanel(title = "Cluster", value = "cluster",
                                 
                            radioButtons(inputId = "cmethod",
                                label = "Method:", choices = .clustMethods,
                                inline = TRUE),
                            
                            conditionalPanel(
                                condition = paste0("input.cmethod == 'Dmm' | ",
                                    "input.cmethod == 'Kmeans'"),
                                
                                numericInput(inputId = "kclusters", value = 3,
                                    label = "Number of clusters:", min = 1,
                                    step = 1)),
                            
                            conditionalPanel(
                                condition = "input.cmethod == 'Dmm'",
                              
                                selectInput(inputId = "dmm.type",
                                    label = "Criterion:",
                                    choices = .DmmCriteria,
                                    selected = .DmmCriteria[1]),
                                
                                numericInput(inputId = "dmm.seed", value = 1L,
                                    label = "Seed:", min = 0, step = 1)),
                            
                            conditionalPanel(
                                condition = "input.cmethod == 'NNGraph'",
                              
                                numericInput(inputId = "kneighbours",
                                    value = 10, label = "Number of neighbours:",
                                    min = 1, step = 1),
                                
                                checkboxInput(inputId = "nn.shared",
                                    label = "Construct shared graph")),
                            
                            checkboxInput(inputId = "clust.full",
                                label = "Add to metadata"),
                            
                            radioButtons(inputId = "clust.margin",
                                label = "Margin:", inline = TRUE,
                                choices = c("samples", "features"),
                                selected = "features")),
                        
                        footer = textInput(inputId = "estimate.name",
                            label = "Name:")),
              
                    actionButton("compute", "Compute", class = "btn-primary"))),
                
                fluidRow(box(id = "visualise.panel", title = "Visualise",
                    width = 4, status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,

                    selectInput(inputId = "panels", label = "Panels:",
                        choices = c(.miaDashDefaultPanels, .miaDashOtherPanels),
                        multiple = TRUE, selected = c(.miaDashDefaultPanels)),
                
                    actionButton("launch", "Launch iSEE",
                        class = "btn-primary")),
              
                box(id = "output.panel", title = "Output", width = 8,
                    status = "primary", solidHeader = TRUE, collapsible = TRUE,

                    addSpinner(verbatimTextOutput(outputId = "object"),
                        color = "#007bff"),
              
                    downloadButton(outputId = "download", label = "Download",
                        class = "btn-primary")))))})
    
    ## Disable navbar buttons that are not linked to any observer yet
    disable("iSEE_INTERNAL_organize_panels")  # organize panels
    disable("iSEE_INTERNAL_link_graph")       # link graph
    disable("iSEE_INTERNAL_export_content")   # export content
    disable("iSEE_INTERNAL_tracked_code")     # tracked code
    disable("iSEE_INTERNAL_panel_settings")   # panel settings
    disable("iSEE_INTERNAL_open_vignette")    # open vignette
    disable("iSEE_INTERNAL_session_info")     # session info
    disable("iSEE_INTERNAL_citation_info")    # citation info
    
    rObjects <- reactiveValues(tse = NULL)
    
    observe({
        .print_message(
            title = "Welcome to the Microbiome Analysis Dashboard! \U0001f9a0",
            "miaDash is actively maintained by the",
            tags$a(href = "https://datascience.utu.fi/",
            "Turku Data Science Group", target = "_blank", .noWS = "after"),
            ", so we are happy to receive feedback from you. Feature requests,",
            "bug reports and other comments can be submitted",
            tags$a(href = "https://github.com/microbiome/miaDash/issues",
            "here", target = "_blank", .noWS = "after"), HTML(".<br/><br/>"),
            "If you are new to this app, you can learn how to use it with",
            tags$a(href = "https://microbiome.github.io/miaDash/articles/miaDash.html",
            "this short tutorial", target = "_blank", .noWS = "after"),
            ". Technical support can be obtained on",
            tags$a(href = "https://app.gitter.im/#/room/#microbiome_miaverse:gitter.im",
            "our Gitter channel", target = "_blank", .noWS = "after"), "." 
        )
    })
    
    .create_import_observers(input, rObjects)
    .create_manipulate_observers(input, rObjects)
    .create_estimate_observers(input, rObjects)
    .update_observers(input, session, rObjects)

    .create_launch_observers(FUN, input, session, rObjects)
    
    .render_overview(output, rObjects)
    .render_download(output, rObjects)

    invisible(NULL)
    # nocov end
}