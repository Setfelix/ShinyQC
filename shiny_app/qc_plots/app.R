#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tools)
library(vroom)
library(Cairo)
library(scales)
library(rmarkdown)
library(knitr)

#make plotting functions
makehist <- function(df, x_var, y_var, x_label, y_label, title_name="", sample_name=""){
    df %>% ggplot(aes(x=x_var, y=y_var)) + 
        geom_histogram(stat = "identity") + xlab(x_label) + ylab(y_label) + theme_bw() + 
        ggtitle(paste(sample_name, "", title_name))
}

makeScatter <- function(df, x_var, y_var, x_label, y_label, title_name="", sample_name=""){
    df %>% ggplot(aes(x=x_var, y=y_var)) + geom_point() + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        xlab(x_label) + ylab(y_label) + ggtitle(paste(sample_name, " ", title_name))
}

# makeStackBar <- function(df, x_var, y_var, x_label, y_label, title_name="", sample_name=""){
#     df %>% ggplot(aes(x=x_var, y=y_var)) + geom_bar(stat = "identity") + theme_bw() + 
#         theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#         xlab(x_label) + ylab(y_label) + ggtitle(paste(sample_name, " ", title_name))
# }

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Quality Control"),
    
    # Sidebar with file input 
     sidebarLayout(
         sidebarPanel(
             fileInput(inputId = "qc_files", buttonLabel = "Upload...",
                       label = "Load QC metrics files", 
                       multiple = T, accept = ".tsv"),
             selectInput(inputId = "sample", label = "Select sample", choices = character()),
             downloadButton("report", "Generate Batch report")),
         mainPanel(
             tabsetPanel(
                 tabPanel("Batch",
                          br(),
                          plotOutput("ss_histplot"),
                          br(),
                          plotOutput("ds_histplot"),
                          br(),
                          plotOutput("duprate_scatter", dblclick = "duprate_scatter_dblclick", 
                                     brush = brushOpts(id = "duprate_scatter_brush", resetOnNew = TRUE)
                                     ),
                          br(),
                          plotOutput("nreads_barplot")
                          ),
                 tabPanel("Sample",
                          br(),
                          tableOutput("sample_table_summary"),
                          br(),
                          plotOutput("sample_ss_histplot"),
                          br(),
                          plotOutput("sample_ds_histplot")
                          )
             )
        )
    )
)


server <- function(input, output, session) {
    #in_files<-reactive({input$qc_files})

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    input_data<-reactive({
        req(input$qc_files)
        
        #check file extension
        # ext<-tools::file_ext(input$qc_files$name)
        # switch(ext, tsv = vroom::vroom(input$qc_files$datapath, delim = "\t"),
        #        validate("Invalid file; Please upload a .csv or .tsv file"))
    })
    #load sample names into selectInput
    observeEvent(input$qc_files, {
        freezeReactiveValue(input, "sample")
        test_duprate_df<-read.delim(input_data()[input_data()$name=='all_samples_ss_duplication_rate.tsv','datapath'],
                                    stringsAsFactors = F)
        updateSelectInput(inputId = "sample", 
                          choices = unique(test_duprate_df$sample))
    })
    
    #Batch tab results
    output$ss_histplot <- renderPlot({
            #qc_file1<-input$qc_files
            #qc_files_list<-list()
            #input$qc_files['name'=='all_samples_ss_read_pairs_per_bc_hist.tsv', 'datapath']
            ss_read_pairs_per_bc_hist<-read.delim(input_data()[input_data()$name=='all_samples_ss_read_pairs_per_bc_hist.tsv','datapath'], 
                                                  stringsAsFactors = F)
            makehist(df = ss_read_pairs_per_bc_hist, x_var = ss_read_pairs_per_bc_hist$READ_PAIRS_PER_BC_HISTOGRAM_X,
                     y_var = ss_read_pairs_per_bc_hist$READ_PAIRS_PER_BC_HISTOGRAM_Y, x_label = "Number of Barcodes",
                     y_label = "Read pairs", title_name = "Single strand read pairs per barcode")
            # ss_read_pairs_per_bc_hist %>% ggplot(aes(x=READ_PAIRS_PER_BC_HISTOGRAM_X, y=READ_PAIRS_PER_BC_HISTOGRAM_Y)) + 
            #     geom_histogram(stat = "identity") + theme_bw() + labs(title = "Single strand read pairs per barcode")
        })
        
    output$ds_histplot <- renderPlot({
            ds_read_pairs_per_bc_hist<-read.delim(input_data()[input_data()$name=='all_samples_ds_read_pairs_per_bc_hist.tsv','datapath'], 
                                                  stringsAsFactors = F)
            makehist(df = ds_read_pairs_per_bc_hist, x_var = ds_read_pairs_per_bc_hist$READ_PAIRS_PER_BC_HISTOGRAM_X,
                     y_var = ds_read_pairs_per_bc_hist$READ_PAIRS_PER_BC_HISTOGRAM_Y, x_label = "Number of Barcodes",
                     y_label = "Read pairs", title_name = "Duplex read pairs per barcode")
            # ds_read_pairs_per_bc_hist %>% ggplot(aes(x=READ_PAIRS_PER_BC_HISTOGRAM_X, y=READ_PAIRS_PER_BC_HISTOGRAM_Y)) + 
            #     geom_histogram(stat = "identity") + theme_bw() + labs(title = "Duplex read pairs per barcode")
        })
        
    #point duprate with zoom after brush and double-click
    #adapted from https://shiny.rstudio.com/gallery/plot-interaction-zoom.html
    ranges_duprate_scatter <- reactiveValues(x = NULL, y = NULL)
    output$duprate_scatter <- renderPlot({
        duprate<-read.delim(input_data()[input_data()$name=='all_samples_ss_duplication_rate.tsv','datapath'], 
                            stringsAsFactors = F)
        duprate %>% arrange(desc(DUPLICATE_RATE)) %>% mutate(sample=factor(x=sample, levels = sample)) %>%
            ggplot(aes(x=sample, y=DUPLICATE_RATE)) + geom_point() + theme_bw() + labs(title = "Duplication rate per sample") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            coord_cartesian(xlim = ranges_duprate_scatter$x, ylim = ranges_duprate_scatter$y, expand = TRUE)
    })
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$duprate_scatter_dblclick, {
        brush <- input$duprate_scatter_brush
        if (!is.null(brush)) {
            ranges_duprate_scatter$x <- c(brush$xmin, brush$xmax)
            ranges_duprate_scatter$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges_duprate_scatter$x <- NULL
            ranges_duprate_scatter$y <- NULL
        }
    })
    #group_by(sample) %>%
    output$nreads_barplot <- renderPlot({
        nreads<-read.delim(input_data()[input_data()$name=='all_samples_duplex_single_reads.tsv','datapath'], stringsAsFactors = F)
        nreads %>% separate(DUPLEX_VS_SINGLE_CONSENSUS_READS, into = c("Duplex_consensus_reads", "Single_strand_consensus_reads"), sep = ",") %>% 
            mutate(Duplex_consensus_reads=as.numeric(Duplex_consensus_reads), Single_strand_consensus_reads=as.numeric(Single_strand_consensus_reads)) %>%
            mutate(perc_duplex_reads=(Duplex_consensus_reads/Single_strand_consensus_reads)*100, .after = Single_strand_consensus_reads) %>%
            pivot_longer(Duplex_consensus_reads:Single_strand_consensus_reads, names_to = "Metric", values_to="Value") %>% 
            #arrange(desc(perc_duplex_reads)) %>% mutate(sample1=factor(x=sample, labels = sample)) %>%
            select(sample, Metric, Value) %>%
            ggplot(aes(x=sample, y=Value, fill=Metric)) + geom_bar(stat = "identity", position = "fill") + theme_bw() +
            theme(legend.position = "top") + 
            labs(y="Fraction of reads", x="", title = "Duplex versus single strand consensus reads") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
    
    #Sample tab results
    input_sample<-reactive({
        req(input$sample)
    })
    output$sample_ss_histplot<-renderPlot({
        sample_ss_read_pairs_per_bc_hist<-read.delim(input_data()[input_data()$name=='all_samples_ss_read_pairs_per_bc_hist.tsv',
                                                                  'datapath'], stringsAsFactors = F)
        #filter for selected sample
        sample_ss_read_pairs_per_bc_hist <- sample_ss_read_pairs_per_bc_hist %>% filter(.data$sample==.env$input_sample())
        makehist(df = sample_ss_read_pairs_per_bc_hist, x_var = sample_ss_read_pairs_per_bc_hist$READ_PAIRS_PER_BC_HISTOGRAM_X,
                 y_var = sample_ss_read_pairs_per_bc_hist$READ_PAIRS_PER_BC_HISTOGRAM_Y, x_label = "Number of Barcodes",
                 y_label = "Read pairs", title_name = "Single strand read pairs per barcode", 
                 sample_name = input_sample())
        # sample_ss_read_pairs_per_bc_hist %>%
        #     ggplot(aes(x=READ_PAIRS_PER_BC_HISTOGRAM_X, y=READ_PAIRS_PER_BC_HISTOGRAM_Y)) +
        #     geom_histogram(stat = "identity") + theme_bw() +
        #     ggtitle(paste("Single strand read pairs per barcode for ", input_sample()))
    })
    output$sample_ds_histplot<-renderPlot({
        sample_ds_read_pairs_per_bc_hist<-read.delim(input_data()[input_data()$name=='all_samples_ds_read_pairs_per_bc_hist.tsv',
                                                                  'datapath'], stringsAsFactors = F)
        #filter for selected sample
        sample_ds_read_pairs_per_bc_hist <- sample_ds_read_pairs_per_bc_hist %>% filter(.data$sample==.env$input_sample())
        makehist(df = sample_ds_read_pairs_per_bc_hist, x_var = sample_ds_read_pairs_per_bc_hist$READ_PAIRS_PER_BC_HISTOGRAM_X,
                 y_var = sample_ds_read_pairs_per_bc_hist$READ_PAIRS_PER_BC_HISTOGRAM_Y, x_label = "Number of Barcodes",
                 y_label = "Read pairs", title_name = "Duplex read pairs per barcode", sample_name = input_sample())
        
    })
    output$sample_table_summary<-renderTable({
        sample_nreads<-read.delim(input_data()[input_data()$name=='all_samples_duplex_single_reads.tsv', 'datapath'], 
                                  stringsAsFactors = F)
        sample_nreads<-sample_nreads %>% filter(.data$sample==.env$input_sample()) %>% 
            separate(DUPLEX_VS_SINGLE_CONSENSUS_READS, into = c("Duplex_consensus_reads", "Single_strand_consensus_reads"), 
                     sep = ",") %>% pivot_longer(!.data$sample, names_to = "Metric", values_to="Value") %>% select(Metric, Value) %>%
            mutate(Value=number(as.numeric(Value), big.mark = ","))
        
    }
        ,striped = T, bordered = T)
    #report
    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.html",
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            #data_set=input_data()
            #out = knit2pdf
            #tempReport <- file.path(getwd(), fsep = "\\", "report.Rmd")
            #tempReport <- file.path(normalizePath(getwd()), fsep = "\\", "report.Rmd")
            tempReport <- file.path(getwd(), "batch_report.Rmd")
            #file.copy("batch_report.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            #parameters <- list(ss = output$ss_histplot, ds = output$ds_histplot, scatter = output$duprate_scatter, nreads = output$nreads_barplot)
            parameters <- list(inputs = input_data())
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            #output_format = "html_document", envir = new.env(parent = globalenv()
            rmarkdown::render(tempReport, output_file = file,
                              params = parameters,
                              envir = new.env(parent = globalenv()))
            # readBin(con = "built_report.pdf", what = "raw", n = file.info("built_report.pdf")[, "size"]) %>%
            #     writeBin(con = file)
        }
        )
}


# Run the application 
shinyApp(ui = ui, server = server)
