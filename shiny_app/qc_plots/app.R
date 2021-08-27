#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tools)
library(vroom)

#make plotting functions
makehist <- function(df, x_var, y_var, title_name="", sample_name=""){
    df %>% ggplot(aes(x=READ_PAIRS_PER_BC_HISTOGRAM_X, y=READ_PAIRS_PER_BC_HISTOGRAM_Y)) + 
        geom_histogram(stat = "identity") + theme_bw() + 
        ggtitle(paste(sample_name, " ", title_name))
}

makeScatter <-

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Quality Control"),
    
    # Sidebar with file input 
     sidebarLayout(
         sidebarPanel(
             fileInput(inputId = "qc_files", buttonLabel = "Upload...",
                       label = "Load QC metrics files", 
                       multiple = T, accept = ".tsv"),
             selectInput(inputId = "sample", label = "Select sample", choices = character())),
         mainPanel(
             tabsetPanel(
                 tabPanel("Batch",
                          plotOutput("ss_histplot"),
                          plotOutput("ds_histplot"),
                          plotOutput("duprate_scatter")),
                 tabPanel("Sample",
                          plotOutput("sample_ss_histplot")
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
                          choices = unique(test_duprate_df$X.sample))
    })
    
    #Batch tab results
    output$ss_histplot <- renderPlot({
            #qc_file1<-input$qc_files
            #qc_files_list<-list()
            #input$qc_files['name'=='all_samples_ss_read_pairs_per_bc_hist.tsv', 'datapath']
            ss_read_pairs_per_bc_hist<-read.delim(input_data()[input_data()$name=='all_samples_ss_read_pairs_per_bc_hist.tsv','datapath'], 
                                                  stringsAsFactors = F)
            #new_qc_file<-read.delim(qc_file1$datapath, stringsAsFactors=FALSE)
            ss_read_pairs_per_bc_hist %>% ggplot(aes(x=READ_PAIRS_PER_BC_HISTOGRAM_X, y=READ_PAIRS_PER_BC_HISTOGRAM_Y)) + 
                geom_histogram(stat = "identity") + theme_bw() + labs(title = "Single strand read pairs per barcode")
        })
        
    output$ds_histplot <- renderPlot({
            ds_read_pairs_per_bc_hist<-read.delim(input_data()[input_data()$name=='all_samples_ds_read_pairs_per_bc_hist.tsv','datapath'], 
                                                  stringsAsFactors = F)
            #new_qc_file<-read.delim(qc_file1$datapath, stringsAsFactors=FALSE)
            ds_read_pairs_per_bc_hist %>% ggplot(aes(x=READ_PAIRS_PER_BC_HISTOGRAM_X, y=READ_PAIRS_PER_BC_HISTOGRAM_Y)) + 
                geom_histogram(stat = "identity") + theme_bw() + labs(title = "Duplex read pairs per barcode")
        })
        
    output$duprate_scatter <- renderPlot({
            duprate<-read.delim(input_data()[input_data()$name=='all_samples_ss_duplication_rate.tsv','datapath'], 
                                stringsAsFactors = F)
            duprate %>% arrange(desc(DUPLICATE_RATE)) %>% rename(sample=X.sample) %>% 
                mutate(sample=factor(x=sample, levels = sample)) %>%
                ggplot(aes(x=sample, y=DUPLICATE_RATE)) + geom_point() + theme_bw() + labs(title = "Duplication rate per sample") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
        })
    
    #Sample tab results
    input_sample<-reactive({
        req(input$sample)
    })
    output$sample_ss_histplot<-renderPlot({
        sample_ss_read_pairs_per_bc_hist<-read.delim(input_data()[input_data()$name=='all_samples_ss_read_pairs_per_bc_hist.tsv',
                                                                  'datapath'], stringsAsFactors = F)
        #filter for selected sample
        sample_ss_read_pairs_per_bc_hist <- sample_ss_read_pairs_per_bc_hist %>% filter(.data$X.sample==.env$input_sample())
        sample_ss_read_pairs_per_bc_hist %>%
            ggplot(aes(x=READ_PAIRS_PER_BC_HISTOGRAM_X, y=READ_PAIRS_PER_BC_HISTOGRAM_Y)) +
            geom_histogram(stat = "identity") + theme_bw() +
            ggtitle(paste("Single strand read pairs per barcode for ", input_sample()))
    })
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)
