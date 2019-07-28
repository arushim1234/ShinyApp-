
# Group Members
# Arushi Mahajan, ID:11915079
# Kapil Bindal, ID:11915052
# Sivesh Kango, ID:11915002

# ShinyApp

# Installing Packages
try(require(shiny) || install.packages("shiny"))
if (!require(udpipe)){install.packages("udpipe")}
if (!require(textrank)){install.packages("textrank")}
if (!require(lattice)){install.packages("lattice")}
if (!require(igraph)){install.packages("igraph")}
if (!require(ggraph)){install.packages("ggraph")}
if (!require(wordcloud)){install.packages("wordcloud")}
try(require("fmsb")||install.packages("fmsb"))
if (!require(DT)){install.packages("DT")}
library("shiny")
library("fmsb")
library(udpipe)
library(textrank)
library(lattice)
library(igraph)
library(ggraph)
library(ggplot2)
library(wordcloud)
library(stringr)


# Defining ui function
ui <- shinyUI(
  fluidPage(
    
    titlePanel("Shiny App- Around the UDPipe NLP workflow"),
    
    sidebarLayout( 
      
      sidebarPanel(  
        
        fileInput("text_file", "Upload data (Text File)"),
        
        
        checkboxGroupInput(inputId = 'upos_cooc',
                           label = h3('Universal Parts-of-speech tags (Co-occurrences)'),
                           choices =list("adjective"= "ADJ",
                                         "Noun" = "NOUN",
                                         "proper noun" = "PROPN",
                                         "adverb"="ADV","verb"= "VERB"),
                           selected = c("ADJ","NOUN","PROPN"))
        
      ),
      
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    
                    tabPanel("Overview",
                             h4(p("Data input")),
                             p("This app supports only text documents (.txt) data file. ",align="justify"),
                             p("Please refer to the link below for sample txt file."),
                             a(href="https://raw.githubusercontent.com/arushim1234/ShinyApp-/master/IBM%20Report.txt"
                               ,"Sample data file"),   
                             br(),
                             h4('How to use this App'),
                             p('To start with, click on', 
                               span(strong("Upload data (Text File)")),
                               'and upload the  data file. '),
                             br(),
                             h4('Analysis'),
                             p('The Second tab displays
                               a table of annoted documents'),
                             br(),
                             p('The third tab displays  wordclouds,
                              one for all the nouns in the corpus
                              and another for all the verbs in the corpus.'),
                             br(),
                             p('The fourth tab displays 2 co-occurrences
                               network plots(top 30)')),
                    tabPanel("Table of annotated documents", 
                             dataTableOutput('datatableOutput'),
                             downloadButton("downloadData", "Download Annotated Data")),
                    
                    tabPanel("Wordclouds",
                             h3("Nouns"),
                             plotOutput('wordcloudplot1'),
                             h3("Verbs"),
                             plotOutput('wordcloudplot2')),
                    
                    tabPanel("Co-Occurrences",
                             h3("Co-occurrences"),
                             plotOutput('coocplot'))
                    
        ) # end of tabsetPanel
      )# end of main panel
    ) # end of sidebarLayout
  )  # end if fluidPage
) # end of UI

# Defining Server function

getwd()
setwd('C:/Users/ARUSHI/Desktop/ISB_Residency/Residency 2/Text Analytics/Assignment/Deliverables')
server <- shinyServer(function(input, output) {
  
  DS <- reactive({
    
    if (is.null(input$text_file)) {
      return(NULL) } 
    else{
      DT <- readLines(input$text_file$datapath)
      DT  =  str_replace_all(DT, "<.*?>", "")  
      DT = DT[DT!= ""]
      str(DT)
      return(DT)
    }
  })
  
  english_model = reactive({
    english_model = udpipe_download_model(language = "english")  
    english_model$file_model
    ud_english <- udpipe_load_model(english_model$file_model)
    return(ud_english)
  })
  
  annotation.obj = reactive({
    A <- udpipe_annotate(english_model(),x = DS())
    A <- as.data.frame(A)
    return(A)
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){
      "Annoted Data Output.csv"
    },
    content = function(text_file){
      write.csv(annotation.obj()[,-4],text_file,row.names = FALSE)
    }
  )
  
  output$datatableOutput = renderDataTable( 
    {
      if(is.null(input$text_file)){return(NULL)}
      else{
        output_Data = annotation.obj()[,-4]
        return(DT::datatable(output_Data, options = list(orderClasses = TRUE, pageLength = 100)))
        # return(output_Data)
        
      }
    })
  
  output$wordcloudplot1 = renderPlot({
    if(is.null(input$text_file)){return(NULL)}
    else{
      nouns = annotation.obj() %>% subset(., upos %in% "NOUN") 
      nouns_T = txt_freq(nouns$lemma)  
      
      wordcloud(nouns_T$key,nouns_T$freq, min.freq = 2,colors = 1:20 )
    }
  })
  
  output$wordcloudplot2 = renderPlot({
    if(is.null(input$text_file)){return(NULL)}
    else{
      verbs = annotation.obj() %>% subset(., upos %in% "VERB") 
      verbs_T = txt_freq(verbs$lemma)
      
      wordcloud(verbs_T$key,verbs_T$freq, min.freq = 2,colors = 1:20 )
    }
  })
  
  output$coocplot = renderPlot({
    if(is.null(input$text_file)){return(NULL)}
    else{
      cooc <- cooccurrence(   	
        x = subset(annotation.obj(), upos %in% input$upos_cooc), 
        term = "lemma", 
        group = c("doc_id"))
      
      cooc_network <- head(cooc, 30)
      cooc_network <- igraph::graph_from_data_frame(cooc_network)
      
      ggraph(cooc_network, layout = "fr") +  
        
        geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +  
        geom_node_text(aes(label = name), col = "darkblue", size = 5) +
        
        theme_graph(base_family = "Arial Narrow") +  
        theme(legend.position = "none") +
        
        labs(title = "Top 30 Co-occurrences", subtitle = "Select UPOS check boxes from the left panel")
    }
  })
})

# Defining ShinyApp
shinyApp(ui=ui,server = server)

