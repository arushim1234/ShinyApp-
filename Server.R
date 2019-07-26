
# Group Members
# Arushi Mahajan, ID: 11915079
# Kapil Bindal, ID:
# Sivesh Kango, ID:

library(stringr)

getwd()
setwd('C:/Users/ARUSHI/Desktop/ISB_Residency/Residency 2/Text Analytics/Assignment')
server <- shinyServer(function(input, output) {
  
  Dataset <- reactive({
    
    if (is.null(input$file)) {
      return(NULL) } 
    else{
      Data <- readLines(input$file$datapath)
      Data  =  str_replace_all(Data, "<.*?>", "")  
      Data = Data[Data!= ""]
      str(Data)
      return(Data)
    }
  })
  
  english_model = reactive({
    # load english model for annotation from working dir
    english_model = udpipe_download_model(language = "english")  
    english_model$file_model
    ud_english <- udpipe_load_model(english_model$file_model)
    return(ud_english)
  })
  
  annot.obj = reactive({
    x <- udpipe_annotate(english_model(),x = Dataset())
    x <- as.data.frame(x)
    return(x)
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){
      "annotated_data.csv"
    },
    content = function(file){
      write.csv(annot.obj()[,-4],file,row.names = FALSE)
    }
  )
  
  output$datatableOutput = renderDataTable({
    if(is.null(input$file)){return(NULL)}
    else{
      out = annot.obj()[,-4]
      return(out)
    }
  })
  
  output$wcplot1 = renderPlot({
    if(is.null(input$file)){return(NULL)}
    else{
      all_nouns = annot.obj() %>% subset(., upos %in% "NOUN") 
      top_nouns = txt_freq(all_nouns$lemma)  
      
      wordcloud(top_nouns$key,top_nouns$freq, min.freq = 3,colors = 1:10 )
    }
  })
  
  output$wcplot2 = renderPlot({
    if(is.null(input$file)){return(NULL)}
    else{
      all_verbs = annot.obj() %>% subset(., upos %in% "VERB") 
      top_verbs = txt_freq(all_verbs$lemma)
      
      wordcloud(top_verbs$key,top_verbs$freq, min.freq = 3,colors = 1:10 )
    }
  })
  
  output$coocplot3 = renderPlot({
    if(is.null(input$file)){return(NULL)}
    else{
      IBM_cooc <- cooccurrence(   	
        x = subset(annot.obj(), upos %in% input$upos), 
        term = "lemma", 
        group = c("doc_id", "paragraph_id", "sentence_id"))
      
      wordnetwork <- head(IBM_cooc, 30)
      wordnetwork <- igraph::graph_from_data_frame(wordnetwork)
      
      ggraph(wordnetwork, layout = "fr") +  
        
        geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +  
        geom_node_text(aes(label = name), col = "darkgreen", size = 6) +
        
        theme_graph(base_family = "Arial Narrow") +  
        theme(legend.position = "none") +
        
        labs(title = "Top 30 Co-occurrences", subtitle = "Select the check boxes in the left pane")
    }
  })
})
