# Group Members
# Arushi Mahajan, ID: 11915079
# Kapil Bindal, ID:
# Sivesh Kango, ID:


library("shiny")

# Defining ui function
ui <- shinyUI(
  fluidPage(
    
    titlePanel("Shiny App- Around the UDPipe NLP workflow"),
    
    sidebarLayout( 
      
      sidebarPanel(  
        
        fileInput("file", "Upload data (Text File)"),
        
        
        checkboxGroupInput(inputId = 'upos',
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
                             plotOutput('wcplot1'),
                             h3("Verbs"),
                             plotOutput('wcplot2')),
                    
                    tabPanel("Co-Occurrences",
                             h3("Co-occurrences"),
                             plotOutput('coocplot3'))
                    
        ) # end of tabsetPanel
      )# end of main panel
    ) # end of sidebarLayout
  )  # end if fluidPage
) # end of UI