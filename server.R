
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#
library(reshape2)
library(shiny)
library(plyr)
library(ggplot2)
library(shinyIncubator)
library(googleVis)
library(sqldf)

shinyServer(function(input, output, session) {
  ###############################################################################################################
  #
  #                                                INITIAL DATA LOAD
  #
  ###############################################################################################################
  
  commit.extraction = reactive({
    inFile<-input$fileInput
    if(is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header=input$header, sep=input$sep)
  })
  
  createDataFrames <- function(){
    if (!exists("commit.extraction")){
      commit.extraction <- read.csv("commit-extraction.csv")
    }
    if (!exists("dataFrame")){
      dataFrame <<- as.data.frame.matrix(commit.extraction) 
      
      # Adds columns with different format dates
      
      dataFrame$newFormatDate <<- as.Date(dataFrame$Date, format="%Y-%m-%d")
      dataFrame$month_year <<- format(dataFrame$newFormatDate,"%Y-%m")
      dataFrame$week_year <<- format(dataFrame$newFormatDate,"%Y-%U")
      dataFrame$weekYear <<- as.numeric(format(dataFrame$newFormatDate,"%U"))
      dataFrame$dayYear <<- as.numeric(format(dataFrame$newFormatDate,"%j"))
      dataFrame$year <<- as.numeric(format(dataFrame$newFormatDate,"%Y"))
      
      ########################## Calculate the quantiles for whole period of the time by project
      
      # Identify the maximun and minimal year by repository
      temp <- ddply(dataFrame, c("Repository" , "Branch"), summarise, min =  min(year), max =  max(year))
      
      # Join the dataframe table with the maximun and minimal year identified in temp table
      dataFrame <<- sqldf("select dataFrame.* , temp.min, temp.max  from dataFrame join temp on dataFrame.Repository = temp.Repository and dataFrame.Branch = temp.Branch") 
      
      # Subtract the year which the commit was performed, with the minimum year of the project and multiply by 360 days, the reason is consider the number of days for whole project 
      # For instance: 
      # Project Start Date: 01/01/2012
      # Commit Date: 01/01/2014
      # 2014 - 2012 = 2 years
      # 360 days x 2 years = 720 days
      # dayYear is between 1 and 360, therefore the commit day is not 01 it will be 721
      dataFrame$day <<- with(dataFrame , (as.numeric(year) - as.numeric(min)) * 360)
      dataFrame$day <<- dataFrame$day +  dataFrame$dayYear
      
      # Identify the quantile by the days of the project
      temp2 <- ddply(dataFrame, .(Repository, Branch), function(x) quantile(x$day))
      
      # Join the dataframe table with the quantiles identify by repository
      dataFrame <<- sqldf("select dataFrame.* , X0_, X25_, X50_, X75_, X100_  from dataFrame join temp2 on dataFrame.Repository = temp2.Repository and dataFrame.Branch = temp2.Branch")
      
      # Identify which days belongs a specific quantile, and add the column
      temp1 <- subset(dataFrame, dataFrame$day <= as.numeric(dataFrame$X25_))
      temp1$quantile <- "0 - 0.25"
      temp2 <- subset(dataFrame, dataFrame$day > as.numeric(dataFrame$X25_) & dataFrame$day <= as.numeric(dataFrame$X50_))
      temp2$quantile <- "0.25 - 0.50"
      temp3 <- subset(dataFrame, dataFrame$day > as.numeric(dataFrame$X50_) & dataFrame$day <= as.numeric(dataFrame$X75_))
      temp3$quantile <- "0.50 - 0.75"
      temp4 <- subset(dataFrame, dataFrame$day > as.numeric(dataFrame$X75_))
      temp4$quantile <- "0.75 - 1"
      
      # Delete the dataframe
      rm(dataFrame)
      
      # Rebuild the dataframe
      dataFrame <<- rbind(temp1,temp2,temp3,temp4)
      
      # Delete temporal data
      rm(temp1)
      rm(temp2)
      rm(temp3)
      rm(temp4)
    }
  }

  ###############################################################################################################
  #
  #   LOAD DATA "dataFrameRepositories" BASED ON USER PREFERENCES (REPOSITORIES, BRANCHES AND CATEGORIES)
  #
  ###############################################################################################################
 
  getDataFrameRepositories <- function (){
    repositoriesSelected <- as.character(input$checkboxRepositories)
    dataFrameRepositories <- subset(dataFrame, Repository %in% repositoriesSelected)
    return (dataFrameRepositories)
  }

  getDataFrameBranches <- function (branchesSelected, dataFrameRepositories){
    dataFrameRepositories <- subset(dataFrameRepositories, Branch %in% branchesSelected)
    return (dataFrameRepositories)
  }
  
  getDataFrameCategories <- function (categoriesSelected, branchesSelected, dataFrameRepositories){
    dataFrameRepositories <- subset(dataFrameRepositories, Branch %in% branchesSelected)
    dataFrameRepositories <- subset(dataFrameRepositories, Category %in% categoriesSelected)
    return (dataFrameRepositories)
  }

  ###############################################################################################################
  #
  #               GET BRANCHES AND CATEGORIES NAMES, TO BE USED IN THE INPUTS CONTROLS
  #
  ###############################################################################################################
  
  getRepositories <- function (){
    dataFrameMain <- ddply(dataFrame, c("Repository"), summarize, N=length(Repository))
    listRepositories <- as.character(dataFrameMain$Repository)
    return (listRepositories)
  }
    
  getBranches <- function (dataFrameRepositories){
    dataFrameBranches <- ddply(dataFrameRepositories, c("Branch"), summarize, N=length(Branch))
    listBranchesByRepo <- as.character(dataFrameBranches$Branch)
    return (listBranchesByRepo)
  }
  
  getCategories <- function (dataFrameRepositories){
    branchesSelected <- as.character(input$checkboxBranches)
    dataFrameRepositories <- getDataFrameBranches(branchesSelected, dataFrameRepositories)
    dataFrameCategories <- ddply(dataFrameRepositories, c("Category"), summarize, N=length(Category))
    listCategoriesByRepo <- as.character(dataFrameCategories$Category)
    return (listCategoriesByRepo)
  }
  
  setUserConfiguration <- function (dataFrameRepositories){
    branchesSelected <- as.character(input$checkboxBranches)
    categoriesSelected <- as.character(input$checkboxCategories)
    lengthCategories <- length(categoriesSelected)
    lengthBranches <- length(branchesSelected)
    isDisplayGraph <- FALSE
    if (lengthCategories > 0 && lengthBranches > 0  && !is.null(dataFrameRepositories)){
      isDisplayGraph <- TRUE
    }
    return (isDisplayGraph)
  }
  
  output$repositoriesControls <- renderUI({ 
    createDataFrames()
    if (!exists("listRepositories")) {
      listRepositories <- getRepositories()
    }
    isShowAllRepo <- as.character(input$checkboxShowAllRepo)
    if(isShowAllRepo){
      checkboxGroupInput("checkboxRepositories", label = h3("Repositories"), choices = listRepositories, selected = listRepositories)
    }else{
      checkboxGroupInput("checkboxRepositories", label = h3("Repositories"), choices = listRepositories, selected = listRepositories[1])
    } 
  })
  
  output$branchesControls <- renderUI({ 
    if (length(input$checkboxRepositories) > 0) {
      dataFrameRepositories <- getDataFrameRepositories()
      dataFrameRepositories <- setGeneralStuffsForGraphs(dataFrameRepositories)
      listBranchesByRepo <- getBranches(dataFrameRepositories)
      checkboxGroupInput("checkboxBranches", label = h3("Branches"), choices = listBranchesByRepo, selected = listBranchesByRepo)
    }
  })

  output$categoriesControls <- renderUI({ 
    if (length(input$checkboxRepositories) > 0 & length(input$checkboxBranches) >0) {
      dataFrameRepositories <- getDataFrameRepositories()
      dataFrameRepositories <- setGeneralStuffsForGraphs(dataFrameRepositories)
      listCategoriesByRepo <- getCategories(dataFrameRepositories)
      checkboxGroupInput("checkboxCategories", label = h3("Categories"), choices = listCategoriesByRepo, selected = listCategoriesByRepo)
    }
  }) 
  
  ###############################################################################################################
  #
  #                                          TRANSFORM DATA
  #
  ###############################################################################################################
  
  setGeneralStuffsForGraphs <- function (dataFrameRepositories){
     isIgnoreHighValues <- as.character(input$checkboxIgnoreHighValues)
     if(isIgnoreHighValues)
          dataFrameRepositories <- subset(dataFrameRepositories, Changes < quantile(dataFrame$Changes,0.80))
     return (dataFrameRepositories)
  }
  
  ###############################################################################################################
  #
  #                                         TAB - GRAPH BOXPLOT
  #
  ###############################################################################################################
    
  output$graph1 <- renderPlot({
    isShowByRepo <- as.character(input$checkboxByRepo)
    branchesSelected <- as.character(input$checkboxBranches)
    categoriesSelected <- as.character(input$checkboxCategories)
    repositoriesSelected <- as.character(input$checkboxRepositories)
    lengthRepositories <- length(repositoriesSelected)
    if (lengthRepositories > 0){
      dataFrameRepositories <- getDataFrameRepositories()
    }
    isDisplayGraph = setUserConfiguration(dataFrameRepositories)
    if (isDisplayGraph){
      dataFrameRepositories <- setGeneralStuffsForGraphs(dataFrameRepositories)
      dataFrameRepositories <- getDataFrameBranches(branchesSelected, dataFrameRepositories)
      dataFrameRepositories <- getDataFrameCategories(categoriesSelected, branchesSelected, dataFrameRepositories)
      if (isShowByRepo)
         ggplot(dataFrameRepositories, aes(x = Category, y = Changes, fill = Category)) + geom_boxplot() +  labs(title="Changes by Category", x="Categories", y="Number of Changes") + facet_grid(Repository ~ ., scale = "free_y")
      else
         ggplot(dataFrameRepositories, aes(x = Category, y = Changes, fill = Category)) + geom_boxplot() +  labs(title="Changes by Category", x="Categories", y="Number of Changes")
    }
  })
  
  ###############################################################################################################
  #
  #                                         TAB - GRAPH BAR CHART  
  #
  ###############################################################################################################
  
  output$graph2 <- renderPlot({
    branchesSelected <- as.character(input$checkboxBranches)
    categoriesSelected <- as.character(input$checkboxCategories)
    repositoriesSelected <- as.character(input$checkboxRepositories)
    lengthRepositories <- length(repositoriesSelected)
    if (lengthRepositories > 0){
      dataFrameRepositories <- getDataFrameRepositories()
    }
    isDisplayGraph = setUserConfiguration(dataFrameRepositories)
    if (isDisplayGraph){
      dataFrameRepositories <- setGeneralStuffsForGraphs(dataFrameRepositories)
      dataFrameRepositories <- getDataFrameBranches(branchesSelected, dataFrameRepositories)
      dataFrameRepositories <- getDataFrameCategories(categoriesSelected,branchesSelected,  dataFrameRepositories)
      dataFrameRepositories <- ddply(dataFrameRepositories, c("Category", "quantile"), summarize, sumChanges = sum(Changes))
      ggplot(dataFrameRepositories, aes(x = quantile, y = sumChanges, fill = Category)) + geom_bar(stat = "identity") +  labs(title="Total Number of Changes by Category", x="Time", y="Total Number of Changes by Category") 
    } 
  })
  
  ###############################################################################################################
  #
  #                                         TAB - GOOGLE
  #
  ###############################################################################################################
  
  output$motionchart <- renderGvis({
    branchesSelected <- as.character(input$checkboxBranches)
    categoriesSelected <- as.character(input$checkboxCategories)
    repositoriesSelected <- as.character(input$checkboxRepositories)
    lengthRepositories <- length(repositoriesSelected)
    if (lengthRepositories > 0){
      dataFrameRepositories <- getDataFrameRepositories()
    }
    isDisplayGraph = setUserConfiguration(dataFrameRepositories)
    if (isDisplayGraph){
      dataFrameRepositories <- setGeneralStuffsForGraphs(dataFrameRepositories)
      dataFrameRepositories <- getDataFrameBranches(branchesSelected, dataFrameRepositories)
      dataFrameRepositories <- getDataFrameCategories(categoriesSelected, branchesSelected, dataFrameRepositories)
      dataFrameRepositories <- ddply(dataFrameRepositories, c("Category", "quantile"), summarize, sumChanges = sum(Changes))
      dataFrameRepositories <- dcast(dataFrameRepositories, quantile ~ Category, value.var="sumChanges")
      gvisColumnChart(dataFrameRepositories,xvar="quantile", yvar=categoriesSelected, options=list(isStacked=TRUE,focusTarget='category', height=500) )
    }
  })  
    
  ###############################################################################################################
  #
  #                              TAB - GRAPH BAR CHART 3.1, 3.2, 3.3, 3.4
  #
  ###############################################################################################################
  
  ################################################# Graph Bar Chart 3.1,  quantile == "0 - 0.25"
  
  output$graph3.1 <- renderPlot({
    branchesSelected <- as.character(input$checkboxBranches)
    categoriesSelected <- as.character(input$checkboxCategories)
    repositoriesSelected <- as.character(input$checkboxRepositories)
    lengthRepositories <- length(repositoriesSelected)
    if (lengthRepositories > 0){
      dataFrameRepositories <- getDataFrameRepositories()
    }
    isDisplayGraph = setUserConfiguration(dataFrameRepositories)
    if (isDisplayGraph){
      dataFrameRepositories <- setGeneralStuffsForGraphs(dataFrameRepositories)
      dataFrameRepositories <- getDataFrameBranches(branchesSelected, dataFrameRepositories)
      dataFrameRepositories <- getDataFrameCategories(categoriesSelected, branchesSelected, dataFrameRepositories)
      dataFrameRepositories <- ddply(dataFrameRepositories, c("Category", "quantile"), summarize, sumChanges = sum(Changes))
      dataFrameRepositories <- subset(dataFrameRepositories, quantile == "0 - 0.25")
      withProgress(session, min=1, max=30, expr={
        for(i in 1:30) {
          setProgress(message = 'Calculation in progress', detail = 'This may take a while...', value=i)
          print(i)
          Sys.sleep(0.1)
        }
      })
      ggplot(dataFrameRepositories, aes(x = Category, y = sumChanges, fill = Category, Group = Category)) + geom_bar(stat = "identity") +  labs(title="Total Number of Changes by Category Q1", x="Time Q1", y="Total Number of Changes by Category")
    }
  })
  
  ################################################# Graph Bar Chart 3.2, quantile == "0.25 - 0.50"
  
  output$graph3.2 <- renderPlot({
    branchesSelected <- as.character(input$checkboxBranches)
    categoriesSelected <- as.character(input$checkboxCategories)
    repositoriesSelected <- as.character(input$checkboxRepositories)
    lengthRepositories <- length(repositoriesSelected)
    if (lengthRepositories > 0){
      dataFrameRepositories <- getDataFrameRepositories()
    }
    isDisplayGraph = setUserConfiguration(dataFrameRepositories)
    if (isDisplayGraph){
      dataFrameRepositories <- setGeneralStuffsForGraphs(dataFrameRepositories)
      dataFrameRepositories <- getDataFrameBranches(branchesSelected, dataFrameRepositories)
      dataFrameRepositories <- getDataFrameCategories(categoriesSelected,branchesSelected, dataFrameRepositories)
      dataFrameRepositories <- ddply(dataFrameRepositories, c("Category", "quantile"), summarize, sumChanges = sum(Changes))
      dataFrameRepositories <- subset(dataFrameRepositories, quantile == "0.25 - 0.50")
      ggplot(dataFrameRepositories, aes(x = Category, y = sumChanges, fill = Category, Group = Category)) + geom_bar(stat = "identity") +  labs(title="Total Number of Changes by Category Q2", x="Time Q2", y="Total Number of Changes by Category")
    }
  })
  
  ################################################# Graph Bar Chart 3.3,  quantile == "0.50 - 0.75"
  
  output$graph3.3 <- renderPlot({
    branchesSelected <- as.character(input$checkboxBranches)
    categoriesSelected <- as.character(input$checkboxCategories)
    repositoriesSelected <- as.character(input$checkboxRepositories)
    lengthRepositories <- length(repositoriesSelected)
    if (lengthRepositories > 0){
      dataFrameRepositories <- getDataFrameRepositories()
    }
    isDisplayGraph = setUserConfiguration(dataFrameRepositories)
    if (isDisplayGraph){
      dataFrameRepositories <- setGeneralStuffsForGraphs(dataFrameRepositories)
      dataFrameRepositories <- getDataFrameBranches(branchesSelected, dataFrameRepositories)
      dataFrameRepositories <- getDataFrameCategories(categoriesSelected, branchesSelected, dataFrameRepositories)
      dataFrameRepositories <- ddply(dataFrameRepositories, c("Category", "quantile"), summarize, sumChanges = sum(Changes))
      dataFrameRepositories <- subset(dataFrameRepositories, quantile == "0.50 - 0.75")
      ggplot(dataFrameRepositories, aes(x = Category, y = sumChanges, fill = Category, Group = Category)) + geom_bar(stat = "identity") +  labs(title="Total Number of Changes by Category Q3", x="Time Q3", y="Total Number of Changes by Category")
    }
    
  })
  
  ################################################# Graph Bar Chart 3.4, quantile == "0.75 - 1"
  
  output$graph3.4 <- renderPlot({
    branchesSelected <- as.character(input$checkboxBranches)
    categoriesSelected <- as.character(input$checkboxCategories)
    repositoriesSelected <- as.character(input$checkboxRepositories)
    lengthRepositories <- length(repositoriesSelected)
    if (lengthRepositories > 0){
      dataFrameRepositories <- getDataFrameRepositories()
    }
    isDisplayGraph = setUserConfiguration(dataFrameRepositories)
    if (isDisplayGraph){
      dataFrameRepositories <- setGeneralStuffsForGraphs(dataFrameRepositories)
      dataFrameRepositories <- getDataFrameBranches(branchesSelected, dataFrameRepositories)
      dataFrameRepositories <- getDataFrameCategories(categoriesSelected, branchesSelected, dataFrameRepositories)
      dataFrameRepositories <- ddply(dataFrameRepositories, c("Category", "quantile"), summarize, sumChanges = sum(Changes))
      dataFrameRepositories <- subset(dataFrameRepositories, quantile == "0.75 - 1")
      ggplot(dataFrameRepositories, aes(x = Category, y = sumChanges, fill = Category, Group = Category)) + geom_bar(stat = "identity") +  labs(title="Total Number of Changes by Category Q4", x="Time Q4", y="Total Number of Changes by Category")
    }
    
  })
  
  ###############################################################################################################
  #
  #                              TAB4 - TABLE Commits
  #
  ###############################################################################################################
   
  output$table <- renderTable({
    branchesSelected <- as.character(input$checkboxBranches)
    categoriesSelected <- as.character(input$checkboxCategories)
    repositoriesSelected <- as.character(input$checkboxRepositories)
    lengthRepositories <- length(repositoriesSelected)
    if (lengthRepositories > 0){
      dataFrameRepositories <- getDataFrameRepositories()
    }
    isDisplayGraph = setUserConfiguration(dataFrameRepositories)
    if (isDisplayGraph){
      dataFrameRepositories <- setGeneralStuffsForGraphs(dataFrameRepositories)
      dataFrameRepositories <- getDataFrameBranches(branchesSelected, dataFrameRepositories)
      dataFrameRepositories <- getDataFrameCategories(categoriesSelected, branchesSelected, dataFrameRepositories)
      dataFrameCommits <- ddply(dataFrameRepositories, c("From","Repository", "Branch", "Author", "SHA"), summarize, N=length(SHA))
      dataFrameCommits <- ddply(dataFrameCommits, c("From", "Repository", "Branch", "Author"), summarize, Commits=length(Author)) 
    }
 
  })
  
  ###############################################################################################################
  #
  #                              TAB4 - TABLE Commits Details
  #
  ###############################################################################################################
  
  output$table2 <- renderTable({
    branchesSelected <- as.character(input$checkboxBranches)
    categoriesSelected <- as.character(input$checkboxCategories)
    repositoriesSelected <- as.character(input$checkboxRepositories)
    lengthRepositories <- length(repositoriesSelected)
    if (lengthRepositories > 0){
      dataFrameRepositories <- getDataFrameRepositories()
    }
    isDisplayGraph = setUserConfiguration(dataFrameRepositories)
    if (isDisplayGraph){
      dataFrameRepositories <- setGeneralStuffsForGraphs(dataFrameRepositories)
      dataFrameRepositories <- getDataFrameBranches(branchesSelected, dataFrameRepositories)
      dataFrameRepositories <- getDataFrameCategories(categoriesSelected, branchesSelected, dataFrameRepositories)
      dataFrameCommits <- ddply(dataFrameRepositories, c("From","Repository", "Branch", "Author", "Date", "SHA", "Message"), summarize, Files=length(Message), Changes = sum(Changes))
    }
    
  })
  
  ###############################################################################################################
  #
  #                              TAB -  GRAPH BAR CHART 4.1, 4.2, 4.3 AND 4.4
  #
  ###############################################################################################################

  ################################################# Graph Bar Chart 4.1 
  output$graph4.1 <- renderPlot({
    quantileSelected <- as.character(input$selectQuantile)
    branchesSelected <- as.character(input$selectBranch4.1)
    repositoriesSelected <- as.character(input$selectRepository4.1)
    lengthRepositories <- length(repositoriesSelected)
    lengthBranches <- length(branchesSelected)
    if (lengthRepositories > 0 && lengthBranches > 0){
      dataFrameRepositories <- subset(dataFrame, Repository %in% repositoriesSelected)
      dataFrameRepositories <- subset(dataFrameRepositories, Branch %in% branchesSelected)
      dataFrameRepositories <- setGeneralStuffsForGraphs(dataFrameRepositories)
      dataFrameRepositories <- ddply(dataFrameRepositories, c("Category", "quantile"), summarize, sumChanges = sum(Changes))
      if (quantileSelected == "Q1")
        dataFrameRepositories <- subset(dataFrameRepositories, quantile == "0 - 0.25")
      else if (quantileSelected == "Q2")
        dataFrameRepositories <- subset(dataFrameRepositories, quantile == "0.25 - 0.50")
      else if (quantileSelected == "Q2")
        dataFrameRepositories <- subset(dataFrameRepositories, quantile == "0.50 - 0.75")
      else
        dataFrameRepositories <- subset(dataFrameRepositories, quantile == "0.75 - 1")
      withProgress(session, min=1, max=30, expr={
        for(i in 1:30) {
          setProgress(message = 'Calculation in progress',detail = 'This may take a while...', value=i)
          print(i)
          Sys.sleep(0.1)
        }
      })
      ggplot(dataFrameRepositories, aes(x = Category, y = sumChanges, fill = Category, Group = Category)) + geom_bar(stat = "identity") +  labs(title="Total Number of Changes by Category Q1", x="Time Q1", y="Total Number of Changes by Category")
    }
  })
  
  output$controls4.1.1 <- renderUI({ 
    if (!exists("listRepositories")) {
      listRepositories <- getRepositories()
    }
    selectInput("selectRepository4.1", label = h3("Select Repository"), choices = listRepositories, selected = listRepositories[1])
  })
  
  output$controls4.1.2 <- renderUI({ 
    repositoriesSelected <- as.character(input$selectRepository4.1)
    dataFrameRepositories <- subset(dataFrame, Repository %in% repositoriesSelected)
    listBranchesByRepo <- getBranches(dataFrameRepositories)
    selectInput("selectBranch4.1", label = h3("Select Branch"), choices = listBranchesByRepo, selected = listBranchesByRepo[1])
  })
  
  ################################################# Graph Bar Chart 4.2
  
  output$graph4.2 <- renderPlot({
    quantileSelected <- as.character(input$selectQuantile)
    branchesSelected <- as.character(input$selectBranch4.2)
    repositoriesSelected <- as.character(input$selectRepository4.2)
    lengthRepositories <- length(repositoriesSelected)
    lengthBranches <- length(branchesSelected)
    if (lengthRepositories > 0 && lengthBranches > 0){
      dataFrameRepositories <- subset(dataFrame, Repository %in% repositoriesSelected)
      dataFrameRepositories <- subset(dataFrameRepositories, Branch %in% branchesSelected)
      dataFrameRepositories <- setGeneralStuffsForGraphs(dataFrameRepositories)
      dataFrameRepositories <- ddply(dataFrameRepositories, c("Category", "quantile"), summarize, sumChanges = sum(Changes))
      if (quantileSelected == "Q1")
        dataFrameRepositories <- subset(dataFrameRepositories, quantile == "0 - 0.25")
      else if (quantileSelected == "Q2")
        dataFrameRepositories <- subset(dataFrameRepositories, quantile == "0.25 - 0.50")
      else if (quantileSelected == "Q2")
        dataFrameRepositories <- subset(dataFrameRepositories, quantile == "0.50 - 0.75")
      else
        dataFrameRepositories <- subset(dataFrameRepositories, quantile == "0.75 - 1")
      ggplot(dataFrameRepositories, aes(x = Category, y = sumChanges, fill = Category, Group = Category)) + geom_bar(stat = "identity") +  labs(title="Total Number of Changes by Category Q1", x="Time Q1", y="Total Number of Changes by Category")
    }
  })
  
  output$controls4.2.1 <- renderUI({ 
    if (!exists("listRepositories")) {
      listRepositories <- getRepositories()
    }
    selectInput("selectRepository4.2", label = h3("Select Repository"), choices = listRepositories, selected = listRepositories[1])
  })
  
  output$controls4.2.2 <- renderUI({ 
    repositoriesSelected <- as.character(input$selectRepository4.2)
    dataFrameRepositories <- subset(dataFrame, Repository %in% repositoriesSelected)
    listBranchesByRepo <- getBranches(dataFrameRepositories)
    selectInput("selectBranch4.2", label = h3("Select Branch"), choices = listBranchesByRepo, selected = listBranchesByRepo[1])
  })
  
  ######################################################## Graph Bar Chart 4.3

  output$graph4.3 <- renderPlot({
    quantileSelected <- as.character(input$selectQuantile)
    branchesSelected <- as.character(input$selectBranch4.3)
    repositoriesSelected <- as.character(input$selectRepository4.3)
    lengthRepositories <- length(repositoriesSelected)
    lengthBranches <- length(branchesSelected)
    if (lengthRepositories > 0 && lengthBranches > 0){
      dataFrameRepositories <- subset(dataFrame, Repository %in% repositoriesSelected)
      dataFrameRepositories <- subset(dataFrameRepositories, Branch %in% branchesSelected)
      dataFrameRepositories <- setGeneralStuffsForGraphs(dataFrameRepositories)
      dataFrameRepositories <- ddply(dataFrameRepositories, c("Category", "quantile"), summarize, sumChanges = sum(Changes))
      if (quantileSelected == "Q1")
        dataFrameRepositories <- subset(dataFrameRepositories, quantile == "0 - 0.25")
      else if (quantileSelected == "Q2")
        dataFrameRepositories <- subset(dataFrameRepositories, quantile == "0.25 - 0.50")
      else if (quantileSelected == "Q2")
        dataFrameRepositories <- subset(dataFrameRepositories, quantile == "0.50 - 0.75")
      else
        dataFrameRepositories <- subset(dataFrameRepositories, quantile == "0.75 - 1")
      ggplot(dataFrameRepositories, aes(x = Category, y = sumChanges, fill = Category, Group = Category)) + geom_bar(stat = "identity") +  labs(title="Total Number of Changes by Category Q1", x="Time Q1", y="Total Number of Changes by Category")
    }
  })
  
  output$controls4.3.1 <- renderUI({ 
    if (!exists("listRepositories")) {
      listRepositories <- getRepositories()
    }
    selectInput("selectRepository4.3", label = h3("Select Repository"), choices = listRepositories, selected = listRepositories[1])
  })
  
  output$controls4.3.2 <- renderUI({ 
    repositoriesSelected <- as.character(input$selectRepository4.3)
    dataFrameRepositories <- subset(dataFrame, Repository %in% repositoriesSelected)
    listBranchesByRepo <- getBranches(dataFrameRepositories)
    selectInput("selectBranch4.3", label = h3("Select Branch"), choices = listBranchesByRepo, selected = listBranchesByRepo[1])
  })
  
  ######################################################## Graph Bar Chart 4.4 

  output$graph4.4 <- renderPlot({
    quantileSelected <- as.character(input$selectQuantile)
    branchesSelected <- as.character(input$selectBranch4.4)
    repositoriesSelected <- as.character(input$selectRepository4.4)
    lengthRepositories <- length(repositoriesSelected)
    lengthBranches <- length(branchesSelected)
    if (lengthRepositories > 0 && lengthBranches > 0){
      dataFrameRepositories <- subset(dataFrame, Repository %in% repositoriesSelected)
      dataFrameRepositories <- subset(dataFrameRepositories, Branch %in% branchesSelected)
      dataFrameRepositories <- setGeneralStuffsForGraphs(dataFrameRepositories)
      dataFrameRepositories <- ddply(dataFrameRepositories, c("Category", "quantile"), summarize, sumChanges = sum(Changes))
      if (quantileSelected == "Q1")
        dataFrameRepositories <- subset(dataFrameRepositories, quantile == "0 - 0.25")
      else if (quantileSelected == "Q2")
        dataFrameRepositories <- subset(dataFrameRepositories, quantile == "0.25 - 0.50")
      else if (quantileSelected == "Q2")
        dataFrameRepositories <- subset(dataFrameRepositories, quantile == "0.50 - 0.75")
      else
        dataFrameRepositories <- subset(dataFrameRepositories, quantile == "0.75 - 1")
      ggplot(dataFrameRepositories, aes(x = Category, y = sumChanges, fill = Category, Group = Category)) + geom_bar(stat = "identity") +  labs(title="Total Number of Changes by Category Q1", x="Time Q1", y="Total Number of Changes by Category")
    }
  })
  
  output$controls4.4.1 <- renderUI({ 
    if (!exists("listRepositories")) {
      listRepositories <- getRepositories()
    }
    selectInput("selectRepository4.4", label = h3("Select Repository"), choices = listRepositories, selected = listRepositories[1])
  })
  
  output$controls4.4.2 <- renderUI({ 
    repositoriesSelected <- as.character(input$selectRepository4.4)
    dataFrameRepositories <- subset(dataFrame, Repository %in% repositoriesSelected)
    listBranchesByRepo <- getBranches(dataFrameRepositories)
    selectInput("selectBranch4.4", label = h3("Select Branch"), choices = listBranchesByRepo, selected = listBranchesByRepo[1])
  })
  
  ###############################################################################################################
  #
  #                              TAB -  GRAPH AREA
  #
  ###############################################################################################################
  
  output$graph5 <- renderPlot({
    period <- as.character(input$radioButtonsPeriod)
    isShowByRepo <- as.character(input$checkboxByRepo2)
    typeGraph <- as.character(input$radioButtonsTypeGraph)
    isShowCumulative <- as.character(input$checkboxCumulative)
    branchesSelected <- as.character(input$checkboxBranches)
    categoriesSelected <- as.character(input$checkboxCategories)
    repositoriesSelected <- as.character(input$checkboxRepositories)
    lengthRepositories <- length(repositoriesSelected)
    if (lengthRepositories > 0){
      dataFrameRepositories <- getDataFrameRepositories()
    }
    isDisplayGraph = setUserConfiguration(dataFrameRepositories)
    if (isDisplayGraph){
      dataFrameRepositories <- setGeneralStuffsForGraphs(dataFrameRepositories)
      dataFrameRepositories <- getDataFrameBranches(branchesSelected, dataFrameRepositories)
      dataFrameRepositories <- getDataFrameCategories(categoriesSelected, branchesSelected, dataFrameRepositories)
      
      ################################## Display the data Weekly
      if (period == "Weekly"){
        dataFrameRepositories <- ddply(dataFrameRepositories, c("Repository","Branch","Category", "week_year"), summarize, sumChanges = sum(Changes))
        if (isShowCumulative){
          dataFrameRepositories <- ddply(dataFrameRepositories, c("Repository","Branch","Category"), transform, cumChanges = cumsum(sumChanges))
          plot <- ggplot(dataFrameRepositories, aes(x = week_year, y = cumChanges, fill = Category, group = Category)) 
        }
        else{
          plot <- ggplot(dataFrameRepositories, aes(x = week_year, y = sumChanges, fill = Category, group = Category)) 
        }
      }
      ################################## Display the data Montly
      else if (period == "Monthly"){
        dataFrameRepositories <- ddply(dataFrameRepositories, c("Repository","Branch","Category", "month_year"), summarize, sumChanges = sum(Changes))
        if (isShowCumulative){
          dataFrameRepositories <- ddply(dataFrameRepositories, c("Repository","Branch","Category"), transform, cumChanges = cumsum(sumChanges))
          plot <- ggplot(dataFrameRepositories, aes(x = month_year, y = cumChanges, fill = Category, group = Category)) 
        }
        else{
          plot <- ggplot(dataFrameRepositories, aes(x = month_year, y = sumChanges, fill = Category, group = Category)) 
        }
      }
      ################################## Display the data Anualy
      else{
        dataFrameRepositories <- ddply(dataFrameRepositories, c("Repository","Branch","Category", "year"), summarize, sumChanges = sum(Changes))
        if (isShowCumulative){
          dataFrameRepositories <- ddply(dataFrameRepositories, c("Repository","Branch","Category"), transform, cumChanges = cumsum(sumChanges))
          plot <- ggplot(dataFrameRepositories, aes(x = year, y = cumChanges, fill = Category, group = Category)) 
        }
        else{
          plot <- ggplot(dataFrameRepositories, aes(x = year, y = sumChanges, fill = Category, group = Category))  
        }
      }
     ################################## Display type of grapth (stack)
     if (typeGraph == "Stack"){
        position <- "stack"
        plot <- plot + geom_area(position=position) 
      }
     ################################## Display type of grapth (identity)
      else{
        position <- "identity"
        plot <- plot +  geom_point(aes(colour = factor(Category))) + geom_line() + geom_area(position=position, alpha=.5) 
      }
      plot  <- plot + labs(title="Total Number of Changes", x="Date", y="Total Number of Changes by Category")
    
    ################################## Display graphs by repository
      if (isShowByRepo)
        plot  <- plot + facet_grid(Repository ~ ., scale = "free_y")
      return (plot)
    
     }
  })
})
