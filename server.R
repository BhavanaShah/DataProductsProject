#Coursera Data Product Project
#Bhavana Shah

#server.R
library(shiny)

#Read in the Heart Data
hd <- read.csv("HeartData.csv", stringsAsFactors=FALSE, na.strings = c("", "NA", "NULL", "?"))

#Pre-process

#Add a column to put result 0 or 1
hd$result  <- apply(hd, 1, function(row) 
        {if (row["num"] > 0) row["result"] = 1 else row["result"] = 0 })

#Filtering only the complete cases
ccIndx <- complete.cases(hd)
hd <- hd[ccIndx,]

#Diagnosis/Result: This really should be a factor, so 
hd$result <- factor(hd$result, levels = c(0, 1), labels = c("Negative", "Positive"))

#Dropping the num column, as it has been replaced by result
hd  <- hd[-c(14)]

# ------------ Random Forest

###Splitting data into training and testing sets

library(caret)
library(randomForest)
library(corrplot)

set.seed(999)
trainIndex  <- createDataPartition(hd$result, p = 0.70, list = FALSE)
training  <- hd[trainIndex,] #70%
testing  <- hd[-trainIndex,] #30%

set.seed(999)

shinyServer(function(input, output, session) {
        #About
        output$about <- renderPrint({
        HTML("<br/>
        Project: Coursera Data Products Project<br/>
        By: Bhavana Shah<br/>
        <p>
        Dataset: The dataset is obtained from UCI Machine Learning Repository<br/>        
        <a href = https://archive.ics.uci.edu/ml/datasets/Heart+Disease>UCI Site Link</a><br/>
        </p>
        Title: Heart Disease Databases
        <p>
        Source Information:<br/>
           (a) Creators: <br/>
               -- 1. Hungarian Institute of Cardiology. Budapest: Andras Janosi, M.D.<br/>
               -- 2. University Hospital, Zurich, Switzerland: William Steinbrunn, M.D.<br/>
               -- 3. University Hospital, Basel, Switzerland: Matthias Pfisterer, M.D.<br/>
               -- 4. V.A. Medical Center, Long Beach and Cleveland Clinic Foundation:
                     Robert Detrano, M.D., Ph.D.<br/>
           (b) Donor: David W. Aha (aha@ics.uci.edu) (714) 856-8779   <br/>
           (c) Date: July, 1988<br/>
        </p>
        <p>  For the project, 'processed.cleveland.data' was used.                
             This database contains 76 attributes, but only a subset of 14 of them have been used.<br/> 
      -- 1. #3  (age)       <br/>
      -- 2. #4  (sex)       <br/>
      -- 3. #9  (cp)        <br/>
      -- 4. #10 (trestbps)  <br/>
      -- 5. #12 (chol)      <br/>
      -- 6. #16 (fbs)       <br/>
      -- 7. #19 (restecg)   <br/>
      -- 8. #32 (thalach)   <br/>
      -- 9. #38 (exang)     <br/>
      -- 10. #40 (oldpeak)   <br/>
      -- 11. #41 (slope)     <br/>
      -- 12. #44 (ca)        <br/>
      -- 13. #51 (thal)      <br/>
      -- 14. #58 (num)       (the predicted attribute)<br/>

             The 'num' field refers to the presence of heart disease
             in the patient.  It is integer valued from 0 (no presence) to 4.
             Experiments with the Cleveland database have concentrated on simply
             attempting to distinguish presence (values 1,2,3,4) from absence (value
             0).  
        </p> 
<p><br/>
        Pre-processing on the dataset<br>
        The 'num' was replaced with 'result' with 0 indicating 'Negative' and 1 indicating 'positive', presence of heart disease. In addition, rows with missing values were removed, prior to model creation.
</p>
        ")})
        
        #Data
        output$dtOutput <- renderDataTable(
                {hd}, options = list(lengthMenu = c(10,15,30), pageLength = 10))
        
        #Exploration
        output$corrPlot  <- renderPlot({              
                cor_pt <- cor(hd[, sapply(hd, is.numeric)])
                corrplot.mixed(cor_pt, order = "alphabet", tl.cex = 1.0, tl.col ="steelblue")                
        }, width = 800, height = 400)
        
        output$pairsPlot <- renderPlot({
                par(mar=c(7,4,4,2))
                pairs(~., data = hd, panel = panel.smooth,
                      pch = 8, cex = 0.5,
                      font.labels = 0.5, upper.panel = NULL)                
        }, width = 800, height = 500)
        
        #Visualizations
         output$aboutRF <- renderPrint({
                HTML("<br/>There are numerous machine learning algorithms to build prediction models. For our classification problem, we choose Random Forest method. Random forests are an ensemble learning method for classification (and regression) that operate by constructing a multitude of decision trees at training time and outputting the class that is the mode of the classes output by individual trees (ref Wikipedia). This algorithm is best-known for its accuracy, handles large datasets and large number of variables very efficiently. It provides estimates of which variables are important in the classification. First we will build the prediction model using only the training set. Then we explore importance and accuracy results using test data.")})
        
        modelData <- reactive({
                rfModel <- randomForest(result ~ ., type= "classification", data = training, 
                                        ntree = input$numtrees, importance = TRUE)                
        })        
        predData <- reactive({
                rfPred <- predict(modelData(), testing)
        })
        
        output$modelSummary  <- renderPrint({print(modelData())})
        
        #Confusion Matrix
        output$cmRF <- renderPrint({ 
                print(confusionMatrix(predData(), testing$result) )                            
        })
        
        #Variable Importance 
        output$aboutVI <- renderPrint({
                HTML("<br/>With the plot below we can see which predictors have higher importance (sorted in decreasing order of importance)")})
        output$varImpPlot <- renderPlot({
                varImpPlot(modelData(), main = "", cex = 0.9, col ="steelblue")})
        #Error Rate
output$aboutER <- renderPrint({
        HTML("<br/>Plotting the error rates of the randomForest object, we observe that, as the number of trees increase, the error rates (miss-classification) decrease. Black line is the out-of-bag estimate and other colors denote each class error.")})
        output$oobPlot <- renderPlot({
                layout(matrix(c(1,2), nrow = 1), width = c(4,1))
                par(mar=c(5,4,4,0)) 
                plot(modelData(), main = "")
                par(mar=c(5,0,4,2)); plot(c(0,1), type = "n", axes=F, xlab = "", ylab = "")
                legend("top", colnames(modelData()$err.rate), col = 1:6, cex = 0.8, fill = 1:6)
        }) 
        
        #Margin
        output$aboutMP <- renderPrint({
                HTML("<br/>The margin of a data point is defined as the proportion of votes for the correct class minus maximum proportion of votes for the other classes. Thus under majority votes, positive margin means correct classification.")})
        output$plotRFmp <- renderPlot({
                plot(margin(modelData(), testing$result), cex = 0.8, main = "")
        })
                
})