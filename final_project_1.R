#install.packages('quantmod)
library(quantmod)
#install.packages('dplyr)
library(dplyr)
#install.packages('infotheo)
library(infotheo)
#install.packages('caret')
library(caret)

# get market data
getSymbols(c("^AAPL"))
head(AAPL)
tail(AAPL)
plot(AAPL$AAPL.Volume)

# transfer market data to a simple data frame
AAPL <- data.frame(AAPL)

# extract the date row name into a date column
AAPL$Close.Date <- row.names(AAPL)

# take random sets of sequential rows 
new_data <- c()
for (row_set in seq(10000)) {
  row_quant <- sample(10:30, 1)
  print(row_quant)
  row_start <- sample(1:(nrow(AAPL) - row_quant), 1)
  market_subset <- AAPL[row_start:(row_start + row_quant),]
  market_subset <- dplyr::mutate(market_subset, 
                                 Close_Date = max(market_subset$Close.Date),
                                 Close_Gap=(AAPL.Close - lag(AAPL.Close))/lag(AAPL.Close) ,
                                 High_Gap=(AAPL.High - lag(AAPL.High))/lag(AAPL.High) ,
                                 Low_Gap=(AAPL.Low - lag(AAPL.Low))/lag(AAPL.Low),
                                 Volume_Gap=(AAPL.Volume - lag(AAPL.Volume))/lag(AAPL.Volume),
                                 Daily_Change=(AAPL.Close - AAPL.Open)/AAPL.Open,
                                 Outcome_Next_Day_Direction= (lead(AAPL.Volume)-AAPL.Volume)) %>%
    dplyr::select(-AAPL.Open, -AAPL.High, -AAPL.Low, -AAPL.Close, -AAPL.Volume, -AAPL.Adjusted, -Close.Date) %>%
    na.omit
  market_subset$Sequence_ID <- row_set
  new_data <- rbind(new_data, market_subset)
}

dim(new_data)

# create sequences
# simplify the data by binning values into three groups

# Close_Gap
range(new_data$Close_Gap)
divided_data <- discretize(new_data$Close_Gap, disc="equalfreq", nbins=3)
new_data$Close_Gap <- divided_data$X
new_data$Close_Gap_LMH <- ifelse(new_data$Close_Gap == 1, 'L', 
                                ifelse(new_data$Close_Gap ==2, 'M','H'))


# Volume_Gap
range(new_data$Volume_Gap)
divided_data <- discretize(new_data$Volume_Gap, disc="equalfreq", nbins=3)
new_data$Volume_Gap <- divided_data$X
new_data$Volume_Gap_LMH <- ifelse(new_data$Volume_Gap == 1, 'L', 
                                 ifelse(new_data$Volume_Gap ==2, 'M','H'))

# Daily_Change
range(new_data$Daily_Change)
divided_data <- discretize(new_data$Daily_Change, disc="equalfreq", nbins=3)
new_data$Daily_Change <- divided_data$X
new_data$Daily_Change_LMH <- ifelse(new_data$Daily_Change == 1, 'L', 
                                   ifelse(new_data$Daily_Change ==2, 'M','H'))

# new set
new_data <- new_data[,c("Sequence_ID", "Close_Date", "Close_Gap_LMH", "Volume_Gap_LMH", "Daily_Change_LMH", "Outcome_Next_Day_Direction")]
new_data$Event_Pattern <- paste0(new_data$Close_Gap_LMH,new_data$Volume_Gap_LMH,new_data$Daily_Change_LMH) 

# reduce set 
compressed_data <- dplyr::group_by(new_data, Sequence_ID, Close_Date) %>% dplyr::summarise(Event_Pattern = paste(Event_Pattern, collapse = ",")) %>% data.frame
#compressed_data <- merge(x=compressed_data,y=dplyr::select(new_data, Sequence_ID, Outcome_Next_Day_Direction) %>% dplyr::group_by(Sequence_ID) %>% dplyr::slice(n()) %>% dplyr::distinct(Sequence_ID), by='Sequence_ID')

# if you have issues with the above dplyr line, try a workaround from reader - Dysregulation, thanks! 
compressed_data <- merge(x=compressed_data,y=new_data[,c(1,6)], by="Sequence_ID")


# use last x days of data for validation
library(dplyr)
compressed_data_validation <- dplyr::filter(compressed_data, Close_Date >= Sys.Date()-90)
dim(compressed_data_validation)
compressed_data <- dplyr::filter(compressed_data, Close_Date < Sys.Date()-90)
dim(compressed_data)

compressed_data <- dplyr::select(compressed_data, -Close_Date)
compressed_data_validation <- dplyr::select(compressed_data_validation, -Close_Date)

# only keep big moves
summary(compressed_data$Outcome_Next_Day_Direction)
compressed_data <- compressed_data[abs(compressed_data$Outcome_Next_Day_Direction) > 5000000,]
compressed_data$Outcome_Next_Day_Direction <- ifelse(compressed_data$Outcome_Next_Day_Direction > 0, 1, 0)
summary(compressed_data$Outcome_Next_Day_Direction)
dim(compressed_data)
compressed_data_validation$Outcome_Next_Day_Direction <- ifelse(compressed_data_validation$Outcome_Next_Day_Direction > 0, 1, 0)

# create two data sets - won/not won
compressed_data_pos <- dplyr::filter(compressed_data, Outcome_Next_Day_Direction==1) %>% dplyr::select(-Outcome_Next_Day_Direction)
dim(compressed_data_pos)
compressed_data_neg <- dplyr::filter(compressed_data, Outcome_Next_Day_Direction==0) %>% dplyr::select(-Outcome_Next_Day_Direction)
dim(compressed_data_neg)

# build the markov transition grid
build_transition_grid <- function(compressed_grid, unique_patterns) {
  grids <- c()
  for (from_event in unique_patterns) {
    print(from_event)
    
    # how many times 
    for (to_event in unique_patterns) {
      pattern <- paste0(from_event, ',', to_event)
      IDs_matches <- compressed_grid[grep(pattern, compressed_grid$Event_Pattern),]
      if (nrow(IDs_matches) > 0) {
        Event_Pattern <- paste0(IDs_matches$Event_Pattern, collapse = ',', sep='~~')
        found <- gregexpr(pattern = pattern, text = Event_Pattern)[[1]]
        grid <- c(pattern,  length(found))
      } else {
        grid <- c(pattern,  0)
      }
      grids <- rbind(grids, grid)
    }
  }
  
  # create to/from grid
  grid_Df <- data.frame(pairs=grids[,1], counts=grids[,2])
  grid_Df$x <- sapply(strsplit(as.character(grid_Df$pairs), ","), `[`, 1)
  grid_Df$y <- sapply(strsplit(as.character(grid_Df$pairs), ","), `[`, 2)
  head(grids)
  
  all_events_count <- length(unique_patterns)
  transition_matrix = t(matrix(as.numeric(as.character(grid_Df$counts)), ncol=all_events_count, nrow=all_events_count))
  transition_dataframe <- data.frame(transition_matrix)
  names(transition_dataframe) <- unique_patterns
  row.names(transition_dataframe) <- unique_patterns
  head(transition_dataframe)
  
  # replace all NaN with zeros
  transition_dataframe[is.na(transition_dataframe)] = 0
  # transition_dataframe <- opp_matrix
  transition_dataframe <- transition_dataframe/rowSums(transition_dataframe) 
  return (transition_dataframe)
}
unique_patterns <- unique(strsplit(x = paste0(compressed_data$Event_Pattern, collapse = ','), split = ',')[[1]])

grid_pos <- build_transition_grid(compressed_data_pos, unique_patterns)
grid_neg <- build_transition_grid(compressed_data_neg, unique_patterns)

# predict on out of sample data
actual = c()
predicted = c()
for (event_id in seq(nrow(compressed_data_validation))) {
  patterns <- strsplit(x = paste0(compressed_data_validation$Event_Pattern[event_id], collapse = ','), split = ',')[[1]]
  pos <- c()
  neg <- c()
  log_odds <- c()
  for (id in seq(length(patterns)-1)) {
    
    # logOdds = log(tp(i,j) / tn(i,j)
    log_value <- log(grid_pos[patterns[id],patterns[id+1]] / grid_neg[patterns[id],patterns[id+1]])
    if (is.na(log_value) || (length(log_value)==0) || (is.nan(log(grid_pos[patterns[id],patterns[id+1]] / grid_neg[patterns[id],patterns[id+1]]))==TRUE)) {
      log_value <- 0.0
    } else if (log_value == -Inf) {
      log_value <- log(0.00001 / grid_neg[patterns[id],patterns[id+1]])
    } else if (log_value == Inf) {
      log_value <- log(grid_pos[patterns[id],patterns[id+1]] / 0.00001)
      
    }
    log_odds <- c(log_odds, log_value)
    
    pos <- c(pos, grid_pos[patterns[id],patterns[id+1]])
    neg <- c(neg, grid_neg[patterns[id],patterns[id+1]])
  }
  print(paste('outcome:', compressed_data_validation$Outcome_Next_Day_Direction[event_id]))
  print(sum(pos)/sum(neg))
  print(sum(log_odds))
  
  actual <- c(actual, compressed_data_validation$Outcome_Next_Day_Direction[event_id])
  predicted <- c(predicted, sum(log_odds))
  
}

result <- confusionMatrix(ifelse(predicted>0,1,0), actual)
result 