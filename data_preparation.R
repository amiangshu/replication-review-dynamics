setwd('~/replication_package/')
library(tidyverse)

for(project in c('openstack','qt')){
##### Prepare Review Datasets

#get code review data
patch_data = read.csv(paste0("cleaned_data/",project,"_patch_chracteristics.csv"),stringsAsFactors = F)
experience = read.csv(paste0("cleaned_data/",project,"_expertise_characteristics.csv"),stringsAsFactors = F)
discussion = read.csv(paste0("cleaned_data/",project,"_within_discussion_metrics.csv"),stringsAsFactors = F)
history = read.csv(paste0("cleaned_data/",project,"_historical_interaction_metrics.csv"),stringsAsFactors = F)


data = inner_join(discussion,experience, by=c("ReviewId","ReviewerId"))
data = inner_join(data, history, by=c("ReviewId","ReviewerId"))
data = inner_join(data,patch_data,by="ReviewId")

#Get the unique rows
data = data %>% distinct()

if(project == "qt"){
  data = data %>% filter(grepl("qt%2Fqt",Project))
}

#Check how many patches with reviewers more than 1
sum = data %>% group_by(ReviewId) %>% summarize(reviewers = n_distinct(ReviewerId), positive_voters = sum(FinalVote > 0))
# print(table(sum$reviewers > 1))
data$IsVotePositive = ifelse(data$FinalVote > 0,TRUE,FALSE)
# print(table(data$IsVotePositive))


data$PositiveVoters_Norm = data$PositiveVoters/data$NumberOfPriorReviewers
data$PositiveVoters_Norm[is.nan(data$PositiveVoters_Norm)] = 0

data$NegativeVoters_Norm = (data$NumberOfPriorReviewers - data$PositiveVoters)/data$NumberOfPriorReviewers
data$NegativeVoters_Norm[is.nan(data$NegativeVoters_Norm)] = 0

data$RemainingOtherComments = data$RemainingOtherComments/data$OtherComments
data$RemainingOtherComments[is.nan(data$RemainingOtherComments)] = 0
data$StdReviewer_InvolvementLevel = data$StdReviewer_Comments/data$Comments
data$StdReviewer_InvolvementLevel[is.nan(data$StdReviewer_InvolvementLevel)] = 0

data$Churn = data$AddedLines + data$DeletedLines
data$Author_AuthoringExp = data$Author_AuthoringExp > 0.05
data$Author_ReviewingExp = data$Author_ReviewingExp > 0.05

if(project == "openstack"){
  data = data[data$CreatedDate > '2011-11-01',]
}else if(project == "qt"){
  data = data[data$CreatedDate > '2013-09-01',]
}


reviews = data
remove(data)
write.csv(reviews,paste0("studied_data/",project,"_dynamic_data.csv"), row.names = F)


#### Prepare commit data
blamed_commits = read.csv(paste0("cleaned_data/",project,"_blamed_commits.csv"), stringsAsFactors = F) 

#From SQL:`select * from CommitMetrics where PriorChanges is not null and PriorChanges < 500;`
commits = read.csv(paste0("cleaned_data/",project,"_commits.csv"), stringsAsFactors = F)

commit_data = commits %>% distinct() %>% left_join(blamed_commits,by = c("commit_hash" = "blamed_commit")) %>% group_by(commit_hash) %>% summarize(fixingCommits = sum(!is.na(bug_fixing_commit))) %>% inner_join(commits, ., by=c("commit_hash" = "commit_hash"))
commit_data$IsFixIntroducing = commit_data$fixingCommits > 0
write.csv(commit_data,paste0("studied_data/",project,"_commits.csv"),row.names = F)

### Prepare dynamic-commit level metrics
review_commit_link = read.csv(paste0("cleaned_data/",project,"_review_commit.csv"),stringsAsFactors = F)
data = commit_data %>% inner_join(review_commit_link,by=c("commit_hash"= "commit_hash")) %>% inner_join(reviews, by=c("review_id" = "ReviewId"))                           


## Count total number of reviewers
#Since the data in chromium_dynamic have only voter id. This file count all reviewers (voters + commenters)
#Query in the Activity table `select distinct review_id, author as reviewer_id from Activity where hist_isPatchOwner is null and (hist_voteScore is not null or hist_feedbackType is not null) `
reviewers = read.csv(paste0("cleaned_data/",project,"_reviewers.csv"),stringsAsFactors = F) 
reviewers = reviewers[reviewers$review_id %in% reviews$ReviewId,]
  
  

  
  
  #########Summary the data
print(project)
print(reviews %>% group_by(IsVotePositive) %>% summarize(num_reviews = n_distinct(ReviewId)))
num_reviewers = reviewers %>% group_by(review_id) %>% summarize(num_reviewers = n_distinct(reviewer_id)) %>% summarize(average = round(mean(num_reviewers),digits=0), min = min(num_reviewers), max=max(num_reviewers), moreThanOne = sum(num_reviewers > 1))
total_reviewers = reviewers %>% summarize(total_reviewers = n_distinct(reviewer_id))
patch_sum = reviews %>% summarize(from = min(CreatedDate), to = max(CreatedDate), NumberOfPatches= n_distinct(ReviewId), NumInstance = n(), VotePositive = sum(IsVotePositive == T))
commit_sum = data %>% select(commit_hash, commit_date, IsFixIntroducing) %>% distinct()  %>%  summarize(minDate = min(commit_date),maxDate = max(commit_date), num_commits=n_distinct(commit_hash), fixInducing=sum(IsFixIntroducing))
# %>% unite(commit_hash, c("commit_hash", "project"), sep="-")
print(cbind(project=project, patch_sum,total_reviewers, num_reviewers,commit_sum))
}
#Print commit summary

