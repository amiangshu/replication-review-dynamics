setwd('~/replication_package/')


library(tidyverse)


for(project in c("openstack","qt")){
### Prepare defect data
commit_data = read.csv(paste0("studied_data/",project,"_commits.csv"),stringsAsFactors = F)
reviews =  read.csv(paste0("studied_data/",project,"_dynamic_data.csv"),stringsAsFactors = F) %>% select(-Entropy) %>% group_by(ReviewId, ReviewerId) %>% filter(row_number()==1 )
review_commit_link = read.csv(paste0("cleaned_data/",project,"_review_commit.csv"),stringsAsFactors = F)
data = commit_data %>% inner_join(review_commit_link,by=c("commit_hash"="commit_hash")) %>% inner_join(reviews, by=c("review_id" = "ReviewId"))                           


votes = read.csv(paste0("cleaned_data/",project,"_preliminary_vote_change.csv"),stringsAsFactors = F)
votes = votes[votes$ReviewId %in% data$review_id, ]

#Compute Overall Review activity
review_activity = read.csv(paste0("cleaned_data/",project,"_review_activity.csv"),stringsAsFactors = F) %>% rename(ReviewId = review_id)
review_activity = review_activity[review_activity$ReviewId %in% reviews$ReviewId,]
review_activity_metrics = review_activity %>% group_by(ReviewId) %>% summarize(NumReviewers = n_distinct(reviewer_id), NumComments = sum(isAuthorMsg == "NULL" & commentType != "NULL"), 
                                                     ReviewingTime = difftime(max(parse_datetime(commentDate, "%Y-%m-%d %H:%M:%S")),min(parse_datetime(commentDate, "%Y-%m-%d %H:%M:%S")),units="hours"))

#Compute Metrics for Preliminary Analysis
social_data = votes %>% group_by(ReviewId, ReviewerId) %>% summarize(VoteChangeFreq = sum(!is.na(ChangeTo) & (FromVote != ChangeTo) & MSG_CNT > 0), 
                                                       IsFinalVoteChanged=sum(!is.na(ChangeTo) & (FromVote != ChangeTo) & MSG_CNT > 0 & isFinal=='True'))


social_data = social_data %>% right_join(reviews , by=c("ReviewId" = "ReviewId", "ReviewerId" = "ReviewerId")) %>% distinct() %>% mutate(VoteChangeFreq = replace_na(VoteChangeFreq,0), IsFinalVoteChanged=replace_na(IsFinalVoteChanged,0))




author_relationship_t = quantile(social_data$InteractionFreqWithAuthor, 0.8)
## Compute all social metrics at commit level
metrics = social_data %>% group_by(ReviewId) %>% summarize(
                                                  # total_voters = n_distinct(ReviewerId),
                                                 Author_AuthoringExp = as.logical(first(Author_AuthoringExp)),
                                                 Author_ReviewingExp = as.logical(first(Author_ReviewingExp)),
                                                 positive_voters = sum(FinalVote > 0),                
                                                 NumReviewers_AuthoringExp = sum(StudiedReviewer_AuthoringExp > 0.05),
                                                 NumReviewers_ReviewingExp = sum(StudiedReviewer_ReviewingExp > 0.05),
                                                 
                                                 
                                                 consistentPositiveVote = sum(FinalVote > 0 & PositiveVoters_Norm > 0),
                                                 inconsistentNegativeVote = sum(FinalVote > 0 & NegativeVoters_Norm > 0),
                                                 changeVote = sum(VoteChangeFreq > 0),
                                                 changeVoteFinal = sum(IsFinalVoteChanged > 0),
                                                 consistentCorePositiveVotes = sum(NumberOfCorePositiveVoters > 0 & FinalVote > 0),
                                                 consistentWithPriorComments = sum(RemainingOtherComments == 0 & FinalVote > 0),
                                                 strongRelationshipAuthor = sum(InteractionFreqWithAuthor > author_relationship_t & FinalVote > 0),
                                                 coreAuthor = sum(isAuthorCore == "True" & FinalVote > 0)
                                                 ) 
social_vars = colnames(metrics)
metrics = metrics %>% left_join(review_activity_metrics, by="ReviewId") %>% mutate_at(vars(-ReviewId,-Author_AuthoringExp,-Author_ReviewingExp), funs(. / NumReviewers)) %>% mutate(ReviewingTime = as.numeric(ReviewingTime))#Divide all metrics by total_voters

metrics = metrics %>%  
  inner_join(review_commit_link, by=c("ReviewId"="review_id")) %>%
  inner_join(commit_data, by=c("commit_hash" = "commit_hash"))


print(summary(metrics))
write.csv(metrics, paste0("studied_data/",project,"_dynamic_commits.csv"),row.names = F)

}
