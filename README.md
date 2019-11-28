A Replication Package for "Review Dynamics and Its Impact on Software Quality: A Case Study of OpenStack and Qt"
===================
# Environment
- R version 3.5.1+
- R Libralies
   - tidyverse 1.2.1
   - lme4
   - pROC
   - Rnalytica
   - car
   - doParallel

# Datasets
- Cleaned_datasets, studied_datasets, and models can be found in [this link](https://unimelbcloud-my.sharepoint.com/:u:/g/personal/patanamon_t_unimelb_edu_au/EfdRyLkhE8tPkT_R2-rGevoBAYV-FcV5vPN91ecROJu85g?e=uUCKe0)

# Analysis Scripts
- **Data Preparation**: `Rscript data_preparation.R` for linking and selecting data
    - The output will be stored at `studied_data/`   
- **Analysis 1**: `Rscript Analysis1_modelling.R <project_name>` for build a mixed-effect logistict regression models
    -  The input files are the files in `studied_data/`
    -  The model will be saved in `models/`
- **Analysis 1**: `Rscript Analysis1_validate.R <project_name>` for runing boostrap validation.
    - The script will run in multi-processing 
    - The results will be saved as an object in `models/`
- **Analysis 1**: `Rescript Analysis1_evaluate.R` for analyzing variable importance, LR Chisq, and present in a table
- **Analysis 2**: `Rscript Analysis2_compute_metrics.R` for aggregate metrics in Analysis 1 into the patch level
- **Analysis 2**: `Rscript Analysis2_modelling.R <project_name> <prelim|all_metrics>` for building models and show the model evaluation results.
- **Analysis 2**: `Rscript Analysis2_validate.R <project_name>` prints the optimism values of the models


Additional Details
-----
**Model Formula**
- Analysis 1
  - Openstack: `IsVotePositive ~ PositiveVoters_Norm + NumberOfCorePositiveVoters + RemainingOtherComments + InteractionFreqWithAuthor + isAuthorCore + AddedLines + Entropy + DescriptionLength + Author_AuthoringExp + Author_ReviewingExp+ (1|ReviewerId)`
  - Qt: `IsVotePositive ~ PositiveVoters_Norm + NegativeVoters_Norm + NumberOfCorePositiveVoters + RemainingOtherComments + InteractionFreqWithAuthor + isAuthorCore + AddedLines + NumberOfModifiedDirectories + Entropy + DescriptionLength + Author_AuthoringExp + Author_ReviewingExp+ (1|ReviewerId)`

- Analysis 2
  - OpenStack: `IsFixInducing  ~ ND+Entropy+LA+LD+LT+Age+PriorBugFixes+AvgComplexity+NumComments+positive_voters+ReviewingTime+consistentPositiveVote+inconsistentNegativeVote+changeVoteFinal+strongRelationshipAuthor+consistentCorePositiveVotes+coreAuthor+Author_AuthoringExp+Author_ReviewingExp+Fix`
  - Qt: `IsFixInducing  ~ ND+Entropy+LA+LD+Age+PriorBugFixes+AvgComplexity+NumComments+positive_voters+ReviewingTime+consistentPositiveVote+inconsistentNegativeVote+changeVoteFinal+strongRelationshipAuthor+consistentCorePositiveVotes+coreAuthor+Author_AuthoringExp+Author_ReviewingExp+Fix`

**Regular expressions**
- *ClassifyHistory.sql* provides a set of regular expressions for cleaning and extracting vote score in review messages

