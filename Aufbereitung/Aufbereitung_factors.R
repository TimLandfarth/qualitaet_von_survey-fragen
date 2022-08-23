########### Prep-file to change the numericals of the original data into factors


### Changing wrong numericals to factors

DF_original <- read_excel("updated_data1706.xlsx")


DF_original$Position <- as.factor(DF_original$Position)
DF_original$`Visual or oral presentation` <- as.factor(DF_original$`Visual or oral presentation`)
DF_original$`Request on the visual aid` <- as.factor(DF_original$`Request on the visual aid`)
DF_original$Interviewer <- as.factor(DF_original$Interviewer)
DF_original$`Computer assisted` <- as.factor(DF_original$`Computer assisted`)
### End Block 5 - Showcard used
DF_original$`Picture provided?` <- as.factor(DF_original$`Request on the visual aid`)
DF_original$`Start of the response sentence on the visual aid` <- as.factor(DF_original$`Start of the response sentence on the visual aid`)
DF_original$`Scale with only numbers or numbers in boxes` <- as.factor(DF_original$`Scale with only numbers or numbers in boxes`)
DF_original$`Numbers or letters before the answer categories` <- as.factor(DF_original$`Numbers or letters before the answer categories`)
DF_original$`Overlap of scale labels and categories` <- as.factor(DF_original$`Overlap of scale labels and categories`)
DF_original$`Horizontal or vertical scale` <- as.factor(DF_original$`Horizontal or vertical scale`)
DF_original$`Showcard or other visual aids used` <- as.factor(DF_original$`Showcard or other visual aids used`)
### Beginning Block 5 - Showcard used
DF_original$`Total number of abstract nouns in answer scale` <- as.factor(DF_original$`Total number of abstract nouns in answer scale`)
DF_original$`Total number of nouns in answer scale` <- as.factor(DF_original$`Total number of nouns in answer scale`)
DF_original$`Number of sentences in the request` <- as.factor(DF_original$`Number of sentences in the request`)
DF_original$`Number of words in introduction` <- as.factor(DF_original$`Number of words in introduction`)
DF_original$`Number of sentences in introduction` <- as.factor(DF_original$`Number of sentences in introduction`)
DF_original$`Number of subordinated clauses in introduction` <- as.factor(DF_original$`Number of subordinated clauses in introduction`)
DF_original$`Request present in the introduction` <- as.factor(DF_original$`Request present in the introduction`)
DF_original$`Introduction available?` <- as.factor(DF_original$`Introduction available?`)
### End Block 4 - extra Information or definition
DF_original$`Knowledge provided` <-as.factor(DF_original$`Knowledge provided`)
DF_original$`Extra information or definition` <- as.factor(DF_original$`Extra information or definition`)
### Beginning Block 4 - extra Information or definition

DF_original$`Respondent instruction` <- as.factor(DF_original$`Respondent instruction`)
DF_original$`Interviewer instruction` <- as.factor(DF_original$`Interviewer instruction`)
### End block 3 - response scale: basic choice
DF_original$`Don't know option` <- as.factor(DF_original$`Don't know option`)
DF_original$`Neutral category` <- as.factor(DF_original$`Neutral category`)
DF_original$`Symmetry of response scale` <- as.factor(DF_original$`Symmetry of response scale`)
DF_original$`Range of the used scale bipolar/unipolar` <- as.factor(DF_original$`Range of the used scale bipolar/unipolar`)
DF_original$`Theoretical range of the concept bipolar/unipolar` <- as.factor(DF_original$`Theoretical range of the concept bipolar/unipolar`)
DF_original$`Correspondence between labels and numbers of the scale` <- as.factor(DF_original$`Correspondence between labels and numbers of the scale`)
DF_original$`Order of the labels` <- as.factor(DF_original$`Order of the labels`)
DF_original$`Labels with short text or complete sentences` <- as.factor(DF_original$`Labels with short text or complete sentences`)
DF_original$`Labels of categories` <- as.factor(DF_original$`Labels of categories`)
DF_original$`Response scale: basic choice` <- as.factor(DF_original$`Response scale: basic choice`)
### Beginning block 3 - response scale: basic choice

DF_original$`Absolute or comparative judgment` <- as.factor(DF_original$`Absolute or comparative judgment`)
### End block 2 Variablen - Formulation of a request
DF_original$`Use of stimulus or statement in the request` <- as.factor(DF_original$`Use of stimulus or statement in the request`)
DF_original$`Emphasis on subjective opinion in request` <- as.factor(DF_original$`Emphasis on subjective opinion in request`)
DF_original$`Presence of encouragement to answer` <- as.factor(DF_original$`Presence of encouragement to answer`)
DF_original$`Balance of the request` <- as.factor(DF_original$`Balance of the request`)
DF_original$`Use of gradation` <- as.factor(DF_original$`Use of gradation`)
DF_original$`Request for an answer type` <- as.factor(DF_original$`Request for an answer type`)
DF_original$`'WH' word` <- as.factor(DF_original$`'WH' word`)
DF_original$`WH word used in the request` <- as.factor(DF_original$`WH word used in the request`)
DF_original$`Formulation of the request for an answer: basic choice` <- as.factor(DF_original$`Formulation of the request for an answer: basic choice`)
### Beginning block 2 Variablen - Formulation of a request

DF_original$`Reference period` <- as.factor(DF_original$`Reference period`)
DF_original$Centrality <- as.factor(DF_original$Centrality)
DF_original$`Social Desirability` <- as.factor(DF_original$`Social Desirability`)
DF_original$Concept <- as.factor(DF_original$Concept)
DF_original$Language <- as.factor(DF_original$Language)

