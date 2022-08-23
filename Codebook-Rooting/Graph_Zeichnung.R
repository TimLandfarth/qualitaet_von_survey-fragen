# 0. Zeichnung der Frageboegen als Graph----
library(igraph)

# 0.1 Funktionen----
#### Zur Erstellung von Vectoren im Sinne von bspw. c(Domain, National politics, Domain, European Union politics, Domain, ...)
graph_draw_one_parent_multiple_child <- function(parent, child, abk){
  retvec <- NA
  for(i in 0:length(child)){
    retvec[(i*2)+1] <- parent[1]
    if(!is.na(abk)){
    retvec[i*2] <- paste0(child[i], " ", abk)
    }
    else{
    retvec[i*2] <- paste0(child[i])
    }
  }
  retvec <- retvec[1:length(retvec) - 1]
  return(retvec)
} 

graph_draw_multiple_parent_one_child <- function(parent, child, abk){
  retvec <- NA
  for(i in 0:(length(parent))){
    retvec[(i*2)+1] <- parent[i+1]
    if(!is.na(abk)){
      retvec[i*2] <- paste0(child[1], " ", abk)
    }
    else{
      retvec[i*2] <- paste0(child[1])
    }
  }
  retvec <- retvec[1:length(retvec) - 1]
  return(retvec)
} 

graph_draw_multiple_parent_multiple_child <- function(parent, child, abk){
  retvec <- NA
  retvec <- graph_draw_multiple_parent_one_child(parent = parent, child = child[1], abk = abk)
  for(j in 2:length(child)){
  retvec <- c(retvec, graph_draw_multiple_parent_one_child(parent = parent, child = child[j], abk = abk))
  }
  return(retvec)
}

# 1. Graph----
## 1.1 Domain----
### 1.1.1 Domain----
domain <- graph_draw_one_parent_multiple_child(
  "Domain",
  c(
    "national politics",
    "European Union politics",
    "International politics",
    "Family",
    "Personal relations",
    "Work",
    "Consumer behaviour",
    "Leisure activities",
    "Health",
    "Living conditions and background variables",
    "Other beliefs"
  ), abk = NA
  
)

### 1.1.2 national politics----
national_politics <- graph_draw_one_parent_multiple_child(
  "national politics",
  c(
    "National government",
    "Local government",
    "National institutions (ministries, parliament, etc.)",
    "Local institutions",
    "Political parties",
    "Elections",
    "Trade unions and employee organisations",
    "Employer's organisations",
    "Pressure groups",
    "National issues",
    "Legal matters",
    "Economic / financial matters",
    "Defense matters",
    "Environmental matters",
    "Technological matters",
    "Traffic matters",
    "Agricultural matters",
    "Educational matters",
    "Prominent persons (ministers, members of parliament, etc.)",
    "Other national politics"
  ),
  abk = "nat_pol"
)

### 1.1.3 international politics----
international_politics <- graph_draw_one_parent_multiple_child(
  "International politics",
  c(
    "Relations with other European countries (non EC members)",
    "Relations with Unites States / Canada",
    "Relations with Latin America",
    "Relations with Asian countries",
    "Relations with African countries",
    "Relations with United Nations",
    "Other international institutions",
    "Prominent persons international politics",
    "Other international politics"
  ),
  abk = "int_pol"
)

### 1.1.4 european politics----
european_politics <- graph_draw_one_parent_multiple_child(
  "European Union politics",
  c(
    "European Community government",
    "European Community institutions",
    "European Community issues",
    "Political parties",
    "Elections",
    "Trade unions and employee organisations",
    "Employer's organisations",
    "Pressure groups",
    "Legal matters",
    "Economic / financial matters",
    "Defense matters",
    "Social matters",
    "Environmental matters",
    "Technological matters",
    "Traffic matters",
    "Agricultural matters",
    "Educational matters",
    "Prominent persons European Politics",
    "Other European Politics"
  ),
  abk = "eu_pol"
)

### 1.1.5 family----
family <- graph_draw_one_parent_multiple_child(
  "Family",
  c("Size/composition",
    "Relations to members",
    "Relations to relatives",
    "Household matters",
    "Sexual relations",
    "Personal life history (childhood, adults, retirement)",
    "Personal time budget",
    "Accidents",
    "Other"
  ),
  abk = "fam"
)

### 1.1.6 work----
work <- graph_draw_one_parent_multiple_child(
  "Work",
  c(
    "Place of work",
    "Kind of work",
    "Working hours",
    "Size of the company",
    "Structure of the company",
    "Occupation",
    "Prospects/career",
    "Further education",
    "Change in occupation",
    "Business conditions",
    "Other"
  ),
  abk = "work" 
)

### 1.1.7 personal relations----
personal_relations <- graph_draw_one_parent_multiple_child(
  "Personal relations",
  c(
    "Friends",
    "Neighbours",
    "Workplace",
    "Norms of other people",
    "Membership of organisations",
    "Religion/philosophy",
    "Other"
  ),
  abk = "pers_rel"
)

### 1.1.8 consumer behaviour----
consumer_behaviour <- graph_draw_one_parent_multiple_child(
  "Consumer behaviour",
  c(
    "Kind of housing",
    "Housing expenditures",
    "Housing conditions (furniture, heating, garden, etc.)",
    "Durables (car, tv, computer, etc.)",
    "Food and nutrition expenditures (not in restaurants)",
    "Tobacco, liquor",
    "Clothing",
    "Preferences for shops, brands",
    "Preferences for payment",
    "Household budgetting",
    "Consumer organisations",
    "Saving and investment of money",
    "Loans, mortgages",
    "Banks",
    "Insurances",
    "Other"
  ),
  abk = "cons_behav"
)

### 1.1.9 leisure activities----
leisure_activities <- graph_draw_one_parent_multiple_child(
  "Leisure activities",
  c(
    "Cultural activities (theatre, concert, exhibitions, etc.)",
    "Sports",
    "Do-it-yourself",
    "Gambling",
    "Restaurants/bars",
    "Holidays/travel",
    "Newspapers/periodicals",
    "Radio",
    "Television",
    "Internet",
    "Other activities"
  ), 
  abk = "leis_act"
)

### 1.1.10 health----
health <- graph_draw_one_parent_multiple_child(
  "Health",
  c(
    "Personal physical health condition",
    "Personal mental health condition",
    "Physical illnesses",
    "Mental illnesses",
    "Disabilities",
    "Use of medicine",
    "Use of drugs",
    "Medical institutions and hospitals",
    "Doctor's treatment",
    "Other"
  ),
  abk = "health"
)

### 1.1.11 living conditions and background variables----
living_condition_background <- graph_draw_one_parent_multiple_child(
  "Living conditions and background variables",
  c(
    "Age",
    "Sex",
    "Marital status",
    "Place of birth",
    "Place of residence",
    "Nationality",
    "Ethnicity",
    "Income",
    "Education (schools, degrees, courses)",
    "Religion",
    "Other"
  ),
  abk = "liv_cond"
)

### 1.1.12 other beliefes----
other_beliefs <- graph_draw_one_parent_multiple_child(
  "Other beliefs",
  c(
    "Religion",
    "Philosophy",
    "Sexuality",
    "Race",
    "Norms",
    "Life in general",
    "Happiness",
    "Yourself",
    "Other"
  ),
  abk = "other_bel"
)

### 1.1.13 zusammenfuegen----
#### 1.1.13.1 Alle bisherigen nodes----
zsf_concept <- c(domain, national_politics, international_politics, european_politics, family, work, personal_relations,
  consumer_behaviour, leisure_activities, health, living_condition_background, other_beliefs)

#### 1.1.13.2 Concept----
df <- graph_draw_multiple_parent_one_child(unique(zsf_concept)[13:length(unique(zsf_concept))], "Concept", abk = NA)
zsf_concept <- c(df, zsf_concept)

#### 1.1.13.3 entfernen----
#rm(df, domain, national_politics, international_politics, european_politics, family, work, personal_relations,
#   consumer_behaviour, leisure_activities, health, living_condition_background, other_beliefs)

## 1.2 Concept----
concept <- graph_draw_one_parent_multiple_child(
  "Concept",
  c(
    "Evaluative belief",
    "Feeling",
    "Importance of something",
    "Expectation of future events",
    "Facts, background, or behaviour",
    "All other simple concepts",
    "Complex concepts"
  ),
  abk = NA
)

### 1.2.1 other simple concepts----
other_concepts <- graph_draw_one_parent_multiple_child(
  "All other simple concepts",
  c(
    "Judgement",
    "Relationship",
    "Evaluation",
    "Preference",
    "Norm",
    "Policy",
    "Right",
    "Action tendency"
  ),
  abk = "other_concepts"
)

### 1.2.2 complex concept----
complex_concepts <- graph_draw_one_parent_multiple_child(
  "Complex concepts",
  c(
    "Importance of a judgement",
    "Certainty of a judgement",
    "Other"
  ),
  abk = "complex_concepts"
)

### 1.2.3 Social Desirability----
social_desirability <- graph_draw_multiple_parent_one_child(
  c(unique(concept[which(concept != "Complex concepts" & concept != "All other simple concepts" & concept != "Concept")]), 
    unique(other_concepts[which(other_concepts != "All other simple concepts")]),
    unique(complex_concepts[which(complex_concepts != "Complex concepts")])),
  "Social Desirability",
  abk = NA
)

### 1.2.4 Centrality----
#### 1.2.4.1 Zusammenfuegen----
zsf_centrality <- c(concept, other_concepts, complex_concepts, social_desirability)

## 1.4 Social Desirability----
social_desirability_kat <- graph_draw_one_parent_multiple_child(
  "Social Desirability",
  c("Not present",
    "A bit",
    "A lot"
  ), abk = NA
)

## 1.5 Centrality----
Centrality <- graph_draw_multiple_parent_one_child(
  c("Not present",
    "A bit",
    "A lot"),
  "Centrality",
  abk = NA
)

## 1.6 Reference Period----
### 1.6.1 Reference Period as parent----
Centrality_kat <- graph_draw_one_parent_multiple_child(
  "Centrality",
  c(
    "Not at all central/salient",
    "A bit central",
    "Rather central",
    "Central",
    "Very central/salient"
  ),
  abk = NA
)

### 1.6.2 Reference Period as child----
reference_period <- graph_draw_multiple_parent_one_child(
  c(
    "Not at all central/salient",
    "A bit central",
    "Rather central",
    "Central",
    "Very central/salient"
  ),
  "Reference period",
  abk = NA
)

## 1.7 Reference Period kat----
reference_period_kat <- graph_draw_one_parent_multiple_child(
  "Reference period",
  c(
    "Future",
    "Present",
    "Past"
  ),
  abk = NA
)

## 1.8 Formulation of the request for an answer: basic choice----
### 1.8.1 As child----
formulation_basic_choices <- graph_draw_multiple_parent_one_child(
  c(
    "Future",
    "Present",
    "Past"
  ),
  "Formulation of the request for an answer: basic choice",
  abk = NA
)

### 1.8.3 ZSF----
zsf_reference_period <- c(c(concept, other_concepts, complex_concepts, social_desirability, Centrality, social_desirability_kat, Centrality_kat, reference_period, formulation_basic_choices,reference_period_kat))


### 1.8.4 Entfernen----
#rm(concept, other_concepts, complex_concepts, social_desirability, Centrality, social_desirability_kat, Centrality_kat, reference_period, reference_period_kat)


## 1.8 Formulation of the request for an answer: basic choice----
### 1.8.1 As child----
formulation_basic_choices <- graph_draw_multiple_parent_one_child(
  c(
    "Future",
    "Present",
    "Past"
  ),
  "Formulation of the request for an answer: basic choice",
  abk = NA
)

### 1.8.2 As parent----
formulation_basic_choices_kat <- graph_draw_one_parent_multiple_child(
  "Formulation of the request for an answer: basic choice",
  c(
    "Indirect request",
    "Direct request",
    "No request present (e.g. not the first item of battery)"
  ),
  abk = "basic"
)

## 1.9 WH word----
### 1.9.1 into wh-word----
wh_word_1 <- graph_draw_multiple_parent_one_child(
  c("Indirect request basic",
  "Direct request basic"),
  "WH word used in the request",
  abk = "wh_word"
)

### 1.9.2 into use of stimulus----
stimulus <- graph_draw_multiple_parent_one_child(
  "No request present (e.g. not the first item of battery) basic",
  "Use of stimulus or statement in the request",
  abk = "stimulus"
)

### 1.9.3 WH word used in the request----
wh_word_in_request <- graph_draw_one_parent_multiple_child(
  "WH word used in the request wh_word",
  c(
    "WH word used",
    "Request without WH word"
  ),
  abk = "wh_word_request"
)

### 1.9.4 WH word used in the request parent----
wh_word_in_request_kat <- graph_draw_multiple_parent_one_child(
  "WH word used wh_word_request",
  "WH Word",
  abk = NA
)

### 1.9.5 Request for an answer type----
request_for_an_answer_type_1 <- graph_draw_multiple_parent_one_child(
  "Request without WH word wh_word_request",
  "Request for an answer type",
  abk = NA
)

### 1.9.6 WH word----
wh_word_2 <- graph_draw_one_parent_multiple_child(
  "WH Word",
  c(
    "Who",
    "Which",
    "What",
    "When TIME",
    "Where PLACE",
    "How (procedure)",
    "How (relationship)",
    "How (opinion)",
    "How (quantity)",
    "How (extremity)",
    "How (intensity)",
    "Why"
  ),
  abk = "wh word"
)

## 1.10 request for an answer type----
### 1.10.1 as child----
request_for_an_answer_type_2 <- graph_draw_multiple_parent_one_child(
  c(
    "Who wh word",
    "Which wh word",
    "What wh word",
    "When TIME wh word",
    "Where PLACE wh word",
    "How (procedure) wh word",
    "How (relationship) wh word",
    "How (opinion) wh word",
    "How (quantity) wh word",
    "How (extremity) wh word",
    "How (intensity) wh word",
    "Why wh word"
  ),
  "Request for an answer type",
  abk = NA
  
)

### 1.10.2 as parent----
request_for_an_answer_type_3 <- graph_draw_one_parent_multiple_child(
  "Request for an answer type",
  c(
    "Interrogative",
    "Imperative",
    "Declarative",
    "None of the above"
  ),
  abk = "request for answer type"
)

## 1.10.3 Zusammengefasst----
zsf_request_for_an_answer_type <- c(formulation_basic_choices_kat, wh_word_1, stimulus,  wh_word_in_request,
                                    wh_word_in_request_kat, request_for_an_answer_type_1, wh_word_2, request_for_an_answer_type_2)

## 1.10.4 entfernen----
#rm(formulation_basic_choices, formulation_basic_choices_kat, wh_word_1, stimulus,  wh_word_in_request,
#     wh_word_in_request_kat, request_for_an_answer_type_1, wh_word_2, request_for_an_answer_type_2)

##### Zusaetzlich: Use of stimulus manuell eingefuegt

### 1.10.5 kat----
request_for_an_answer_type_4 <- graph_draw_multiple_parent_one_child(
  c(
    "Interrogative request for answer type",
    "Imperative request for answer type",
    "Declarative request for answer type",
    "None of the above request for answer type"
  ),
  "Use of gradation",
  abk = NA
)

## 1.11 gradation----
### 1.11.1 as child
use_of_gradation <- graph_draw_one_parent_multiple_child(
  "Use of gradation",
  c(
    "No gradation used",
    "Gradation used"
  ),
  abk = "gradation"
)
  
### 1.11.2 as parent----
use_of_gradation_1 <- graph_draw_multiple_parent_one_child(
  c(
    "No gradation used gradation",
    "Gradation used gradation"
  ),
  "Balance of the request",
  abk = "gradation"
)

## 1.12 Balance of the request----
### 1.12.1 as parent----
balance_request <- graph_draw_one_parent_multiple_child(
  "Balance of the request gradation",
  c(
    "Balanced or not applicable",
    "Unbalanced"
  ),
  abk = "balance"
)

### 1.12.2 as grandparent----
balance_request_1 <- graph_draw_multiple_parent_one_child(
  c(
    "Balanced or not applicable balance",
    "Unbalanced balance"
  ),
  "Presence of encouragement to answer",
  abk = "balance"
)

## 1.13 presence of encouragement to answer----
### 1.13.1 as parent----
encrouragement_to_answer <- graph_draw_one_parent_multiple_child(
  "Presence of encouragement to answer balance",
  c(
    "No particular encouragement present",
    "Encouragement present"
  ),
  abk = "encouragement"
)

##ä 1.13.2 as grandparent----
encouragement_to_answer_1 <- graph_draw_multiple_parent_one_child(
  c(
    "No particular encouragement present encouragement",
    "Encouragement present encouragement"
  ),
  "Emphasis on subjective opinion in request",
  abk = "emphasis"
)

## 1.14 emphasis on subject opinion in request----
### 1.14.1 as parent----
emphasis_on_opinion <- graph_draw_one_parent_multiple_child(
  "Emphasis on subjective opinion in request emphasis",
  c(
    "No emphasis on opinion present",
    "Emphasis on opinion present"
  ),
  abk = "emphasis"
)

### 1.14.2 as grandparent----
emphasis_on_opinion_1 <- graph_draw_multiple_parent_one_child(
  c(
    "No emphasis on opinion present emphasis",
    "Emphasis on opinion present emphasis"
  ),
  "Information about the opinion of other people",
  abk = "info opinion"
)

## 1.15 Information about the opinion of other people----
info_opinion_other <- graph_draw_one_parent_multiple_child(
  "Information about the opinion of other people info opinion",
  c(
    "No information about opinions of others",
    "Information about opinions of other present"
  ),
  abk = "info opinion"
)

### 1.15.1 ZSF----
zsf_info_opinion_other <- c(request_for_an_answer_type_3, request_for_an_answer_type_4, use_of_gradation, use_of_gradation_1, 
                              balance_request, balance_request_1, encrouragement_to_answer, encouragement_to_answer_1, emphasis_on_opinion, emphasis_on_opinion_1)

### 1.15.2 entfernen----
#rm(request_for_an_answer_type_4, use_of_gradation, use_of_gradation_1, 
#     balance_request, balance_request_1, encrouragement_to_answer, encouragement_to_answer_1, emphasis_on_opinion, emphasis_on_opinion_1)
## 1.16 Use of stimulus or statement in the request----
### 1.16.1 as parent----
stimulus_1 <- graph_draw_multiple_parent_one_child(
  c(
    "No information about opinions of others info opinion",
    "Information about opinions of other present info opinion"
  ),
  "Use of stimulus or statement in the request",
  abk = "stimulus"
)

### 1.16.2 as gradnparent----
stimulus_2 <- graph_draw_one_parent_multiple_child(
  "Use of stimulus or statement in the request stimulus",
  c(
    "No stimulus or statement",
    "Stimulus or statement is present"
  ),
  abk = "stimulus"
)

## 1.17 Absolute or comparative judgment----
### 1.17.1 as parent----
abs_comp_judgment <- graph_draw_multiple_parent_one_child(
  c(
    "No stimulus or statement stimulus",
    "Stimulus or statement is present stimulus"
  ),
  "Absolute or comparative judgment",
  abk = "abs comp judgment"
)

### 1.17.2 as grandparent----
abs_comp_judgment_1 <- graph_draw_one_parent_multiple_child(
  "Absolute or comparative judgment abs comp judgment",
  c(
    "An absolute judgement",
    "A comparative judgement"
  ),
  abk = "abs comp judgment"
)

## 1.18 Response scale_ basic choice----
### 1.18.1 as parent----
response_scale_basic_choice <- graph_draw_multiple_parent_one_child(
  c(
    "An absolute judgement abs comp judgment",
    "A comparative judgement abs comp judgment"
  ),
  "Response scale basic choice",
  abk = "response basic"
)

### 1.18.2 as grandparent----
response_scale_basic_choice_1 <- graph_draw_one_parent_multiple_child(
  "Response scale basic choice response basic",
  c(
    "More than 2 categories scales",
    "Two-category scales",
    "Numerical open-ended answers",
    "Not available Magnitude estimation",
    "Not available Line production",
    "More steps procedures"
  ),
  abk = "response basic"
)

### 1.18.3 erste choice----
respone_scale_basic_choice_2 <- graph_draw_multiple_parent_one_child(
  "More than 2 categories scales response basic",
  "number of categories",
  abk = "response basic"
)

### 1.18.4 zweite choice----
response_scale_basic_choice_3 <- graph_draw_multiple_parent_one_child(
  "Two-category scales response basic",
  "Dont know option",
  abk = NA
)

### 1.18.5 dritte choice----
response_scale_basic_choice_4 <- graph_draw_multiple_parent_one_child(
  "Numerical open-ended answers response basic",
  "Maximum value possible 75",
  abk = "response basic"
)

### 1.18.6 vierte choice----
response_scale_basic_choice_5 <- graph_draw_multiple_parent_one_child(
  c("Not available Magnitude estimation response basic",
    "Not available Line production response basic"),
  "Maximum value possible 76",
  abk = "response basic"
)

### 1.18.7 fuenfte choice----
response_scale_basic_choice_6 <- graph_draw_multiple_parent_one_child(
  "More steps procedures response basic",
  "number of categories",
  abk = "response basic"
)

### 1.18.8 zusammenfassen----
zsf_response_basic <- c(info_opinion_other, stimulus_1, stimulus_2, abs_comp_judgment, abs_comp_judgment_1, response_scale_basic_choice, response_scale_basic_choice_1, respone_scale_basic_choice_2,
                        response_scale_basic_choice_3, response_scale_basic_choice_4, response_scale_basic_choice_5, response_scale_basic_choice_6,
                        max_val_75, max_val_75_1, max_val_76, max_val_76_1, stimulus)

### 1.18.9 entfernen----
#rm(info_opinion_other, stimulus_1, stimulus_2, abs_comp_judgment, abs_comp_judgment_1, response_scale_basic_choice, response_scale_basic_choice_1)

## 1.19 Maximum value possible _ 75----
max_val_75 <- graph_draw_multiple_parent_one_child(
  "Maximum value possible 75 response basic",
  "numeric input max val 75",
  abk = NA
)

max_val_75_1 <- graph_draw_multiple_parent_one_child(
  "numeric input max val 75",
  "Dont know option",
  abk = NA
)

## 1.20 Maximum value possible _76----
max_val_76 <- graph_draw_multiple_parent_one_child(
  "Maximum value possible 76 response basic",
  "numeric input max val 76",
  abk = NA
)

max_val_76_1 <- graph_draw_multiple_parent_one_child(
  "numeric input max val 76",
  "Theoretical range of concept bipolar/unipolar",
  abk = NA
)

## 1.21 Number of categories
num_categories <- graph_draw_multiple_parent_one_child(
  "number of categories response basic",
  "numeric input num cat",
  abk = NA
)

num_categories_1 <- graph_draw_multiple_parent_one_child(
  "numeric input num cat",
  "Labels of categories",
  abk = NA
)

## 1.22 labels of categories----
lab_categories <- graph_draw_one_parent_multiple_child(
  "Labels of categories",
  c("No labels",
  "Partially labelled",
  "Fully labelled"),
  abk = NA
)

lab_categories_1 <- graph_draw_multiple_parent_one_child(
  "No labels",
  "Theoretical range of concept bipolar/unipolar",
  abk = NA
)

lab_categories_2 <- graph_draw_multiple_parent_one_child(
  c("Partially labelled",
    "Fully labelled"),
  "Labels with short text or complete sentences",
  abk = NA
)

## 1.23 Labels with short text or complete sentences----
lab_short_complete <- graph_draw_one_parent_multiple_child(
  "Labels with short text or complete sentences",
  c("Short text",
    "Complete sentences"
  ),
  abk = NA
)

lab_short_complete_1 <- graph_draw_multiple_parent_one_child(
  c("Short text",
    "Complete sentences"
  ),
  "Order of the labels",
  abk = NA
)

## 1.24 Order of the labels----
lab_order <- graph_draw_one_parent_multiple_child(
  "Order of the labels",
  c(
    "First label negative or not applicable",
    "First label positive"
  ),
  abk = NA
)

lab_order_1 <- graph_draw_multiple_parent_one_child(
  c(
    "First label negative or not applicable",
    "First label positive"
  ),
  "Correspondence between labels and numbers of the scale",
  abk = NA
)


## 1.25 Correspondence between labels and numbers of the scale----

corr_bet_lab_numbers <- graph_draw_one_parent_multiple_child(
  "Correspondence between labels and numbers of the scale",
  c(
    "High correspondence",
    "Medium correspondence",
    "Low correspondence",
    "Not applicable"
  ),
  abk = NA
)

corr_bet_lab_numbers_1 <- graph_draw_multiple_parent_one_child(
  c(
    "High correspondence",
    "Medium correspondence",
    "Low correspondence",
    "Not applicable"
  ),
  "Theoretical range of concept bipolar/unipolar",
  abk = NA
)

### 1.25.1 Zusammenfassen----
zsf_corr_bet_lab_numbers <- c(num_categories, num_categories_1, lab_categories, lab_categories_1, lab_categories_2, 
                              lab_short_complete, lab_short_complete_1, lab_order, lab_order_1, corr_bet_lab_numbers, corr_bet_lab_numbers_1)

### 1.25.2 entfernen----
#rm(respone_scale_basic_choice_2, response_scale_basic_choice_3, response_scale_basic_choice_4,
#   response_scale_basic_choice_5, response_scale_basic_choice_6, max_val_75, max_val_75_1, max_val_76, max_val_76_1, num_categories, num_categories_1, lab_categories, lab_categories_1, lab_categories_2, 
#   lab_short_complete, lab_short_complete_1, lab_order, lab_order_1, corr_bet_lab_numbers, corr_bet_lab_numbers_1)

## 1.26 Theoretical range of concept bipolar/unipolar----
theo_range_concept <- graph_draw_one_parent_multiple_child(
  "Theoretical range of concept bipolar/unipolar",
  c(
    "Theoretically unipolar",
    "Theoretically bipolar"
  ),
  abk = NA
)

theo_range_concept_1 <- graph_draw_one_parent_multiple_child(
  "Theoretically unipolar",
  "Number of fixed reference points",
  abk = NA
)

theo_range_concept_2 <- graph_draw_one_parent_multiple_child(
  "Theoretically bipolar",
  "Range of the used scale bipolar/unipolar",
  abk = NA
)
  
## 1.27 Range of the used scale bipolar/unipolar----
range_of_scale <- graph_draw_one_parent_multiple_child(
  "Range of the used scale bipolar/unipolar",
  c("Unipolar",
    "Bipolar"
  ),
  abk = NA
)

range_of_scale_1 <- graph_draw_multiple_parent_one_child(
  "Unipolar",
  "Neutral category",
  abk = NA
)

range_of_scale_2 <- graph_draw_multiple_parent_one_child(
  "Bipolar",
  "Symmetry of response scale",
  abk = NA
)

## 1.28 Symmetry of response scale----
sym_of_response <- graph_draw_one_parent_multiple_child(
  "Symmetry of response scale",
  c(
    "Asymmetric",
    "Symmetric"
  ),
  abk = NA
)

sym_of_response_1 <- graph_draw_multiple_parent_one_child(
  c(
    "Asymmetric",
    "Symmetric"
  ),
  "Neutral category",
  abk = NA
)

## 1.29 Neutral category----
neutral_category <- graph_draw_one_parent_multiple_child(
  "Neutral category",
  c(
    "Present Neutral",
    "Not present Neutral"
  ),
  abk = NA
)

neutral_category_1 <- graph_draw_multiple_parent_one_child(
  c(
    "Present Neutral",
    "Not present Neutral"
  ),
  "Number of fixed reference points",
  abk = NA
)

## 1.30 Dont know option----
dont_know_option <- graph_draw_one_parent_multiple_child(
  "Dont know option",
  c(
    "DK option present",
    "DK option only registered",
    "DK option not present"
  ),
  abk = NA
)

dont_know_option_1 <- graph_draw_multiple_parent_one_child(
  c(
    "DK option present",
    "DK option only registered",
    "DK option not present"
  ),
  "Interviewer instruction",
  abk = NA
)

## 1.31 Interviewer instruction----
interviewer_instruction <- graph_draw_one_parent_multiple_child(
  "Interviewer instruction",
  c("Absent",
    "Present"
  ),
  abk = "Interviewer instruction"
)

interviewer_instruction_1 <- graph_draw_multiple_parent_one_child(
  c("Absent Interviewer instruction",
    "Present Interviewer instruction"
  ),
  "Respondent instruction",
  abk = NA
)

## 1.32 Respondent instruction----
respondent_instruction <- graph_draw_one_parent_multiple_child(
  "Respondent instruction",
  c("Absent",
    "Present"
  ),
  abk = "Respondent instruction"
)

respondent_instruction_1 <- graph_draw_multiple_parent_one_child(
  c("Absent Respondent instruction",
    "Present Respondent instruction"
  ),
  "Extra information or definition",
  abk = NA
)

## 1.33 Extra information or definition----
extra_information <- graph_draw_one_parent_multiple_child(
  "Extra information or definition",
  c("Absent",
    "Present"
  ),
  abk = "Extra Information"
)

extra_information_1 <- graph_draw_multiple_parent_one_child(
  "Absent Extra Information",
  "Introduction available?",
  abk = NA
)

extra_information_2 <- graph_draw_multiple_parent_one_child(
  "Present Extra Information",
  "Knowledge provided",
  abk = NA
)

### 1.34 Knowledge provided----
knowledge_provided <- graph_draw_one_parent_multiple_child(
  "Knowledge provided",
  c(
    "No extra information provided",
    "Definitions only",
    "Other explanations",
    "Both definitions and other explanations"
  ),
  abk = NA
)

knowledge_provided_1 <- graph_draw_multiple_parent_one_child(
  c(
    "No extra information provided",
    "Definitions only",
    "Other explanations",
    "Both definitions and other explanations"
  ),
  "Introduction available?",
  abk = NA
)

## 1.35 Introduction available?----
introduction_available <- graph_draw_one_parent_multiple_child(
  "Introduction available?",
  c(
    "Available",
    "Not available"
  ),
  abk = NA
)

introduction_available_1 <- graph_draw_one_parent_multiple_child(
  "Available",
  "Request present in the introduction",
  abk = NA
)

introduction_available_2 <- graph_draw_one_parent_multiple_child(
  "Not available",
  "Number of sentences in the request",
  abk = NA
)
#### HILFSPARAMETER
#introdutcion_available_3 <- graph_draw_one_parent_multiple_child(
#  "Available",
#  "...",
#  abk = NA
#)
#introduction_available_4 <- graph_draw_one_parent_multiple_child(
#  "...",
#  "Number of sentences in introduction",
#  abk = NA
#)
 
## 1.36 Request present in the introduction----
request_in_introduction <- graph_draw_one_parent_multiple_child(
  "Request present in the introduction",
  c(
    "Request not present",
    "Request present"
  ),
  abk = NA
)

request_in_introduction_1 <- graph_draw_multiple_parent_one_child(
  c(
    "Request not present",
    "Request present"
  ),
  "Number of sentences in introduction",
  abk = NA
)


### 1.36.1 Zusammenfassen----
zsf_request_in_instruction <- c(theo_range_concept, theo_range_concept_1, theo_range_concept_2, range_of_scale,
                                range_of_scale_1, range_of_scale_2, sym_of_response, sym_of_response_1, neutral_category,
                                neutral_category_1, dont_know_option, dont_know_option_1, interviewer_instruction, interviewer_instruction_1,
                                respondent_instruction, respondent_instruction_1, extra_information, extra_information_1, extra_information_2, 
                                knowledge_provided, knowledge_provided_1, introduction_available, introduction_available_1, introduction_available_2,
                                request_in_introduction, request_in_introduction_1)


### 1.36.2 entfernen ----
#rm(theo_range_concept, theo_range_concept_1, theo_range_concept_2, range_of_scale,
#   range_of_scale_1, range_of_scale_2, sym_of_response, sym_of_response_1, neutral_category,
#   neutral_category_1, dont_know_option, dont_know_option_1, interviewer_instruction, interviewer_instruction_1,
#   respondent_instruction, respondent_instruction_1, extra_information, extra_information_1, extra_information_2, 
#   knowledge_provided, knowledge_provided_1, introduction_available, introduction_available_1, introduction_available_2,
#   request_in_introduction, request_in_introduction_1)

## 1.37 Introduction evaluation----
### 1.37.1 Number of sentences in introduction----
num_sent_intro <- graph_draw_one_parent_multiple_child(
  "Number of sentences in introduction",
  "numeric input: Number of sentences in introduction",
  abk = NA
)

num_sent_intro_1 <- graph_draw_one_parent_multiple_child(
  "numeric input: Number of sentences in introduction",
  "Number of words in introduction",
  abk = NA
)

### 1.37.2 Number of wordds in introduction----
num_words_intro <- graph_draw_one_parent_multiple_child(
  "Number of words in introduction",
  "numeric input: Number of words in introduction",
  abk = NA
)

num_words_intro_1 <- graph_draw_one_parent_multiple_child(
  "numeric input: Number of words in introduction",
  "Number of subordinate clauses in introduction",
  abk = NA
)

### 1.37.3 Number of subordinate clauses in introduction----
num_subordinate_intro <- graph_draw_one_parent_multiple_child(
  "Number of subordinate clauses in introduction",
  "numeric input: Number of subordinate clauses in introduction",
  abk = NA
)

num_subordinate_intro_1 <- graph_draw_one_parent_multiple_child(
  "numeric input: Number of subordinate clauses in introduction",
  "Number of sentences in the request",
  abk = NA
)

### 1.37.4 Number of sentences in the request----
num_sent_request <- graph_draw_one_parent_multiple_child(
  "Number of sentences in the request",
  "numeric input: Number of sentences in the request",
  abk = NA
)

num_sent_request_1 <- graph_draw_one_parent_multiple_child(
  "numeric input: Number of sentences in the request",
  "Number of words in request",
  abk = NA
)

### 1.37.5 Number of words in request----
num_words_request <- graph_draw_one_parent_multiple_child(
  "Number of words in request",
  "numeric input: Number of words in request",
  abk = NA
)

num_words_request_1 <- graph_draw_one_parent_multiple_child(
  "numeric input: Number of words in request",
  "Total number of nouns in request for an answer",
  abk = NA
)

### 1.37.6 Total number of nouns in request for an answer----
total_num_nouns_request <- graph_draw_one_parent_multiple_child(
  "Total number of nouns in request for an answer",
  "numeric input: Total number of nouns in request for an answer",
  abk = NA
)

total_num_nouns_request_1 <- graph_draw_one_parent_multiple_child(
  "numeric input: Total number of nouns in request for an answer",
  "Total number of abstract nouns in request for an answer",
  abk = NA
)

### 1.37.7 Total number of abstract nouns in request for an answer----
total_num_abstract_nouns_request <- graph_draw_one_parent_multiple_child(
  "Total number of abstract nouns in request for an answer",
  "numeric input: Total number of abstract nouns in request for an answer",
  abk = NA
)

total_num_abstract_nouns_request_1 <- graph_draw_one_parent_multiple_child(
  "numeric input: Total number of abstract nouns in request for an answer",
  "Total number of syllables in request",
  abk = NA
)

### 1.37.8 Total number of syllables in request----
total_num_syll_request <- graph_draw_one_parent_multiple_child(
  "Total number of syllables in request",
  "numeric input: Total number of syllables in request",
  abk = NA
)

total_num_syll_request_1 <- graph_draw_one_parent_multiple_child(
  "numeric input: Total number of syllables in request",
  "Number of subordinate clauses in request",
  abk = NA
)

### 1.37.9 Number of subordinate clauses in request----
num_subordinate_request <- graph_draw_one_parent_multiple_child(
  "Number of subordinate clauses in request",
  "numeric input: Number of subordinate clauses in request",
  abk = NA
)

num_subordinate_request_1 <- graph_draw_one_parent_multiple_child(
  "numeric input: Number of subordinate clauses in request",
  "Number of syllables in answer scale",
  abk = NA
)

### 1.37.10 Number of syllables in answer scale----
num_syll_answer_scale <- graph_draw_one_parent_multiple_child(
  "Number of syllables in answer scale",
  "numeric input: Number of syllables in answer scale",
  abk = NA
)

num_syll_answer_scale_1 <- graph_draw_one_parent_multiple_child(
  "numeric input: Number of syllables in answer scale",
  "Total number of nouns in answer scale",
  abk = NA
)

### 1.37.11 Total number of nouns in answer scale----
total_num_nouns_answer_scale <- graph_draw_one_parent_multiple_child(
  "Total number of nouns in answer scale",
  "numeric input: Total number of nouns in answer scale",
  abk = NA
)

total_num_nouns_answer_scale_1 <- graph_draw_one_parent_multiple_child(
  "numeric input: Total number of nouns in answer scale",
  "Total number of abstract nouns in answer scale",
  abk = NA
)

### 1.37.12 Total number of abstract nouns in answer scale----
total_num_abstract_nouns_answer_scale <- graph_draw_one_parent_multiple_child(
  "Total number of abstract nouns in answer scale",
  "numeric input: Total number of abstract nouns in answer scale",
  abk = NA
)

total_num_abstract_nouns_answer_scale_1 <- graph_draw_one_parent_multiple_child(
  "numeric input: Total number of abstract nouns in answer scale",
  "Showcard or other visual aids used",
  abk = NA
)

### 1.37.13 Zusammenfassen----
zsf_total_number_request_intro_answer <- c(num_sent_intro, num_sent_intro_1, num_words_intro, num_words_intro_1, num_subordinate_intro, num_subordinate_intro_1,
                                           num_sent_request, num_sent_request_1, num_words_request, num_words_request_1, total_num_nouns_request, total_num_nouns_request_1,
                                           total_num_abstract_nouns_request, total_num_abstract_nouns_request_1, total_num_syll_request, total_num_syll_request_1,
                                           num_subordinate_request, num_subordinate_request_1, num_syll_answer_scale, num_syll_answer_scale_1, total_num_nouns_answer_scale,
                                           total_num_nouns_answer_scale_1, total_num_abstract_nouns_answer_scale, total_num_abstract_nouns_answer_scale_1, introduction_available,
                                           introduction_available_2)


### 1.37.14 entfernen----
#rm(num_sent_intro, num_sent_intro_1, num_words_intro, num_words_intro_1, num_subordinate_intro, num_subordinate_intro_1,
#   num_sent_request, num_sent_request_1, num_words_request, num_words_request_1, total_num_nouns_request, total_num_nouns_request_1,
#   total_num_abstract_nouns_request, total_num_abstract_nouns_request_1, total_num_syll_request, total_num_syll_request_1,
#   num_subordinate_request, num_subordinate_request_1, num_syll_answer_scale, num_syll_answer_scale_1, total_num_nouns_answer_scale,
#   total_num_nouns_answer_scale_1, total_num_abstract_nouns_answer_scale, total_num_abstract_nouns_answer_scale_1)


## 1.38 Showcard or other visual aids used----
showcard_other_used <- graph_draw_one_parent_multiple_child(
  "Showcard or other visual aids used",
  c(
    "Not used",
    "Used"
  ),
  abk = "showcard"
)

showcard_other_used_1 <- graph_draw_one_parent_multiple_child(
  "Not used showcard",
  "Computer assissted",
  abk = NA
)

showcard_other_used_2 <- graph_draw_one_parent_multiple_child(
  "Used showcard",
  "Horizontal or vertical scale",
  abk = NA
)

## 1.39 Horizontal or vertical scale----
horizontal_vertical_scale <- graph_draw_one_parent_multiple_child(
  "Horizontal or vertical scale",
  c(
    "Horizontal",
    "Vertical"
  ),
  abk = NA
)

horizontal_vertical_scale_1 <- graph_draw_multiple_parent_one_child(
  c(
    "Horizontal",
    "Vertical"
  ),
  "Overlap of scale labels and categories",
  abk = NA
)

## 1.40 Overlap of scale labels and categories----
overlap_scale_labels_categories <- graph_draw_one_parent_multiple_child(
  "Overlap of scale labels and categories",
  c(
    "Overlap present",
    "Text clearly connected to category"
  ),
  abk = NA
)

overlap_scale_labels_categories_1 <- graph_draw_multiple_parent_one_child(
  c(
    "Overlap present",
    "Text clearly connected to category"
  ),
  "Numbers or letters before the answer categories",
  abk = NA
)

## 1.41 Numbers or letters before the answer categories----
num_letter_before_answercat <- graph_draw_one_parent_multiple_child(
  "Numbers or letters before the answer categories",
  c(
    "Numbers",
    "-Not available- Letters",
    "Neither"
  ),
  abk = NA
) 

num_letter_before_answercat_1 <- graph_draw_multiple_parent_one_child(
  c(
    "Numbers",
    "-Not available- Letters"),
  "Scale with only numbers or numbers in boxes",
  abk = NA
)

num_letter_before_answercat_2 <- graph_draw_one_parent_multiple_child(
  "Neither",
  "Start of the response sentence on the visual aid",
  abk = NA
)

## 1.42 Scale with only numbers or numbers in boxes----
scale_only_num <- graph_draw_one_parent_multiple_child(
  "Scale with only numbers or numbers in boxes",
  c(
    "Only numbers",
    "Numbers in boxes"
  ),
  abk = NA
)

scale_only_num_1 <- graph_draw_multiple_parent_one_child(
  c(
    "Only numbers",
    "Numbers in boxes"
  ),
  "Start of the response sentence on the visual aid",
  abk = NA
)

## 1.43 Start of the response sentence on the visual aid----
start_response_visual <- graph_draw_one_parent_multiple_child(
  "Start of the response sentence on the visual aid",
  c(
    "Yes",
    "No"
  ),
  abk = "start response visual"
)

start_response_visual_1 <- graph_draw_multiple_parent_one_child(
  c(
    "Yes start response visual",
    "No start response visual"
  ),
  "Request on the visual aid",
  abk = NA
)

## 1.44 Request on the visual aid----
request_visual_aid <- graph_draw_one_parent_multiple_child(
  "Request on the visual aid",
  c(
    "Yes",
    "No"
  ),
  abk = "request visual aid"
)

request_visual_aid_1 <- graph_draw_multiple_parent_one_child(
  c(
    "Yes request visual aid",
    "No request visual aid"
  ),
  "Picture provided?",
  abk = NA
)

## 1.45 picture provided----
picture_provided <- graph_draw_one_parent_multiple_child(
  "Picture provided?",
  c(
    "Picture provided",
    "No picture provided"
  ),
  abk = NA
)

picture_provided_1 <- graph_draw_multiple_parent_one_child(
  c(
    "Picture provided",
    "No picture provided"
  ),
  "Computer assissted",
  abk = NA
)

## 1.46 Computer assissted----
computer_assisted <- graph_draw_one_parent_multiple_child(
  "Computer assissted",
  c(
    "Yes",
    "No"
  ),
  abk = "computer assisted"
)

computer_assisted_1 <- graph_draw_multiple_parent_one_child(
  c(
    "Yes computer assisted",
    "No computer assisted"
  ),
  "Interviewer",
  abk = NA
)

## 1.47 Interviewer----
intervierwer <- graph_draw_one_parent_multiple_child(
  "Interviewer",
  c(
    "Yes",
    "No"
  ),
  abk = "interviewer"
)

intervierwer_1 <- graph_draw_multiple_parent_one_child(
  c(
    "Yes interviewer",
    "No interviewer"
  ),
  "Visual or oral presentation",
   abk = NA
)

## 1.48 Visual or oral presentation----
visual_oral_presentation <- graph_draw_one_parent_multiple_child(
  "Visual or oral presentation",
  c(
    "Visual",
    "Oral"
  ),
  abk = NA
)

visual_oral_presentation_1 <- graph_draw_multiple_parent_one_child(
  c(
    "Visual",
    "Oral"
  ),
  "Position",
  abk = NA
)

## 1.48.1 Zusammenfassung----
zsf_visual_oral <- c(showcard_other_used, showcard_other_used_1, showcard_other_used_2, horizontal_vertical_scale,
                     horizontal_vertical_scale_1, overlap_scale_labels_categories, overlap_scale_labels_categories_1, 
                     num_letter_before_answercat, num_letter_before_answercat_1, num_letter_before_answercat_2, 
                     scale_only_num, scale_only_num_1, start_response_visual, start_response_visual_1, request_visual_aid, 
                     request_visual_aid_1, picture_provided, picture_provided_1, computer_assisted, computer_assisted_1, intervierwer,
                     intervierwer_1, visual_oral_presentation, visual_oral_presentation, visual_oral_presentation_1)

## 1.48.2 entfernen----
#rm(showcard_other_used, showcard_other_used_1, showcard_other_used_2, horizontal_vertical_scale,
#   horizontal_vertical_scale_1, overlap_scale_labels_categories, overlap_scale_labels_categories_1, 
#   num_letter_before_answercat, num_letter_before_answercat_1, num_letter_before_answercat_2, 
#   scale_only_num, scale_only_num_1, start_response_visual, start_response_visual_1, request_visual_aid, 
#   request_visual_aid_1, picture_provided, picture_provided_1, computer_assisted, computer_assisted_1, intervierwer,
#   intervierwer_1, visual_oral_presentation, visual_oral_presentation, visual_oral_presentation_1)

# 3. PLots----
## 3.1 Concept----
##### Aufbereitung
g_concept <- graph(zsf_concept)
V(g_concept)$first <- ifelse(V(g_concept)$name == "Domain", "first", ifelse(V(g_concept)$name == "national politics" |  V(g_concept)$name == "European Union politics" | 
                                                                              V(g_concept)$name == "International politics" | V(g_concept)$name == "Family" | 
                                                                              V(g_concept)$name == "Consumer behaviour" | V(g_concept)$name == "Living conditions and background variables" |
                                                                              V(g_concept)$name == "Work" | V(g_concept)$name == "Health" | V(g_concept)$name == "Personal relations" | 
                                                                              V(g_concept)$name == "Leisure activities" | V(g_concept)$name == "Other beliefs", "second", ifelse(
                                                                                V(g_concept)$name == "Concept", "fourth", "third"
                                                                              )))
g_concept_color <- ifelse(V(g_concept)$first == "first", "#fb9a99", ifelse(V(g_concept)$first == "second", "#33a02c", ifelse(V(g_concept)$first == "third", "#b2df8a", "#1f78b4")))
g_concept_size <- ifelse(V(g_concept)$first == "first", 20, ifelse(V(g_concept)$first == "second", 15, ifelse(V(g_concept)$first == "third", 7, 20)))
g_concept_label_size <- ifelse(V(g_concept)$first == "first", 1, ifelse(V(g_concept)$first == "second", 1, ifelse(V(g_concept)$first == "third", .5, 1)))
g_concept_shape <- ifelse(V(g_concept)$first == "first", "square", ifelse(V(g_concept)$first == "second", "rectangle", ifelse(V(g_concept)$first == "third", "rectangle", "square")))
g_concept_used <- ifelse(V(g_concept)$first == "first", "#e41a1c", ifelse(V(g_concept)$first == "second", "#e66101", ifelse(V(g_concept)$first == "third", "black", "black")))
g_concept_forward <- ifelse(V(g_concept)$first == "first", "black", ifelse(V(g_concept)$first == "second", "black", ifelse(V(g_concept)$first == "third", "black", "#e41a1c")))

E(g_concept)[from("Domain")]$first <- "first"
E(g_concept)[from("national politics")]$first <- "second"
E(g_concept)[from("European Union politics")]$first <- "second"
E(g_concept)[from("International politics")]$first <- "second"
E(g_concept)[from("Family")]$first <- "second"
E(g_concept)[from("Consumer behaviour")]$first <- "second"
E(g_concept)[from("Living conditions and background variables")]$first <- "second"
E(g_concept)[from("Work")]$first <- "second"
E(g_concept)[from("Health")]$first <- "second"
E(g_concept)[from("Personal relations")]$first <- "second"
E(g_concept)[from("Leisure activities")]$first <- "second"
E(g_concept)[from("Other beliefs")]$first <- "second"
E(g_concept)[to("Concept")]$first <- "third"

g_concept_edge_width <- ifelse(E(g_concept)$first == "first", 2, ifelse(E(g_concept)$first == "second", .5, .5))
g_concept_edge_color <- ifelse(E(g_concept)$first == "first", "grey50", ifelse(E(g_concept)$first == "second", "grey75", "grey75"))

##### plot
tkplot(graph(zsf_concept), edge.arrow.size = .75, vertex.color = g_concept_color, vertex.label.dist=.75, canvas.height = 595, canvas.width = 842, vertex.shape = "square", vertex.size = g_concept_size, vertex.label.cex = g_concept_label_size,
       vertex.frame.color = g_concept_forward, vertex.label.color = g_concept_used, edge.width = g_concept_edge_width, edge.color = g_concept_edge_color)

## 3.2 Centrality----
#### Aufbereitung
g_centrality <- graph(zsf_centrality)
V(g_centrality)$first <- ifelse(V(g_centrality)$name == "Concept", "first", ifelse(V(g_centrality)$name == "Importance of something" |  V(g_centrality)$name == "Feeling" | 
                                                                              V(g_centrality)$name == "Evaluative belief" | V(g_centrality)$name == "Expectation of future events" | 
                                                                              V(g_centrality)$name == "Facts, background, or behaviour" | V(g_centrality)$name == "All other simple concepts" |
                                                                              V(g_centrality)$name == "Complex concepts", "second", ifelse( V(g_centrality)$name == "Social Desirability", "fourth", "third")
                                                                              ))

g_centrality_vertex <- data.frame(color = ifelse(V(g_centrality)$first == "first", "#fb9a99", ifelse(V(g_centrality)$first == "second", "#33a02c", ifelse(V(g_centrality)$first == "third", "#b2df8a", "#1f78b4"))))
g_centrality_vertex$size <- ifelse(V(g_centrality)$first == "first", 20, ifelse(V(g_centrality)$first == "second", 15, ifelse(V(g_centrality)$first == "third", 7, 20)))
g_centrality_vertex$label_size <- ifelse(V(g_centrality)$first == "first", 1, ifelse(V(g_centrality)$first == "second", 1, ifelse(V(g_centrality)$first == "third", .5, 1)))
g_centrality_vertex$shape <- ifelse(V(g_centrality)$first == "first", "square", ifelse(V(g_centrality)$first == "second", "rectangle", ifelse(V(g_centrality)$first == "third", "rectangle", "square")))
g_centrality_vertex$used <- ifelse(V(g_centrality)$first == "first", "#e41a1c", ifelse(V(g_centrality)$first == "second", "#e66101", ifelse(V(g_centrality)$first == "third", "black", "black")))
g_centrality_vertex$forward <- ifelse(V(g_centrality)$first == "first", "black", ifelse(V(g_centrality)$first == "second", "black", ifelse(V(g_centrality)$first == "third", "black", "#e41a1c")))

E(g_centrality)$first <- "second"
E(g_centrality)[from("Concept")]$first <- "first"

g_centrality_edge_width <- ifelse(E(g_centrality)$first == "first", 2, .5)
g_centrality_edge_color <- ifelse(E(g_centrality)$first == "first", "grey50", "grey75")

#### plot
tkplot(graph(zsf_centrality), vertex.label.dist=1, canvas.height = 595, canvas.width = 842, edge.arrow.size = .75, vertex.color = g_centrality_vertex$color, vertex.size = g_centrality_vertex$size, 
       vertex.frame.colour = g_centrality_vertex$forward, vertex.label.color = g_centrality_vertex$used, edge.width = g_centrality_edge_width, edge.color = g_centrality_edge_color, 
       vertex.label.cex = g_centrality_vertex$label_size)

## 3.3 Reference period----
g_reference <- graph(zsf_reference_period)
 
##### first: Durch welchen Node angekommen              - Farben: #fb9a99     - Größe: 15     -Größe Label: 1     - Farbe Label: black / #e41a1c  - Farbe Rand: #e41a1c
##### second: Alle Fragen welche wichtig sind           - Farben: #33a02c     - Größe: 15     -Größe Label: 1     - Farbe Label: #e41a1c          - Farbe Rand: black
##### third: Alle Antworten welche wichtig sind         - Farben: #b2df8a     - Größe: 15     -Größe Label: 1     - Farbe Label: #e66101          - Farbe Rand: black
##### fourth: Alle Fragen, welche nicht wichtig sind    - Farben: #33a02c     - Größe: 7      -Größe Label: .5    - Farbe Label: black            - Farbe Rand: black
##### fifth: Alle Antworten, welche nicht wichtig sind  - Farben: #b2df8a     - Größe: 7      -Größe Label: .5    - Farbe Label: black            - Farbe Rand: black
##### sixth: Welcher Node geht raus                     - Farben: "#1f78b4"   - Größe: 15     -Größe Label: 1     - Farbe Label: black            - Farbe Rand: #e41a1c

##### first: Alle Antworten zu Fragen, welche wichtig sind    - Farben: "grey75"
##### second: Rest                                            - Farben: "grey50"

# V(g_answertype)$first <- ifelse(V(g_answertype)$name == "Formulation of the request for an answer: basic choice", "first",
#ifelse(V(g_answertype)$name %in% c("WH word used in the request wh_word"), "second",
#       ifelse(V(g_answertype)$name %in% c("Indirect request basic", "Direct request basic"), "third",
#              ifelse(V(g_answertype)$name %in% "Formulation of the request for an answer: basic choice", "fourth",
#                     ifelse(V(g_answertype)$name %in% c(), "fifth", "sixth")))))

V(g_reference)$first <- ifelse(V(g_reference)$name == "Concept", "first", ifelse(V(g_reference)$name %in% c("Social Desirability", "Centrality", "Reference period"), "second",
                                                                                 ifelse(V(g_reference)$name %in% c("Importance of something", "Feeling", "Evaluative belief", "Expectation of future events",
                                                                                                                   "Facts, background, or behaviour", "All other simple concepts", "Complex concepts", "A lot",
                                                                                                                   "A bit", "Not present", "Very central/salient", "Rather central", "Central", "A bit central", "Not at all central/salient",
                                                                                                                   "Present", "Future", "Past"), "third",
                                                                                        ifelse(V(g_reference)$name %in% "Formulation of the request for an answer: basic choice", "sixth", "fifth"))
))

E(g_reference)$first <- "second"
E(g_reference)[from("Concept") | from("Social Desirability") | from("Centrality") | from("Reference period")]$first <- "first"




graph_out_vertex <- function(graph_data, graph_data_edge){
  df <- data.frame(color = ifelse(V(graph_data)$first == "first", "#fb9a99",
                                   ifelse(V(graph_data)$first == "second", "#33a02c",
                                          ifelse(V(graph_data)$first == "third", "#b2df8a",
                                                 ifelse(V(graph_data)$first == "fourth", "#33a02c",
                                                        ifelse(V(graph_data)$first == "fifth", "#b2df8a", "#1f78b4"))))))
  
  df$size <- ifelse(V(graph_data)$first == "first", 15,
                    ifelse(V(graph_data)$first == "second", 15,
                           ifelse(V(graph_data)$first == "third", 15,
                                  ifelse(V(graph_data)$first == "fourth", 7,
                                         ifelse(V(graph_data)$first == "fifth", 7, 15)))))
  
  df$label_size <- ifelse(V(graph_data)$first == "first", 1,
                          ifelse(V(graph_data)$first == "second", 1,
                                 ifelse(V(graph_data)$first == "third", 1,
                                        ifelse(V(graph_data)$first == "fourth", .5,
                                               ifelse(V(graph_data)$first == "fifth", .5, 1)))))
  
  df$used <- ifelse(V(graph_data)$first == "first", "#e41a1c",
                    ifelse(V(graph_data)$first == "second", "#e41a1c",
                           ifelse(V(graph_data)$first == "third", "#e66101",
                                  ifelse(V(graph_data)$first == "fourth", "black",
                                         ifelse(V(graph_data)$first == "fifth", "black", "black")))))
  
  df$forward <- ifelse(V(graph_data)$first == "first", "#e41a1c",
                       ifelse(V(graph_data)$first == "second", "black",
                              ifelse(V(graph_data)$first == "third", "black",
                                     ifelse(V(graph_data)$first == "fourth", "black",
                                            ifelse(V(graph_data)$first == "fifth", "black", "#e41a1c")))))
  
  
  df1 <- data.frame(width = ifelse(E(graph_data_edge)$first == "first", 2, .5))
  df1$color <- ifelse(E(graph_data_edge)$first == "first", "grey50", "grey75")
return(list(vertex = df, edge = df1))
  }

df <- graph_out_vertex(g_reference, g_reference)


tkplot(g_reference, vertex.label.dist=1, canvas.height = 595, canvas.width = 842, edge.arrow.size = .75,
       vertex.color = df$vertex$color, vertex.size = df$vertex$size, vertex.frame.colour = df$vertex$forward, 
       vertex.label.color = df$vertex$used, vertex.label.cex = df$vertex$label_size,
       edge.width = df$edge$width, edge.color = df$edge$color, edge.curved = .25)


## 3.4 request for an answer type----
g_answertype <- graph(zsf_request_for_an_answer_type)

##### first: Durch welchen Node angekommen              - Farben: #fb9a99     - Größe: 15     -Größe Label: 1     - Farbe Label: black / #e41a1c  - Farbe Rand: #e41a1c
##### second: Alle Fragen welche wichtig sind           - Farben: #33a02c     - Größe: 15     -Größe Label: 1     - Farbe Label: #e41a1c          - Farbe Rand: black
##### third: Alle Antworten welche wichtig sind         - Farben: #b2df8a     - Größe: 15     -Größe Label: 1     - Farbe Label: #e66101          - Farbe Rand: black
##### fourth: Alle Fragen, welche nicht wichtig sind    - Farben: #33a02c     - Größe: 7      -Größe Label: .5    - Farbe Label: black            - Farbe Rand: black
##### fifth: Alle Antworten, welche nicht wichtig sind  - Farben: #b2df8a     - Größe: 7      -Größe Label: .5    - Farbe Label: black            - Farbe Rand: black
##### sixth: Welcher Node geht raus                     - Farben: "#1f78b4"   - Größe: 15     -Größe Label: 1     - Farbe Label: black            - Farbe Rand: #e41a1c

##### first: Alle Antworten zu Fragen, welche wichtig sind    - Farben: "grey75"
##### second: Rest                                            - Farben: "grey50"


V(g_answertype)$first <- ifelse(V(g_answertype)$name == "Formulation of the request for an answer: basic choice", "first",
                                ifelse(V(g_answertype)$name %in% c("WH word used in the request wh_word"), "second",
                                       ifelse(V(g_answertype)$name %in% c("Indirect request basic", "Direct request basic", "WH word used wh_word_request",
                                                                          "Request without WH word wh_word_request", "No request present (e.g. not the first item of battery) basic"), "third",
                                              ifelse(V(g_answertype)$name %in% c("WH Word"), "fourth",
                                                     ifelse(V(g_answertype)$name %in% c("Use of stimulus or statement in the request stimulus",
                                                                                        "Request for an answer type"),"sixth", "fifth")))))

E(g_answertype)$first <- "second"
E(g_answertype)[from("Formulation of the request for an answer: basic choice") | from("WH word used in the request wh_word")]$first <- "first"

df <- graph_out_vertex(g_answertype, g_answertype)

tkplot(g_answertype, vertex.label.dist=1, canvas.height = 595, canvas.width = 842, edge.arrow.size = .75,
       vertex.color = df$vertex$color, vertex.size = df$vertex$size, vertex.frame.colour = df$vertex$forward, 
       vertex.label.color = df$vertex$used, vertex.label.cex = df$vertex$label_size,
       edge.width = df$edge$width, edge.color = df$edge$color, edge.curved = .25)

## 3.5 Info opinion other----
g_infoother <- graph(zsf_info_opinion_other)

##### first: Durch welchen Node angekommen              - Farben: #fb9a99     - Größe: 15     -Größe Label: 1     - Farbe Label: black / #e41a1c  - Farbe Rand: #e41a1c
##### second: Alle Fragen welche wichtig sind           - Farben: #33a02c     - Größe: 15     -Größe Label: 1     - Farbe Label: #e41a1c          - Farbe Rand: black
##### third: Alle Antworten welche wichtig sind         - Farben: #b2df8a     - Größe: 15     -Größe Label: 1     - Farbe Label: #e66101          - Farbe Rand: black
##### fourth: Alle Fragen, welche nicht wichtig sind    - Farben: #33a02c     - Größe: 7      -Größe Label: .5    - Farbe Label: black            - Farbe Rand: black
##### fifth: Alle Antworten, welche nicht wichtig sind  - Farben: #b2df8a     - Größe: 7      -Größe Label: .5    - Farbe Label: black            - Farbe Rand: black
##### sixth: Welcher Node geht raus                     - Farben: "#1f78b4"   - Größe: 15     -Größe Label: 1     - Farbe Label: black            - Farbe Rand: #e41a1c

##### first: Alle Antworten zu Fragen, welche wichtig sind    - Farben: "grey75"
##### second: Rest                                            - Farben: "grey50"

V(g_infoother)$first <- ifelse(V(g_infoother)$name == "Request for an answer type", "first",
                                ifelse(V(g_infoother)$name %in% c("Use of gradation", "Balance of the request gradation", 
                                                                  "Presence of encouragement to answer balance",
                                                                  "Emphasis on subjective opinion in request emphasis"), "second",
                                       ifelse(V(g_infoother)$name %in% c("Information about the opinion of other people info opinion"), "sixth", "third")))

E(g_infoother)$first <- "second"
E(g_infoother)[from("Request for an answer type") | from("Use of gradation") | from("Balance of the request gradation") | from("Presence of encouragement to answer balance")
               | from("Emphasis on subjective opinion in request emphasis")]$first <- "first"

df <- graph_out_vertex(g_infoother, g_infoother)

tkplot(g_infoother, vertex.label.dist=1, canvas.height = 595, canvas.width = 842, edge.arrow.size = .75,
       vertex.color = df$vertex$color, vertex.size = df$vertex$size, vertex.frame.colour = df$vertex$forward, 
       vertex.label.color = df$vertex$used, vertex.label.cex = df$vertex$label_size,
       edge.width = df$edge$width, edge.color = df$edge$color, edge.curved = .25)

## 3.6 Response basic
g_responsebasic <- graph(zsf_response_basic)
##### first: Durch welchen Node angekommen              - Farben: #fb9a99     - Größe: 15     -Größe Label: 1     - Farbe Label: black / #e41a1c  - Farbe Rand: #e41a1c
##### second: Alle Fragen welche wichtig sind           - Farben: #33a02c     - Größe: 15     -Größe Label: 1     - Farbe Label: #e41a1c          - Farbe Rand: black
##### third: Alle Antworten welche wichtig sind         - Farben: #b2df8a     - Größe: 15     -Größe Label: 1     - Farbe Label: #e66101          - Farbe Rand: black
##### fourth: Alle Fragen, welche nicht wichtig sind    - Farben: #33a02c     - Größe: 7      -Größe Label: .5    - Farbe Label: black            - Farbe Rand: black
##### fifth: Alle Antworten, welche nicht wichtig sind  - Farben: #b2df8a     - Größe: 7      -Größe Label: .5    - Farbe Label: black            - Farbe Rand: black
##### sixth: Welcher Node geht raus                     - Farben: "#1f78b4"   - Größe: 15     -Größe Label: 1     - Farbe Label: black            - Farbe Rand: #e41a1c

##### first: Alle Antworten zu Fragen, welche wichtig sind    - Farben: "grey75"
##### second: Rest                                            - Farben: "grey50"


V(g_responsebasic)$first <- ifelse(V(g_responsebasic)$name %in% c("Information about the opinion of other people info opinion", "No request present (e.g. not the first item of battery) basic"), "first",
                                ifelse(V(g_responsebasic)$name %in% c("Use of stimulus or statement in the request stimulus", 
                                                                      "Absolute or comparative judgment abs comp judgment",
                                                                      "Response scale basic choice response basic"), "second",
                                       ifelse(V(g_responsebasic)$name %in% c("No information about opinions of others info opinion", "Information about opinions of other present info opinion",
                                                                             "Stimulus or statement is present stimulus", "No stimulus or statement stimulus",
                                                                             "An absolute judgement abs comp judgment", "A comparative judgement abs comp judgment",
                                                                             "Not available Line production response basic", "Not available Magnitude estimation response basic",
                                                                             "Two-category scales response basic", "Numerical open-ended answers response basic",
                                                                             "More steps procedures response basic", "More than 2 categories scales response basic"), "third",
                                              ifelse(V(g_responsebasic)$name %in% c("Maximum value possible 76 response basic", "Maximum value possible 75 response basic"
                                                                                    ), "fourth",
                                                     ifelse(V(g_responsebasic)$name %in% c("number of categories response basic", "Dont know option",
                                                                                           "Theoretical range of concept bipolar/unipolar"),"sixth", "fifth")))))

E(g_responsebasic)$first <- "second"
E(g_responsebasic)[from("Information about the opinion of other people info opinion") | from("Use of stimulus or statement in the request stimulus") |
                 from("Absolute or comparative judgment abs comp judgment") | from("Response scale basic choice response basic")]$first <- "first"

df <- graph_out_vertex(g_responsebasic, g_responsebasic)

tkplot(g_responsebasic, vertex.label.dist=1, canvas.height = 595, canvas.width = 842, edge.arrow.size = .75,
       vertex.color = df$vertex$color, vertex.size = df$vertex$size, vertex.frame.colour = df$vertex$forward, 
       vertex.label.color = df$vertex$used, vertex.label.cex = df$vertex$label_size,
       edge.width = df$edge$width, edge.color = df$edge$color, edge.curved = .25)

## 3.6  corr bet lab----
g_corrbet <- graph(zsf_corr_bet_lab_numbers)
##### first: Durch welchen Node angekommen              - Farben: #fb9a99     - Größe: 15     -Größe Label: 1     - Farbe Label: black / #e41a1c  - Farbe Rand: #e41a1c
##### second: Alle Fragen welche wichtig sind           - Farben: #33a02c     - Größe: 15     -Größe Label: 1     - Farbe Label: #e41a1c          - Farbe Rand: black
##### third: Alle Antworten welche wichtig sind         - Farben: #b2df8a     - Größe: 15     -Größe Label: 1     - Farbe Label: #e66101          - Farbe Rand: black
##### fourth: Alle Fragen, welche nicht wichtig sind    - Farben: #33a02c     - Größe: 7      -Größe Label: .5    - Farbe Label: black            - Farbe Rand: black
##### fifth: Alle Antworten, welche nicht wichtig sind  - Farben: #b2df8a     - Größe: 7      -Größe Label: .5    - Farbe Label: black            - Farbe Rand: black
##### sixth: Welcher Node geht raus                     - Farben: "#1f78b4"   - Größe: 15     -Größe Label: 1     - Farbe Label: black            - Farbe Rand: #e41a1c

##### first: Alle Antworten zu Fragen, welche wichtig sind    - Farben: "grey75"
##### second: Rest                                            - Farben: "grey50"


V(g_corrbet)$first <-
  ifelse(
    V(g_corrbet)$name %in% c("number of categories response basic"),
    "first",
    ifelse(
      V(g_corrbet)$name %in% c("numeric input num cat"),
      "third",
      ifelse(
        V(g_corrbet)$name %in% c(
          "Labels of categories",
          "Labels with short text or complete sentences",
          "Order of the labels",
          "Correspondence between labels and numbers of the scale"
        ),
        "fourth",
        ifelse(
          V(g_corrbet)$name %in% c(
            "Theoretical range of concept bipolar/unipolar"
          ),
          "sixth",
          "fifth"
        )
      )
    )
  )

E(g_corrbet)$first <- "second"
E(g_corrbet)[from("number of categories response basic")]$first <- "first"

df <- graph_out_vertex(g_corrbet, g_corrbet)

tkplot(g_corrbet, vertex.label.dist=1, canvas.height = 595, canvas.width = 842, edge.arrow.size = .75,
       vertex.color = df$vertex$color, vertex.size = df$vertex$size, vertex.frame.colour = df$vertex$forward, 
       vertex.label.color = df$vertex$used, vertex.label.cex = df$vertex$label_size,
       edge.width = df$edge$width, edge.color = df$edge$color, edge.curved = .25)

## 3.7 request in instruction ----
g_reqinst <- graph(zsf_request_in_instruction)
##### first: Durch welchen Node angekommen              - Farben: #fb9a99     - Größe: 15     -Größe Label: 1     - Farbe Label: black / #e41a1c  - Farbe Rand: #e41a1c
##### second: Alle Fragen welche wichtig sind           - Farben: #33a02c     - Größe: 15     -Größe Label: 1     - Farbe Label: #e41a1c          - Farbe Rand: black
##### third: Alle Antworten welche wichtig sind         - Farben: #b2df8a     - Größe: 15     -Größe Label: 1     - Farbe Label: #e66101          - Farbe Rand: black
##### fourth: Alle Fragen, welche nicht wichtig sind    - Farben: #33a02c     - Größe: 7      -Größe Label: .5    - Farbe Label: black            - Farbe Rand: black
##### fifth: Alle Antworten, welche nicht wichtig sind  - Farben: #b2df8a     - Größe: 7      -Größe Label: .5    - Farbe Label: black            - Farbe Rand: black
##### sixth: Welcher Node geht raus                     - Farben: "#1f78b4"   - Größe: 15     -Größe Label: 1     - Farbe Label: black            - Farbe Rand: #e41a1c

##### first: Alle Antworten zu Fragen, welche wichtig sind    - Farben: "grey75"
##### second: Rest                                            - Farben: "grey50"


V(g_reqinst)$first <-
  ifelse(
    V(g_reqinst)$name %in% c("Dont know option", "Theoretical range of concept bipolar/unipolar"),
    "first",
    ifelse(
      V(g_reqinst)$name %in% c(
        "Interviewer instruction",
        "Respondent instruction",
        "Extra information or definition", 
        "Knowledge provided",
        "Introduction available?",
        "Request present in the introduction",
        "Range of the used scale bipolar/unipolar",
        "Symmetry of response scale",
        "Neutral category"),
      "second",
        ifelse(
          V(g_reqinst)$name %in% c(
            "Number of sentences in introduction",
            "Number of sentences in the request"
          ),
          "sixth",
          "third"
        )
      )
    )
  

E(g_reqinst)$first <- "second"
E(g_reqinst)[from("Dont know option") | 
               from("Interviewer instruction") | 
               from("Respondent instruction") | 
               from("Extra information or definition") | 
               from("Knowledge provided") |
               from("Introduction available?") |
               from("Request present in the introduction") |
               from("Theoretical range of concept bipolar/unipolar") |
               from("Range of the used scale bipolar/unipolar") |
               from("Symmetry of response scale") |
               from("Neutral category")]$first <- "first"

df <- graph_out_vertex(g_reqinst, g_reqinst)
df$vertex$color[V(g_reqinst)$name == "Number of fixed reference points"] <- "#810f7c"

tkplot(g_reqinst, vertex.label.dist=1, canvas.height = 595, canvas.width = 842, edge.arrow.size = .75,
       vertex.color = df$vertex$color, vertex.size = df$vertex$size, vertex.frame.colour = df$vertex$forward, 
       vertex.label.color = df$vertex$used, vertex.label.cex = df$vertex$label_size,
       edge.width = df$edge$width, edge.color = df$edge$color, edge.curved = .25)

## 3.8  total number----
g_totalnum <- graph(zsf_total_number_request_intro_answer)
##### first: Durch welchen Node angekommen              - Farben: #fb9a99     - Größe: 15     -Größe Label: 1     - Farbe Label: black / #e41a1c  - Farbe Rand: #e41a1c
##### second: Alle Fragen welche wichtig sind           - Farben: #33a02c     - Größe: 15     -Größe Label: 1     - Farbe Label: #e41a1c          - Farbe Rand: black
##### third: Alle Antworten welche wichtig sind         - Farben: #b2df8a     - Größe: 15     -Größe Label: 1     - Farbe Label: #e66101          - Farbe Rand: black
##### fourth: Alle Fragen, welche nicht wichtig sind    - Farben: #33a02c     - Größe: 7      -Größe Label: .5    - Farbe Label: black            - Farbe Rand: black
##### fifth: Alle Antworten, welche nicht wichtig sind  - Farben: #b2df8a     - Größe: 7      -Größe Label: .5    - Farbe Label: black            - Farbe Rand: black
##### sixth: Welcher Node geht raus                     - Farben: "#1f78b4"   - Größe: 15     -Größe Label: 1     - Farbe Label: black            - Farbe Rand: #e41a1c

##### first: Alle Antworten zu Fragen, welche wichtig sind    - Farben: "grey75"
##### second: Rest                                            - Farben: "grey50"


V(g_totalnum)$first <-
  ifelse(
    V(g_totalnum)$name %in% c("Number of sentences in introduction", "Number of sentences in the request"),
    "first",
    ifelse(
      V(g_totalnum)$name %in% c(
        "Number of words in introduction",
        "Number of subordinate clauses in introduction",
        "Number of words in request", 
        "Total number of nouns in request for an answer",
        "Total number of abstract nouns in request for an answer",
        "Total number of syllables in request",
        "Number of subordinate clauses in request",
        "Number of syllables in answer scale",
        "Total number of nouns in answer scale",
        "Total number of abstract nouns in answer scale"),
      "second",
      ifelse(
        V(g_totalnum)$name %in% c(
          "Showcard or other visual aids used"
        ),
        "sixth",
        "third"
      )
    )
  )


E(g_totalnum)$first <- "second"
E(g_totalnum)[from("Number of words in introduction") |
                from("Number of subordinate clauses in introduction") |
                from("Number of words in request") |
                from("Total number of nouns in request for an answer") |
                from("Total number of abstract nouns in request for an answer") |
                from("Total number of syllables in request") |
                from("Number of subordinate clauses in request") |
                from("Number of syllables in answer scale") |
                from("Total number of nouns in answer scale") |
                from("Total number of abstract nouns in answer scale") |
                from("Number of sentences in introduction") |
                from("Number of sentences in the request")]$first <- "first"
  
df <- graph_out_vertex(g_totalnum, g_totalnum)
df$vertex$color[V(g_totalnum)$name %in% c("Introduction available?", "Not available", "Available", "...") ] <- "grey90"
df$vertex$size[V(g_totalnum)$name %in% c("Introduction available?", "Not available", "Available", "...")] <- c(10,10,10)
df$vertex$label_size[V(g_totalnum)$name %in% c("Introduction available?", "Not available", "Available", "...")] <- .75
df$vertex$used[V(g_totalnum)$name %in% c("Introduction available?", "Not available", "Available", "...")] <- "grey80"


tkplot(g_totalnum, vertex.label.dist=1, canvas.height = 595, canvas.width = 842, edge.arrow.size = .75,
       vertex.color = df$vertex$color, vertex.size = df$vertex$size, vertex.frame.colour = df$vertex$forward, 
       vertex.label.color = df$vertex$used, vertex.label.cex = df$vertex$label_size,
       edge.width = df$edge$width, edge.color = df$edge$color, edge.curved = .25)

## 3.9 Visual oral----
g_visualoral <- graph(zsf_visual_oral)
##### first: Durch welchen Node angekommen              - Farben: #fb9a99     - Größe: 15     -Größe Label: 1     - Farbe Label: black / #e41a1c  - Farbe Rand: #e41a1c
##### second: Alle Fragen welche wichtig sind           - Farben: #33a02c     - Größe: 15     -Größe Label: 1     - Farbe Label: #e41a1c          - Farbe Rand: black
##### third: Alle Antworten welche wichtig sind         - Farben: #b2df8a     - Größe: 15     -Größe Label: 1     - Farbe Label: #e66101          - Farbe Rand: black
##### fourth: Alle Fragen, welche nicht wichtig sind    - Farben: #33a02c     - Größe: 7      -Größe Label: .5    - Farbe Label: black            - Farbe Rand: black
##### fifth: Alle Antworten, welche nicht wichtig sind  - Farben: #b2df8a     - Größe: 7      -Größe Label: .5    - Farbe Label: black            - Farbe Rand: black
##### sixth: Welcher Node geht raus                     - Farben: "#1f78b4"   - Größe: 15     -Größe Label: 1     - Farbe Label: black            - Farbe Rand: #e41a1c

##### first: Alle Antworten zu Fragen, welche wichtig sind    - Farben: "grey75"
##### second: Rest                                            - Farben: "grey50"


V(g_visualoral)$first <-
  ifelse(
    V(g_visualoral)$name %in% c("Showcard or other visual aids used"),
    "first",
    ifelse(
      V(g_visualoral)$name %in% c(
        "Horizontal or vertical scale",
        "Overlap of scale labels and categories",
        "Numbers or letters before the answer categories",
        "Scale with only numbers or numbers in boxes",
        "Start of the response sentence on the visual aid",
        "Request on the visual aid",
        "Picture provided?",
        "Computer assissted",
        "Interviewer",
        "Visual or oral presentation"
        ),
      "second",
      ifelse(
        V(g_visualoral)$name %in% c(
          "No start response visual",
          "Position"
        ),
        "sixth",
        "third"
      )
    )
  )


E(g_visualoral)$first <- "second"
E(g_visualoral)[from("Horizontal or vertical scale") |
                from("Overlap of scale labels and categories") |
                from("Numbers or letters before the answer categories") |
                from("Scale with only numbers or numbers in boxes") |
                from("Start of the response sentence on the visual aid") |
                from("Request on the visual aid") |
                from("Picture provided?") |
                from("Computer assissted") |
                from("Interviewer") |
                from("Visual or oral presentation") |
                from("Showcard or other visual aids used")]$first <-  "first"

df <- graph_out_vertex(g_visualoral, g_visualoral)
df$vertex$color[V(g_visualoral)$name %in% c("Position")] <- "#810f7c"


tkplot(g_visualoral, vertex.label.dist=1, canvas.height = 595, canvas.width = 842, edge.arrow.size = .75,
       vertex.color = df$vertex$color, vertex.size = df$vertex$size, vertex.frame.colour = df$vertex$forward, 
       vertex.label.color = df$vertex$used, vertex.label.cex = df$vertex$label_size,
       edge.width = df$edge$width, edge.color = df$edge$color, edge.curved = .25)

## 3.10 Zusammengefasst----
zsf_zsf <- c(zsf_concept, zsf_centrality, zsf_reference_period, zsf_request_for_an_answer_type,
             zsf_response_basic, zsf_request_in_instruction, zsf_total_number_request_intro_answer, zsf_visual_oral, zsf_info_opinion_other, zsf_corr_bet_lab_numbers)

g_zsf <- graph(zsf_zsf)

### 3.10.1 concept----
V(g_zsf)$first <-
  ifelse(
    V(g_zsf)$name %in% c(
      "Domain",
      "Formulation of the request for an answer: basic choice",
      "Use of stimulus or statement in the request stimulus",
      "Request for an answer type",
      "Information about the opinion of other people info opinion",
      "number of categories response basic",
      "Dont know option",
      "Theoretical range of concept bipolar/unipolar",
      "Number of sentences in introduction",
      "Number of sentences in the request",
      "Showcard or other visual aids used",
      "Concept",
      "Position",
      "Number of fixed reference points"
    ),
    "first",
    ifelse(
      V(g_zsf)$name %in% c(
        "national politics",
        "European Union politics",
        "International politics",
        "Family",
        "Consumer behaviour",
        "Living conditions and background variables",
        "Work",
        "Health",
        "Personal relations",
        "Leisure activities",
        "Other beliefs",
        "Social Desirability",
        "Centrality",
        "Reference period",
        "WH word used in the request wh_word",
        "Use of gradation",
        "Balance of the request gradation",
        "Presence of encouragement to answer balance",
        "Emphasis on subjective opinion in request emphasis",
        "Use of stimulus or statement in the request stimulus",
        "Absolute or comparative judgment abs comp judgment",
        "Response scale basic choice response basic",
        "Interviewer instruction",
        "Respondent instruction",
        "Extra information or definition",
        "Knowledge provided",
        "Introduction available?",
        "Request present in the introduction",
        "Range of the used scale bipolar/unipolar",
        "Symmetry of response scale",
        "Neutral category",
        "Number of words in introduction",
        "Number of subordinate clauses in introduction",
        "Number of words in request",
        "Total number of nouns in request for an answer",
        "Total number of abstract nouns in request for an answer",
        "Total number of syllables in request",
        "Number of subordinate clauses in request",
        "Number of syllables in answer scale",
        "Total number of nouns in answer scale",
        "Total number of abstract nouns in answer scale",
        "Horizontal or vertical scale",
        "Overlap of scale labels and categories",
        "Numbers or letters before the answer categories",
        "Scale with only numbers or numbers in boxes",
        "Start of the response sentence on the visual aid",
        "Request on the visual aid",
        "Picture provided?",
        "Computer assissted",
        "Interviewer",
        "Visual or oral presentation"
      ),
      "second",
      ifelse(V(g_zsf)$name %in% c(
        "All other simple concepts",
        "Feeling",
        "Expectation of future events",
        "Importance of something",
        "Evaluative belief",
        "Complex concepts",
        "Facts, background, or behaviour",
        "A lot",
        "A bit",
        "Not present",
        "Central",
        "Very central/salient",
        "Rather central",
        "A bit central",
        "Not at all central/salient",
        "Present Neutral",
        "Past",
        "Future",
        "Interrogative request for answer type",
        "Imperative request for answer type",
        "Declarative request for answer type", 
        "None of the above request for answer type",
        "No gradation used gradation",
        "Gradation used gradation",
        "Balanced or not applicable balance",
        "Unbalanced balance",
        "No particular encouragement present encouragement",
        "Encouragement present encouragement",
        "No emphasis on opinion present emphasis",
        "Emphasis on opinion present emphasis",
        "No information about opinions of others info opinion",
        "Information about opinions of other present info opinion",
        "No stimulus or statement stimulus",
        "Stimulus or statement is present stimulus",
        "An absolute judgement abs comp judgment",
        "A comparative judgement abs comp judgment",
        "More than 2 categories scales response basic",
        "Two-category scales response basic",
        "Numerical open-ended answers response basic",
        "Not available Magnitude estimation response basic",
        "Not available Line production response basic",
        "More steps procedures response basic",
        "numeric input num cat",
        "DK option present",
        "DK option only registered",
        "DK option not present",
        "Absent Interviewer instruction",
        "Present Interviewer instruction",
        "Absent Respondent instruction",
        "Present Respondent instruction",
        "Absent Extra Information",
        "Present Extra Information",
        "No extra information provided",
        "Definitions only",
        "Other explanations",
        "Both definitions and other explanations",
        "Available",
        "Not available",
        "Request not present",
        "Request present",
        "Theoretically unipolar",
        "Theoretically bipolar",
        "Unipolar",
        "Bipolar",
        "Asymmetric",
        "Symmetric",
        "Present",
        "Not present Neutral",
        "numeric input: Number of sentences in introduction",
        "numeric input: Number of words in introduction",
        "numeric input: Number of subordinate clauses in introduction",
        "numeric input: Number of sentences in the request",
        "numeric input: Number of words in request",
        "numeric input: Total number of nouns in request for an answer",
        "numeric input: Total number of abstract nouns in request for an answer",
        "numeric input: Total number of syllables in request",
        "numeric input: Number of subordinate clauses in request",
        "numeric input: Number of syllables in answer scale",
        "numeric input: Total number of nouns in answer scale",
        "numeric input: Total number of abstract nouns in answer scale",
        "Not used showcard",
        "Used showcard",
        "Horizontal",
        "Vertical",
        "Overlap present",
        "Text clearly connected to category",
        "Numbers",
        "-Not available- Letters",
        "Neither",
        "Only numbers",
        "Numbers in boxes",
        "Yes start response visual",
        "Yes request visual aid",
        "No request visual aid",
        "Picture provided",
        "No picture provided",
        "Yes computer assisted",
        "No computer assisted",
        "Yes interviewer",
        "No interviewer",
        "Visual",
        "Oral",
        "Indirect request basic",
        "Direct request basic",
        "No request present (e.g. not the first item of battery) basic",
        "WH word used wh_word_request",
        "Request without WH word wh_word_request"
      ), "third", 
      ifelse(V(g_zsf)$name %in% c(
        "WH Word",
        "Maximum value possible 75 response basic",
        "Maximum value possible 76 response basic",
        "Labels of categories",
        "Labels with short text or complete sentences",
        "Order of the labels",
        "Correspondence between labels and numbers of the scale",
        "No request present (e.g. not the first item of battery) basic"
        ), "fourth", "fifth")
    )
  )
)

E(g_zsf)$first <- "second"
E(g_zsf)[from("Horizontal or vertical scale") |
           from("national politics") |
           from("European Union politics") |
           from("International politics") |
           from("Family") |
           from("Consumer behaviour") |
           from("Living conditions and background variables") |
           from("Work") |
           from("Health") |
           from("Personal relations") |
           from("Leisure activities") |
           from("Other beliefs") |
           from("Social Desirability") |
           from("Centrality") |
           from("Reference period") |
           from("WH word used in the request wh_word") |
           from("Use of gradation") |
           from("Balance of the request gradation") |
           from("Presence of encouragement to answer balance") |
           from("Emphasis on subjective opinion in request emphasis") |
           from("Use of stimulus or statement in the request stimulus") |
           from("Absolute or comparative judgment abs comp judgment") |
           from("Response scale basic choice response basic") |
           from("Interviewer instruction") |
           from("Respondent instruction") |
           from("Extra information or definition") |
           from("Knowledge provided") |
           from("Introduction available?") |
           from("Request present in the introduction") |
           from("Range of the used scale bipolar/unipolar") |
           from("Symmetry of response scale") |
           from("Neutral category") |
           from("Number of words in introduction") |
           from("Number of subordinate clauses in introduction") |
           from("Number of words in request") |
           from("Total number of nouns in request for an answer") |
           from("Total number of abstract nouns in request for an answer") |
           from("Total number of syllables in request") |
           from("Number of subordinate clauses in request") |
           from("Number of syllables in answer scale") |
           from("Total number of nouns in answer scale") |
           from("Total number of abstract nouns in answer scale") |
           from("Horizontal or vertical scale") |
           from("Overlap of scale labels and categories") |
           from("Numbers or letters before the answer categories") |
           from("Scale with only numbers or numbers in boxes") |
           from("Start of the response sentence on the visual aid") |
           from("Request on the visual aid") |
           from("Picture provided?") |
           from("Computer assissted") |
           from("Interviewer") |
           from("Visual or oral presentation") |
           from("Domain") |
           from("Formulation of the request for an answer: basic choice") |
           from("Use of stimulus or statement in the request stimulus") |
           from("Request for an answer type") |
           from("Information about the opinion of other people info opinion") |
           from("number of categories response basic") |
           from("Dont know option") |
           from("Theoretical range of concept bipolar/unipolar") |
           from("Number of sentences in introduction") |
           from("Number of sentences in the request") |
           from("Showcard or other visual aids used") |
           from("Concept") |
           from("Position") |
           from("Number of fixed reference points")]$first <-
  "first"

df <- graph_out_vertex(g_zsf, g_zsf)

df$vertex$size <- ifelse(df$vertex$size == 15, 10, 4)
df$vertex$label <- ifelse( !(V(g_zsf)$first %in% c("first")), NA , V(g_zsf)$name)
df$vertex$color[V(g_zsf)$name %in% c("Position", "Number of fixed reference points")] <- "#810f7c"


tkplot(g_zsf, vertex.label.dist = 1, edge.arrow.size = .75, 
       vertex.color = df$vertex$color, vertex.size = df$vertex$size, vertex.label.color = df$vertex$used,
       vertex.label.cex = df$vertex$label_size, edge.width = df$edge$width, edge.color = df$edge$color, edge.curved = .25, vertex.label = df$vertex$label)
