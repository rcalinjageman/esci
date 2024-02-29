#' Altruism Happiness - Ch12 - from Brethel-Haurwitz and Marsh (2014)
#' 
#' Happiness may not be important just for the person feeling it; happiness may
#' also promote kind, altruistic behavior. Brethel-Haurwitz and Marsh (2014)
#' examined this idea by collecting data on U.S. states. A Gallup poll in 2010
#' was used to measure each state’s well-being index, a measure of mean
#' happiness for the state’s residents on a scale from 0 to 100. Next, a kidney
#' donation database for 1999–2010 was used to figure out each state’s rate
#' (number of donations per 1 million people) of non-directed kidney
#' donations—that’s giving one kidney to a stranger, an extremely generous and
#' altruistic thing to do!
#'
#' @format ## `data_altruism_happiness`
#' A data frame with 50 rows and 6 columns:
#' \describe{
#'   \item{State}{factor - State where data was collected}
#'   \item{Abbreviation}{factor - State where data was collected}
#'   \item{Well_Being_2010}{numeric - State where data was collected}
#'   \item{Well_Being_2013}{numeric - State where data was collected}
#'   \item{Kidney_Rate, per million population}{numeric - State where data was collected}
#'   \item{WB Change 2013-2010}{numeric - State where data was collected}
#' }
#' @source <https://journals.sagepub.com/doi/full/10.1177/0956797613516148>
"data_altruism_happiness"

#' Anchor Estimate ma - Ch9 - Many Labs replications of Jacowitz and Kahneman (1995)
#' 
#' To what extent does the wording of a question influence one’s judgment? In
#' their classic study, Jacowitz and Kahneman (1995) asked participants to
#' estimate how many babies are born each day in the United States. Participants
#' were given either a low anchor (“more than 100 babies/day”) or a high anchor
#' (“less than 50,000 babies/day”). Those who saw the low anchor estimated many
#' fewer births/day than those who saw the high anchor, which suggests that the
#' wording can have a profound influence. The correct answer, as it happens, is
#' ~11,000 births/day in 2014. To investigate the extent that these results are
#' replicable, the Many Labs project repeated this classic study at many
#' different labs around the world. You can find the summary data for 30 of
#' these labs in the Anchor Estimate ma data file
#'
#' @format ## `data_anchor_estimate_ma`
#' A data frame with 30 rows and 9 columns:
#' \describe{
#'   \item{Location}{factor}
#'   \item{M Low}{numeric}
#'   \item{s Low}{numeric}
#'   \item{n Low}{integer}
#'   \item{M High}{numeric}
#'   \item{s High}{numeric}
#'   \item{n High}{integer}
#'   \item{USAorNot}{factor}
#'   \item{Country}{factor}
#' }
#' @source <https://econtent.hogrefe.com/doi/10.1027/1864-9335/a000178>
"data_anchor_estimate_ma"

#' Basol badnews - Ch07 - from Basol et al. (2020)
#' 
#' Climate change? Vaccines? Fake news and conspiracy theories on these and
#' numerous other issues can be highly damaging, but are thriving in this social
#' media age. Trying to debunk a conspiracy theory by presenting facts and
#' evidence often doesn’t work, alas. Psychological inoculation, also similar to
#' prebunking, presents a mild form of misinformation, preferably with
#' explanation, in the hope of building resistance to real-life fake news—a sort
#' of vaccine for fake news. The Bad News game is a spin-off from research on
#' psychological inoculation. Basol et al. (2020) assessed the possible
#' effectiveness of this game as a fake news vaccine. At getbadnews.com you can
#' click “About” for information, or just start playing the game— it’s easy and
#' maybe even fun. You encounter mock Twitter (now X) fake news messages that
#' illustrate common strategies for making fake news memorable or believable.
#' You make choices between messages and decide which ones to “forward” as you
#' try to spread fake news while building your credibility score and number of
#' “followers”—rather like real life for a conspiracy theorist wanting to spread
#' the word. Compete with your friends for credibility and number of followers.
#' Basol’s online participants first saw 18 fictitious fake news tweets and
#' rated each for reliability (accuracy, believability), and also rated their
#' confidence in that reliability rating. Both ratings were on a 1 to 7 scale.
#' Those in the BadNews group then played the game for 15 minutes, whereas those
#' in the Control group played Tetris. Then all once again gave reliability and
#' confidence ratings for the 18 tweets.
#'
#' @format ## `data_basol_badnews`
#' A data frame with 198 rows and 3 columns:
#' \describe{
#'   \item{Diff reliability}{numeric}
#'   \item{Diff confidence}{numeric}
#'   \item{Condition}{factor}
#' }
#' @source <https://doi.org/10.5334%2Fjoc.91>
"data_basol_badnews"

#' Bem Psychic - Ch13 - from Bem and Honorton (1994)
#' 
#' Daryl Bem was an experienced mentalist and research psychologist, who, a
#' decade earlier, had been one of several outside experts invited to scrutinize
#' the laboratory and experimental procedures of parapsychology researcher
#' Charles Honorton. Bem not only judged them adequate, but joined the research
#' effort and became a coauthor with Honorton. Bem and Honorton (1994) first
#' reviewed early ganzfeld studies and described how the experimental procedure
#' had been improved to reduce the chance that results could be influenced by
#' various possible biases, or leakages of information from sender to receiver.
#' For example, the randomization procedure was carried out automatically by
#' computer, and all stimuli were presented under computer control. Bem and
#' Honorton then presented data from studies conducted with the improved
#' procedure. Table 13.1 presents basic data from 10 studies reported by Bem and
#' Honorton (1994). Participants each made a single judgment, so in Pilot 1, for
#' example, 22 participants responded, with 8 of them giving a correct response.
#' Three pilot studies helped refine the procedures, then four studies used
#' novice receivers. Study 5 used 20 students of music, drama, or dance as
#' receivers, in response to suggestions that creative people might be more
#' likely to show telepathy. Studies 6 and 7 used receivers who had participated
#' in an earlier study. The proportion of hits expected by chance is .25, and
#' Table 13.1 shows that all but Study 1 found proportions higher than .25.
#'
#' @format ## `data_bem_psychic`
#' A data frame with 10 rows and 5 columns:
#' \describe{
#'   \item{Study}{factor}
#'   \item{Participants}{factor}
#'   \item{N(Trials)}{integer}
#'   \item{N(Hits)}{integer}
#'   \item{Proportion Hits}{numeric}
#' }
#' @source <https://psycnet.apa.org/record/1994-20286-001>
"data_bem_psychic"

#' BodyWellF - Ch12 - Body Satisfaction and Well-being data for females from Figure 11.24 right panel
#' 
#' A subset of data_bodywell_fm, reports only for those participants
#' who identified as female.  Data is Subjective Wellbeing abd Body Satisfaction.
#'
#' @format ## `data_bodywellf`
#' A data frame with 59 rows and 2 columns:
#' \describe{
#'   \item{Body Satisfaction}{numeric}
#'   \item{Well-being}{numeric}
#' }
"data_bodywellf"

#' BodyWellFM - Ch12 - Body Satisfaction and Well-being data from Figure 11.1
#' 
#' Survey data from a convenience sample Dominican University students.
#' Reported are measures of Subjective Wellbeing abd Body Satisfaction.
#'
#' @format ## `data_bodywellfm`
#' A data frame with 106 rows and 2 columns:
#' \describe{
#'   \item{Body Satisfaction}{numeric}
#'   \item{Well-being}{numeric}
#' }
"data_bodywellfm"

#' BodyWellM - Ch12 - Body Satisfaction and Well-being data for males from Figure 11.24 left panel
#' 
#' A subset of data_bodywell_fm, reports only for those participants
#' who identified as male.  Data is Subjective Wellbeing abd Body Satisfaction.
#'
#' @format ## `data_bodywellm`
#' A data frame with 47 rows and 2 columns:
#' \describe{
#'   \item{Body Satisfaction}{numeric}
#'   \item{Well-being}{numeric}
#' }
"data_bodywellm"

#' 
#' 
#' @format ## `data_chap_8_paired_ex_8.18`
#' A data frame with 8 rows and 2 columns:
#' \describe{
#'   \item{Before}{numeric}
#'   \item{After}{numeric}
#' }
"data_chap_8_paired_ex_8.18"

#' Clean moral - Ch07 - from Schnall et al. (2008), Study 1, and Johnson et al. (2014)
#' 
#' @format ## `data_clean_moral`
#' A data frame with 208 rows and 4 columns:
#' \describe{
#'   \item{Schnall Condition}{factor}
#'   \item{Schnall Moral judgment}{numeric}
#'   \item{Johnson Condition}{factor}
#'   \item{Johnson Moral judgment}{numeric}
#' }
"data_clean_moral"

#' College survey 1 - Ch03 - for End-of-Chapter Exercise 3.3
#' 
#' @format ## `data_college_survey_1`
#' A data frame with 243 rows and 23 columns:
#' \describe{
#'   \item{ID}{integer}
#'   \item{Gender}{factor}
#'   \item{Gender_Code}{factor}
#'   \item{Age}{integer}
#'   \item{Shool_Year}{factor}
#'   \item{School_Year_Code}{factor}
#'   \item{Transfer}{factor}
#'   \item{Transfer_Code}{logical}
#'   \item{Student_Athlete}{factor}
#'   \item{Student_Athlete_Code}{logical}
#'   \item{Wealth_SR}{numeric}
#'   \item{GPA}{numeric}
#'   \item{ACT}{integer}
#'   \item{Subjective_Well_Being}{numeric}
#'   \item{Positive_Affect}{numeric}
#'   \item{Negative_Affect}{numeric}
#'   \item{Relationship_Confidence}{numeric}
#'   \item{Exercise}{numeric}
#'   \item{Academic_Motivation_Intrinsic}{numeric}
#'   \item{Academic_Motivation_Extrinsic}{numeric}
#'   \item{Academic_Motivation_Amotivation}{numeric}
#'   \item{Intelligence_Value}{numeric}
#'   \item{Raven_Score}{numeric}
#' }
"data_college_survey_1"

#' College survey 2 - Ch05 - for End-of-Chapter Exercise 5.4
#' 
#' @format ## `data_college_survey_2`
#' A data frame with 138 rows and 17 columns:
#' \describe{
#'   \item{ID}{integer}
#'   \item{Gender}{factor}
#'   \item{Gender_Code}{factor}
#'   \item{Age}{numeric}
#'   \item{Wealth_SR}{numeric}
#'   \item{School_Year}{factor}
#'   \item{School_Year_Code}{factor}
#'   \item{Transfer}{factor}
#'   \item{Transfer_Code}{logical}
#'   \item{GPA}{numeric}
#'   \item{Subjective_Well_Being}{numeric}
#'   \item{Positive_Affect}{numeric}
#'   \item{Negative_Affect}{numeric}
#'   \item{Academic_Engagement}{numeric}
#'   \item{Religious_Meaning}{numeric}
#'   \item{Health}{numeric}
#'   \item{Emotion_Recognition}{factor}
#' }
"data_college_survey_2"

#' DamischRCJ - Ch9 - from 6 Damisch studies, and Calin-Jageman and Caldwell (2014)
#' 
#' @format ## `data_damischrcj`
#' A data frame with 8 rows and 5 columns:
#' \describe{
#'   \item{Study}{factor}
#'   \item{Cohen's d unbiased}{numeric}
#'   \item{n Control}{integer}
#'   \item{n Lucky}{integer}
#'   \item{Research Group}{factor}
#' }
"data_damischrcj"

#' EffronRaj fakenews - Ch8 - from Effron and Raj (2020)
#' 
#' @format ## `data_effronraj_fakenews`
#' A data frame with 138 rows and 5 columns:
#' \describe{
#'   \item{ID}{factor}
#'   \item{UnethOld}{numeric}
#'   \item{UnethNew}{numeric}
#'   \item{AccurOld}{numeric}
#'   \item{AccurNew}{numeric}
#' }
"data_effronraj_fakenews"

#' Emotion heartrate - Ch8 - from Lakens (2013)
#' 
#' @format ## `data_emotion_heartrate`
#' A data frame with 68 rows and 3 columns:
#' \describe{
#'   \item{ID}{integer}
#'   \item{HR_baseline}{numeric}
#'   \item{HR_anger}{numeric}
#' }
"data_emotion_heartrate"

#' Exam Scores - Ch11 - for End-of-Chapter Exercise 11.2
#' 
#' @format ## `data_exam_scores`
#' A data frame with 9 rows and 3 columns:
#' \describe{
#'   \item{StudentID}{factor}
#'   \item{First Exam}{numeric}
#'   \item{Final Exam}{numeric}
#' }
"data_exam_scores"

#' Flag Priming ma - Ch9 - Many Labs replications of Carter et al. (2011)
#' 
#' @format ## `data_flag_priming_ma`
#' A data frame with 25 rows and 7 columns:
#' \describe{
#'   \item{Location}{factor}
#'   \item{M Flag}{numeric}
#'   \item{s Flag}{numeric}
#'   \item{n Flag}{integer}
#'   \item{M Noflag}{numeric}
#'   \item{s Noflag}{numeric}
#'   \item{n Noflag}{integer}
#' }
"data_flag_priming_ma"

#' Gender math IAT ma - Ch9 - Many Labs replications of Nosek et al. (2002)
#' 
#' @format ## `data_gender_math_iat_ma`
#' A data frame with 30 rows and 9 columns:
#' \describe{
#'   \item{Location}{factor}
#'   \item{M Male}{numeric}
#'   \item{s Male}{numeric}
#'   \item{n Male}{integer}
#'   \item{M Female}{numeric}
#'   \item{s Female}{numeric}
#'   \item{n Female}{integer}
#'   \item{USAorNot}{factor}
#'   \item{Country}{factor}
#' }
"data_gender_math_iat_ma"

#' Gender math IAT - Ch07 - Ithaca and SDSU replications of Nosek et al. (2002)
#' 
#' @format ## `data_gender_math_iat`
#' A data frame with 155 rows and 4 columns:
#' \describe{
#'   \item{Ithaca gender}{factor}
#'   \item{Ithaca IAT}{numeric}
#'   \item{SDSU gender}{factor}
#'   \item{SDSU IAT}{numeric}
#' }
"data_gender_math_iat"

#' Halagappa - Ch14 - from Halagappa et al. (2007)
#' 
#' @format ## `data_halagappa`
#' A data frame with 6 rows and 4 columns:
#' \describe{
#'   \item{Groups}{factor}
#'   \item{Mean}{numeric}
#'   \item{SD}{numeric}
#'   \item{n}{integer}
#' }
"data_halagappa"

#' Home Prices - Ch12 - for End-of-Chapter Exercise 12.2
#' 
#' @format ## `data_home_prices`
#' A data frame with 300 rows and 8 columns:
#' \describe{
#'   \item{MLS}{integer}
#'   \item{Location}{factor}
#'   \item{Price}{integer}
#'   \item{Bedrooms}{integer}
#'   \item{Bathrooms}{integer}
#'   \item{Size (m2)}{numeric}
#'   \item{Status}{factor}
#'   \item{Status_Code}{logical}
#' }
"data_home_prices"

#' Kardas Expt 3 - Ch07 - from Kardas and O’Brien (2018), Experiment 3
#' 
#' @format ## `data_kardas_expt_3`
#' A data frame with 100 rows and 3 columns:
#' \describe{
#'   \item{Exposure}{factor}
#'   \item{Prediction}{numeric}
#'   \item{Performance}{numeric}
#' }
"data_kardas_expt_3"

#' Kardas Expt 4 - Ch07 - from Kardas and O’Brien (2018), Experiment 4
#' 
#' @format ## `data_kardas_expt_4`
#' A data frame with 270 rows and 4 columns:
#' \describe{
#'   \item{Exposure}{factor}
#'   \item{Prediction}{integer}
#'   \item{Performance}{integer}
#'   \item{Time}{numeric}
#' }
"data_kardas_expt_4"

#' Labels flavor - Ch8 - from Floretta-Schiller et al. (2015)
#' 
#' @format ## `data_labels_flavor`
#' A data frame with 51 rows and 6 columns:
#' \describe{
#'   \item{ParticipantID}{integer}
#'   \item{Enjoy_Generic}{numeric}
#'   \item{Enjoy_Organic}{numeric}
#'   \item{Pay_Generic}{numeric}
#'   \item{Pay_Organic}{numeric}
#'   \item{Suspicious}{logical}
#' }
"data_labels_flavor"

#' Latimier 3Groups - Ch14 - 3 groups in Latimier et al. (2019)
#' 
#' @format ## `data_latimier_3groups`
#' A data frame with 285 rows and 3 columns:
#' \describe{
#'   \item{Group}{factor}
#'   \item{Test%}{numeric}
#'   \item{Time}{numeric}
#' }
"data_latimier_3groups"

#' Latimier Prequiz - Ch03 - Prequiz group in Latimier et al. (2019)
#' 
#' @format ## `data_latimier_prequiz`
#' A data frame with 95 rows and 3 columns:
#' \describe{
#'   \item{Group}{factor}
#'   \item{Test%}{numeric}
#'   \item{TIme}{numeric}
#' }
"data_latimier_prequiz"

#' Latimier Quiz Prequiz - Ch07 - Quiz and Prequiz groups in Latimier et al. (2019)
#' 
#' @format ## `data_latimier_quiz_prequiz`
#' A data frame with 190 rows and 3 columns:
#' \describe{
#'   \item{Group}{factor}
#'   \item{Test%}{numeric}
#'   \item{Time}{numeric}
#' }
"data_latimier_quiz_prequiz"

#' Latimier Quiz - Ch03 - Quiz group in Latimier et al. (2019)
#' 
#' @format ## `data_latimier_quiz`
#' A data frame with 95 rows and 3 columns:
#' \describe{
#'   \item{Group}{factor}
#'   \item{Test%}{numeric}
#'   \item{Time}{numeric}
#' }
"data_latimier_quiz"

#' Latimier Reread Prequiz - Ch07 - Reread and Prequiz groups in Latimier et al. (2019)
#' 
#' @format ## `data_latimier_reread_prequiz`
#' A data frame with 190 rows and 3 columns:
#' \describe{
#'   \item{Group}{factor}
#'   \item{Test%}{numeric}
#'   \item{Time}{numeric}
#' }
"data_latimier_reread_prequiz"

#' Latimier Reread Quiz - Ch07 - Reread and Quiz groups in Latimier et al. (2019)
#' 
#' @format ## `data_latimier_reread_quiz`
#' A data frame with 190 rows and 3 columns:
#' \describe{
#'   \item{Group}{factor}
#'   \item{Test%}{numeric}
#'   \item{Time}{numeric}
#' }
"data_latimier_reread_quiz"

#' Latimier Reread - Ch03 - Reread group in Latimier et al. (2019)
#' 
#' @format ## `data_latimier_reread`
#' A data frame with 95 rows and 3 columns:
#' \describe{
#'   \item{Group}{factor}
#'   \item{Test%}{numeric}
#'   \item{Time}{numeric}
#' }
"data_latimier_reread"

#' Macnamara r ma - Ch11 - from Macnamara et al. (2014)
#' 
#' @format ## `data_macnamara_r_ma`
#' A data frame with 16 rows and 4 columns:
#' \describe{
#'   \item{Study}{factor}
#'   \item{r}{numeric}
#'   \item{N}{integer}
#'   \item{Instrument_Type}{factor}
#' }
"data_macnamara_r_ma"

#' McCabeMichael brain - Ch9 - from Michael et al. (2013)
#' 
#' @format ## `data_mccabemichael_brain`
#' A data frame with 10 rows and 9 columns:
#' \describe{
#'   \item{Study name}{factor}
#'   \item{M No Brain}{numeric}
#'   \item{s No Brain}{numeric}
#'   \item{n No Brain}{numeric}
#'   \item{M Brain}{numeric}
#'   \item{s Brain}{numeric}
#'   \item{n Brain}{numeric}
#'   \item{SimpleCritique}{factor}
#'   \item{Research group}{factor}
#' }
"data_mccabemichael_brain"

#' McCabeMichael brain2 - Ch9 - from Michael et al. (2013)
#' 
#' @format ## `data_mccabemichael_brain2`
#' A data frame with 12 rows and 9 columns:
#' \describe{
#'   \item{Study name}{factor}
#'   \item{M No Brain}{numeric}
#'   \item{s No Brain}{numeric}
#'   \item{n No Brain}{numeric}
#'   \item{M Brain}{numeric}
#'   \item{s Brain}{numeric}
#'   \item{n Brain}{numeric}
#'   \item{SimpleCritique}{factor}
#'   \item{Research group}{factor}
#' }
"data_mccabemichael_brain2"

#' MeditationBrain - Ch15 - from Hölzel et al. (2011)
#' 
#' @format ## `data_meditationbrain`
#' A data frame with 33 rows and 7 columns:
#' \describe{
#'   \item{Pretest}{numeric}
#'   \item{Posttest}{numeric}
#'   \item{Group}{factor}
#'   \item{ControlPre}{numeric}
#'   \item{ControlPost}{numeric}
#'   \item{MeditationPre}{numeric}
#'   \item{MeditationPost}{numeric}
#' }
"data_meditationbrain"

#' OrganicMoral - Ch14 - from Eskine (2013)
#' 
#' @format ## `data_organicmoral`
#' A data frame with 106 rows and 6 columns:
#' \describe{
#'   \item{Group}{factor}
#'   \item{Mean}{numeric}
#'   \item{SD}{numeric}
#'   \item{N}{integer}
#'   \item{ReplicationGroup}{factor}
#'   \item{MoralJudgmentment}{numeric}
#' }
"data_organicmoral"

#' 
#' 
#' @format ## `data_penlaptop1`
#' A data frame with 65 rows and 2 columns:
#' \describe{
#'   \item{condition}{factor}
#'   \item{transcription}{numeric}
#' }
"data_penlaptop1"

#' PowerPerformance ma - Ch9 - from Burgmer and Englich (2012), and Cusack et al. (2015)
#' 
#' @format ## `data_powerperformance_ma`
#' A data frame with 8 rows and 12 columns:
#' \describe{
#'   \item{StudyName}{factor}
#'   \item{Country}{factor}
#'   \item{Population}{factor}
#'   \item{Difficulty}{factor}
#'   \item{Task}{factor}
#'   \item{M Control}{numeric}
#'   \item{s Control}{numeric}
#'   \item{M Power}{numeric}
#'   \item{s Power}{numeric}
#'   \item{Cohen d unb}{numeric}
#'   \item{n Control}{integer}
#'   \item{n Power}{integer}
#' }
"data_powerperformance_ma"

#' RattanMotivation - Ch14 - from Rattan et al. (2012)
#' 
#' @format ## `data_rattanmotivation`
#' A data frame with 54 rows and 2 columns:
#' \describe{
#'   \item{Group}{factor}
#'   \item{Motivation}{numeric}
#' }
"data_rattanmotivation"

#' ReligionSharing - Ch14 - for End-of-Chapter Exercise 14.3
#' 
#' @format ## `data_religionsharing`
#' A data frame with 3 rows and 4 columns:
#' \describe{
#'   \item{Group}{factor}
#'   \item{Mean}{numeric}
#'   \item{SD}{numeric}
#'   \item{N}{integer}
#' }
"data_religionsharing"

#' Religious belief - Ch03 - for End-of-Chapter Exercise 3.5
#' 
#' @format ## `data_religious_belief`
#' A data frame with 213 rows and 3 columns:
#' \describe{
#'   \item{Response_ID}{character}
#'   \item{Belief_in_God}{factor}
#'   \item{Age}{integer}
#' }
"data_religious_belief"

#' SelfExplain - Ch15 - from McEldoon et al. (2013)
#' 
#' @format ## `data_selfexplain`
#' A data frame with 52 rows and 4 columns:
#' \describe{
#'   \item{Student ID}{factor}
#'   \item{Condition}{factor}
#'   \item{Pretest}{numeric}
#'   \item{Posttest}{numeric}
#' }
"data_selfexplain"

#' SimmonsCredibility - Ch14 - from Simmons and Nelson (2020)
#' 
#' @format ## `data_simmonscredibility`
#' A data frame with 3 rows and 4 columns:
#' \describe{
#'   \item{Groups}{factor}
#'   \item{Mean}{numeric}
#'   \item{SD}{numeric}
#'   \item{n}{integer}
#' }
"data_simmonscredibility"

#' Sleep Beauty - Ch11 - for End-of-Chapter Exercise 11.6
#' 
#' @format ## `data_sleep_beauty`
#' A data frame with 70 rows and 2 columns:
#' \describe{
#'   \item{Sleep (hours)}{numeric}
#'   \item{Rated_Attractiveness}{numeric}
#' }
"data_sleep_beauty"

#' SmithRecall - Ch15 - from Smith et al. (2016)
#' 
#' @format ## `data_smithrecall`
#' A data frame with 120 rows and 6 columns:
#' \describe{
#'   \item{ID}{integer}
#'   \item{Study Technique}{factor}
#'   \item{Stress Status}{factor}
#'   \item{%Recalled}{numeric}
#'   \item{Items Recalled}{numeric}
#'   \item{Group}{factor}
#' }
"data_smithrecall"

#' Stickgold - Ch06 - from Stickgold et al. (2000)
#' 
#' @format ## `data_stickgold`
#' A data frame with 11 rows and 3 columns:
#' \describe{
#'   \item{Sleep deprived}{numeric}
#'   \item{B}{factor}
#'   \item{C}{factor}
#' }
"data_stickgold"

#' StudyStrategies - Ch14 - from O’Reilly et al. (1998)
#' 
#' @format ## `data_studystrategies`
#' A data frame with 3 rows and 10 columns:
#' \describe{
#'   \item{Group}{factor}
#'   \item{TestMean}{numeric}
#'   \item{TestSD}{numeric}
#'   \item{TestN}{integer}
#'   \item{PrevKnowMean}{numeric}
#'   \item{PrevKnowSD}{numeric}
#'   \item{PrevKnowN}{integer}
#'   \item{EaseUseMean}{numeric}
#'   \item{EaseUseSD}{numeric}
#'   \item{EaseUseN}{integer}
#' }
"data_studystrategies"

#' Thomason 1 - Ch11 - from Thomason 1
#' 
#' @format ## `data_thomason_1`
#' A data frame with 12 rows and 3 columns:
#' \describe{
#'   \item{Participant ID}{factor}
#'   \item{Pretest}{numeric}
#'   \item{Posttest}{numeric}
#' }
"data_thomason_1"

#' VideogameAggression - Ch15 - from Hilgard (2015)
#' 
#' @format ## `data_videogameaggression`
#' A data frame with 223 rows and 3 columns:
#' \describe{
#'   \item{Violence}{factor}
#'   \item{Difficulty}{factor}
#'   \item{Agression}{numeric}
#' }
"data_videogameaggression"


