#' Altruism Happiness - Ch12 - from Brethel-Haurwitz and Marsh (2014)
#'
#' Happiness may not be important just for the person feeling it; happiness may
#' also promote kind, altruistic behavior. Brethel-Haurwitz and Marsh (2014)
#' examined this idea by collecting data on U.S. states. A Gallup poll in 2010
#' was used to measure each state's well-being index, a measure of mean
#' happiness for the state's residents on a scale from 0 to 100. Next, a kidney
#' donation database for 1999-2010 was used to figure out each state's rate
#' (number of donations per 1 million people) of non-directed kidney
#' donations-that's giving one kidney to a stranger, an extremely generous and
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
#' @source \doi{10.1177/0956797613516148}
"data_altruism_happiness"

#' Anchor Estimate ma - Ch9 - Many Labs replications of Jacowitz and Kahneman (1995)
#'
#' To what extent does the wording of a question influence one's judgment? In
#' their classic study, Jacowitz and Kahneman (1995) asked participants to
#' estimate how many babies are born each day in the United States. Participants
#' were given either a low anchor (more than 100 babies/day) or a high anchor
#' (less than 50,000 babies/day). Those who saw the low anchor estimated many
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
#' @source \doi{10.1027/1864-9335/a000178}
"data_anchor_estimate_ma"

#' Basol badnews - Ch07 - from Basol et al. (2020)
#'
#' Climate change? Vaccines? Fake news and conspiracy theories on these and
#' numerous other issues can be highly damaging, but are thriving in this social
#' media age. Trying to debunk a conspiracy theory by presenting facts and
#' evidence often doesn't work, alas. Psychological inoculation, also similar to
#' prebunking, presents a mild form of misinformation, preferably with
#' explanation, in the hope of building resistance to real-life fake news-a sort
#' of vaccine for fake news. The Bad News game is a spin-off from research on
#' psychological inoculation. Basol et al. (2020) assessed the possible
#' effectiveness of this game as a fake news vaccine. At getbadnews.com you can
#' click 'About' for information, or just start playing the game- it's easy and
#' maybe even fun. You encounter mock Twitter (now X) fake news messages that
#' illustrate common strategies for making fake news memorable or believable.
#' You make choices between messages and decide which ones to 'forward' as you
#' try to spread fake news while building your credibility score and number of
#' 'followers'-rather like real life for a conspiracy theorist wanting to spread
#' the word. Compete with your friends for credibility and number of followers.
#' Basol's online participants first saw 18 fictitious fake news tweets and
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
#' @source \doi{10.5334/joc.91}
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
#' @source \doi{10.1037/0033-2909.115.1.25}
"data_bem_psychic"

#' BodyWellF - Ch12 - Body Satisfaction and Well-being data for females from Figure 11.24 right panel
#'
#' A subset of data_bodywell_fm, reports only for those participants
#' who identified as female.  Data is Subjective Wellbeing and Body Satisfaction.
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

#' Campus Involvement - Ch11 - for End-of-Chapter Exercise 11.7
#'

#' Clinton conducted a survey of college students to determine the extent to
#' which subjective well-being is related to campus involvement (Campus
#' Involvement data set on the book website). Participants completed a measure
#' of subjective well-being (scale from 1 to 5) and a measure of campus
#' involvement (scale from 1 to 5). Participants also reported gender (male or
#' female) and commuter status (resident or commuter).  Synthetic data simulated
#' to mimic survey data from a class project.
#'
#' @format ## `data_campus_involvement`
#' A data frame with 113 rows and 6 columns:
#' \describe{
#'   \item{ID}{integer}
#'   \item{Gender}{factor}
#'   \item{GPA}{numeric}
#'   \item{CommuterStatus}{factor}
#'   \item{SWB}{numeric}
#'   \item{Campus Involvement}{numeric}
#' }
"data_campus_involvement"

#'
#'

#' *Fictitious* data from an unrealistically small HEAT
#' study comparing scores for a single group of students before and after a
#' workshop on climate change.
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

#' Some researchers claim that moral judgments are based not only on rational
#' considerations but also on one's current emotional state. To what extent can
#' recent emotional experiences influence moral judgments? Schnall et al. (2008)
#' examined this question by manipulating feelings of cleanliness and purity and
#' then observing the extent that this changes how harshly participants judge
#' the morality of others. Inscho Study 1, Schnall et al. asked participants to
#' complete a word scramble task with either neutral words (neutral prime) or
#' words related to cleanliness (cleanliness prime). All students then completed
#' a set of moral judgments about controversial scenarios: Moral judgment is the
#' average of six items, each rated on a scale from 0 to 9, with high meaning
#' harsh. The data from this study are in the Clean moral file, which also
#' contains data from a replication by Johnson et al. (2014)
#'
#'
#' @format ## `data_clean_moral`
#' A data frame with 208 rows and 4 columns:
#' \describe{
#'   \item{Schnall Condition}{factor}
#'   \item{Schnall Moral judgment}{numeric}
#'   \item{Johnson Condition}{factor}
#'   \item{Johnson Moral judgment}{numeric}
#' }
#' @source \doi{10.1027/1864-9335/a000186}
"data_clean_moral"

#' College survey 1 - Ch03 - for End-of-Chapter Exercise 3.3
#'
#' Data from an additional survey of Dominican University students; reports
#' various psychological and behavioral measures.
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

#' EffronRaj fakenews - Ch8 - from Effron and Raj (2020), v1.1
#'
#' *Synthetic data* meant to represent Experiment 1 of Effron & Raj, 2020.
#' 138 U.S. adults, recruited in August 2018 on Prolific Academic, worked
#' online. First, they saw six fake headlines four times, each time being asked
#' to rate how interesting/engaging/ funny/well-written the headline was. This
#' rating task simply ensured that the participants paid some attention to each
#' headline. The stimuli were 12 actual fake-news headlines about American
#' politics, with accompanying photographs. Half appealed to Republicans and
#' half to Democrats. Later, 12 fake headlines were presented one at a time, a
#' random mix of the six Old headlines-those seen before-and six New headlines
#' not seen previously. It was stated very clearly that independent,
#' non-partisan fact-checking had established that all the headlines were not
#' true. Participants first rated, on a 0 (not at all) to 100 (extremely) scale,
#' the degree to which to which they judged it unethical to publish that
#' headline. That's the Unethicality DV. They also rated how likely they would
#' be to share the headline if they saw it posted by an acquaintance on social
#' media; there were three further similar ratings. Finally, they rated how
#' accurate they believed the headline to be.
#'
#' v1.1 -- Participant 46 had an invalid negative value for UnethOld; replaced
#'   with 0.
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
#' @source \doi{10.1177/0956797619887896}
"data_effronraj_fakenews"

#' Emotion heartrate - Ch8 - from Lakens (2013)
#'
#' Anger is a powerful emotion. To what extent can feeling angry actually change
#' your heart rate? To investigate, Lakens (2013) asked students to record their
#' heart rate (in beats per minute) at rest before (baseline) and then while
#' recalling a time of intense anger. This is a conceptual replication of a
#' classic study by Ekman et al. (1983). Load the Emotion heartrate data set
#' from the book website.
#'
#' @format ## `data_emotion_heartrate`
#' A data frame with 68 rows and 3 columns:
#' \describe{
#'   \item{ID}{integer}
#'   \item{HR_baseline}{numeric}
#'   \item{HR_anger}{numeric}
#' }
#' @source \doi{10.1109/T-AFFC.2013.3}
"data_emotion_heartrate"

#' Exam Scores - Ch11 - for End-of-Chapter Exercise 11.2
#'

#' To what extent does initial performance in a class relate to performance on a
#' final exam? First exam and final exam scores for nine students enrolled in an
#' introductory psychology course. Exam scores are percentages, where 0 = no
#' answers correct and 100 = all answers correct.
#' Data is synthetic to represent patterns found in a previous psych
#' stats course.
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

#' To what extent does being exposed to the American flag influence political
#' attitudes? One seminal study (Carter et al., 2011) explored this issue by
#' subtly exposing participants either to images of the American flag or to
#' control images. Next, participants were asked about their political
#' attitudes, using a 1-7 rating scale where high scores indicate conservative
#' attitudes. Participants exposed to the flag were found to express
#' substantially more conservative attitudes. The Many Labs project replicated
#' this finding at 25 different locations in the United States.
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
#' @source \doi{10.5334/jopd.ad}
"data_flag_priming_ma"

#' Gender math IAT ma - Ch9 - Many Labs replications of Nosek et al. (2002)
#'
#' In EOC Exercise 4 in Chapter 7 we encountered the classic study of Nosek et
#' al. (2002), in which male and female participants completed an Implicit
#' Association Test (IAT) that measured the extent of negative attitudes towards
#' mathematics, compared with art. The study found that women, compared with
#' men, tended to have more negative implicit attitudes towards mathematics. The
#' Many Labs project repeated this study at locations around the world (Klein et
#' al., 2014a, 2014b). Summary data for 30 of these labs are available in Gender
#' math IAT ma. Higher scores indicate more implicit bias against mathematics.
#' See also data_gender_math_iat for raw data from two specific sites from this
#' replication effort.
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
#' @source \doi{10.1027/1864-9335/a000178}
"data_gender_math_iat_ma"

#' Gender math IAT - Ch07 - Ithaca and SDSU replications of Nosek et al. (2002)
#'
#' To what extent do men and women differ in their attitudes towards
#' mathematics? To investigate, Nosek et al. (2002) asked male and female
#' students to complete an Implicit Association Test (IAT)-this is a task
#' designed to measure a participant's implicit (non-conscious) feelings towards
#' a topic. (If you've never heard of the IAT, try it out here:
#' tiny.cc/harvardiat) On this IAT, students were tested for negative feelings
#' towards mathematics and art. Scores reflect the degree to which a student had
#' more negative implicit attitudes about mathematics than art (positive score:
#' more negative feelings about mathematics; 0: same level of negativity to
#' both; negative score: more negative feelings about art). data_gender_math_iat
#' has data from two labs that participated in a large-scale replication of the
#' original study (Klein et al., 2014a, 2014b)
#'
#' @format ## `data_gender_math_iat`
#' A data frame with 155 rows and 4 columns:
#' \describe{
#'   \item{Ithaca gender}{factor}
#'   \item{Ithaca IAT}{numeric}
#'   \item{SDSU gender}{factor}
#'   \item{SDSU IAT}{numeric}
#' }
#' @source \doi{10.1027/1864-9335/a000178}
"data_gender_math_iat"

#' Halagappa - Ch14 - from Halagappa et al. (2007)
#'

#' Could eating much less delay Alzheimer's? If so, that would be great news.
#' Halagappa et al. (2007) investigated the possibility by using a mouse model,
#' meaning they used Alzheimer-prone mice, which were genetically predisposed to
#' develop neural degeneration typical of Alzheimer's. The researchers used six
#' independent groups of mice, three tested in mouse middle age when 10 months
#' old, and three in mouse old age when 17 months. At each age there was a
#' control group of normal mice that ate freely (the NFree10 and NFree17
#' groups), a group of Alzheimer-prone mice that also ate freely (the AFree10
#' and AFree17 groups), and another Alzheimer-prone group restricted to 40% less
#' food than normal (the ADiet10 and ADiet17 groups). Table 14.2 lists the
#' factors that define the groups, and group labels. I'll discuss one measure of
#' mouse cognition: the percent time spent near the target of a water maze, with
#' higher values indicating better learning and memory. Table 14.2 reports the
#' means and standard deviations for this measure, and group sizes.
#'
#' @format ## `data_halagappa`
#' A data frame with 6 rows and 4 columns:
#' \describe{
#'   \item{Groups}{factor}
#'   \item{Mean}{numeric}
#'   \item{SD}{numeric}
#'   \item{n}{integer}
#' }
#' @source \doi{10.1016/j.nbd.2006.12.019}
"data_halagappa"

#' Home Prices - Ch12 - for End-of-Chapter Exercise 12.2
#'

#' Maybe you're thinking about buying a house after college? Regression can help
#' you hunt for a bargain. Download the Home Prices data set. This file contains
#' real estate listings from 1997 to 2003 in a city in California. Let's explore
#' the extent to which the size of the home (in square meters) predicts the sale
#' price.
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

#' Kardas Expt 3 - Ch07 - from Kardas and O'Brien (2018), Experiment 3
#'

#' Suppose you want to change the battery in your phone, cook the perfect
#' souffle, or perform a three-ball juggle. Just as numerous people do every
#' day, you might search online to find a video that shows what to do. Suppose
#' you watch such a video just once. First question: How well would you then
#' predict you could perform the task? Second question: How well would you
#' actually perform the task, the first time you tried? Now suppose you watch
#' the video many times: Again consider the two questions. These questions were
#' investigated in a series of studies by Kardas and O'Brien (2018). Let's first
#' do some quick analyses of Kardas Experiments 3 and 4-let's call them Expt 3
#' and Expt 4-focusing on the effect of watching a video many times rather than
#' once. In Expt 3, participants first watched a brief video of a person
#' performing the moonwalk. The Low Exposure group watched the video once, the
#' High Exposure group 20 times. Then participants predicted, on a 1 to 10
#' scale, how well they felt they would be able to perform the moonwalk
#' themselves. Finally, they attempted a single performance of the moonwalk,
#' which was videoed. These videos were rated, on the same 1 to 10 scale, by
#' independent raters.
#'
#' @format ## `data_kardas_expt_3`
#' A data frame with 100 rows and 3 columns:
#' \describe{
#'   \item{Exposure}{factor}
#'   \item{Prediction}{numeric}
#'   \item{Performance}{numeric}
#' }
#' @source \doi{10.1177/0956797617740646}
"data_kardas_expt_3"

#' Kardas Expt 4 - Ch07 - from Kardas and O'Brien (2018), Experiment 4
#'
#' Suppose you want to change the battery in your phone, cook the perfect
#' souffle, or perform a three-ball juggle. Just as numerous people do every
#' day, you might search online to find a video that shows what to do. Suppose
#' you watch such a video just once. First question: How well would you then
#' predict you could perform the task? Second question: How well would you
#' actually perform the task, the first time you tried? Now suppose you watch
#' the video many times: Again consider the two questions. These questions were
#' investigated in a series of studies by Kardas and O'Brien (2018). Let's first
#' do some quick analyses of Kardas Experiments 3 and 4-let's call them Expt 3
#' and Expt 4-focusing on the effect of watching a video many times rather than
#' once. Expt 4 was conducted online with participants recruited from Amazon's
#' Mechanical Turk, who are typically more diverse than students. The online
#' task was based on a mirror-drawing game developed by Bob and students (Cusack
#' et al., 2015, tiny.cc/bobmirrortrace). Participants first read a description
#' of the game and the scoring procedure. To play, you use your computer
#' trackpad to trace a target line, as accurately and quickly as you can. The
#' task is tricky because you can see only a mirror image of the path you are
#' tracing with a finger on the trackpad. A running score is displayed. The
#' final score is the percentage match between the target line and the path you
#' traced, so scores can range from 0 to 100
#'
#' @format ## `data_kardas_expt_4`
#' A data frame with 270 rows and 4 columns:
#' \describe{
#'   \item{Exposure}{factor}
#'   \item{Prediction}{integer}
#'   \item{Performance}{integer}
#'   \item{Time}{numeric}
#' }
#' @source \doi{10.1177/0956797617740646}
"data_kardas_expt_4"

#' Labels flavor - Ch8 - from Floretta-Schiller et al. (2015)
#'
#' To what extent do brand labels influence perceptions of a product? To
#' investigate, participants were asked to participate in a taste test. All
#' participants were actually given the same grape juice, but one glass was
#' poured from a bottle labeled 'Organic' and the other glass from a bottle
#' labeled 'Generic'. After each tasting (in counterbalanced order),
#' participants were asked to rate how much they enjoyed the juice on a scale
#' from 1 (not at all) to 10 (very much). Participants were also asked to say
#' how much they'd be willing to pay for a large container of that juice on a
#' scale from $1 to $10. Load the Labels flavor data set from the book website.
#' These data were collected as part of a class project by Floretta-Schiller et
#' al. (2015), whose work was inspired by a very clever study looking at the
#' effects of fast-food wrappers on children's enjoyment of food (Robinson et
#' al., 2007).
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

#' The researchers were interested in how different study approaches might
#' impact learning. Working in France, they created three independent groups,
#' each comprising 95 adults. Participants worked online through seven learning
#' modules about DNA. The Reread group worked through a module, then worked
#' through it a second time before going on to the next module. The Quiz group
#' worked through a module, then had to complete a quiz before going on to the
#' next module. The Prequiz group had to work through the quiz before seeing the
#' presentation of a module, then went on to the quiz and presentation of the
#' next module. Participants received feedback and a brief explanation after
#' answering each question in a quiz, and could take as long as they wished to
#' work through each module and quiz. Seven days later, participants completed a
#' final test.
#' data_latimier_3groups is the full data set.
#' To facilitate different student exercises, there are also separate data
#' entities for each group (data_latimier_prequiz, data_latimier_reread, etc.),
#' and for every *pair* of groups (data_latimier_quiz_prequiz, etc.).
#'
#' @format ## `data_latimier_3groups`
#' A data frame with 285 rows and 3 columns:
#' \describe{
#'   \item{Group}{factor}
#'   \item{Test%}{numeric}
#'   \item{Time}{numeric}
#' }
#' @source \doi{10.7910/DVN/XPYPMF}
"data_latimier_3groups"

#' Latimier Prequiz - Ch03 - Prequiz group in Latimier et al. (2019)
#'

#' Just the Prequiz group from Latimier et al., 2019
#' See full details in data_latimier_3_groups
#'
#' @format ## `data_latimier_prequiz`
#' A data frame with 95 rows and 3 columns:
#' \describe{
#'   \item{Group}{factor}
#'   \item{Test%}{numeric}
#'   \item{TIme}{numeric}
#' }
#' @source \doi{10.7910/DVN/XPYPMF}
"data_latimier_prequiz"

#' Latimier Quiz Prequiz - Ch07 - Quiz and Prequiz groups in Latimier et al. (2019)
#'

#' Just the Quiz (RQ) an Prequiz (QR) groups from Latimier et al., 2019
#' See full details in data_latimier_3_groups
#'
#' @format ## `data_latimier_quiz_prequiz`
#' A data frame with 190 rows and 3 columns:
#' \describe{
#'   \item{Group}{factor}
#'   \item{Test%}{numeric}
#'   \item{Time}{numeric}
#' }
#' @source \doi{10.7910/DVN/XPYPMF}
"data_latimier_quiz_prequiz"

#' Latimier Quiz - Ch03 - Quiz group in Latimier et al. (2019)
#'

#' Just the Quiz group from Latimier et al., 2019
#' See full details in data_latimier_3_groups
#'
#' @format ## `data_latimier_quiz`
#' A data frame with 95 rows and 3 columns:
#' \describe{
#'   \item{Group}{factor}
#'   \item{Test%}{numeric}
#'   \item{Time}{numeric}
#' }
#' @source \doi{10.7910/DVN/XPYPMF}
"data_latimier_quiz"

#' Latimier Reread Prequiz - Ch07 - Reread and Prequiz groups in Latimier et al. (2019)
#'

#' Just the Reread (RR) an Prequiz (QR) groups from Latimier et al., 2019
#' See full details in data_latimier_3_groups
#'
#' @format ## `data_latimier_reread_prequiz`
#' A data frame with 190 rows and 3 columns:
#' \describe{
#'   \item{Group}{factor}
#'   \item{Test%}{numeric}
#'   \item{Time}{numeric}
#' }
#' @source \doi{10.7910/DVN/XPYPMF}
"data_latimier_reread_prequiz"

#' Latimier Reread Quiz - Ch07 - Reread and Quiz groups in Latimier et al. (2019)
#'

#' Just the Reread Quiz groups from Latimier et al., 2019
#' See full details in data_latimier_3_groups
#'
#' @format ## `data_latimier_reread_quiz`
#' A data frame with 190 rows and 3 columns:
#' \describe{
#'   \item{Group}{factor}
#'   \item{Test%}{numeric}
#'   \item{Time}{numeric}
#' }
#' @source \doi{10.7910/DVN/XPYPMF}
"data_latimier_reread_quiz"

#' Latimier Reread - Ch03 - Reread group in Latimier et al. (2019)
#'

#' Just the Reread group from Latimier et al., 2019
#' See full details in data_latimier_3_groups
#'
#' @format ## `data_latimier_reread`
#' A data frame with 95 rows and 3 columns:
#' \describe{
#'   \item{Group}{factor}
#'   \item{Test%}{numeric}
#'   \item{Time}{numeric}
#' }
#' @source \doi{10.7910/DVN/XPYPMF}
"data_latimier_reread"

#' Macnamara r ma - Ch11 - from Macnamara et al. (2014)
#'
#' Is genius born or made? Could any of us be Michael Jordan or Mozart if we
#' worked sufficiently hard to develop the requisite skills? Meta-analysis of
#' correlations can help answer such questions. The issue here is the extent
#' that practice and effort may be sufficient for achieving the highest levels
#' of expertise. Ericsson et al. (1993) argued that years of effort is what
#' matters most: 'Many characteristics once believed to reflect innate talent
#' are actually the result of intense practice extended for a minimum of 10
#' years' (p. 363). This view was enormously popularized by Malcolm Gladwell
#' (2008), who argued in his book Outliers that 10,000 hours of focused practice
#' is the key to achieving expertise. However, this view is now being
#' challenged, with one important contribution being a large meta-analysis of
#' correlations between amount of intense practice and level of achievement:
#' Macnamara et al. (2014) combined 157 correlations reported in a wide range of
#' fields, from sports to music and education, and found correlation of only r =
#' .35 (.30, .39). Table 11.1 shows the 16 main correlations for music, from
#' Macnamara et al. (2014).
#'
#' @format ## `data_macnamara_r_ma`
#' A data frame with 16 rows and 4 columns:
#' \describe{
#'   \item{Study}{factor}
#'   \item{r}{numeric}
#'   \item{N}{integer}
#'   \item{Instrument_Type}{factor}
#' }
#' @source \doi{10.1177/0956797614535810}
"data_macnamara_r_ma"

#' McCabeMichael brain - Ch9 - from Michael et al. (2013)
#'
#' You've probably seen cross sections of the brain with
#' brightly colored areas indicating which brain regions are most active during
#' a particular type of cognition or emotion. Search online for fMRI (functional
#' magnetic resonance imaging) brain scans to see such pictures and learn how
#' they are made. They can be fascinating-are we at last able to see how
#' thinking works? In 2008, McCabe and Castel published studies that
#' investigated how adding a brain picture might alter judgments of the
#' credibility of a scientific article. For one group of participants, an
#' article was accompanied by a brain image that was irrelevant to the article.
#' For a second, independent group, there was no image. Participants read the
#' article, then gave a rating of the statement 'The scientific reasoning in the
#' article made sense'. The response options were 1 (strongly disagree), 2
#' (disagree), 3 (agree), and 4 (strongly agree). The researchers reported that
#' mean ratings were higher with a brain picture than without, but that the
#' difference was small. It seemed that an irrelevant brain picture may have
#' some, but only a small influence. The authors drew appropriately cautious
#' conclusions, but the result quickly attracted attention and there were many
#' media reports that greatly overstated it. At least according to the popular
#' media, it seemed that adding a brain picture made any story convincing.
#' Search on 'McCabe seeing is believing', or similar, to find media reports and
#' blog posts. Some warned readers to watch out for brain pictures, which, they
#' said, can trick you into believing things that aren't true. The result
#' intrigued some New Zealander colleagues of mine who discovered that, despite
#' its wide recognition, the finding hadn't been replicated. They ran
#' replication studies using the materials used by the original researchers, and
#' found generally small ESs. I joined the team at the data analysis stage and
#' the research was published (Michael et al., 2013). I'll discuss here a
#' meta-analysis of two of the original studies and eight replications by our
#' team. The studies were sufficiently similar for meta-analysis, especially
#' considering that all the Michael studies were designed to have many features
#' that matched the original studies.  This data set does *not* include
#' two additional critique studies run by the Michael team.  See also
#' data_mccabemichael_brain2
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
#' @source \doi{10.3758/s13423-013-0391-6}
"data_mccabemichael_brain"

#' McCabeMichael brain2 - Ch9 - from Michael et al. (2013)
#'
#' Same as data_mccabemichael_brain but includes two additional critique studies
#' run by the Michael team.
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
#' @source \doi{10.3758/s13423-013-0391-6}
"data_mccabemichael_brain2"

#' MeditationBrain - Ch15 - from Holzel et al. (2011)
#'
#' My example is a well-known study of mindfulness meditation by Holzel et al.
#' (2011). People who wanted to reduce stress, and were not experienced
#' meditators, were assigned to a Meditation (n = 16) or a Control (n = 17)
#' group. The Meditation group participated in 8 weeks of intensive training and
#' practice of mindfulness meditation. The researchers used a questionnaire to
#' assess a range of emotional and cognitive variables both before (Pretest) and
#' after (Posttest) the 8-week period. All assessment was conducted while the
#' participants were not meditating. The study is notable for including brain
#' imaging to assess possible changes in participants' brains from Pretest to
#' Posttest. The researchers measured gray matter concentration, which increases
#' in brain regions that experience higher and more frequent activation. The
#' researchers expected that the hippocampus may be especially responsive to
#' meditation because it has been implicated in the regulation of emotion,
#' arousal, and general responsiveness. They therefore included in their planned
#' analysis the assessment of any changes to gray matter concentration in the
#' hippocampus.
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
#' @source \doi{10.1007/978-94-007-2079-4_9}
"data_meditationbrain"

#' OrganicMoral - Ch14 - from Eskine (2013)
#'

#' To what extent might choosing organic foods make us morally smug? To
#' investigate, Eskine (2013) asked participants to rate images of organic food,
#' neutral (control) food, or comfort food. Next, under the guise of a different
#' study, all participants completed a moral judgment scale in which they read
#' different controversial scenarios and rated how morally wrong they judged
#' them to be (scale of 1-7, high judgments mean more wrong). Table 14.7 shows
#' summary data, which are also available in the first four variables in the
#' OrganicMoral file. In that file you can see two further variables, which
#' report full data-we'll come to these shortly. Here we use the summary data.
#' After the results of Eskine (2013) were published, Moery and Calin-Jageman
#' (2016) conducted a series of close replications. We obtained original
#' materials from Eskine, piloted the procedure, and preregistered our sampling
#' and analysis plan. The OSF page, osf.io/atkn7, has all the details. The data
#' from one of these close replications are in the last two variables of the
#' OrganicMoral file. For this replication study, group names are in the
#' variable ReplicationGroup and moral judgments in MoralJudgment. (You may need
#' to scroll right to see these variables.)
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
#' @source \doi{10.1177/1948550616639649}
"data_organicmoral"

#'
#'
#' % transcription scores from pen and laptop group of Meuller et al., 2014
#'
#' @format ## `data_penlaptop1`
#' A data frame with 65 rows and 2 columns:
#' \describe{
#'   \item{condition}{factor}
#'   \item{transcription}{numeric}
#' }
#' @source \doi{10.1177/0956797614524581}
"data_penlaptop1"

#' PowerPerformance ma - Ch9 - from Burgmer and Englich (2012), and Cusack et al. (2015)
#'
#' To what extent could feeling powerful affect your performance at motor
#' skills? To investigate, Burgmer and Englich (2012) assigned German
#' participants to either power or control conditions and then asked them to
#' play golf (Experiment 1) or darts (Experiment 2). They found that
#' participants manipulated to feel powerful performed substantially better than
#' those in the control condition. To study this finding further, Cusack et al.
#' (2015) conducted five replications in the United States. Across these
#' replications they tried different ways of manipulating power, different types
#' of tasks (golf, mirror tracing, and a cognitive task), different levels of
#' difficulty, and different types of participant pools (undergraduates and
#' online). Summary data from all seven studies are available in
#' PowerPerformance ma.
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
#' @source \doi{10.1371/journal.pone.0140806}
"data_powerperformance_ma"

#' RattanMotivation - Ch14 - from Rattan et al. (2012)
#'
#' How do you think you would react to feedback that gave encouragement and
#' reassurance, or, instead, encouragement and challenge? Carol Dweck and her
#' colleagues have investigated many such questions about how people respond to
#' different types of feedback. My next example comes from Dweck's research
#' group and illustrates data analysis that starts with the full data, rather
#' than only summary statistics. Rattan et al. (2012) asked their college
#' student participants to imagine they were undertaking a mathematics course
#' and had just received a low score (65%) on the first test of the year.
#' Participants were assigned randomly into three groups, which received
#' different feedback along with the low score. The Comfort group received
#' positive encouragement and also reassurance, the Challenge group received
#' positive encouragement and also challenge, and the Control group received
#' just the positive encouragement. Participants then responded to a range of
#' questions about how they felt about the course and their professor. I'll
#' discuss data for their ratings of their own motivation toward mathematics,
#' made after they had received the feedback.
#'
#' @format ## `data_rattanmotivation`
#' A data frame with 54 rows and 2 columns:
#' \describe{
#'   \item{Group}{factor}
#'   \item{Motivation}{numeric}
#' }
#' @source \doi{10.1016/j.jesp.2011.12.012}
"data_rattanmotivation"

#' ReligionSharing - Ch14 - **RETRACTED DATA** used in End-of-Chapter Exercise 14.3
#'
#' To what extent is a religious upbringing related to prosocial behavior in
#' childhood? To investigate, a large international sample of children was asked
#' to play a game in which they were given 10 stickers but then asked if they
#' would give some of these stickers away to another child who had not been able
#' to be tested that day. The number of stickers donated was considered a
#' measure of altruistic sharing. In addition, the parents of each child
#' reported the family's religion. Summary data provided.  **THIS STUDY
#' HAS BEEN RETRACTED DUE TO AN ERRONEOUS ANALYSIS - THE DATASET WILL BE
#' REMOVED FROM FUTURE VERSIONS OF ESCI AND THE BOOK**
#'
#' @format ## `data_religionsharing`
#' A data frame with 3 rows and 4 columns:
#' \describe{
#'   \item{Group}{factor}
#'   \item{Mean}{numeric}
#'   \item{SD}{numeric}
#'   \item{N}{integer}
#' }
#' @source \doi{10.1016/j.cub.2015.09.056}
"data_religionsharing"

#' Religious belief - Ch03 - for End-of-Chapter Exercise 3.5
#'
#' Let's look at some data about religious beliefs. The Religious belief file
#' has data from a large online survey in which participants were asked to
#' report, on a scale from 0 to 100, their belief in the existence of God.  Age
#' was also reported.
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
#' Self-explaining is a learning strategy where students write or say their own
#' explanations of the material they are studying. Self-explaining has generally
#' been found to be more effective than standard studying, but it may also take
#' more time. This raises the question of whether it's the study strategy or the
#' extra time that benefits learning. To explore this issue, grade school
#' children took a pretest of mathematics conceptual knowledge, studied
#' mathematics problems, and then took a similar posttest (McEldoon et al.,
#' 2013). Participants were randomly assigned to one of two study conditions:
#' normal study + more practice (More Practice group), or self-explaining
#' (Self-Explain group). The first condition was intended to make time spent
#' learning to be similar for the two groups. You can find part of the data from
#' this study in SelfExplain, the scores being percent correct.
#'
#' @format ## `data_selfexplain`
#' A data frame with 52 rows and 4 columns:
#' \describe{
#'   \item{Student ID}{factor}
#'   \item{Condition}{factor}
#'   \item{Pretest}{numeric}
#'   \item{Posttest}{numeric}
#' }
#' @source \doi{10.1111/j.2044-8279.2012.02083.x}
"data_selfexplain"

#' SimmonsCredibility - Ch14 - from Simmons and Nelson (2020)
#'
#' You're excited! Your company has developed a wonderful new weight-loss
#' program, and now it's your job to develop the ad campaign. Should you choose
#' a BeforeAfter pair of pictures, as in Figure 14.1, top panel? Or might a
#' Progressive sequence of pictures of the same person, as in the bottom panel,
#' be more effective? Pause, think, and discuss. Which would you choose, and
#' why? You might think that BeforeAfter is simpler and more dramatic. On the
#' other hand, Progressive highlights the steady improvement that you claim the
#' program will deliver. You're probably not surprised to learn that BeforeAfter
#' is used often and has long been a favorite of the advertising industry,
#' whereas Progressive is used only rarely. Luca Cian and colleagues (Cian et
#' al., 2020) were curious to know the extent to which BeforeAfter is actually
#' more effective, appealing, and credible than Progressive, or, indeed, whether
#' Progressive might score more highly. They reported seven studies of various
#' aspects of that question. I'll focus on their Study 2, in which they used
#' three independent groups to compare all three conditions illustrated in
#' Figure 14.1. The BeforeAfterInfo condition, in the middle panel, comprises
#' three BeforeAfter pairs, thus providing extra information about the before
#' and after endpoints. The researchers included this condition in case any
#' advantage of Progressive might stem simply from having more images, rather
#' than because it illustrates a clear progressive sequence. They randomly
#' assigned 213 participants from MTurk to one of the three groups. Participants
#' were asked to 'imagine that you have decided to lose some weight', then saw
#' one of the three ads for a weight loss program called MRMDiets. They then
#' answered the question 'How would you evaluate MRMDiets?' by choosing a 1-7
#' response on several scales, including Unlikeable-Likable,
#' Ineffective-Effective, and Not credible-Credible. The researchers averaged
#' six such scores to give an overall Credibility score, on the 1-7 scale, with
#' 7 being the most credible. Simmons and Nelson (2020) were sufficiently
#' intrigued to carry out two substantial very close replications. With the
#' cooperation of the original researchers, they used the same materials and
#' procedure. They used much larger groups and preregistered their research
#' plan, including data analysis plan. I'll focus on their first replication, in
#' which 761 participants from MTurk were randomized to the three groups.
#'
#' @format ## `data_simmonscredibility`
#' A data frame with 3 rows and 4 columns:
#' \describe{
#'   \item{Groups}{factor}
#'   \item{Mean}{numeric}
#'   \item{SD}{numeric}
#'   \item{n}{integer}
#' }
#' @source <http://datacolada.org/94>
"data_simmonscredibility"

#' Sleep Beauty - Ch11 - for End-of-Chapter Exercise 11.6
#'
#' Is there really such a thing as beauty sleep? To investigate, researchers
#' decided to examine the extent to which sleep relates to attractiveness. Each
#' of 70 college students self-reported the amount of sleep they had the night
#' before. In addition, a photograph was taken of each participant and rated for
#' attractiveness on a scale from 1 to 10 by two judges of the opposite gender.
#' The average rating score was used. You can download this data set (Sleep
#' Beauty) from the book website.
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

#' Stickgold et al. (2000) found that, remarkably, performance on a visual
#' discrimination task actually improved over the 48-96 hours after initial
#' training, even without practice during that time. However, what if
#' participants were sleep deprived during that period? They trained 11
#' participants in that new skill, then all were sleep deprived. The data were
#' (-14.7, -10.7, -10.7, 2.2, 2.4, 4.5, 7.2, 9.6, 10, 21.3, 21.8)-or download
#' the Stickgold data set from the book website. The data are the changes in
#' performance scores from immediately after training to after the night without
#' sleep: 0 represents no change, positive scores represent improvement, and
#' negative scores represent decline. Data set courtesy of
#' DataCrunch (tiny.cc/Stickgold)
#'
#' @format ## `data_stickgold`
#' A data frame with 11 rows and 3 columns:
#' \describe{
#'   \item{Sleep deprived}{numeric}
#'   \item{B}{factor}
#'   \item{C}{factor}
#' }
#' @source <https://www.statcrunch.com/app/index.html?dataid=1053539>
"data_stickgold"

#' StudyStrategies - Ch14 - from O'Reilly et al. (1998)
#'

#' To what extent does study strategy influence learning? To investigate,
#' psychology students were randomly assigned to three groups and asked to learn
#' biology facts using one of three different strategies: a) Self-Explain
#' (explaining for each fact what new knowledge is gained and how it relates to
#' what is already known), b) Elab Interrogation (elaborative interrogation:
#' stating for each fact why it makes sense), or c) Repetition Control (stating
#' each fact over and over). After studying, students took a 25-point
#' fill-the-blank test (O'Reilly et al., 1998)
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
#' @source \doi{10.1006/ceps.1997.0977}
"data_studystrategies"

#' Thomason 1 - Ch11 - from Thomason 1
#'
#' Summary data from an unpublished study by Neil Thomason and colleagues, who
#' were interested in ways to enhance students' critical thinking. They were
#' investigating argument mapping, which is a promising way to use diagrams to
#' represent the structure of arguments. Students in their study completed an
#' established test of critical thinking (the Pretest), then a critical thinking
#' course based on argument mapping, then a second version of the test (the
#' Posttest).
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
#' Video games can be violent and they can also be challenging. To what extent
#' might these factors cause aggressive behavior? To explore, Hilgard (2015)
#' asked male participants to play one of four versions of a video game for 15
#' minutes. The game was customized so that it could vary in violence (shooting
#' zombies or helping aliens) and difficulty (targets controlled by tough AI or
#' dumb AI). After the game, players were provoked by being given an insulting
#' evaluation by a confederate. Participants then got to decide how long the
#' confederate should hold their hand in painfully cold ice water (between 0 and
#' 80 seconds), and this was taken as a measure of aggressive behavior. You can
#' find the materials and analysis plan for this study on the Open Science
#' Framework: osf. io/cwenz. This is a simplified version of the full data set.
#'
#' @format ## `data_videogameaggression`
#' A data frame with 223 rows and 3 columns:
#' \describe{
#'   \item{Violence}{factor}
#'   \item{Difficulty}{factor}
#'   \item{Agression}{numeric}
#' }
#' @source \doi{10.1177/0956797619829688}
"data_videogameaggression"


