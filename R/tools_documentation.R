jesci_document_object <- function(estimate) {
  tdoc <- NULL
  for (x in 1:length(estimate)) {
    if (is(estimate[[x]], "data.frame")) {
      tdoc <- paste(
        tdoc,
        "#' - **",
        names(estimate)[[x]],
        "**\n",
        sep = ""
      )
      for (mycol in colnames(estimate[[x]])) {
        tdoc <- paste(
          tdoc,
          "#'     - *",
          mycol,
          "* - \n",
          sep = ""
        )
      }
    }
  }
  cat(tdoc)
}



jesci_document_data <- function(save_files = FALSE) {

descriptions <- list()
sources <- list()

descriptions["data_altruism_happiness"] <-
"#' Happiness may not be important just for the person feeling it; happiness may
#' also promote kind, altruistic behavior. Brethel-Haurwitz and Marsh (2014)
#' examined this idea by collecting data on U.S. states. A Gallup poll in 2010
#' was used to measure each state's well-being index, a measure of mean
#' happiness for the state's residents on a scale from 0 to 100. Next, a kidney
#' donation database for 1999-2010 was used to figure out each state's rate
#' (number of donations per 1 million people) of non-directed kidney
#' donations-that's giving one kidney to a stranger, an extremely generous and
#' altruistic thing to do!
"

sources$data_altruism_happiness <-
  "https://journals.sagepub.com/doi/full/10.1177/0956797613516148"

descriptions["data_anchor_estimate_ma"] <-
"#' To what extent does the wording of a question influence one's judgment? In
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
"

sources$data_anchor_estimate_ma <-
  "https://econtent.hogrefe.com/doi/10.1027/1864-9335/a000178"


descriptions["data_basol_badnews"] <-
"#' Climate change? Vaccines? Fake news and conspiracy theories on these and
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
"

sources$data_basol_badnews <-
  "https://doi.org/10.5334%2Fjoc.91"

descriptions$data_bem_psychic <-
"#' Daryl Bem was an experienced mentalist and research psychologist, who, a
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
"

sources$data_bem_psychic <-
  "https://psycnet.apa.org/record/1994-20286-001"


descriptions$data_bodywellf <-
"#' A subset of data_bodywell_fm, reports only for those participants
#' who identified as female.  Data is Subjective Wellbeing abd Body Satisfaction.
"

descriptions$data_bodywellm <-
"#' A subset of data_bodywell_fm, reports only for those participants
#' who identified as male.  Data is Subjective Wellbeing abd Body Satisfaction.
"

descriptions$data_bodywellfm <-
"#' Survey data from a convenience sample Dominican University students.
#' Reported are measures of Subjective Wellbeing abd Body Satisfaction.
"


taglines <- data.frame(
  filenames = c(
    'Latimier Quiz.omv',
    'Latimier Reread.omv',
    'Latimier Prequiz.omv',
    'College survey 1.omv',
    'Religious belief.omv',
    'College survey 2.omv',
    'Stickgold.omv',
    'Latimier Reread Quiz.omv',
    'Latimier Quiz Prequiz.omv',
    'Latimier Reread Prequiz.omv',
    'Kardas Expt 3.omv',
    'Kardas Expt 4.omv',
    'Clean moral.omv',
    'Gender math IAT.omv',
    'Basol badnews.omv',
    'EffronRaj fakenews.omv',
    'Emotion heartrate.omv',
    'Labels flavor.omv',
    'McCabeMichael brain.omv',
    'McCabeMichael brain2.omv',
    'DamischRCJ.omv',
    'Anchor Estimate ma.omv',
    'Flag Priming ma.omv',
    'Gender math IAT ma.omv',
    'PowerPerformance ma.omv',
    'Thomason 1.omv',
    'Macnamara r ma.omv',
    'Exam Scores.omv',
    'Campus Involvement.omv',
    'Sleep Beauty.omv',
    'BodyWellFM.omv',
    'BodyWellM.omv',
    'BodyWellF.omv',
    'Home Prices.omv',
    'Altruism Happiness.omv',
    'Bem Psychic.omv',
    'SimmonsCredibility.omv',
    'RattanMotivation.omv',
    'Latimier 3Groups.omv',
    'Halagappa.omv',
    'StudyStrategies.omv',
    'ReligionSharing.omv',
    'OrganicMoral.omv',
    'SmithRecall.omv',
    'MeditationBrain.omv',
    'SelfExplain.omv',
    'VideogameAggression.omv'
  ),
  tags = c(
    'Latimier Quiz - Ch03 - Quiz group in Latimier et al. (2019)',
    'Latimier Reread - Ch03 - Reread group in Latimier et al. (2019)',
    'Latimier Prequiz - Ch03 - Prequiz group in Latimier et al. (2019)',
    'College survey 1 - Ch03 - for End-of-Chapter Exercise 3.3',
    'Religious belief - Ch03 - for End-of-Chapter Exercise 3.5',
    'College survey 2 - Ch05 - for End-of-Chapter Exercise 5.4',
    'Stickgold - Ch06 - from Stickgold et al. (2000)',
    'Latimier Reread Quiz - Ch07 - Reread and Quiz groups in Latimier et al. (2019)',
    'Latimier Quiz Prequiz - Ch07 - Quiz and Prequiz groups in Latimier et al. (2019)',
    'Latimier Reread Prequiz - Ch07 - Reread and Prequiz groups in Latimier et al. (2019)',
    "Kardas Expt 3 - Ch07 - from Kardas and O'Brien (2018), Experiment 3",
    "Kardas Expt 4 - Ch07 - from Kardas and O'Brien (2018), Experiment 4",
    'Clean moral - Ch07 - from Schnall et al. (2008), Study 1, and Johnson et al. (2014)',
    'Gender math IAT - Ch07 - Ithaca and SDSU replications of Nosek et al. (2002)',
    'Basol badnews - Ch07 - from Basol et al. (2020)',
    'EffronRaj fakenews - Ch8 - from Effron and Raj (2020)',
    'Emotion heartrate - Ch8 - from Lakens (2013)',
    'Labels flavor - Ch8 - from Floretta-Schiller et al. (2015)',
    'McCabeMichael brain - Ch9 - from Michael et al. (2013)',
    'McCabeMichael brain2 - Ch9 - from Michael et al. (2013)',
    'DamischRCJ - Ch9 - from 6 Damisch studies, and Calin-Jageman and Caldwell (2014)',
    'Anchor Estimate ma - Ch9 - Many Labs replications of Jacowitz and Kahneman (1995)',
    'Flag Priming ma - Ch9 - Many Labs replications of Carter et al. (2011)',
    'Gender math IAT ma - Ch9 - Many Labs replications of Nosek et al. (2002)',
    'PowerPerformance ma - Ch9 - from Burgmer and Englich (2012), and Cusack et al. (2015)',
    'Thomason 1 - Ch11 - from Thomason 1',
    'Macnamara r ma - Ch11 - from Macnamara et al. (2014)',
    'Exam Scores - Ch11 - for End-of-Chapter Exercise 11.2',
    'Campus Involvement - Ch11 - for End-of-Chapter Exercise 11.7',
    'Sleep Beauty - Ch11 - for End-of-Chapter Exercise 11.6',
    'BodyWellFM - Ch12 - Body Satisfaction and Well-being data from Figure 11.1',
    'BodyWellM - Ch12 - Body Satisfaction and Well-being data for males from Figure 11.24 left panel',
    'BodyWellF - Ch12 - Body Satisfaction and Well-being data for females from Figure 11.24 right panel',
    'Home Prices - Ch12 - for End-of-Chapter Exercise 12.2',
    'Altruism Happiness - Ch12 - from Brethel-Haurwitz and Marsh (2014)',
    'Bem Psychic - Ch13 - from Bem and Honorton (1994)',
    'SimmonsCredibility - Ch14 - from Simmons and Nelson (2020)',
    'RattanMotivation - Ch14 - from Rattan et al. (2012)',
    'Latimier 3Groups - Ch14 - 3 groups in Latimier et al. (2019)',
    'Halagappa - Ch14 - from Halagappa et al. (2007)',
    "StudyStrategies - Ch14 - from O'Reilly et al. (1998)",
    'ReligionSharing - Ch14 - for End-of-Chapter Exercise 14.3',
    'OrganicMoral - Ch14 - from Eskine (2013)',
    'SmithRecall - Ch15 - from Smith et al. (2016)',
    "MeditationBrain - Ch15 - from Holzel et al. (2011)",
    'SelfExplain - Ch15 - from McEldoon et al. (2013)',
    'VideogameAggression - Ch15 - from Hilgard (2015)'
  )
)


  #prep comments

  tdoc <- NULL

  badfiles <- c(
    "./data/Campus Involvement.omv"
  )

  for (myfile in list.files(path = "./data", pattern="*.omv", full.names = TRUE)) {

    if (! myfile %in% badfiles) {

      # get the jamovi data
      f <- jmvReadWrite::read_omv(myfile)
      f_attribs <- f
      f[] <- lapply(f, c)

      # make the name of the r data object
      thisfilename <- gsub("./data/", "", myfile)

      dataname <- gsub("./data/", "", myfile)
      dataname <- gsub(".omv", "", dataname)

      friendly_name <- dataname
      if (!is.null(taglines[taglines$filenames == thisfilename, ]$tags)) {
        friendly_name <- taglines[taglines$filenames == thisfilename, ]$tags
      }

      dataname <- gsub(" ", "_", dataname)
      dataname <- tolower(dataname)
      dataname <- paste("data_", dataname, sep = "")

      # Save rda
      if (save_files) {
        to_object <- paste(
          dataname, " <- f", sep = ""
        )
        save_rda <- paste(
          "usethis::use_data(", dataname, ", overwrite = TRUE)", sep = ""
        )
        eval(parse(text = to_object), envir = .GlobalEnv)
        eval(parse(text = save_rda), envir = .GlobalEnv)

      }

      # Build documents
      tdoc <- paste(
        tdoc,
        "#' ", friendly_name, "\n#' \n",
        sep = ""
      )

      if (! is.null(descriptions[[dataname]])) {
        tdoc <- paste(
          tdoc,
          descriptions[[dataname]],
          "#'\n",
          sep = ""
        )
      }

      tdoc <- paste(
        tdoc,
        "#' @format ## `", dataname, "`\n",
        "#' A data frame with ", nrow(f), " rows ",
        "and ", ncol(f), " columns:\n",
        sep = ""
      )


      tdoc <- paste(
        tdoc,
        "#' \\describe{\n",
        sep = ""
      )

      for (mycol in colnames(f_attribs)) {
        if (is.null( attr(f_attribs$State, "jmv-desc") )) {
          coldesc <- class(f_attribs[[mycol]])
        } else {
          coldesc <- paste(
            class(f_attribs[[mycol]]),
            attr(f_attribs$State, "jmv-desc"),
            sep = " - "
          )
        }

        tdoc <- paste(
          tdoc,
          "#'   \\item{", mycol, "}{", coldesc, "}\n",
          sep = ""
        )
      }

      tdoc <- paste(
        tdoc,
        "#' }\n",
        sep = ""
      )

      if (! is.null(sources[[dataname]])) {
        tdoc <- paste(
          tdoc,
          "#' @source <",
          sources[[dataname]],
          ">\n",
          sep = ""
        )
      }


      tdoc <- paste(
        tdoc,
        '"', dataname, '"\n\n',
        sep = ""
      )


    }


  }

  write(tdoc, file = "./R/data.R")

}
