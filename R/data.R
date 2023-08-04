#' Sample data for estimate_mdiff_ind_contrast: Test score and times from
#' Latimier et al (2019).
#'
#' The researchers were interested in how different study approaches might
#' impact learning. Working in France, they created three independent groups,
#' each comprising 95 adults. Participants worked online through seven learning
#' modules about DNA. The Reread group worked through a module then worked
#' through it a second time, before going on to the next module. The Quiz group
#' worked through a module then had to complete a quiz before going on to the
#' next module. The Prequiz group had to work through the quiz before seeing the
#' presentation of a module, then went on to the quiz and presentation of the
#' next module. Participants received feedback and a brief explanation after
#' answering each question in a quiz, and could take as long as they wished to
#' work through each module and quiz. Seven days later participants completed a
#' final test.
#'
#' This data set is suitable for estimating the magnitude of difference in a
#' contrast of independent groups.
#'
#' @format ## `data_latimier` A data frame with 285 rows and 3 columns:
#' \describe{
#'   \item{Group}{A factor indicating membership in the Reread, Quiz, or Prequiz group}
#'   \item{Test}{% correct on the final test}
#'   \item{Time}{Time taken on final test}
#' }
#'
#'
#' @source <https://osf.io/23yad>
#' @source Latimier, A., Riegert, A., Peyre, H., Ly, S. T., Casati, R., & Ramus,
#'   F. (2019). Does pre-testing promote better retention than post-testing?
#'   *Npj Science of Learning*, 4(1), 1–7.
#'   https://doi.org/10.1038/s41539-019-0053-1
"data_latimier"


#' Sample data for estimate_mdiff_two: Transcription scores from Study 1 of
#' Mueller & Oppenheimer (2014)
#'
#' Mueller & Oppenheimer (2014) asked students to take notes on a video lecture
#' using a pen or a laptop.  One dependent measure was % transcription
#' (verbatim) copying, measured as the number of 3-gram matches to the text of
#' the video lecture. This data set is the % transcription for the pen and
#' laptop groups.  It is suitable for estimating differences between two
#' independent groups for a continuous measure.
#'
#' @format ## `data_penlaptop1` A data frame with 65 rows and 2 columns:
#' \describe{
#'   \item{condition}{A factor indicating membership in the pen or laptop group}
#'   \item{transcription}{% of notes transcribed verbatim from lecture}
#' }
#'
#'
#' @source <https://osf.io/23yad>
#' @source Mueller, P. A., & Oppenheimer, D. M. (2014). The Pen Is Mightier Than
#'   the Keyboard: Advantages of Longhand Over Laptop Note Taking.
#' *Psychological Science*, 25(6), 1159–1168.
#'   <https://doi.org/10.1177/0956797614524581>
"data_penlaptop1"


#' Sample data for estimate_mdiff_paired: LSAT scores from Thomason (2014)
#'
#' Thomason et al. (2014) reported 7 studies evaluating a promising approach to
#' teaching critical thinking that combines argument mapping with a form of
#' mastery learning (Mazur, 1997). Studies were conducted in the United States,
#' Canada, and the United Kingdom, and each study used a single group of
#' students. Students were tested on various well-established measures of
#' critical thinking, both before (the Pretest) and after (the Posttest)
#' training. Group sizes ranged from 7 to 39. All the Thomason studies compared
#' the two conditions, Pretest and Posttest, within participants, and therefore
#' used the paired design. This dataset is from the  first Thomason study,
#' Thomason 1.  IT used a group of N = 12 students, whose critical- thinking
#' ability was assessed at Pretest and Posttest using the Logical Reasoning
#' section of the Law School Aptitude Test (LSAT).
#'
#' @format ## `data_thomason1` A data frame with 12 rows and 2 columns:
#' \describe{
#'   \item{pretest}{LSAT score before argument mapping and mastery training}
#'   \item{posttest}{LSAT score after argument mapping and mastery training}
#' }
#'
#'
#' @source Thomason, N. R., Adajian, T., Barnett, A. E., Boucher, S., van der
#'   Brugge, E., Campbell, J., Knorpp, W., Lempert, R., Lengbeyer, L., Mandel,
#'   D. R., Rider, Y., van Gelder, T., & Wilkins, J. (2014). Critical thinking
#'   final report. The University of Melbourne, N66001-12-C-2004.
"data_thomason1"


#' Sample data for estimate_proportion: Ten Ganzfeld studies from Bem and
#' Honorton (1994)
#'
#' Three pilot studies helped refine the procedures, then four studies used
#' novice receivers. Study 5 used 20 students of music, drama, or dance as
#' receivers, in response to suggestions that creative people might be more
#' likely to show telepathy. Studies 6 and 7 used receivers who had participated
#' in an earlier study. The proportion of hits expected by chance is .25
#'
#' @format ## `data_bem_and_honorton_1994` A data frame with 10 rows and 3
#'   columns:
#' \describe{
#'   \item{study}{Name of the study}
#'   \item{hits}{Number of correct responses}
#'   \item{trials}{Number of trials}
#' }
#'
#'
#' @source Bem, D. J., & Honorton, C. (1994). Does Psi exist? Replicable
#'   evidence for an anomalous process of information transfer.
#' *Psychological Bulletin*, 115, 4–-18. doi:10.1037/0033-2909.115.1.4
"data_bem_and_honorton_1994"


#' Sample data for estimate_mdiff_ind_contrast: Bushman (2005)
#'
#' Bushman (2005) reported an investigation of people’s memory for ads that were
#' presented during different types of television show. He wanted to estimate to
#' what extent violent content, compared with neutral content, might lead to
#' reduced memory for ads, and perhaps reduced purchasing intentions for
#' advertised products. He investigated the same questions for shows with sexual
#' content. He chose a three- group design as an efficient way to investigate
#' both types of content. A sample of 252 typical TV viewers were randomly
#' assigned to watch one of three types of show. Some watched a Neutral show
#' (e.g., America’s Funniest Animals), some a show with Violent content (e.g.,
#' Cops), and others a show with Sexual content (e.g., Sex in the City). The
#' viewers watched different shows, but all saw the same 12 ads inserted into
#' the shows. The ads were genuine advertisements, but for little-known
#' products, so most viewers had never seen the ads before. To enhance realism,
#' viewers watched the shows in easy chairs, with snacks and soda available.
#' After the viewing came a surprise memory test for the ads (participants had
#' not been told the purpose of the study). I’ll report data for memory
#' recognition: Participants saw a list of 12 products, with four 4 brand names
#' for each type of product, just one of which had appeared in an ad. For each
#' set of four 4 brands, participants had to choose which one they felt they
#' recognized from the ads they had just seen, so the maximum score was 12. This
#' is a summary data set, providing sample sizes, means, and standard deviations
#' for each group in the study.
#'
#' @format ## `data_bushman_2005` A data frame with 3 rows and 4 columns:
#' \describe{
#'   \item{condition}{Factor indicating group: Neutral, Violent, or Sexual}
#'   \item{n}{Group sample sizes}
#'   \item{m}{Group means}
#'   \item{s}{Group standard deviation}
#' }
#'
#'
#' @source Bushman, B. (2005). Violence and sex in television programs do not
#'   sell products in advertisements. *Psychological Science*, 16, 702-–708.
#'   doi:10.1111/j.1467-9280.2005.01599.x
"data_bushman_2005"


#' Sample data for estimate_mdiff_ind_contrast: Halagappa (2007)
#'
#' Could eating much less delay Alzheimer’s? If so, that would be great news.
#' Halagappa et al. (2007) investigated the possibility by using a mouse model,
#' meaning they used Alzheimer-prone mice, which were genetically predisposed to
#' develop neural degeneration typical of Alzheimer’s. The researchers used six
#' independent groups of mice, three tested in mouse middle age when 10 months
#' old, and three in mouse old age when 17 months. At each age there was a
#' control group of normal mice that ate freely (the NFree10 and NFree17
#' groups), a group of Alzheimer-prone mice that also ate freely (the AFree10
#' and AFree17 groups), and another Alzheimer-prone group restricted to 40% less
#' food than normal (the ADiet10 and ADiet17 groups). Table 14.2 lists the
#' factors that define the groups, the group labels, and their means, M1, M2, …
#' (as we’ll see them displayed in ESCI). I’ll discuss one measure of mouse
#' cognition: the percent time spent near the target of a water maze, with
#' higher values indicating better learning and memory. This is a summary data
#' set, providing sample sizes, means, and standard deviations for each group in
#' the study.
#'
#' @format ## `data_halagappa_et_al_2007` A data frame with 3 rows and 4
#'   columns:
#' \describe{
#'   \item{condition}{Factor indicating group: NFree10, AFree10, ADiet10, NFree17, AFree17, ADiet17}
#'   \item{n}{Group sample sizes}
#'   \item{m}{Group means}
#'   \item{s}{Group standard deviation}
#' }
#'
#'
#' @source Halagappa, V. K. M., Guo, Z., Pearson, M., Matsuoka, Y., Cutler, R.
#'   G., LaFerla, F. M., & Mattson, M. P. (2007). Intermittent fasting and
#'   caloric restriction ameliorate age-related behavioral deficits in the
#'   triple-transgenic mouse model of Alzheimer's disease.
#' *Neurobiology of Disease*, 26, 212–220. doi:10.1016/j.nbd.2006.12.019
"data_halagappa_et_al_2007"
