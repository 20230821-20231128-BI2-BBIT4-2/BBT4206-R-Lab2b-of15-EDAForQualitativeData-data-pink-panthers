# STEP 1. Install and Load the Required Packages ----
# The following packages can be installed and loaded before proceeding to the
# subsequent steps.

## dplyr - For data manipulation ----
if (!is.element("dplyr", installed.packages()[, 1])) {
  install.packages("dplyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("dplyr")

## ggplot2 - For data visualizations using the Grammar for Graphics package ----
if (!is.element("ggplot2", installed.packages()[, 1])) {
  install.packages("ggplot2", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("ggplot2")

## ggrepel - Additional options for the Grammar for Graphics package ----
if (!is.element("ggrepel", installed.packages()[, 1])) {
  install.packages("ggrepel", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("ggrepel")

## ggraph - Additional options for the Grammar for Graphics package ----
if (!is.element("ggraph", installed.packages()[, 1])) {
  install.packages("ggraph", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("ggraph")

## tidytext - For text mining ----
if (!is.element("tidytext", installed.packages()[, 1])) {
  install.packages("tidytext", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("tidytext")

## tidyr - To tidy messy data ----
if (!is.element("tidyr", installed.packages()[, 1])) {
  install.packages("tidyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("tidyr")

## widyr - To widen, process, and re-tidy a dataset ----
if (!is.element("widyr", installed.packages()[, 1])) {
  install.packages("widyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("widyr")

## gridExtra - to arrange multiple grid-based plots on a page ----
if (!is.element("gridExtra", installed.packages()[, 1])) {
  install.packages("gridExtra", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("gridExtra")

## knitr - for dynamic report generation ----
if (!is.element("knitr", installed.packages()[, 1])) {
  install.packages("knitr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("knitr")

## kableExtra - for nicely formatted output tables ----
if (!is.element("kableExtra", installed.packages()[, 1])) {
  install.packages("kableExtra", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("kableExtra")

## formattable -  To create a formattable object ----
# A formattable object is an object to which a formatting function and related
# attributes are attached.
if (!is.element("formattable", installed.packages()[, 1])) {
  install.packages("formattable", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("formattable")

## circlize - To create a cord diagram or visualization ----
# by Gu et al. (2014)
if (!is.element("circlize", installed.packages()[, 1])) {
  install.packages("circlize", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("circlize")

## memery - For creating data analysis related memes ----
# The memery package generates internet memes that optionally include a
# superimposed inset plot and other atypical features, combining the visual
# impact of an attention-grabbing meme with graphic results of data analysis.
if (!is.element("memery", installed.packages()[, 1])) {
  install.packages("memery", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("memery")

## magick - For image processing in R ----
if (!is.element("magick", installed.packages()[, 1])) {
  install.packages("magick", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("magick")

## yarrr - To create a pirate plot ----
if (!is.element("yarrr", installed.packages()[, 1])) {
  install.packages("yarrr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("yarrr")

## radarchart - To create interactive radar charts using ChartJS ----
if (!is.element("radarchart", installed.packages()[, 1])) {
  install.packages("radarchart", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("radarchart")

## igraph - To create ngram network diagrams ----
if (!is.element("igraph", installed.packages()[, 1])) {
  install.packages("igraph", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("igraph")

## wordcloud2 - For creating wordcloud by using 'wordcloud2.JS ----
if (!is.element("wordcloud2", installed.packages()[, 1])) {
  install.packages("wordcloud2", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("wordcloud2")

## readr - Load datasets from CSV files ----
if (!is.element("readr", installed.packages()[, 1])) {
  install.packages("readr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("readr")

# STEP 2. Customize the Visualizations, Tables, and Colour Scheme ----
# The following defines a blue-grey colour scheme for the visualizations:
## shades of blue and shades of grey
blue_grey_colours_11 <- c("#27408E", "#304FAF", "#536CB5", "#6981c7", "#8da0db",
                          "#dde5ec", "#c8c9ca", "#B9BCC2", "#A7AAAF", "#888A8E",
                          "#636569")

blue_grey_colours_6 <- c("#27408E", "#304FAF", "#536CB5",
                         "#B9BCC2", "#A7AAAF", "#888A8E")

blue_grey_colours_4 <- c("#27408E", "#536CB5",
                         "#B9BCC2", "#888A8E")

blue_grey_colours_3 <- c("#6981c7", "#304FAF", "#888A8E")

blue_grey_colours_2 <- c("#27408E",
                         "#888A8E")

blue_grey_colours_1 <- c("#6981c7")

# Custom theme for visualizations
blue_grey_theme <- function() {
  theme(
    axis.ticks = element_line(
      linewidth = 1, linetype = "dashed",
      lineend = NULL, color = "#dfdede",
      arrow = NULL, inherit.blank = FALSE),
    axis.text = element_text(
      face = "bold", color = "#3f3f41",
      size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold", color = "#3f3f41",
                              size = 14, hjust = 0.5),
    plot.title = element_text(face = "bold", color = "#3f3f41",
                              size = 16, hjust = 0.5),
    panel.grid = element_line(
      linewidth = 0.1, linetype = "dashed",
      lineend = NULL, color = "#dfdede",
      arrow = NULL, inherit.blank = FALSE),
    panel.background = element_rect(fill = "#f3eeee"),
    legend.title = element_text(face = "plain", color = "#3f3f41",
                                size = 12, hjust = 0),
    legend.position = "right"
  )
}

# Customize the text tables for consistency using HTML formatting
kable_theme <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}


# STEP 3. Load the Dataset ----
student_performance_dataset <-
  readr::read_csv(
    "data/20230412-20230719-BI1-BBIT4-1-StudentPerformanceDataset.CSV", # nolint
    col_types =
      readr::cols(
        class_group =
          readr::col_factor(levels = c("A", "B", "C")),
        gender = readr::col_factor(levels = c("1", "0")),
        YOB = readr::col_date(format = "%Y"),
        regret_choosing_bi =
          readr::col_factor(levels = c("1", "0")),
        drop_bi_now =
          readr::col_factor(levels = c("1", "0")),
        motivator =
          readr::col_factor(levels = c("1", "0")),
        read_content_before_lecture =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        anticipate_test_questions =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        answer_rhetorical_questions =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        find_terms_I_do_not_know =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        copy_new_terms_in_reading_notebook =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        take_quizzes_and_use_results =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        reorganise_course_outline =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        write_down_important_points =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        space_out_revision =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        studying_in_study_group =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        schedule_appointments =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        goal_oriented =
          readr::col_factor(levels =
                              c("1", "0")),
        spaced_repetition =
          readr::col_factor(levels =
                              c("1", "2", "3", "4")),
        testing_and_active_recall =
          readr::col_factor(levels =
                              c("1", "2", "3", "4")),
        interleaving =
          readr::col_factor(levels =
                              c("1", "2", "3", "4")),
        categorizing =
          readr::col_factor(levels =
                              c("1", "2", "3", "4")),
        retrospective_timetable =
          readr::col_factor(levels =
                              c("1", "2", "3", "4")),
        cornell_notes =
          readr::col_factor(levels =
                              c("1", "2", "3", "4")),
        sq3r = readr::col_factor(levels =
                                   c("1", "2", "3", "4")),
        commute = readr::col_factor(levels =
                                      c("1", "2",
                                        "3", "4")),
        study_time = readr::col_factor(levels =
                                         c("1", "2",
                                           "3", "4")),
        repeats_since_Y1 = readr::col_integer(),
        paid_tuition = readr::col_factor(levels =
                                           c("0", "1")),
        free_tuition = readr::col_factor(levels =
                                           c("0", "1")),
        extra_curricular = readr::col_factor(levels =
                                               c("0", "1")),
        sports_extra_curricular =
          readr::col_factor(levels = c("0", "1")),
        exercise_per_week = readr::col_factor(levels =
                                                c("0", "1",
                                                  "2",
                                                  "3")),
        meditate = readr::col_factor(levels =
                                       c("0", "1",
                                         "2", "3")),
        pray = readr::col_factor(levels =
                                   c("0", "1",
                                     "2", "3")),
        internet = readr::col_factor(levels =
                                       c("0", "1")),
        laptop = readr::col_factor(levels = c("0", "1")),
        family_relationships =
          readr::col_factor(levels =
                              c("1", "2", "3", "4", "5")),
        friendships = readr::col_factor(levels =
                                          c("1", "2", "3",
                                            "4", "5")),
        romantic_relationships =
          readr::col_factor(levels =
                              c("0", "1", "2", "3", "4")),
        spiritual_wellnes =
          readr::col_factor(levels = c("1", "2", "3",
                                       "4", "5")),
        financial_wellness =
          readr::col_factor(levels = c("1", "2", "3",
                                       "4", "5")),
        health = readr::col_factor(levels = c("1", "2",
                                              "3", "4",
                                              "5")),
        day_out = readr::col_factor(levels = c("0", "1",
                                               "2", "3")),
        night_out = readr::col_factor(levels = c("0",
                                                 "1", "2",
                                                 "3")),
        alcohol_or_narcotics =
          readr::col_factor(levels = c("0", "1", "2", "3")),
        mentor = readr::col_factor(levels = c("0", "1")),
        mentor_meetings = readr::col_factor(levels =
                                              c("0", "1",
                                                "2", "3")),
        `Attendance Waiver Granted: 1 = Yes, 0 = No` =
          readr::col_factor(levels = c("0", "1")),
        GRADE = readr::col_factor(levels =
                                    c("A", "B", "C", "D",
                                      "E"))),
    locale = readr::locale())

View(student_performance_dataset)

# Dimensions
dim(student_performance_dataset)

# Data Types
sapply(student_performance_dataset, class)
glimpse(student_performance_dataset)

# Summary of each variable
summary(student_performance_dataset)

# STEP 4. Create a subset of the data using the "dplyr" package ----

evaluation_per_group_per_gender <- student_performance_dataset %>% # nolint
  mutate(`Student's Gender` =
           ifelse(gender == 1, "Male", "Female")) %>%
  select(class_group, gender,
         `Student's Gender`, `Average Course Evaluation Rating`) %>%
  filter(!is.na(`Average Course Evaluation Rating`)) %>%
  group_by(class_group, `Student's Gender`) %>%
  summarise(average_evaluation_rating =
              mean(`Average Course Evaluation Rating`)) %>%
  arrange(desc(average_evaluation_rating), .by_group = TRUE)

# Plain tabular output
View(evaluation_per_group_per_gender)

# Decorated tabular output
library(kableExtra)
library(knitr)
library(dplyr)

remotes::install_github("renkun-ken/formattable")

evaluation_per_group_per_gender %>%
  rename(`Class Group` = class_group) %>%
  rename(`Average Course Evaluation Rating` = average_evaluation_rating) %>%
  select(`Class Group`, `Student's Gender`,
         `Average Course Evaluation Rating`) %>%
  mutate(`Average Course Evaluation Rating` =
           color_tile("#B9BCC2", "#536CB5")
         (`Average Course Evaluation Rating`)) %>%
  kable("html", escape = FALSE, align = "c",
        caption = "Course Evaluation Rating per Group and per Gender") %>%
  kable_styling(bootstrap_options =
                  c("striped", "condensed", "bordered"),
                full_width = FALSE)

# Decorated visual bar chart
evaluation_per_group_per_gender %>%
  ggplot() +
  geom_bar(aes(x = class_group, y = average_evaluation_rating,
               fill = `Student's Gender`),
           stat = "identity", position = "dodge") +
  expand_limits(y = 0) +
  blue_grey_theme() +
  scale_fill_manual(values = blue_grey_colours_2) +
  ggtitle("Course Evaluation Rating per Group and per Gender") +
  labs(x = "Class Group", y = "Average Rating")

# STEP 5. Data Cleansing for Qualitative Data ----
## Contractions ----
expand_contractions <- function(doc) {
  doc <- gsub("I'm", "I am", doc, ignore.case = TRUE)
  doc <- gsub("you're", "you are", doc, ignore.case = TRUE)
  doc <- gsub("he's", "he is", doc, ignore.case = TRUE)
  doc <- gsub("she's", "she is", doc, ignore.case = TRUE)
  doc <- gsub("it's", "it is", doc, ignore.case = TRUE)
  doc <- gsub("we're", "we are", doc, ignore.case = TRUE)
  doc <- gsub("they're", "they are", doc, ignore.case = TRUE)
  doc <- gsub("I'll", "I will", doc, ignore.case = TRUE)
  doc <- gsub("you'll", "you will", doc, ignore.case = TRUE)
  doc <- gsub("he'll", "he will", doc, ignore.case = TRUE)
  doc <- gsub("she'll", "she will", doc, ignore.case = TRUE)
  doc <- gsub("it'll", "it will", doc, ignore.case = TRUE)
  doc <- gsub("we'll", "we will", doc, ignore.case = TRUE)
  doc <- gsub("they'll", "they will", doc, ignore.case = TRUE)
  doc <- gsub("won't", "will not", doc, ignore.case = TRUE)
  doc <- gsub("can't", "cannot", doc, ignore.case = TRUE)
  doc <- gsub("n't", " not", doc, ignore.case = TRUE)
  return(doc)
}

# Evaluation likes and wishes
evaluation_likes_and_wishes <- student_performance_dataset %>%
  mutate(`Student's Gender` =
           ifelse(gender == 1, "Male", "Female")) %>%
  rename(`Class Group` = class_group) %>%
  rename(Likes = `D - 1. Write two things you like about the teaching and learning in this unit so far.`) %>% # nolint
  rename(Wishes = `D - 2. Write at least one recommendation to improve the teaching and learning in this unit (for the remaining weeks in the semester)`) %>% # nolint
  select(`Class Group`,
         `Student's Gender`, `Average Course Evaluation Rating`,
         Likes, Wishes) %>%
  filter(!is.na(`Average Course Evaluation Rating`)) %>%
  arrange(`Class Group`)

#Before expanding contractions
View(evaluation_likes_and_wishes)

evaluation_likes_and_wishes$Likes <- sapply(evaluation_likes_and_wishes$Likes, expand_contractions) # nolint
evaluation_likes_and_wishes$Wishes <- sapply(evaluation_likes_and_wishes$Wishes, expand_contractions) # nolint

# After expanding contractions
View(evaluation_likes_and_wishes)

remove_special_characters <- function(doc) {
  gsub("[^a-zA-Z0-9 ]", "", doc, ignore.case = TRUE)
}

#Before removing special characters
View(evaluation_likes_and_wishes)

evaluation_likes_and_wishes$Likes <- sapply(evaluation_likes_and_wishes$Likes, remove_special_characters) # nolint
evaluation_likes_and_wishes$Wishes <- sapply(evaluation_likes_and_wishes$Wishes, remove_special_characters) # nolint


# Convert everything to lower case (to standardize the text)
evaluation_likes_and_wishes$Likes <- sapply(evaluation_likes_and_wishes$Likes, tolower) # nolint
evaluation_likes_and_wishes$Wishes <- sapply(evaluation_likes_and_wishes$Wishes, tolower) # nolint

# After removing special characters and converting everything to lower case
View(evaluation_likes_and_wishes)

# the file as a CSV
write.csv(evaluation_likes_and_wishes,
          file = "data/evaluation_likes_and_wishes.csv",
          row.names = FALSE)

## Stemming/Lemmatization ----

#install package koRpus
install.packages("koRpus.lang.en")

#Install textstem tool
if (!require("pacman")) install.packages("pacman")
pacman::p_load(textstem, dplyr)

#install Lexicon
if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("trinker/lexicon")

#Load data
data(student_performance_dataset)

# evaluation_likes_and_wishes_stemmed <- stem_words(evaluation_likes_and_wishes)
# View(evaluation_likes_and_wishes_stemmed)
# 
# evaluation_likes_and_wishes_lemmatized <- lemmatize_words(evaluation_likes_and_wishes)
# View(evaluation_likes_and_wishes_lemmatized)


## Tokenization & Stopwords Removal

install.packages("tidyverse")
install.packages("dplyr", dependencies = TRUE)
library(tidyverse)
library(dplyr)
require(tm)
require(dplyr)
install.packages("tidytext")
library(tidytext)

stopwords()
sort(stopwords())
#removed_stopwords_tokenized_data = removeWords(tokenized_data, stopwords())
evaluation_likes_filtered_tokenized <- evaluation_likes_and_wishes %>% # nolint
  unnest_tokens(word, Likes) %>%
  # do not join where the word is in the list of stopwords
  anti_join(stop_words, by = c("word")) %>%
  distinct() %>%
  filter(nchar(word) > 3) %>%
  rename(`Likes` = word) %>%
  select(-Wishes, -"Class Group", -"Student's Gender" , -"Average Course Evaluation Rating")


evaluation_wishes_filtered_tokenized <- evaluation_likes_and_wishes %>% # nolint
  unnest_tokens(word, Wishes) %>%
  # do not join where the word is in the list of stopwords
  anti_join(stop_words, by = c("word")) %>%
  distinct() %>%
  filter(nchar(word) > 3) %>%
  rename(`Wishes ` = word) %>%
  select(-Likes, -"Class Group", -"Student's Gender" , -"Average Course Evaluation Rating")

write.csv(evaluation_likes_filtered_tokenized,
          file = "data/evaluation_likes_filtered_tokenized.csv",
          row.names = FALSE)

write.csv(evaluation_wishes_filtered_tokenized,
          file = "data/evaluation_wishes_filtered_tokenized.csv",
          row.names = FALSE)

View(evaluation_likes_filtered_tokenized)
View(evaluation_wishes_filtered_tokenized)


# STEP 6. Word Count ----
## Evaluation Likes ----
### Word count per gender ----
word_count_per_gender_likes <- evaluation_likes_filtered_tokenized %>%
  group_by(`Student's Gender`) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))

word_count_per_gender_likes %>%
  mutate(num_words = color_bar("lightblue")(num_words)) %>%
  rename(`Number of Words` = num_words) %>%
  kable("html", escape = FALSE, align = "c",
        caption = "Number of Significant Words in Evaluation Likes 
                   per Gender: Minus contractions, special characters, 
                   stopwords, short words, and censored words.") %>%
  kable_styling(bootstrap_options =
                  c("striped", "condensed", "bordered"),
                full_width = FALSE)

### Word count per group ----
word_count_per_group <- evaluation_likes_filtered_tokenized %>%
  group_by(`Class Group`) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))

word_count_per_group %>%
  mutate(num_words = color_bar("lightblue")(num_words)) %>%
  rename(`Number of Words` = num_words) %>%
  kable("html", escape = FALSE, align = "c",
        caption = "Number of Significant Words in Evaluation Likes 
                   per Group: Minus contractions, special characters, 
                   stopwords, short words, and censored words.") %>%
  kable_styling(bootstrap_options =
                  c("striped", "condensed", "bordered"),
                full_width = FALSE)

## Evaluation Wishes ----
### Word count per gender ----
word_count_per_gender_wishes <- evaluation_wishes_filtered_tokenized %>%
  group_by(`Student's Gender`) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))

word_count_per_gender_wishes %>%
  mutate(num_words = color_bar("lightblue")(num_words)) %>%
  rename(`Number of Words` = num_words) %>%
  kable("html", escape = FALSE, align = "c",
        caption = "Number of Significant Words in Evaluation Wishes 
                   per Gender: Minus contractions, special characters, 
                   stopwords, short words, and censored words.") %>%
  kable_styling(bootstrap_options =
                  c("striped", "condensed", "bordered"),
                full_width = FALSE)

### Word count per group ----
word_count_per_group_wishes <- evaluation_wishes_filtered_tokenized %>%
  group_by(`Class Group`) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))

word_count_per_group_wishes %>%
  mutate(num_words = color_bar("lightblue")(num_words)) %>%
  rename(`Number of Words` = num_words) %>%
  kable("html", escape = FALSE, align = "c",
        caption = "Number of Significant Words in Evaluation Wishes 
                   per Group: Minus contractions, special characters, 
                   stopwords, short words, and censored words.") %>%
  kable_styling(bootstrap_options =
                  c("striped", "condensed", "bordered"),
                full_width = FALSE)
# STEP 7. Top Words ----
## Evaluation Likes ----
### Top 10 words for female students ----
evaluation_likes_filtered_tokenized %>%
  select(`Class Group`, `Student's Gender`,
         `Average Course Evaluation Rating`, `Likes (tokenized)`) %>%
  filter(`Student's Gender` == "Female") %>%
  count(`Likes (tokenized)`, sort = TRUE) %>%
  top_n(9) %>%
  mutate(`Likes (tokenized)` = reorder(`Likes (tokenized)`, n)) %>%
  ggplot() +
  geom_col(aes(`Likes (tokenized)`, n), fill = blue_grey_colours_1) +
  blue_grey_theme() +
  xlab("Word in Course Evaluation") +
  ylab("Number of Times Used (Term Frequency)") +
  ggtitle("Most Frequently Used Words in Course Evaluation Likes for Female
          Students") +
  coord_flip()

### Top 10 words for male students ----
evaluation_likes_filtered_tokenized %>%
  select(`Class Group`, `Student's Gender`,
         `Average Course Evaluation Rating`, `Likes (tokenized)`) %>%
  filter(`Student's Gender` == "Male") %>%
  count(`Likes (tokenized)`, sort = TRUE) %>%
  top_n(9) %>%
  mutate(`Likes (tokenized)` = reorder(`Likes (tokenized)`, n)) %>%
  ggplot() +
  geom_col(aes(`Likes (tokenized)`, n), fill = blue_grey_colours_1) +
  blue_grey_theme() +
  xlab("Word in Course Evaluation") +
  ylab("Number of Times Used (Term Frequency)") +
  ggtitle("Most Frequently Used Words in Course Evaluation Likes for Male
          Students") +
  coord_flip()

### Top 10 words per gender ----
popular_words <- evaluation_likes_filtered_tokenized %>%
  group_by(`Student's Gender`) %>%
  count(`Likes (tokenized)`, `Student's Gender`, sort = TRUE) %>%
  slice(seq_len(10)) %>%
  ungroup() %>%
  arrange(`Student's Gender`, n) %>%
  mutate(row = row_number())

popular_words %>%
  ggplot(aes(row, n, fill = `Student's Gender`)) +
  geom_col(fill = blue_grey_colours_1) +
  blue_grey_theme() +
  labs(x = "Word in Course Evaluation",
       y = "Number of Times Used (Term Frequency)") +
  ggtitle("Most Frequently Used Words in Course Evaluation Likes per Gender") +
  facet_wrap(~`Student's Gender`, scales = "free") +
  scale_x_continuous(
    breaks = popular_words$row,
    labels = popular_words$`Likes (tokenized)`) +
  coord_flip()

### Top words for Group A students ----
evaluation_likes_filtered_tokenized %>%
  select(`Class Group`, `Student's Gender`,
         `Average Course Evaluation Rating`, `Likes (tokenized)`) %>%
  filter(`Class Group` == "A") %>%
  count(`Likes (tokenized)`, sort = TRUE) %>%
  top_n(9) %>%
  mutate(`Likes (tokenized)` = reorder(`Likes (tokenized)`, n)) %>%
  ggplot() +
  geom_col(aes(`Likes (tokenized)`, n), fill = blue_grey_colours_1) +
  blue_grey_theme() +
  xlab("Word in Course Evaluation") +
  ylab("Number of Times Used (Term Frequency)") +
  ggtitle("Most Frequently Used Words in Course Evaluation Likes for Group A
          Students") +
  coord_flip()

### Top words for Group B students ----
evaluation_likes_filtered_tokenized %>%
  select(`Class Group`, `Student's Gender`,
         `Average Course Evaluation Rating`, `Likes (tokenized)`) %>%
  filter(`Class Group` == "B") %>%
  count(`Likes (tokenized)`, sort = TRUE) %>%
  top_n(9) %>%
  mutate(`Likes (tokenized)` = reorder(`Likes (tokenized)`, n)) %>%
  ggplot() +
  geom_col(aes(`Likes (tokenized)`, n), fill = blue_grey_colours_1) +
  blue_grey_theme() +
  xlab("Word in Course Evaluation") +
  ylab("Number of Times Used (Term Frequency)") +
  ggtitle("Most Frequently Used Words in Course Evaluation Likes for Group B
          Students") +
  coord_flip()

### Top words for Group C students ----
evaluation_likes_filtered_tokenized %>%
  select(`Class Group`, `Student's Gender`,
         `Average Course Evaluation Rating`, `Likes (tokenized)`) %>%
  filter(`Class Group` == "C") %>%
  count(`Likes (tokenized)`, sort = TRUE) %>%
  top_n(9) %>%
  mutate(`Likes (tokenized)` = reorder(`Likes (tokenized)`, n)) %>%
  ggplot() +
  geom_col(aes(`Likes (tokenized)`, n), fill = blue_grey_colours_1) +
  blue_grey_theme() +
  xlab("Word in Course Evaluation") +
  ylab("Number of Times Used (Term Frequency)") +
  ggtitle("Most Frequently Used Words in Course Evaluation Likes for Group C
          Students") +
  coord_flip()

### Top 10 words per group ----
popular_words <- evaluation_likes_filtered_tokenized %>%
  group_by(`Class Group`) %>%
  count(`Likes (tokenized)`, `Class Group`, sort = TRUE) %>%
  slice(seq_len(10)) %>%
  ungroup() %>%
  arrange(`Class Group`, n) %>%
  mutate(row = row_number())

popular_words %>%
  ggplot(aes(row, n, fill = `Class Group`)) +
  geom_col(fill = blue_grey_colours_1) +
  blue_grey_theme() +
  labs(x = "Word in Course Evaluation", y = "Number of Times Used") +
  ggtitle("Most Frequently Used Words in Course Evaluation Likes per 
          Class Group") +
  facet_wrap(~`Class Group`, scales = "free") +
  scale_x_continuous(
    breaks = popular_words$row,
    labels = popular_words$`Likes (tokenized)`) +
  coord_flip()

## Evaluation Wishes ----
### Top 10 words for female students ----
evaluation_wishes_filtered_tokenized%>%
  select(`Class Group`, `Student's Gender`,
         `Average Course Evaluation Rating`, `Wishes (tokenized)`) %>%
  filter(`Student's Gender` == "Female") %>%
  count(`Wishes (tokenized)`, sort = TRUE) %>%
  top_n(9) %>%
  mutate(`Wishes (tokenized)` = reorder(`Wishes (tokenized)`, n)) %>%
  ggplot() +
  geom_col(aes(`Wishes (tokenized)`, n), fill = blue_grey_colours_1) +
  blue_grey_theme() +
  xlab("Word in Course Evaluation") +
  ylab("Number of Times Used (Term Frequency)") +
  ggtitle("Most Frequently Used Words in Course Evaluation Wishes for Female
          Students") +
  coord_flip()

### Top 10 words for male students ----
evaluation_wishes_filtered_tokenized %>%
  select(`Class Group`, `Student's Gender`,
         `Average Course Evaluation Rating`, `Wishes (tokenized)`) %>%
  filter(`Student's Gender` == "Male") %>%
  count(`Wishes (tokenized)`, sort = TRUE) %>%
  top_n(9) %>%
  mutate(`Wishes (tokenized)` = reorder(`Wishes (tokenized)`, n)) %>%
  ggplot() +
  geom_col(aes(`Wishes (tokenized)`, n), fill = blue_grey_colours_1) +
  blue_grey_theme() +
  xlab("Word in Course Evaluation") +
  ylab("Number of Times Used (Term Frequency)") +
  ggtitle("Most Frequently Used Words in Course Evaluation Wishes for Male
          Students") +
  coord_flip()

### Top 10 words per gender ----
popular_words <- evaluation_wishes_filtered_tokenized %>%
  group_by(`Student's Gender`) %>%
  count(`Wishes (tokenized)`, `Student's Gender`, sort = TRUE) %>%
  slice(seq_len(10)) %>%
  ungroup() %>%
  arrange(`Student's Gender`, n) %>%
  mutate(row = row_number())

popular_words %>%
  ggplot(aes(row, n, fill = `Student's Gender`)) +
  geom_col(fill = blue_grey_colours_1) +
  blue_grey_theme() +
  labs(x = "Word in Course Evaluation", y = "Number of Times Used") +
  ggtitle("Most Frequently Used Words in Course Evaluation Wishes per Gender") +
  facet_wrap(~`Student's Gender`, scales = "free") +
  scale_x_continuous(
    breaks = popular_words$row,
    labels = popular_words$`Wishes (tokenized)`) +
  coord_flip()

### Top words for Group A students ----
evaluation_wishes_filtered_tokenized %>%
  select(`Class Group`, `Student's Gender`,
         `Average Course Evaluation Rating`, `Wishes (tokenized)`) %>%
  filter(`Class Group` == "A") %>%
  count(`Wishes (tokenized)`, sort = TRUE) %>%
  top_n(9) %>%
  mutate(`Wishes (tokenized)` = reorder(`Wishes (tokenized)`, n)) %>%
  ggplot() +
  geom_col(aes(`Wishes (tokenized)`, n), fill = blue_grey_colours_1) +
  blue_grey_theme() +
  xlab("Word in Course Evaluation") +
  ylab("Number of Times Used (Term Frequency)") +
  ggtitle("Most Frequently Used Words in Course Evaluation Wishes for Group A
          Students") +
  coord_flip()

### Top words for Group B students ----
evaluation_wishes_filtered_tokenized %>%
  select(`Class Group`, `Student's Gender`,
         `Average Course Evaluation Rating`, `Wishes (tokenized)`) %>%
  filter(`Class Group` == "B") %>%
  count(`Wishes (tokenized)`, sort = TRUE) %>%
  top_n(9) %>%
  mutate(`Wishes (tokenized)` = reorder(`Wishes (tokenized)`, n)) %>%
  ggplot() +
  geom_col(aes(`Wishes (tokenized)`, n), fill = blue_grey_colours_1) +
  blue_grey_theme() +
  xlab("Word in Course Evaluation") +
  ylab("Number of Times Used (Term Frequency)") +
  ggtitle("Most Frequently Used Words in Course Evaluation Wishes for Group B
          Students") +
  coord_flip()

### Top words for Group C students ----
evaluation_wishes_filtered_tokenized %>%
  select(`Class Group`, `Student's Gender`,
         `Average Course Evaluation Rating`, `Wishes (tokenized)`) %>%
  filter(`Class Group` == "C") %>%
  count(`Wishes (tokenized)`, sort = TRUE) %>%
  top_n(9) %>%
  mutate(`Wishes (tokenized)` = reorder(`Wishes (tokenized)`, n)) %>%
  ggplot() +
  geom_col(aes(`Wishes (tokenized)`, n), fill = blue_grey_colours_1) +
  blue_grey_theme() +
  xlab("Word in Course Evaluation") +
  ylab("Number of Times Used (Term Frequency)") +
  ggtitle("Most Frequently Used Words in Course Evaluation Wishes for Group C
          Students") +
  coord_flip()

### Top 10 words per group ----
popular_words <- evaluation_wishes_filtered_tokenized %>%
  group_by(`Class Group`) %>%
  count(`Wishes (tokenized)`, `Class Group`, sort = TRUE) %>%
  slice(seq_len(10)) %>%
  ungroup() %>%
  arrange(`Class Group`, n) %>%
  mutate(row = row_number())

popular_words %>%
  ggplot(aes(row, n, fill = `Class Group`)) +
  geom_col(fill = blue_grey_colours_1) +
  blue_grey_theme() +
  labs(x = "Word in Course Evaluation",
       y = "Number of Times Used (Term Frequency)") +
  ggtitle("Most Frequently Used Words in Course Evaluation Wishes per 
          Class Group") +
  facet_wrap(~`Class Group`, scales = "free") +
  scale_x_continuous(
    breaks = popular_words$row,
    labels = popular_words$`Wishes (tokenized)`) +
  coord_flip()

# STEP 8. Word Cloud ----
## Evaluation Likes ----
evaluation_likes_filtered_cloud <- evaluation_likes_filtered_tokenized %>% # nolint
  count(`Likes (tokenized)`, sort = TRUE)

wordcloud2(evaluation_likes_filtered_cloud, size = .5)

## Evaluation Wishes ----
evaluation_wishes_filtered_cloud <- evaluation_wishes_filtered_tokenized %>% # nolint
  count(`Wishes (tokenized)`, sort = TRUE)

wordcloud2(evaluation_wishes_filtered_cloud, size = .5)

# STEP 9. Term Frequency - Inverse Document Frequency (TF-IDF) ----
## Evaluation Likes ----
### TF-IDF Score per Gender ----
popular_tfidf_words_gender_likes <- evaluation_likes_filtered_tokenized %>% # nolint
  unnest_tokens(word, `Likes (tokenized)`) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3) %>%
  rename(`Likes (tokenized)` = word) %>%
  select(`Class Group`, `Student's Gender`,
         `Average Course Evaluation Rating`, `Likes (tokenized)`) %>%
  count(`Student's Gender`, `Likes (tokenized)`, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(`Likes (tokenized)`, `Student's Gender`, n)

head(popular_tfidf_words_gender_likes)

top_popular_tfidf_words <- popular_tfidf_words_gender_likes %>%
  arrange(desc(tf_idf)) %>%
  mutate(`Likes (tokenized)` =
           factor(`Likes (tokenized)`,
                  levels = rev(unique(`Likes (tokenized)`)))) %>%
  group_by(`Student's Gender`) %>%
  slice(seq_len(10)) %>%
  ungroup() %>%
  arrange(`Student's Gender`, tf_idf) %>%
  mutate(row = row_number())

top_popular_tfidf_words %>%
  ggplot(aes(x = row, tf_idf, fill = `Student's Gender`)) +
  geom_col(fill = blue_grey_colours_1) +
  blue_grey_theme() +
  labs(x = "Word in Course Evaluation", y = "TF-IDF Score") +
  ggtitle("Important Words using TF-IDF by Chart Level") +
  ggtitle("Most Important Words by TF-IDF Score in Course Evaluation Likes per 
      Class Group") +
  facet_wrap(~`Student's Gender`, scales = "free") +
  scale_x_continuous(
                     breaks = top_popular_tfidf_words$row,
                     labels = top_popular_tfidf_words$`Likes (tokenized)`) +
  coord_flip()

### TF-IDF Score per Group ----
popular_tfidf_words_likes <- evaluation_likes_filtered_tokenized %>% # nolint
  unnest_tokens(word, `Likes (tokenized)`) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3) %>%
  rename(`Likes (tokenized)` = word) %>%
  select(`Class Group`, `Student's Gender`,
         `Average Course Evaluation Rating`, `Likes (tokenized)`) %>%
  count(`Class Group`, `Likes (tokenized)`, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(`Likes (tokenized)`, `Class Group`, n)

head(popular_tfidf_words_likes)

top_popular_tfidf_words <- popular_tfidf_words_likes %>%
  arrange(desc(tf_idf)) %>%
  mutate(`Likes (tokenized)` =
           factor(`Likes (tokenized)`,
                  levels = rev(unique(`Likes (tokenized)`)))) %>%
  group_by(`Class Group`) %>%
  slice(seq_len(10)) %>%
  ungroup() %>%
  arrange(`Class Group`, tf_idf) %>%
  mutate(row = row_number())

top_popular_tfidf_words %>%
  ggplot(aes(x = row, tf_idf, fill = `Class Group`)) +
  geom_col(fill = blue_grey_colours_1) +
  blue_grey_theme() +
  labs(x = "Word in Course Evaluation", y = "TF-IDF Score") +
  ggtitle("Important Words using TF-IDF by Chart Level") +
  ggtitle("Most Important Words by TF-IDF Score in Course Evaluation Likes per 
      Class Group") +
  facet_wrap(~`Class Group`, scales = "free") +
  scale_x_continuous(
                     breaks = top_popular_tfidf_words$row,
                     labels = top_popular_tfidf_words$`Likes (tokenized)`) +
  coord_flip()

## Evaluation Wishes ----
### TF-IDF Score per Gender ----
popular_tfidf_words_gender_wishes <- evaluation_wishes_filtered_tokenized %>% # nolint
  unnest_tokens(word, `Wishes (tokenized)`) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3) %>%
  rename(`Wishes (tokenized)` = word) %>%
  select(`Class Group`, `Student's Gender`,
         `Average Course Evaluation Rating`, `Wishes (tokenized)`) %>%
  count(`Student's Gender`, `Wishes (tokenized)`, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(`Wishes (tokenized)`, `Student's Gender`, n)

head(popular_tfidf_words_gender_wishes)

top_popular_tfidf_words <- popular_tfidf_words_gender_wishes %>%
  arrange(desc(tf_idf)) %>%
  mutate(`Wishes (tokenized)` =
           factor(`Wishes (tokenized)`,
                  levels = rev(unique(`Wishes (tokenized)`)))) %>%
  group_by(`Student's Gender`) %>%
  slice(seq_len(10)) %>%
  ungroup() %>%
  arrange(`Student's Gender`, tf_idf) %>%
  mutate(row = row_number())

top_popular_tfidf_words %>%
  ggplot(aes(x = row, tf_idf, fill = `Student's Gender`)) +
  geom_col(fill = blue_grey_colours_1) +
  blue_grey_theme() +
  labs(x = "Word in Course Evaluation", y = "TF-IDF Score") +
  ggtitle("Important Words using TF-IDF by Chart Level") +
  ggtitle("Most Important Words by TF-IDF Score in Course Evaluation Wishes per 
      Class Group") +
  facet_wrap(~`Student's Gender`, scales = "free") +
  scale_x_continuous(
                     breaks = top_popular_tfidf_words$row,
                     labels = top_popular_tfidf_words$`Wishes (tokenized)`) +
  coord_flip()

### TF-IDF Score per Group ----
popular_tfidf_words_likes <- evaluation_wishes_filtered_tokenized %>% # nolint
  unnest_tokens(word, `Wishes (tokenized)`) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3) %>%
  rename(`Wishes (tokenized)` = word) %>%
  select(`Class Group`, `Student's Gender`,
         `Average Course Evaluation Rating`, `Wishes (tokenized)`) %>%
  count(`Class Group`, `Wishes (tokenized)`, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(`Wishes (tokenized)`, `Class Group`, n)

head(popular_tfidf_words_likes)

top_popular_tfidf_words <- popular_tfidf_words_likes %>%
  arrange(desc(tf_idf)) %>%
  mutate(`Wishes (tokenized)` =
           factor(`Wishes (tokenized)`,
                  levels = rev(unique(`Wishes (tokenized)`)))) %>%
  group_by(`Class Group`) %>%
  slice(seq_len(10)) %>%
  ungroup() %>%
  arrange(`Class Group`, tf_idf) %>%
  mutate(row = row_number())

top_popular_tfidf_words %>%
  ggplot(aes(x = row, tf_idf, fill = `Class Group`)) +
  geom_col(fill = blue_grey_colours_1) +
  blue_grey_theme() +
  labs(x = "Word in Course Evaluation", y = "TF-IDF Score") +
  ggtitle("Important Words using TF-IDF by Chart Level") +
  ggtitle("Most Important Words by TF-IDF Score in Course Evaluation Wishes per 
      Class Group") +
  facet_wrap(~`Class Group`, scales = "free") +
  scale_x_continuous(
                     breaks = top_popular_tfidf_words$row,
                     labels = top_popular_tfidf_words$`Wishes (tokenized)`) +
  coord_flip()
