setwd("/home/agricolamz/work/scripts/r/automate_sound_instrumental_phonetics")
library(tidyverse)

# create a dataframe with stimuli -----------------------------------------
t <- read_lines("task.txt")
tibble(title = t[str_detect(t, "#")],
       words = t[!str_detect(t, "#")]) %>% 
  separate(words, into = c("word_1", "word_2", "word_3", "word_4"), sep = " ", fill = "right") %>% 
  pivot_longer(names_to = "word_order", values_to = "word", word_1:word_4) %>% 
  na.omit() %>% 
  mutate(word_order = str_remove(word_order, "word_") %>% as.double(),
         word_dict = word,
         word_dict = str_remove(word_dict, ".am$"),
         id = 1:n(),
         id_sort = phonfieldwork:::add_leading_symbols(id),
         letter1 = str_sub(word_dict, 1, 1),
         letter3 = str_sub(word_dict, 1, 3),
         letter3 = str_pad(letter3, 3, side = "right", pad = "_"),
         letter5 = str_sub(word_dict, 1, 5),
         letter5 = str_pad(letter5, 5, side = "right", pad = "_")) %>% 
  mutate(letter1 = ifelse(word == "sea", "c", letter1),
         letter3 = ifelse(word == "sea", "c__", letter3),
         letter5 = ifelse(word == "sea",  "c____", letter5),
         word_dict = ifelse(word_dict == "sea",  "c", word_dict),
         letter1 = ifelse(word == "zed", "e", letter1),
         letter3 = ifelse(word == "zed", "eus", letter3),
         letter5 = ifelse(word == "zed",  "eus30", letter5),
         word_dict = ifelse(word_dict == "zed",  "eus30437", word_dict),
         letter3 = ifelse(word == "seem",  "sea", letter3),
         letter5 = ifelse(word == "seem",  "seam_", letter5),
         word_dict = ifelse(word == "seem",  "seam", word_dict),
         letter1 = ifelse(word == "fantasy", "e", letter1),
         letter3 = ifelse(word == "fantasy", "eus", letter3),
         letter5 = ifelse(word == "fantasy",  "eus71", letter5),
         word_dict = ifelse(word_dict == "fantasy",  "eus71768", word_dict),
         word_dict = ifelse(word_dict == "live",  "live_01_00", word_dict),
         word_dict = ifelse(word_dict == "lost",  "lost_lose", word_dict),
         letter1 = ifelse(word == "loo",  "c", letter1),
         letter3 = ifelse(word == "loo",  "cus", letter3),
         letter5 = ifelse(word == "loo",  "cus01", letter5),
         word_dict = ifelse(word_dict == "loo",  "cus01176", word_dict),
         letter1 = ifelse(word == "French",  "u", letter1),
         letter3 = ifelse(word == "French",  "usc", letter3),
         letter5 = ifelse(word == "French",  "uscld", letter5),
         word_dict = ifelse(word_dict == "French",  "uscld04128", word_dict),
         letter1 = ifelse(word == "hazelnut",  "u", letter1),
         letter3 = ifelse(word == "hazelnut",  "ush", letter3),
         letter5 = ifelse(word == "hazelnut",  "ushau", letter5),
         word_dict = ifelse(word_dict == "hazelnut",  "ushaute017", word_dict),
         letter1 = ifelse(word == "sunrise",  "u", letter1),
         letter3 = ifelse(word == "sunrise",  "uss", letter3),
         letter5 = ifelse(word == "sunrise",  "ussun", letter5),
         word_dict = ifelse(word_dict == "sunrise",  "ussunbl017", word_dict),
         letter1 = ifelse(word == "prosody",  "u", letter1),
         letter3 = ifelse(word == "prosody",  "usb", letter3),
         letter5 = ifelse(word == "prosody",  "usb01", letter5),
         word_dict = ifelse(word_dict == "prosody",  "usb01131", word_dict),
         word_order = ifelse(word_dict %in% c("butter", "writer", "military"), 1, word_order),
         word_order = ifelse(word_dict %in% c("car", "herb", "private", "dance", "fragile", "laboratory"), 2, word_order)) ->
  task

# download all sounds -----------------------------------------------------
map(1:nrow(task), function(i){
  download.file(url = str_c("https://dictionary.cambridge.org/media/english/us_pron/",
                      task$letter1[i], "/",
                      task$letter3[i], "/",
                      task$letter5[i], "/",
                      task$word_dict[i], ".mp3"),
                destfile = str_c("audio/", task$id_sort[i], "_", task$word[i], ".mp3"))  
})

# I downloaded car.uk, herb.uk, butter.uk, writer.uk, private.uk, dance.uk, 
# fragile.uk, military.uk and laboratory.uk manually, since uk prononciation
# is coded with speaker codes, not with the word itself

# make a dataset with all sounds ------------------------------------------
task %>% 
  select(id, id_sort, title, word, word_dict, word_order) %>% 
  bind_rows(
    tibble(title = task$title[63:71],
       word = str_c(task$word_dict[63:71], ".uk"),
       word_dict = task$word_dict[63:71],
       id = task$id[63:71],
       id_sort = as.character(id),
       word_order = c(1, 1, 2, 2, 1, 1, 1, 2, 1))) %>% 
  arrange(id, title, word_order) ->
  all_audio

# concatenate sounds by task ----------------------------------------------
library(phonfieldwork)

dir.create("audio_merged")
dir.create("audio_merged/by_slide")
all_audio %>% 
  mutate(file = str_c(id_sort, "_", word, ".mp3")) %>% 
  select(title, file) %>% 
  nest(files = file) %>% 
  mutate(slide = 1:n(),
         slide_name = phonfieldwork:::add_leading_symbols(slide),
         files = map(files, unlist)) ->
  for_concatenate


map(1:length(for_concatenate$title), function(i){
  path <- str_c("audio_merged/slide_", for_concatenate$slide_name[i], "/")
  files <- unlist(for_concatenate$files[i])
  dir.create(path)
  file.copy(from = str_c("audio/", files),
            to = str_c(path, files))
  concatenate_soundfiles(path = path, 
                         result_file_name = str_c("slide_", 
                                                  for_concatenate$slide_name[i]),
                         annotation = NULL, 
                         separate_duration = 0.2)
  file.copy(from = str_c(path, "slide_", for_concatenate$slide_name[i], ".wav"),
            to = str_c("audio_merged/by_slide/slide_", 
                       for_concatenate$slide_name[i], 
                       ".wav"))
})

# create pictures ---------------------------------------------------------
draw_sound(sounds_from_folder = "audio_merged/by_slide/",
           pic_folder_name = "by_slide_pictures",
           title_as_filename = FALSE)

# create a presentation ---------------------------------------------------

c('---
title: "Spectrum reading training"
output: ioslides_presentation
---',
  str_c(for_concatenate$title, 
      "\n\n![](audio_merged/by_slide_pictures/slide_", 
      for_concatenate$slide_name,
      ".png)\n\n")) %>% 
  write_lines("final_presentation.Rmd")

