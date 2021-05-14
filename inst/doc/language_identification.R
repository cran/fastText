## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.width = 50, fig.height = 50, warning = FALSE, message = FALSE, echo = FALSE, cache = FALSE, eval = FALSE)

dir_wili_2018 = '/IN CASE I WANT TO REBUILD THE VIGNETTE MODIFY AND POINT THIS DIR TO THE WILLI 2018 DIRECTORY/wili-2018'
dir_files = '/IN CASE I WANT TO REBUILD THE VIGNETTE MODIFY AND POINT THIS DIR TO THE HUMAN DECLARATION FILES/declaration_human_rights'         # the other 'dir_files' directory in line 663 is mentioned just in case I download the declaration files inside the 'wili-2018' directory


## ---- echo = T----------------------------------------------------------------
#  
#  fasttext_supported_languages = c('af', 'als', 'am', 'an', 'ar', 'arz', 'as', 'ast', 'av',
#                                   'az', 'azb', 'ba', 'bar', 'bcl', 'be', 'bg', 'bh', 'bn',
#                                   'bo', 'bpy', 'br', 'bs', 'bxr', 'ca', 'cbk', 'ce', 'ceb',
#                                   'ckb', 'co', 'cs', 'cv', 'cy', 'da', 'de', 'diq', 'dsb',
#                                   'dty', 'dv', 'el', 'eml', 'en', 'eo', 'es', 'et', 'eu',
#                                   'fa', 'fi', 'fr', 'frr', 'fy', 'ga', 'gd', 'gl', 'gn',
#                                   'gom', 'gu', 'gv', 'he', 'hi', 'hif', 'hr', 'hsb', 'ht',
#                                   'hu', 'hy', 'ia', 'id', 'ie', 'ilo', 'io', 'is', 'it',
#                                   'ja', 'jbo', 'jv', 'ka', 'kk', 'km', 'kn', 'ko', 'krc',
#                                   'ku', 'kv', 'kw', 'ky', 'la', 'lb', 'lez', 'li', 'lmo',
#                                   'lo', 'lrc', 'lt', 'lv', 'mai', 'mg', 'mhr', 'min', 'mk',
#                                   'ml', 'mn', 'mr', 'mrj', 'ms', 'mt', 'mwl', 'my', 'myv',
#                                   'mzn', 'nah', 'nap', 'nds', 'ne', 'new', 'nl', 'nn', 'no',
#                                   'oc', 'or', 'os', 'pa', 'pam', 'pfl', 'pl', 'pms', 'pnb',
#                                   'ps', 'pt', 'qu', 'rm', 'ro', 'ru', 'rue', 'sa', 'sah',
#                                   'sc', 'scn', 'sco', 'sd', 'sh', 'si', 'sk', 'sl', 'so',
#                                   'sq', 'sr', 'su', 'sv', 'sw', 'ta', 'te', 'tg', 'th', 'tk',
#                                   'tl', 'tr', 'tt', 'tyv', 'ug', 'uk', 'ur', 'uz', 'vec',
#                                   'vep', 'vi', 'vls', 'vo', 'wa', 'war', 'wuu', 'xal', 'xmf',
#                                   'yi', 'yo', 'yue', 'zh')

## ---- echo = T----------------------------------------------------------------
#  
#  isocodes = ISOcodes::ISO_639_2
#  # head(isocodes)
#  
#  comp_cases = complete.cases(isocodes$Alpha_2)
#  isocodes_fasttext = isocodes[comp_cases, ]
#  # dim(isocodes_fasttext)
#  
#  idx_keep_fasttext = which(isocodes_fasttext$Alpha_2 %in% fasttext_supported_languages)
#  
#  isocodes_fasttext = isocodes_fasttext[idx_keep_fasttext, ]
#  isocodes_fasttext = data.table::data.table(isocodes_fasttext)
#  # isocodes_fasttext
#  
#  lower_nams = tolower(isocodes_fasttext$Name)
#  lower_nams = trimws(as.vector(unlist(lapply(strsplit(lower_nams, "[;, ]"), function(x) x[1]))), which = 'both')   # remove second or third naming of the country name
#  
#  isocodes_fasttext$Name_tolower = lower_nams
#  isocodes_fasttext

## ---- echo = T----------------------------------------------------------------
#  
#  print_accuracy = function(size_input_data,
#                            true_data,
#                            preds_data,
#                            method) {
#  
#    cat(glue::glue("Total Rows: {size_input_data}"), '\n')
#    rnd_2 = round(length(preds_data)/size_input_data, 4)
#    msg_2 ="Predicted Rows: {length(preds_data)}  ({rnd_2 * 100}% predicted)"
#    cat(glue::glue(msg_2), '\n')
#    cat(glue::glue("Missing Values:  {size_input_data - length(preds_data)}"), '\n')
#    rnd_4 = round(sum(true_data == preds_data) / length(preds_data), 4)
#    msg_4 = "Accuracy on 'Predicted Rows' using '{method}':  {rnd_4 * 100}%"
#    cat(glue::glue(msg_4), '\n')
#  }
#  
#  # fasttext language identification supported languages as described in https://fasttext.cc/docs/en/language-identification.html

## -----------------------------------------------------------------------------
#  
#  list.files(dir_wili_2018)
#  

## -----------------------------------------------------------------------------
#  
#  pth_x_test = file.path(dir_wili_2018, 'x_test.txt')
#  wili_test_x = data.table::fread(file = pth_x_test, sep = '\n', stringsAsFactors = F, header = F, nThread = parallel::detectCores())
#  
#  pth_y_test = file.path(dir_wili_2018, 'y_test.txt')
#  wili_test_y = data.table::fread(file = pth_y_test, sep = '\n', stringsAsFactors = F, header = F, nThread = parallel::detectCores())
#  nrow_init = nrow(wili_test_y)
#  
#  inters_labels = which(wili_test_y$V1 %in% isocodes_fasttext$Alpha_3_B)
#  
#  # subset both the 'x_test' and the 'y_test' data
#  
#  wili_test_x = wili_test_x[inters_labels, ]
#  wili_test_y = wili_test_y[inters_labels, ]
#  
#  cat(glue::glue("Initial observations:  {nrow_init}  Subset based on isocodes:  {nrow(wili_test_y)}   Number of languages based on subset:  {length(unique(wili_test_y$V1))}"), '\n')
#  
#  head(wili_test_y)
#  

## ---- echo = T----------------------------------------------------------------
#  
#  file_ftz = system.file("language_identification/lid.176.ftz", package = "fastText")
#  
#  dtbl_res_in = fastText::language_identification(input_obj = wili_test_x$V1,
#                                                  pre_trained_language_model_path = file_ftz,
#                                                  k = 1,
#                                                  th = 0.0,
#                                                  threads = 1,
#                                                  verbose = TRUE)

## ---- echo = T----------------------------------------------------------------
#  
#  dtbl_res_in$true_label = wili_test_y$V1
#  # dtbl_res_in
#  
#  isocodes_fasttext_subs = isocodes_fasttext[, c(1,3)]   # merge the predicted labels with the 3-letter isocodes
#  
#  merg_labels = merge(dtbl_res_in, isocodes_fasttext_subs, by.x = 'iso_lang_1', by.y = 'Alpha_2')
#  # as.vector(colSums(is.na(merg_labels)))
#  
#  print_accuracy(size_input_data = nrow(wili_test_y),
#                 true_data = merg_labels$true_label,
#                 preds_data = merg_labels$Alpha_3_B,
#                 method = 'fastText (.ftz pre-trained model)')
#  

## ---- echo = T----------------------------------------------------------------
#  
#  file_bin = file.path(dir_wili_2018, 'lid.176.bin')
#  
#  dtbl_res_in = fastText::language_identification(input_obj = wili_test_x$V1,
#                                                  pre_trained_language_model_path = file_bin,
#                                                  k = 1,
#                                                  th = 0.0,
#                                                  threads = 1,
#                                                  verbose = TRUE)

## ---- echo = T----------------------------------------------------------------
#  
#  dtbl_res_in$true_label = wili_test_y$V1
#  # dtbl_res_in
#  
#  isocodes_fasttext_subs = isocodes_fasttext[, c(1,3)]   # merge the predicted labels with the 3-letter isocodes
#  
#  merg_labels = merge(dtbl_res_in, isocodes_fasttext_subs, by.x = 'iso_lang_1', by.y = 'Alpha_2')
#  # as.vector(colSums(is.na(merg_labels)))
#  
#  print_accuracy(size_input_data = nrow(wili_test_y),
#                 true_data = merg_labels$true_label,
#                 preds_data = merg_labels$Alpha_3_B,
#                 method = 'fastText (.ftz pre-trained model)')
#  

## ---- echo = T----------------------------------------------------------------
#  
#  tbl = table(merg_labels$true_label, merg_labels$Alpha_3_B)
#  
#  df = as.data.frame.table(tbl)
#  colnames(df) = c('country_vert', 'country_horiz', 'Freq')
#  # head(df)
#  
#  require(magrittr)
#  require(dplyr)
#  require(ggplot2)
#  
#  df <- df %>%
#    mutate(country_vert = factor(country_vert),                 # alphabetical order by default
#           country_horiz = factor(country_horiz, levels = rev(unique(country_horiz))))
#  
#  plt_tbl = ggplot(df, aes(x=country_vert, y=country_horiz, fill=Freq)) +
#    geom_tile() + theme_bw() + coord_equal() +
#    scale_fill_distiller(palette="Greens", direction=1) +
#    ggplot2::theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))
#  
#  plt_tbl
#  

## ---- echo = T----------------------------------------------------------------
#  
#  require(cld2)
#  
#  t_start = proc.time()
#  cld2_vec = cld2::detect_language(text = wili_test_x$V1, plain_text = TRUE, lang_code = TRUE)
#  
#  cld2_dtbl = data.table::setDT(list(Alpha_2 = cld2_vec))
#  cld2_dtbl$true_label = wili_test_y$V1
#  
#  merg_labels_cld2 = merge(cld2_dtbl, isocodes_fasttext_subs, by = 'Alpha_2')
#  # as.vector(colSums(is.na(merg_labels_cld2)))
#  
#  print_accuracy(size_input_data = nrow(wili_test_y),
#                 true_data = merg_labels_cld2$true_label,
#                 preds_data = merg_labels_cld2$Alpha_3_B,
#                 method = 'cld2')
#  

## ---- echo = F----------------------------------------------------------------
#  
#  fastText:::compute_elapsed_time(t_start)
#  

## ---- echo = T----------------------------------------------------------------
#  
#  require(cld3)
#  
#  t_start = proc.time()
#  cld3_vec = cld3::detect_language(text = wili_test_x$V1)
#  
#  cld3_dtbl = data.table::setDT(list(Alpha_2 = cld3_vec))
#  cld3_dtbl$true_label = wili_test_y$V1
#  
#  merg_labels_cld3 = merge(cld3_dtbl, isocodes_fasttext_subs, by = 'Alpha_2')
#  # as.vector(colSums(is.na(merg_labels_cld3)))
#  
#  print_accuracy(size_input_data = nrow(wili_test_y),
#                 true_data = merg_labels_cld3$true_label,
#                 preds_data = merg_labels_cld3$Alpha_3_B,
#                 method = 'cld3')
#  

## ---- echo = F----------------------------------------------------------------
#  
#  fastText:::compute_elapsed_time(t_start)
#  

## ---- echo = T----------------------------------------------------------------
#  
#  threads = parallel::detectCores()
#  require(textcat)
#  
#  names(textcat::TC_byte_profiles)
#  

## -----------------------------------------------------------------------------
#  
#  nams_profiles = as.vector(unlist(lapply(strsplit(names(textcat::TC_byte_profiles), '-'), function(x) x[1])))
#  nams_profiles = unique(nams_profiles)
#  cat(glue::glue("Isocode-Names:  {length(unique(isocodes_fasttext$Name_tolower))}  TC_byte_profiles:  {length(names(textcat::TC_byte_profiles))}  Intersected Names: {length(intersect(nams_profiles, unique(isocodes_fasttext$Name_tolower)))}"), '\n')
#  

## ---- echo = T----------------------------------------------------------------
#  
#  t_start = proc.time()
#  textc = as.vector(unlist(parallel::mclapply(1:length(wili_test_x$V1), function(x) {
#    textcat(x = wili_test_x$V1[x], p = textcat::TC_byte_profiles, method = "CT")
#  }, mc.cores = threads)))
#  
#  textc = as.vector(unlist(lapply(strsplit(textc, '-'), function(x) x[1])))
#  textc = trimws(textc, which = 'both')
#  
#  unique(textc)

## ---- echo = T----------------------------------------------------------------
#  
#  textc_dtbl = data.table::setDT(list(Name_tolower = textc))
#  textc_dtbl$true_label = wili_test_y$V1
#  
#  fasttext_isoc_name = isocodes_fasttext[, c(1,5)]
#  merg_labels_textc = merge(textc_dtbl, fasttext_isoc_name, by = 'Name_tolower')
#  # as.vector(colSums(is.na(merg_labels_cld2)))
#  
#  print_accuracy(size_input_data = nrow(wili_test_y),
#                 true_data = merg_labels_textc$true_label,
#                 preds_data = merg_labels_textc$Alpha_3_B,
#                 method = 'textcat ( TC_byte_profiles )')

## ---- echo = F----------------------------------------------------------------
#  
#  fastText:::compute_elapsed_time(t_start)
#  

## ---- echo = T----------------------------------------------------------------
#  
#  names(textcat::TC_char_profiles)
#  

## -----------------------------------------------------------------------------
#  
#  nams_profiles = as.vector(unlist(lapply(strsplit(names(textcat::TC_char_profiles), '-'), function(x) x[1])))
#  nams_profiles = unique(nams_profiles)
#  cat(glue::glue("Isocode-Names:  {length(unique(isocodes_fasttext$Name_tolower))}  TC_char_profiles:  {length(names(textcat::TC_char_profiles))}  Intersected Names: {length(intersect(nams_profiles, unique(isocodes_fasttext$Name_tolower)))}"), '\n')
#  

## ---- echo = T----------------------------------------------------------------
#  
#  t_start = proc.time()
#  textc = as.vector(unlist(parallel::mclapply(1:length(wili_test_x$V1), function(x) {
#    textcat(x = wili_test_x$V1[x], p = textcat::TC_char_profiles, method = "CT")
#  }, mc.cores = threads)))
#  
#  textc = as.vector(unlist(lapply(strsplit(textc, '-'), function(x) x[1])))
#  textc = trimws(textc, which = 'both')
#  
#  unique(textc)

## ---- echo = T----------------------------------------------------------------
#  
#  textc_dtbl = data.table::setDT(list(Name_tolower = textc))
#  textc_dtbl$true_label = wili_test_y$V1
#  
#  fasttext_isoc_name = isocodes_fasttext[, c(1,5)]
#  merg_labels_textc = merge(textc_dtbl, fasttext_isoc_name, by = 'Name_tolower')
#  # as.vector(colSums(is.na(merg_labels_cld2)))
#  
#  print_accuracy(size_input_data = nrow(wili_test_y),
#                 true_data = merg_labels_textc$true_label,
#                 preds_data = merg_labels_textc$Alpha_3_B,
#                 method = 'textcat ( TC_char_profiles )')

## ---- echo = F----------------------------------------------------------------
#  
#  fastText:::compute_elapsed_time(t_start)
#  

## ---- echo = T----------------------------------------------------------------
#  
#  require(franc)
#  
#  t_start = proc.time()
#  franc_res = as.vector(unlist(parallel::mclapply(1:length(wili_test_x$V1), function(x) {
#    franc(text = wili_test_x$V1[x], min_speakers = 0, min_length = 10, max_length = 2048)
#  }, mc.cores = threads)))
#  
#  franc_dtbl = data.table::setDT(list(franc = franc_res, true_label = wili_test_y$V1))
#  # as.vector(colSums(is.na(franc_dtbl)))
#  
#  print_accuracy(size_input_data = nrow(wili_test_y),
#                 true_data = franc_dtbl$true_label,
#                 preds_data = franc_dtbl$franc,
#                 method = 'franc')
#  

## ---- echo = F----------------------------------------------------------------
#  
#  fastText:::compute_elapsed_time(t_start)
#  

## ---- echo = F----------------------------------------------------------------
#  
#  dtbl_bench = data.table::setDT(list(method = c('fastText (ftz)', 'fastText (bin)', 'cld2', 'cld3', 'textcat (byte)', 'textcat (char)', 'franc'),
#                                      rows = c(50500, 50500, 50500, 50500, 50500, 50500, 50500),
#                                      pred_rows = c(50211, 50168, 34254, 43560, 47324, 43265, 50500),
#                                      pred_perc = c(99.43, 99.34, 67.83, 86.26, 93.71, 85.67, 100.0),
#                                      NAs = c(289, 332, 16246, 6940, 3176, 7235, 0),
#                                      accuracy = c(83.05, 86.55, 83.13, 74.74, 29.91, 31.1, 62.04),
#                                      seconds = c(5, 5, 2, 18, 83, 100, 179),
#                                      threads = c(1, 1, 1, 1, 8, 8, 8)))

## ---- echo = F----------------------------------------------------------------
#  dtbl_bench = dtbl_bench[order(dtbl_bench$accuracy, decreasing = T), ]
#  dtbl_bench
#  

## ---- echo = F----------------------------------------------------------------
#  dtbl_bench = dtbl_bench[order(dtbl_bench$pred_perc, decreasing = T), ]
#  dtbl_bench
#  

## ---- echo = F----------------------------------------------------------------
#  dtbl_bench = dtbl_bench[order(dtbl_bench$NAs, decreasing = F), ]
#  dtbl_bench
#  

## ---- echo = F----------------------------------------------------------------
#  dtbl_bench = dtbl_bench[order(dtbl_bench$seconds, decreasing = F), ]
#  dtbl_bench
#  

## ---- eval = F, echo = T------------------------------------------------------
#  
#  dir_files = file.path(dir_wili_2018, 'declaration_human_rights')
#  

## ---- echo = T----------------------------------------------------------------
#  
#  lst_files = list.files(dir_files, full.names = T, pattern = '.pdf')
#  
#  decl_dat = lapply(1:length(lst_files), function(x) {
#  
#    iter_dat = pdftools::pdf_text(pdf = lst_files[x])
#    lang = trimws(unlist(strsplit(gsub('.pdf', '', basename(lst_files[x])), '_')), which = 'both')
#    lang = lang[length(lang)]
#    vec_txt = as.vector(unlist(trimws(iter_dat, which = 'both')))
#    vec_txt = as.vector(sapply(vec_txt, function(x) gsub('\n', '', x)))
#  
#    idx_lang = which(isocodes_fasttext$Name_tolower == lang)
#    isocode_3_language = rep(isocodes_fasttext$Alpha_3_B[idx_lang], length(vec_txt))
#    isocode_2_language = rep(isocodes_fasttext$Alpha_2[idx_lang], length(vec_txt))
#    language = rep(lang, length(vec_txt))
#  
#    dtbl = data.table::setDT(list(isocode_3_language = isocode_3_language,
#                                  isocode_2_language = isocode_2_language,
#                                  language = language,
#                                  text = vec_txt))
#    dtbl
#  })
#  
#  decl_dat = data.table::rbindlist(decl_dat)
#  

## ---- echo = F----------------------------------------------------------------
#  
#  decl_dat$language
#  decl_dat$isocode_3_language
#  decl_dat$isocode_2_language
#  

## ---- echo = T----------------------------------------------------------------
#  
#  dtbl_res_in = fastText::language_identification(input_obj = decl_dat$text,
#                                                  pre_trained_language_model_path = file_ftz,
#                                                  k = 1,
#                                                  th = 0.0,
#                                                  threads = 1,
#                                                  verbose = TRUE)

## ---- echo = F----------------------------------------------------------------
#  
#  dtbl_res_in
#  

## ---- echo = T----------------------------------------------------------------
#  
#  print_accuracy(size_input_data = length(dtbl_res_in$iso_lang_1),
#                 true_data = decl_dat$isocode_2_language,
#                 preds_data = dtbl_res_in$iso_lang_1,
#                 method = 'fastText (.ftz pre-trained model)')
#  

## ---- echo = T----------------------------------------------------------------
#  
#  cld2_vec = cld2::detect_language(text = decl_dat$text,
#                                   plain_text = TRUE,
#                                   lang_code = TRUE)
#  cld2_vec
#  

## ---- echo = T----------------------------------------------------------------
#  
#  print_accuracy(size_input_data = nrow(decl_dat),
#                 true_data = decl_dat$isocode_2_language,
#                 preds_data = cld2_vec,
#                 method = 'cld2')
#  

## ---- echo = T----------------------------------------------------------------
#  
#  cld3_vec = cld3::detect_language(text = decl_dat$text)
#  cld3_vec
#  

## ---- echo = T----------------------------------------------------------------
#  
#  print_accuracy(size_input_data = nrow(decl_dat),
#                 true_data = decl_dat$isocode_2_language,
#                 preds_data = cld3_vec,
#                 method = 'cld3')
#  

## ---- echo = T----------------------------------------------------------------
#  
#  textc = textcat(x = decl_dat$text, p = textcat::TC_byte_profiles, method = "CT")
#  textc
#  

## ---- echo = T----------------------------------------------------------------
#  
#  textc = as.vector(unlist(lapply(strsplit(textc, '-'), function(x) x[1])))
#  textc = trimws(textc, which = 'both')
#  textc
#  

## ---- echo = T----------------------------------------------------------------
#  
#  print_accuracy(size_input_data = nrow(decl_dat),
#                 true_data = decl_dat$language,
#                 preds_data = textc,
#                 method = 'textcat')
#  

## ---- echo = T----------------------------------------------------------------
#  
#  franc_vec = as.vector(sapply(decl_dat$text, function(x) {
#    franc(text = x, min_length = 10, max_length = 2048)
#  }))
#  
#  franc_vec
#  

## ---- echo = T----------------------------------------------------------------
#  
#  print_accuracy(size_input_data = nrow(decl_dat),
#                 true_data = decl_dat$isocode_3_language,
#                 preds_data = franc_vec,
#                 method = 'franc')
#  

## ---- echo = T----------------------------------------------------------------
#  
#  dtbl_out = decl_dat[, 1:3]
#  colnames(dtbl_out) = c('true_y_iso_3', 'true_y_iso_2', 'true_y_language')
#  # dtbl_out
#  
#  dtbl_preds = data.table::setDT(list(fastText = dtbl_res_in$iso_lang_1,
#                                      cld2 = cld2_vec,
#                                      cld3 = cld3_vec,
#                                      textcat = textc,
#                                      franc = franc_vec))
#  # dtbl_preds
#  
#  dtbl_out = cbind(dtbl_out, dtbl_preds)
#  dtbl_out
#  

## ---- echo = T----------------------------------------------------------------
#  
#  lst_files = list.files(dir_files, full.names = F, pattern = '.pdf')
#  
#  min_letters_en_es = 3         # min. number of characters for the 'en' and 'es' languages
#  sample_words = 100            # sample that many words from each tokenized file
#  
#  decl_dat = lapply(1:length(lst_files), function(x) {
#  
#    iter_dat = pdftools::pdf_text(pdf = file.path(dir_files, lst_files[x]))
#  
#    dat_txt = sapply(iter_dat, function(y) {
#  
#      if (lst_files[x] == 'declaration_human_rights_chinese.pdf') {
#        res_spl_lang = stringi::stri_split_boundaries(str = y,
#                                                      type = 'word',
#                                                      skip_word_none = TRUE,
#                                                      skip_word_letter = TRUE,
#                                                      skip_word_number = TRUE)
#      }
#      else {
#        res_spl_lang = stringi::stri_split(str = y,
#                                           regex = '[ \n,]',
#                                           omit_empty = TRUE,
#                                           tokens_only = TRUE)
#      }
#  
#      res_spl_lang = trimws(res_spl_lang[[1]], which = 'both')
#      idx_empty = which(res_spl_lang == "")
#      if (length(idx_empty) > 0) {
#        res_spl_lang = res_spl_lang[-idx_empty]
#      }
#      if (!is.null(min_letters_en_es) & lst_files[x] != 'declaration_human_rights_chinese.pdf') {
#        nchars = nchar(res_spl_lang)
#        idx_chars = which(nchars >= min_letters_en_es)
#        if (length(idx_chars) > 0) {
#          res_spl_lang = res_spl_lang[idx_chars]
#        }
#      }
#      res_spl_lang
#    })
#  
#    dat_txt = as.vector(unlist(dat_txt))
#    set.seed(1)
#    sample_words = sample(dat_txt, sample_words)
#    sample_words
#  })
#  
#  
#  decl_dat = as.vector(unlist(decl_dat))
#  decl_dat = decl_dat[sample(1:length(decl_dat), length(decl_dat))]
#  multilingual_sentence = paste(decl_dat, collapse = ' ')
#  multilingual_sentence
#  

## ---- echo = T----------------------------------------------------------------
#  
#  num_languages = 3
#  

## ---- echo = T----------------------------------------------------------------
#  
#  dtbl_multiling = fastText::language_identification(input_obj = multilingual_sentence,
#                                                     pre_trained_language_model_path = file_ftz,
#                                                     k = num_languages,
#                                                     th = 0.0,
#                                                     threads = 1,
#                                                     verbose = FALSE)
#  dtbl_multiling
#  

## ---- echo = T----------------------------------------------------------------
#  
#  cld2::detect_language_mixed(text = multilingual_sentence, plain_text = TRUE)$classification
#  

## ---- echo = T----------------------------------------------------------------
#  
#  cld3::detect_language_mixed(text = multilingual_sentence, size = num_languages)
#  

## ---- echo = T----------------------------------------------------------------
#  
#  # we could use the 'whitelist' parameter but the purpose is to identify languages from unknown text
#  
#  franc::franc_all(text = multilingual_sentence, max_length = nchar(multilingual_sentence) + 1)[1:num_languages, ]
#  

