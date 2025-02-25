
# This file contains common functions and variable descriptions used in the 
# summarise_data.Rmd, training_models.Rmd, testing_models.Rmd and pretty_forest_plots.Rmd

# The codes are based on the codes from Jarkko Toivonen:
## https://github.com/FRCBS/anemia_and_hb_deferral_prediction/blob/main/src/common.R
## https://github.com/FRCBS/anemia_and_hb_deferral_prediction/blob/main/src/anemia_logistic_and_cox.Rmd
## https://github.com/FRCBS/anemia_and_hb_deferral_prediction/blob/main/src/create_anemia_and_deferral_article_results.Rmd
## https://github.com/FRCBS/anemia_and_hb_deferral_prediction/blob/main/src/deferral_logistic_and_cox.Rmd
## 

library(tidyverse)

# Adds the data frame 'descript' into the data frame 'df' 
to_pretty <- function(df, descript) {  
  df %>%
    left_join(descript, by=c("Variable")) %>%
    mutate(Pretty = ifelse(is.na(Pretty), Variable, Pretty)) %>%
    mutate(Pretty = as.factor(Pretty))
}

# Converts column names of a data frame 'df' to pretty format using data frame 'description'.
pretty_col_names <- function(df, description) {
  conversion <- description %>%
    select(Pretty, Variable) %>% deframe()
  df %>% rename(any_of(conversion))
}

# Converts variable 'v' (should be 'character' class) to its Pretty format using 
# the data frame 'description'
to_pretty_vector <- function(v, description) {
  conversion <- description %>%
    select(Variable, Pretty) %>% deframe()
  recode(v, !!!conversion)
}

# descriptions for donation table
donation_descript <- tibble(Variable    = c("donor_id", "sex", "group", "age", "year",
                                            "warm_season", "donation_date", "hour", 
                                            "first_event", "Hb_first", "Hb", "Hb_deferral", 
                                            "days_to_previous_fb", "previous_Hb",
                                            "previous_Hb_def", "consecutive_deferrals",
                                            "recent_donations", "recent_deferrals"),
                            Pretty      = c("Donor ID", "Sex", "Group", "Age", "Year", 
                                            "Warm season", "Donation date", "Hour", 
                                            "First event", "First Hb", "Hemoglobin", "Deferral", 
                                            "Days to previous full blood donation", "Previous Hb", 
                                            "Previous event was deferral", "Consecutive deferrals",
                                            "Donations in last two years", "Deferrals in last two years"),
                            Type        = c("factor", "factor", "factor", "numeric", "numeric (int)", 
                                            "boolean", "factor", "numeric",
                                            "boolean", "numeric", "numeric", "boolean", 
                                            "numeric(int)", "numeric", 
                                            "boolean", "numeric (int)", 
                                            "numeric (int)", "numeric (int)"),
                            Explanation = c("Donor identifier",
                                            "Sex of the donor",
                                            "Indicates if the donor is premenopausal female, 
                                            postmenopausal female or male",
                                            "Age of the donor",
                                            "Year of donation",
                                            "True if donation was given in April-September",
                                            "The date of the donation",
                                            "Time of day when donation was given as hours (e.g. 13:45 = 13.75)",
                                            "True if donation is the first one in the data set",
                                            "Hb value at first donation of this donor (linear mixed model)",
                                            "Amount of hemoglobin",
                                            "Hemoglobin below deferral threshold",
                                            "Time (in days) between Hb measurement and previous full blood donation event",
                                            "Hb value at previous measurement (dynamic linear mixed model)",
                                            "Indicates whether the donor had low hemoglobin at previous donation event",
                                            "Number of times the donor has been deferred due to low hemoglobin since last succesful whole blood donation",
                                            "Number of donations in the last two years",
                                            "Number of deferrals in the last two years"))

# descriptors for donor table
donor_descript <- tibble(
  Variable    = c("smoking","sex", "dob", "weight", "blood_volume", "height_m", "height_cm", 
                  "one_deferral","label"),
  Pretty      = c("Smoking", "Sex", "Date of Birth", "Weight (kg)", "Blood volume", "Height (m)", "Height (cm)", 
                  "At least one deferral", "Partition label"),
  Type        = c("boolean", "factor", "factor", "numeric", "numeric", "numeric", "numeric", 
                  "boolean", "factor"),
  Explanation = c("Does the person smoke", 
                  "Sex of the donor", 
                  "Donors date of birth", 
                  "Weight of the donor (kg)", 
                  "Height of the donor (m)",
                  "Height of the donor (cm)",
                  "Nardler's blood volume",
                  "At least one deferral",
                  "The donors are partitioned into train, validate, and test sets")
)

# Descriptors for the SNP variables
# the ones commended are the ones not included in the final models
# due to the lack of data available
snp_descript <- tribble(
  ~Variable, ~Pretty, ~Type, ~Explanation,
  "snp_1_113834946", "SNP 1:113834946", "numeric", "SNP 1:113834946",
  "snp_1_169549811", "SNP 1:169549811", "numeric", "SNP 1:169549811",
  
  "snp_4_25970243",  "SNP 4:25970243",  "numeric", "SNP 4:25970243",
  
  # "snp_6_21989753",  "SNP 6:21989753",  "numeric", "SNP 6:21989753",
  # "snp_6_22583313",  "SNP 6:22583313",  "numeric", "SNP 6:22583313",
  # "snp_6_23835557",  "SNP 6:23835557",  "numeric", "SNP 6:23835557",
  "snp_6_25857692",  "SNP 6:25857692",  "numeric", "SNP 6:25857692",
  "snp_6_32617727",  "SNP 6:32617727",  "numeric", "SNP 6:32617727",
  "snp_6_32658525",  "SNP 6:32658525",  "numeric", "SNP 6:32658525",
  "snp_6_68206710",  "SNP 6:68206710",  "numeric", "SNP 6:68206710",
  
  "snp_7_75844637",  "SNP 7:75844637",  "numeric", "SNP 7:75844637",
  
  "snp_8_10785723",  "SNP 8:10785723",  "numeric", "SNP 8:10785723",
  "snp_8_23520397",  "SNP 8:23520397",  "numeric", "SNP 8:23520397",
  "snp_8_75503352",  "SNP 8:75503352",  "numeric", "SNP 8:75503352",
  
  "snp_9_133271182", "SNP 9:133271182", "numeric", "SNP 9:133271182",
  
  "snp_10_63174788", "SNP 10:63174788", "numeric", "SNP 10:63174788",
  
  "snp_11_5226799",  "SNP 11:5226799",  "numeric", "SNP 11:5226799",
  
  "snp_12_6030341",  "SNP 12:6030341",  "numeric", "SNP 12:6030341",
  
  "snp_14_33938877", "SNP 14:33938877", "numeric", "SNP 14:33938877",
  
  "snp_15_45099877", "SNP 15:45099877", "numeric", "SNP 15:45099877",
  
  "snp_17_58358769", "SNP 17:58358769", "numeric", "SNP 17:58358769",
  
  "snp_20_35186730", "SNP 20:35186730", "numeric", "SNP 20:35186730",
  
  "snp_22_29795932", "SNP 22:29795932", "numeric", "SNP 22:29795932",
  "snp_22_37066896", "SNP 22:37066896", "numeric", "SNP 22:37066896"
)

# Descrpitors added later than the summarise_data.Rmd can be listed
# here
extra_descript <- tribble(
  ~Variable, ~Pretty, ~Type, ~Explanation,
  "female", "Female", "boolean", "Is the donor female"
)

# Collect all descriptions in one
descript <- bind_rows(donation_descript, donor_descript, snp_descript, extra_descript)

# List of the interactions wanted into the models
interactions <- c("smoking:female", "group:snp_22_37066896", "snp_1_169549811:group")

# Adds the the TRUE to the variable 'v' name in the data frame 'd'
to_encoded <- function(d, v) {
  d %>% filter(Variable==v) %>% mutate(t = ifelse(Type=="boolean", sprintf("%sTRUE", Variable), Variable)) %>% pull(t)
}

# This code produces a interaction description, but the Pretty produced is not
# always the prettiest so interaction Pretty values are modified where needed in the
# Rmd files
add_interaction_descriptions <- function(d, interactions, encoded=FALSE) {
  helper <- function(int) {
    ab <- str_split_fixed(int, ":", n=2)[1,]
    a <- ab[1]
    b <- ab[2]
    ap <- to_pretty_vector(a, d)
    bp <- to_pretty_vector(b, d)
    if (encoded) {
      a <- to_encoded(d, a)
      b <- to_encoded(d, b)
      name <- "Variable2"
    } else {
      name <- "Variable"
    }
    r1  <- tibble({{name}}:=sprintf("%s:%s", a, b), Type=NA, Pretty=sprintf("%s:%s", ap,bp))
    r2  <- tibble({{name}}:=sprintf("%s:%s", b, a), Type=NA, Pretty=sprintf("%s:%s", bp,ap))
    return(bind_rows(r1, r2))
  }
  df <- map_dfr(interactions, helper)
  return(df)
}

# Defining the demographic groups
mygroups <- c("all", "male", "female") #, "pre_menopausal_female", "post_menopausal_female")

# Encode variable names
descript1 <- descript %>%
  mutate(Variable2 = to_encoded(descript, Variable)) %>%
  select(-Variable)

# Add interactions to the descrription table
descript2 <- bind_rows(descript1, add_interaction_descriptions(descript, interactions, encoded=TRUE))


# Takes a object of the class date, and return the time as hours and minutes
hours_to_numeric <- function(date.obj) {
  h <- hour(date.obj)
  m <- minute(date.obj) * (1/60)
  time <- h + m
  return(time)
}

# counts the number of the unique donor IDs in the data frame 'df'
ndonor <- function(df) {
  return(length(unique(df$donor_id)))  
}


# Define colors for the demographic groups
cohort_colors <- c(
  all = "#009E73",                # Green
  male = "#0072B2",               # Blue
  female = "#CC79A7"             # Purple
  # pre_menopausal_female = "#E69F00", # Orange
  # post_menopausal_female = "#D55E00" # Red
)

# Define the names for the demographic groups
cohort_names <- c(
  all = "All",
  male = "Male",
  female = "Female"
  # pre_menopausal_female = "Premenopausal female",
  # post_menopausal_female = "Postmenopausal female"
)

# This code stopped working for some reason. It wasn't finding the Pretty column with the pull()
# from the bayes_cis spec_tbl_df after I made changes into the bayes_cis helper functions
# make_stripes <- function(df, Variable) {
#   v <- df %>% pull({{ Variable }})
#   if (is.factor(v)) {
#     v <- levels(v)
#   } else {
#     v <- sort(unique(v))
#   }
#   tibble({{ Variable }} := factor(v, levels=v)) %>% 
#     mutate(stripe = row_number() %% 2)
# }
# 
# geom_stripes <- function(df, Variable) {
#   geom_rect(data=make_stripes(df, Variable) %>% filter(stripe==1),
#             mapping=aes(ymax = as.numeric(Variable) + 0.5,
#                         ymin = as.numeric(Variable) - 0.5),
#             fill = "gray", xmin=-Inf, xmax=Inf, alpha = 0.5, show.legend = FALSE, colour=NA, inherit.aes = FALSE)
# }


# Helper functions for the forest plots:
# Creates stripes for the the forest plots
make_stripes <- function(df, Variable) {
  Variable <- ensym(Variable)
  v <- df %>% pull(!!Variable)
  if (is.factor(v)) {
    v <- levels(v)
  } else {
    v <- sort(unique(v))
  }
  tibble(!!Variable := factor(v, levels = v)) %>% 
    mutate(stripe = row_number() %% 2)
}

geom_stripes <- function(df, Variable) {
  Variable <- ensym(Variable)
  geom_rect(data = make_stripes(df, !!Variable) %>% filter(stripe == 1),
            mapping = aes(ymax = as.numeric(!!Variable) + 0.5,
                          ymin = as.numeric(!!Variable) - 0.5),
            fill = "gray", xmin = -Inf, xmax = Inf, alpha = 0.5, show.legend = FALSE, colour = NA, inherit.aes = FALSE)
}

StatStripes <- ggproto("StatStripes", Stat, 
                       required_aes = c("stripe"),
                       
                       default_aes = aes(),
                       
                       setup_params = function(data, params) {
                         #cat("In setup_params\n")
                         #print(params)
                         #print(data)
                         if (!is.null(params$row_sizes))
                           return(params)
                         number_of_cols <- 2
                         tmp <- data %>% mutate(row = (as.numeric(as.character(PANEL)) - 1) %/% number_of_cols + 1) 
                         #print(tmp)
                         tmp2 <- tmp %>%
                           group_by(row) %>% summarise(n=n_distinct(stripe)) 
                         #print(tmp2)
                         params$row_sizes <- tmp2 %>% deframe
                         params
                       },
                       
                       compute_panel = function(data, scales, row_sizes, fill, alpha) {
                         #cat("In compute_panel\n")
                         panel <- unique(data$PANEL)
                         number_of_cols <- 2
                         row <- (as.numeric(as.character(panel)) - 1) %/% number_of_cols + 1
                         n <- row_sizes[row]
                         grid <- tibble(stripe=seq(1, n, 2))
                         grid$ymax <- grid$stripe + 0.5
                         grid$ymin <- grid$stripe - 0.5
                         grid$xmin <- -Inf
                         grid$xmax <- Inf
                         grid$fill <- fill
                         grid
                       }
)

stat_stripes <- function(mapping = NULL, data = NULL, geom = "rect",
                         position = "identity", na.rm = FALSE, show.legend = FALSE, 
                         inherit.aes = FALSE, alpha = 0.5, fill="gray", row_sizes = NULL, #group=1,
                         ...) {
  layer(
    stat = StatStripes, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    check.aes=TRUE, check.param = TRUE,
    params = list(row_sizes=row_sizes, alpha=alpha, fill=fill, group=1, 
                  na.rm = na.rm, ...)
  )
}

# Code from create_anemia_and_deferral_article_results.Rmd
# Function to to unify x axis ranges on each row.
# Returns a list of scales that can be passed as a parameter to the ggh4x::facetted_pos_scales function,
# to modify individual panels in a facetted plot
unify_range_by_rows <- function(g) {
  b <- ggplot_build(g)
  L <- length(b$layout$panel_scales_x)
  cols <- n_distinct(b$layout$layout$COL)  # Maybe like this?
  rows <- L / cols
  # Unify ranges of a single facet row
  unify <- function(r2) {
    left <- map_dbl(r2, function(x) x[[1]])  # Left ends of the ranges
    right <- map_dbl(r2, function(x) x[[2]])  # Right ends of the ranges
    u <- c(min(left), max(right))
    u <- rep(list(u), length(left))  # Repeat the same range for each column
    # Change this so that a list is returned
    u
  }
  # Extract limits from each panel
  L <- list()
  for (i in seq(1, rows)) {
    tmp <- b$layout$panel_scales_x
    r <- tmp[seq((i-1)*cols + 1, i*cols)]
    r2 <- map(r, function(x) x$range$range) # each item is numeric vector of length two
    res <- unify(r2)
    L <- append(L, res)
  }
  scales <- map(L, function(r) scale_x_continuous(limits = r))  # Turn list of ranges into a list of scales
  scales
}

# Returns a plot of a type specified with geom argument, where the variables are separated by the status (case/control) and/or groupping
# variable (group or sex).
double_summary_plotter_case_control <- function(df, variable_descriptions, groupping_variable, geom = "freqpoly", breaks=waiver(), ncol=NULL) {
  df <- df %>%
    mutate(across(where(is.logical), as.integer)) %>%
    select(where(is.numeric) | c("status", groupping_variable)) %>%
    pretty_col_names(variable_descriptions) %>%
    pivot_longer(!c("status", str_to_title(groupping_variable)))
  
  #if (df %>% filter(name=="Days to previous full blood donation") %>% nrow() > 0) {
  # Abbreviate this long variable name
  if ("Days to previous full blood donation" %in% levels(df$name)) {
    df <- df %>%
      mutate(name = fct_recode(name, `Days to previous FB donation`="Days to previous full blood donation"))
  }
  
  if (geom=="freqpoly") {
    g <- df %>%
      ggplot(aes(value, color=status)) +
      facet_wrap(~ name, scales = "free", ncol=ncol) +
      geom_freqpoly() +
      scale_x_continuous(breaks=breaks) +
      scale_y_continuous(labels = label_number()) +
      scale_color_manual(values = cohort_colors)
  } else if (geom=="histogram") {
    g <- df %>%
      ggplot(aes(value, fill=!!sym(str_to_title(groupping_variable)))) +
      #facet_wrap(~ name, scales = "free", ncol=ncol) +
      ggh4x::facet_grid2(rows=vars(name), cols=vars(status), scales = "free", independent = TRUE) + 
      geom_histogram(position="identity", alpha=0.8) +
      scale_x_continuous(breaks=breaks) +
      scale_y_continuous(labels = label_number()) +
      scale_fill_manual(values = cohort_colors)
  } else if (geom=="hollow_histogram") {
    g <- df %>%
      ggplot(aes(value, color=groupping_variable)) +
      #facet_grid(name ~ status, scales = "free") +
      ggh4x::facet_grid2(rows=vars(name), cols=vars(status), scales = "free", independent = TRUE) + 
      geom_histogram(fill=NA) +
      scale_x_continuous(breaks=breaks) +
      scale_y_continuous(labels = label_number()) +
      scale_color_manual(values = cohort_colors)
  } else if (geom=="bar") {
    g <- df %>%
      mutate(value = factor(value, levels=0:2),
             name = fct_relevel(name, function(v) str_sort(v, numeric=TRUE))) %>%
      ggplot(aes(value, fill=!!sym(str_to_title(groupping_variable)))) +
      #facet_grid(name ~ status, scales = "free") +
      ggh4x::facet_grid2(rows=vars(name), cols=vars(status), scales = "free", independent = TRUE) + 
      geom_bar(position="dodge", width=0.5) +
      scale_x_continuous(breaks=breaks) +
      scale_y_continuous(labels = label_number()) +
      scale_fill_manual(values = cohort_colors) +
      geom_text(stat='count', aes(label=..count.., y = ..count../2, group=!!sym(str_to_title(groupping_variable))), 
                position=position_dodge(width=0.5), 
                vjust=0.5, angle=90, size=2, hjust=0.0) +
      labs(y = "count") # Add counts
  } else {
    stop(sprintf("Unknow value for the geom parameter: %s", geom))
  }
  
  return(g)
}

# Did not work with the codes from model_training!
# code from deferral_logistic_and_cox.rmd
# Emphasize cells whose value is below the significance threshold
# conditional_format <- function(c) {
#   alpha <- 0.05
#   cell_spec(format(c, scientific=TRUE, digits=3), 
#             background = ifelse(is.na(c) | c >= alpha, "white", "red")) 
# }
# 
# pvalue_table <- function(df) {
#   df %>% 
#     mutate(across(any_of(names(cohort_colors)), conditional_format)) %>% 
#     #mutate(all = conditional_format(all)) %>% 
#     #kable(escape = FALSE) %>% 
#     kable(escape = FALSE) %>% 
#     #kable(escape = FALSE, format.args = list(scientific=TRUE, digits=3)) %>% 
#     kableExtra::kable_styling()
# }














