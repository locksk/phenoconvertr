# phenoconvertR package
# See README.md

#' Calculates phenoconversion corrected activity scores
#'
#' Given a data.frame containing enzyme activity scores and concomitant medication phenoconverteR
#' converts the scores using the conversion factors specified in the key.
#'
#'
#' @param input_data data.frame with activity scores and concomitant medication
#' @param id individual identifier column
#' @param enzyme option that selects enzyme upon which phenoconversions to be applied
#' @param activity_score column in x that stores enzyme activity scores
#' @param medication column in x that stores names of concomitant medication
#'
#' @return returns dataframe with additional column containing phenoconversion corrected activity scores.
#'
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import emojifont
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
#'  \dontrun{
#' phenoconvert(input_data, "Study_ID", "CYP1A2", "CYP1A2_AS",  "other_medication")
#' }




phenoconvert <- function(input_data, id, enzyme, activity_score,  medication){
  x <- input_data

  check_params(x,  id, enzyme, activity_score,  medication)

  ref <- phenoconverter::ref

  # create factors for multiplication/addition
  # moderate and strong inhibitors, moderate and strong inducers only important
  cat(paste("Starting ..."))
  filter_enzymes <- function(enzyme){
    # filter by the selected enzyme
    ref_enzyme <- dplyr::select(ref, c("substance", all_of(enzyme)))
    colnames(ref_enzyme) <- c("substance", "pgx_enzyme")

    # select only drugs that have a moderate/strong effect
    ref_filtered <- ref_enzyme %>% dplyr::filter(grepl('strong|moderate', pgx_enzyme))

    # create multiplier for inhibitors
    ref_filtered <- ref_filtered %>%
      mutate(multiplier = case_when(
        grepl(pattern = "strong inhibitor", x = pgx_enzyme) ~ 0,
        grepl(pattern = "moderate inhibitor", x = pgx_enzyme) ~ 0.5,
        grepl(pattern = "inducer", x = pgx_enzyme) ~ 1.5)) #%>%
    #  mutate(addition = case_when(
    #    grepl(pattern = "strong inducer", x = pgx_enzyme) ~ 1,
    #    grepl(pattern = "moderate inducer", x = pgx_enzyme) ~ 1,
    #    grepl(pattern = "inhibitor", x = pgx_enzyme) ~ 0)) #%>%
    # mutate(keep = case_when(
    #  multiplier + addition == 2 ~ TRUE,
    # multiplier + addition < 1 ~ TRUE,
    #multiplier + addition >= 1 && multiplier + addition < 2 ~ FALSE))



    # if multiplier (1) and addition (1)  :) combined score of 2 is fine (only 2)
    # if multiplier >1 and addition (0) :)   combined score of < 1 is fine (possibly 0 or 0.5)
    # if multiplier >1 and addition (1) :(   combined score of > 1 and < 2 is not fine (may be 1 or 1.5)
    # as we do not have strong evidence on what happens when you have an inducer and inhibitor acting upon the same enzyme.
    # add this in later once I have figured the rest out

    rm(ref_enzyme)
    return(ref_filtered)
  }

  ref_tmp <- filter_enzymes(enzyme)

  in_tmp <- dplyr::select(x, c(all_of(id), all_of(activity_score),  all_of(medication)))
  colnames(in_tmp) <- c(id, "AS", "substance")
  in_tmp$substance <- tolower(in_tmp$substance)
  in_tmp$substance[in_tmp$substance == ""] <- NA

  # accounting for multiple relevant concomittant medications
  # if there are multiple drugs present in the medication column then complete following steps:
  # 1. check for duplicate drugs across rows
  # 2. check to see if they are present in list of relevant drugs
  # if present keep, if not drop
  # if still multiple variables then check if contradiction (if one is an inhibitor and one is an inducers) - if so replace cell with NA
  # FUTURE: if still multiple variables then take the drug of the strongest effect, if both the same take the first drug.

  in_tmp$multiple <- ifelse(grepl(",", in_tmp$substance),
                            TRUE,
                            FALSE)

  # check whether multiple drugs are present
  # and if so prioritise by those that are actually relevant to the enzyme we are looking at
  # this is based on the substances in the enzyme-filtered reference file (ref_tmp)

  cat(paste("Checking whether multiple medications present in ", medication, " ...\n"))



  if (any(in_tmp$multiple)){

    cat(paste("Removing duplicates and filtering by drugs relevant to ", enzyme, " phenoconversion ...\n"))

    # get a list of drugs that are relevant to the enzyme we are converting
    relevant_drugs <- ref_tmp$substance

    # split the substances column into as many columns as relevant medications exist
    M1s <- str_split(in_tmp$substance, "[,\\s]+")
    M1s <- do.call(rbind, lapply(M1s, `length<-`, max(lengths(M1s))))
    colnames(M1s) <- paste0("M1.", seq_len(ncol(M1s)))
    expanded_subs <- cbind(subset(in_tmp, select = -substance), M1s)

    # trim white space
    for (i in 4:ncol(expanded_subs)){
      expanded_subs[[i]] <- str_trim(expanded_subs[[i]])
    }

    # remove any duplicates
    # code from Gwang-Jin Kim https://stackoverflow.com/questions/61961909/replacing-duplicates-within-a-row-with-na-keeping-the-first-in-r-in-the-row
    replace.dup <- function(x, val=NA) {
      x[duplicated(x)] <- val
      x
    }

    replace.row.wise.dups <- function(df, val=NA) {
      for (i in 1:nrow(df)) {
        df[i, ] <- replace.dup(unlist(df[i, , drop=T]), val)
      }
      df
    }

    de_dup <- replace.row.wise.dups(df = expanded_subs[-c(2:3)], val = NA)

    # remove irrelevant drugs
    for (i in 2:ncol(de_dup)){
      de_dup[[i]] <- str_trim(de_dup[[i]])
      de_dup[[i]] <- replace(de_dup[[i]], !(de_dup[[i]] %in% relevant_drugs), NA)
    }

    drugs <- unique(na.omit(as.vector(as.matrix(de_dup[-1]))))

    cat(paste0("Relevant drugs for ", enzyme, " in data include: ", drugs))
    cat(paste("\n"))




    #expanded_subs$M1.1[expanded_subs$M1.1 == "cigarette"] <- "cigarettes"
    #expanded_subs$M1.2[expanded_subs$M1.2 == "cigarette"] <- "cigarettes"
    cat(paste("done!\n"))

    # join together with comma
    de_dupm <- unite(de_dup, substance_relevant,  -1, sep = ", ", remove = FALSE, na.rm = TRUE)
    de_dupm[[id]] <- as.character(de_dupm[[id]])
    in_tmp[[id]] <- as.character(in_tmp[[id]])


    in_tmp1 <- left_join(in_tmp, de_dupm, by = all_of(id))

    # only keep useful columns
    in_tmp1 <- dplyr::select(in_tmp1, c(all_of(id), "AS", "substance_relevant"))
    in_tmp1$substance_relevant[in_tmp1$substance_relevant == ""] <- NA


    # check if there are still multiple drugs present
    in_tmp1$multiple <- ifelse(grepl(",", in_tmp1$substance_relevant),
                               TRUE,
                               FALSE)


    in_tmp <- in_tmp1
    names(in_tmp)[names(in_tmp) == 'substance_relevant'] <-  "substance"


    if (any(in_tmp$multiple)){
      cat(paste("Still multiple drugs present in ", medication, "...\n"))
      cat(paste("Prioritising ...\n"))



      # split the substances column into as many columns as relevant medications exist
      M1s <- str_split(in_tmp$substance, "[,\\s]+")
      M1s <- do.call(rbind, lapply(M1s, `length<-`, max(lengths(M1s))))
      colnames(M1s) <- paste0("M1.", seq_len(ncol(M1s)))
      expanded_subs <- cbind(subset(in_tmp, select = -substance), M1s)

      exp_mult <- dplyr::filter(expanded_subs, multiple == TRUE)

      # Merge dataframes - needs to be flexible for number of drugs remaining
      col_M <- colnames(exp_mult[-c(1:3)])

      df_list <- list()
      for (i in col_M) {
        exp_tmp <- dplyr::select(exp_mult, c(all_of(id), all_of(i)))
        names(exp_tmp)[names(exp_tmp) == i ] <- 'substance'
        exp_tmp[[2]] <- str_trim(exp_tmp[[2]])

        comp_tmp <- left_join(exp_tmp, ref_tmp, by = 'substance' )
        df_list[[i]] <- comp_tmp
        assign(paste0("comp_", i), comp_tmp)
      }


      # Function to prioritize drugs for each ID across dataframes
      prioritize_drugs_across <- function(df_list) {
        result <- list()

        # Create a data frame to store the overall effects for each ID
        overall_effects <- data.frame(ID = unique(unlist(lapply(df_list, function(df) df[all_of(id)]))))

        for (df in df_list) {
          # Check if an ID is taking both an inhibitor and inducer across dataframes
          overall_effects <- merge(overall_effects, df[, c(all_of(id), "pgx_enzyme")], by = id, all.x = TRUE)
        }

        overall_effects$combined_effect <- apply(overall_effects[, -1], 1, function(row) {
          if (any(grepl("inhibitor", row)) && any(grepl("inducer", row))) {
            return("both")
          }
          # if (any(grepl("inducer", row)) && any(grepl("inducer", row))) {
          #  return("either")
          #}
          #if (any(grepl("strong inhibitor", row)) && any(grepl("strong inhibitor", row))) {
          #  return("either")
          #}
          else {
            return(paste(row, collapse = "/"))
          }


        })

        for (df in df_list) {
          df <- merge(df, overall_effects[, c(all_of(id), "combined_effect")], by = id, all.x = TRUE)

          # Apply prioritization logic based on the combined effects

          df$priority[df$combined_effect == "both"] <- "CONTRA"
          # df$priority[df$combined_effect == "either"] <- "EITHER"

          #if any(grepl("/", df$combined_effect){
          #  df$combined_effect <- gsub("moderate inhibitor", "")
          #}

          result[[length(result) + 1]] <- df
        }

        return(result)
      }

      # Apply the function
      df3_list <- prioritize_drugs_across(df_list)

      my_names = c(id, "priority")
      # these columns might be at different positions in the data frames
      result = lapply(df3_list, "[", , my_names)

      # Combine the resulting dataframes into a single dataframe (df3)
      df3 <- do.call(rbind, result) %>% unique()

      # Display the result

      in_tmp <- left_join(in_tmp, df3, by = id)

      in_tmp$AS_P <- ifelse(grepl("CONTRA", in_tmp$priority), NA, in_tmp$AS)

      cat(paste("Multiple drugs resolved after further prioritisation ...\n"))
    }




  }




  tmp <- left_join(in_tmp, ref_tmp, by = "substance")
  names(tmp[2]) <- "AS"

  tmp$pc <- (as.numeric(tmp$AS) * as.numeric(tmp$multiplier))


  # tmp$pc <- (as.numeric(tmp$AS) * as.numeric(tmp$multiplier) + as.numeric(tmp$addition))
  #tmp$pc[tmp$pc > 3] <- 3 #people cannot become super-ultra-rapid-very-fast-metabolisers so AS = 3 is the cap

  tmp <- mutate(
    tmp,
    new = coalesce(as.numeric(pc), as.numeric(AS)))

  pcas <- dplyr::select(tmp, c(all_of(id), "new"))

  names(pcas)[names(pcas) == "new"] <- paste0("PC_", activity_score)  # add PC to AS variable

  input_data[[id]] <- as.character(input_data[[id]])
  pcas[[id]] <- as.character(pcas[[id]])


  cat(paste("Calculating Phenoconversion-corrected Activity Scores for ", enzyme, " ...\n"))

  out <- left_join(input_data, pcas, by = all_of(id))

  n <- sum(out[[activity_score]] != out[, ncol(out)], na.rm = T)

  cat(paste("Phenoconversions applied to ", n, " people ...\n"))


  cat(paste0("Output found in PC_", enzyme, "_AS ", emoji(search_emoji('hat'))[1] ,"\n"))

  return(out) }



#' @noRd

check_params <- function(x,  id, enzyme, activity_score,  medication) {
  if (!(is.data.frame(x))) stop("x must be a data.frame.")

  if (!(id %in% names(x))) stop("id must be a variable in x. For example, 'Study_ID'. ")

  if (!(toupper(enzyme) %in% names(ref[-1]))) stop("enzyme must be included in the reference dataset. Included enzymes are: 'CYP1A2', 'CYP2C19', 'CYP2C9', 'CYP2D6', 'CYP3A4'.")

  if (!(activity_score %in% names(x))) stop("activity_score must be a variable in x. For example, 'CYP2C19_AS' or 'Activity_score' or similar.")

  if (!(medication %in% names(x))) stop("medication must be a variable in x. For example, 'drugs' or 'other_meds' or similar.")
}

#' @noRd

check_params_tc <- function(input_data,  id,  variable_names) {
  if (!(is.data.frame(input_data))) stop("input_data must be a data.frame.")

  if (!(id %in% names(input_data))) stop("id must be a variable in input_data. For example, 'Study_ID'. ")

  if (!(is.character(variable_names))) stop("variable_names must be variables containing medication information found within input_data. For example, c('other_meds1', 'other_meds2', 'other_meds3').")
}

#' @noRd

transform_variable <- function(data, variable_name) {
  # Replace all medications that are not relevant to phenoconversion with NA (i.e., not present in the reference table)
  # load("data/reference_pgx.Rda")
  key_values <- unique(ref$substance)
  data[[variable_name]] <- replace(data[[variable_name]], !(data[[variable_name]] %in% key_values), NA)
  return(data)
}

#' Function to re-code non-relevant medications and extract non-NA columns in instances where there are multiple medication columns
#' @param input_data data.frame with Study ID and multiple columns specifying concomitant medication
#' @param id column denoting Study ID
#' @param variable_names list of columns in input data that store names of concomitant medication
#'
#' @return returns dataframe with cleaner containing only relevant medications, with a summary column
#'
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' med_cols <- c("med1", "med2", "med3", "med4")
#' phenoconverter::transform_and_clean(input_data, med_cols)
#' }
#'
#' @export

transform_and_clean <- function(input_data, id, variable_names) {
  check_params_tc(input_data, id, variable_names)
  x <- dplyr::select(input_data, c(all_of(id)), all_of(variable_names))

  # load("data/reference_pgx.Rda")
 # key_values <- ref$substance
  # Loop through each variable and apply the transformation
  for (i in 2:ncol(x)){
    x[[i]] <- tolower(x[[i]])
  }

  for (variable_name in variable_names) {
    x <- transform_variable(x, variable_name)
  }

  # Extract non-NA columns
  tmp <- x[, colSums(is.na(x)) < nrow(x)]

  # create summary columns
  new_clean <- unite(tmp, substance_all,  -1 , sep = ", ", remove = FALSE, na.rm = TRUE)

  out <- left_join(new_clean[1:2], input_data, by = all_of(id))

  return(out)
}






