library("tidyverse")
library("clue")
library("kableExtra")
library("janitor")

#####Load long data#####
padres <- read_csv("../02Data/PadresLong2025.csv")

#####Create Function for Optimal Lineup#####
opt <- function(data, pitch){
	#Clean and Format Data
	mats <- data %>%
		mutate(WAA = dWAA + get(paste0(pitch, "WAA"))) %>%
		select(MLBAMID, Pos, WAA) %>%
		pivot_wider(id_cols = c("MLBAMID"), names_from = "Pos", values_from = "WAA",
					names_prefix = "ovr_") %>%
		column_to_rownames("MLBAMID") %>%
		mutate(across(everything(), ~ replace_na(.x, min(data[["dWAA"]] + data[[paste0(pitch, "WAA")]]))),
			   across(everything(), ~ .x + abs(min(data[["dWAA"]] + data[[paste0(pitch, "WAA")]])))) %>%
		as.matrix() %>%
		t()
	
	#Implement Kuhn-Munkres Algorithm
	algo <- solve_LSAP(mats, maximum = TRUE) %>%
		cbind(1 : length(.), .)
	
	#Identify Optimal Players' Locations
	loc <- matrix(nrow = nrow(mats), ncol = ncol(mats))
	loc[algo] <- 1
	
	#Pull Optimal Players from Original Matrix
	lineup <- t(loc * mats) %>%
		as.data.frame() %>%
		rownames_to_column(var = "MLBAMID") %>%
		pivot_longer(cols = starts_with("ovr"), names_to = "Pos", 
					 values_to = "WAA", names_pattern = "_([^_]+)$") %>%
		filter(!is.na(WAA)) %>%
		mutate(WAA = WAA - abs(min(data[["dWAA"]] + data[[paste0(pitch, "WAA")]]))) %>%
		merge(data[, c("MLBAMID", "Pos", "Name", paste0(pitch, "WAA"))], by = c("MLBAMID", "Pos")) %>%
		select(Name, Pos, paste0(pitch, "WAA"), WAA) %>%
		arrange(-get(paste0(pitch, "WAA"))) %>%
		mutate(Order = row_number()) %>%
		select(Order, Name : WAA) %>%
		adorn_totals(fill = " ", cols = c(Name, Pos, WAA))
	
	#Return Optimal Lineup
	return(lineup)
}

opt(padres, "r")