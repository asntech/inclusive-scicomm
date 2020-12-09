library('ggplot2')
library('tidyverse')
library('cowplot')
library('europepmc')

#set the output path
project_path = "~/projects/inclusive-naming/inclusive-scicomm/"

dir.create(paste0(project_path,"figures"), recursive = TRUE)
dir.create(paste0(project_path,"data"), recursive = TRUE)

year_cutoff <- 1999

colorsPalette <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#b15928','#ff7f00','#cab2d6','#6a3d9a','#ffff99')

#color_scale_fill <- scale_fill_brewer(palette = "Dark2")
color_scale_fill <- scale_fill_manual(values=colorsPalette)

query <- '(blacklist OR blacklisted OR “black-listed” OR “black-list” OR blacklisting OR whitelist OR whitelisted OR “white-listed” OR “white-list” OR whitelisting) AND (FIRST_PDATE:[2000-01-01 TO 2020-12-31]) NOT (SRC:PPR)'
print(query)
term_trends <- epmc_hits_trend(query, period= 1900:2020, synonym = FALSE)
plot_1Sa <- ggplot(term_trends, aes(year, query_hits)) + 
  geom_point(color="#00AFBB", size=0.6) + geom_line(color="#00AFBB", size=0.6) + theme_cowplot(12) +
  labs(x="Year published", y="Number of articles with terms blacklist/whitelist")
print(plot_1Sa)
write.table(term_trends,paste0(project_path,"data/","Figure_1a_1Sa.tsv"), quote = FALSE, row.names = FALSE, sep = "\t")

terms_all_df <- europepmc::epmc_search(query, limit=4000)
terms_all_df <- terms_all_df %>% mutate_if(is.character, 
                                                 str_replace_all, pattern = "Br Med J", replacement = "BMJ")

write.table(terms_all_df,paste0(project_path,"data/","Figure_1b_1Sb.tsv"), quote = FALSE, row.names = FALSE, sep = "\t")

top_journals = names(sort(table(terms_all_df$journalTitle),decreasing=TRUE)[1:50])
plot_1Sb = ggplot(terms_all_df[which(terms_all_df$journalTitle %in% as.vector(top_journals)),], aes(journalTitle)) + geom_bar() + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(x = "Name of Journals", y = "Number of articles") + color_scale_fill +
  scale_x_discrete(limits = rev(top_journals)) + theme_cowplot(12) + coord_flip() + theme(legend.justification=c(1,0), legend.position=c(0.95, 0.05)) 
print(plot_1Sb)

plot_1S <- plot_grid(plot_1Sa, plot_1Sb, ncol = 2, labels=c('a','b'), rel_widths = c(1, 1))
save_plot(paste0(project_path,"figures/","Figure_1S.pdf"), plot_1S, base_height=10, base_width =16)

### Plot Figure 1 ####
term_trends_subset <- term_trends[which(term_trends$year > year_cutoff),]
plot_1a <- ggplot(term_trends_subset, aes(year, query_hits)) + 
  geom_point(color="#00AFBB", size=3) + geom_line(color="#00AFBB", size=1) + theme_cowplot(12) +
  labs(x="Year published", y="Number of articles with terms blacklist/whitelist")
print(plot_1a)

terms_all_subset <- terms_all_df[which(terms_all_df$pubYear > year_cutoff),]

terms_all_subset$groupYear <- terms_all_subset$pubYear
terms_all_subset$groupYear[terms_all_subset$pubYear > 1999 & terms_all_subset$pubYear <  2011] <- "2000-2010"

print("No of articles:")
print(nrow(terms_all_subset))
print("No of Journals:")
print(length(unique(terms_all_subset$journalTitle)))

top_journals = names(sort(table(terms_all_subset$journalTitle),decreasing=TRUE)[1:25])
plot_1b <- ggplot(terms_all_subset[which(terms_all_subset$journalTitle %in% as.vector(top_journals)),], aes(journalTitle, fill = factor(groupYear))) + geom_bar() + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(x = "Name of Journals", y = "Number of articles", fill="Year published") + color_scale_fill +
  scale_x_discrete(limits = rev(top_journals)) + theme_cowplot(12) + coord_flip() + theme(legend.justification=c(1,0), legend.position=c(0.95, 0.05)) 
plot_1b

plot_1 <- plot_grid(plot_1a, plot_1b, ncol = 2, labels=c('a','b'), rel_widths = c(1, 1.5))

#save_plot(paste0(project_path,"figures/","Figure_1a.pdf"), plot_1a, base_height=5, base_width =4)
#save_plot(paste0(project_path,"figures/","Figure_1b.pdf"), plot_1b, base_height=5, base_width =6)
save_plot(paste0(project_path,"figures/","Figure_1.pdf"), plot_1, base_height=5, base_width =11)


################# Query for terms: Master and Slave ######################

query <- '("master" AND "slave") NOT (SRC:PPR)'
print(query)
ms_term_trends <- epmc_hits_trend(query, period= 1900:2020, synonym = FALSE)
plot_2Sa <- ggplot(ms_term_trends, aes(year, query_hits)) + 
  geom_point(color="#00AFBB", size=0.6) + geom_line(color="#00AFBB", size=0.6) + theme_cowplot(12) +
  labs(x="Year published", y="Number of articles with terms master/slave")
print(plot_2Sa)
write.table(ms_term_trends,paste0(project_path,"data/","Figure_2a_2Sa.tsv"), quote = FALSE, row.names = FALSE, sep = "\t")


ms_terms_all_df <- europepmc::epmc_search(query, limit=5000)
ms_terms_all_df <- ms_terms_all_df %>% mutate_if(is.character, 
                                           str_replace_all, pattern = "Br Med J", replacement = "BMJ")

write.table(ms_terms_all_df,paste0(project_path,"data/","Figure_2b_2Sb.tsv"), quote = FALSE, row.names = FALSE, sep = "\t")

top_journals = names(sort(table(ms_terms_all_df$journalTitle),decreasing=TRUE)[1:50])
plot_2Sb = ggplot(ms_terms_all_df[which(ms_terms_all_df$journalTitle %in% as.vector(top_journals)),], aes(journalTitle)) + geom_bar() + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(x = "Name of Journals", y = "Number of articles") + color_scale_fill +
  scale_x_discrete(limits = rev(top_journals)) + theme_cowplot(12) + coord_flip() + theme(legend.justification=c(1,0), legend.position=c(0.95, 0.05)) 
print(plot_2Sb)

plot_2S <- plot_grid(plot_2Sa, plot_2Sb, ncol = 2, labels=c('a','b'), rel_widths = c(1, 1))
save_plot(paste0(project_path,"figures/","Figure_2S.pdf"), plot_2S, base_height=10, base_width =16)

###Plot Figure 2S ####
ms_term_trends_subset <- ms_term_trends[which(ms_term_trends$year > year_cutoff),]
plot_2a <- ggplot(ms_term_trends_subset, aes(year, query_hits)) + 
  geom_point(color="#00AFBB", size=3) + geom_line(color="#00AFBB", size=1) + theme_cowplot(12) +
  labs(x="Year published", y="Number of articles with terms master/slave")
print(plot_2a)

ms_terms_all_subset <- ms_terms_all_df[which(ms_terms_all_df$pubYear > year_cutoff),]

ms_terms_all_subset$groupYear <- ms_terms_all_subset$pubYear
ms_terms_all_subset$groupYear[ms_terms_all_subset$pubYear > 1999 & ms_terms_all_subset$pubYear <  2011] <- "2000-2010"

print("No of articles:")
print(nrow(ms_terms_all_subset))
print("No of Journals:")
print(length(unique(ms_terms_all_subset$journalTitle)))

top_journals = names(sort(table(ms_terms_all_subset$journalTitle),decreasing=TRUE)[1:25])
plot_2b <- ggplot(ms_terms_all_subset[which(ms_terms_all_subset$journalTitle %in% as.vector(top_journals)),], aes(journalTitle, fill = factor(groupYear))) + geom_bar() + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(x = "Name of Journals", y = "Number of articles", fill="Year published") + color_scale_fill +
  scale_x_discrete(limits = rev(top_journals)) + theme_cowplot(12) + coord_flip() + theme(legend.justification=c(1,0), legend.position=c(0.95, 0.05)) 
print(plot_2b)

plot_2 <- plot_grid(plot_2a, plot_2b, ncol = 2, labels=c('a','b'), rel_widths = c(1, 1.5))

#save_plot(paste0(project_path,"figures/","Figure_2a.pdf"), plot_2a, base_height=5, base_width =4)
#save_plot(paste0(project_path,"figures/","Figure_2b.pdf"), plot_2b, base_height=5, base_width =6)
save_plot(paste0(project_path,"figures/","Figure_2.pdf"), plot_2, base_height=5, base_width =11)


## Query for terms: master TFs or master regulators 

query <- '("master TFs" OR "Master transcription factor" OR "master regulator" OR "master TF") NOT (SRC:PPR)'
print(query)
mtf_term_trends <- epmc_hits_trend(query, period= 1900:2020, synonym = FALSE)
plot_trends <- ggplot(mtf_term_trends, aes(year, query_hits)) + 
  geom_point(color="#00AFBB", size=0.6) + geom_line(color="#00AFBB", size=0.6) + theme_cowplot(12) +
  labs(x="Year published", y="Number of articles with terms master TF/regulator")
print(plot_trends)

write.table(mtf_term_trends,paste0(project_path,"data/","Figure_3Sa.tsv"), quote = FALSE, row.names = FALSE, sep = "\t")

mtf_terms_all_df <- europepmc::epmc_search(query, limit=52100)
mtf_terms_all_df <- mtf_terms_all_df %>% mutate_if(is.character, 
                                                 str_replace_all, pattern = "Br Med J", replacement = "BMJ")

write.table(mtf_terms_all_df,paste0(project_path,"data/","Figure_3Sb.tsv"), quote = FALSE, row.names = FALSE, sep = "\t")

top_journals = names(sort(table(mtf_terms_all_df$journalTitle),decreasing=TRUE)[1:50])
plot_topjournals = ggplot(mtf_terms_all_df[which(mtf_terms_all_df$journalTitle %in% as.vector(top_journals)),], aes(journalTitle)) + geom_bar() + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(x = "Name of Journals", y = "Number of articles") + color_scale_fill +
  scale_x_discrete(limits = rev(top_journals)) + theme_cowplot(12) + coord_flip() + theme(legend.justification=c(1,0), legend.position=c(0.95, 0.05)) 
print(plot_topjournals)

#plot_supp <- plot_grid(plot_trends, plot_topjournals, ncol = 2, labels=c('a','b'), rel_widths = c(1, 1))
#save_plot(paste0(project_path,"figures/","Figure_4S.pdf"), plot_supp, base_height=10, base_width =16)

###Plot Figure S3 ####
ms_term_trends_subset <- mtf_term_trends[which(mtf_term_trends$year > year_cutoff),]
plot_3Sa <- ggplot(ms_term_trends_subset, aes(year, query_hits)) + 
  geom_point(color="#00AFBB", size=3) + geom_line(color="#00AFBB", size=1) + theme_cowplot(12) +
  labs(x="Year published", y="Number of articles with terms master TF/regulator")
print(plot_3Sa)

mtf_terms_all_subset <- mtf_terms_all_df[which(mtf_terms_all_df$pubYear > year_cutoff),]

mtf_terms_all_subset$groupYear <- mtf_terms_all_subset$pubYear
mtf_terms_all_subset$groupYear[mtf_terms_all_subset$pubYear > 1999 & mtf_terms_all_subset$pubYear <  2011] <- "2000-2010"

print("No of articles:")
print(nrow(mtf_terms_all_subset))
print("No of Journals:")
print(length(unique(mtf_terms_all_subset$journalTitle)))

top_journals = names(sort(table(mtf_terms_all_subset$journalTitle),decreasing=TRUE)[1:25])
plot_3Sb <- ggplot(mtf_terms_all_subset[which(mtf_terms_all_subset$journalTitle %in% as.vector(top_journals)),], aes(journalTitle, fill = factor(groupYear))) + geom_bar() + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(x = "Name of Journals", y = "Number of articles", fill="Year published") + color_scale_fill +
  scale_x_discrete(limits = rev(top_journals)) + theme_cowplot(12) + coord_flip() + theme(legend.justification=c(1,0), legend.position=c(0.95, 0.05)) 
print(plot_3Sb)

plot_3S <- plot_grid(plot_3Sa, plot_3Sb, ncol = 2, labels=c('a','b'), rel_widths = c(1, 1.5))

#save_plot(paste0(project_path,"figures/","Figure_3Sa.pdf"), plot_3Sa, base_height=5, base_width =4)
#save_plot(paste0(project_path,"figures/","Figure_3Sb.pdf"), plot_3Sb, base_height=5, base_width =6)
save_plot(paste0(project_path,"figures/","Figure_3S.pdf"), plot_3S, base_height=5, base_width =11)

