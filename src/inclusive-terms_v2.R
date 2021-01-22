library('ggplot2')
library('tidyverse')
library('cowplot')
library('europepmc')

#set the output path
project_path = "~/projects/inclusive-naming/inclusive-scicomm/"

dir.create(paste0(project_path,"figures"), recursive = TRUE)
dir.create(paste0(project_path,"data"), recursive = TRUE)

## limit serach year_cutoff:2020
year_cutoff <- 2000

# colors
colorsPalette <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#b15928','#ff7f00','#cab2d6','#6a3d9a','#ffff99')
color_blue <- '#1f78b4'
color_orange <- '#d95f02'
color_green <- '#1b9e77'
color_purple <- '#7570b3'

#color_scale_fill <- scale_fill_brewer(palette = "Dark2")
color_scale_fill <- scale_fill_manual(values=colorsPalette)


################  Trends of articles with terms per year ####################

## Query for terms blacklist and whitelist
query_1 <- '(blacklist OR blacklisted OR “black-listed” OR “black-list” OR blacklisting OR whitelist OR whitelisted OR “white-listed” OR “white-list” OR whitelisting) NOT (SRC:PPR)'

term_trends <- epmc_hits_trend(query_1, period= year_cutoff:2020, synonym = FALSE)

## Query for terms master and slave
query2 <- '("master" AND "slave") NOT (SRC:PPR)'

ms_term_trends <- epmc_hits_trend(query2, period= year_cutoff:2020, synonym = FALSE)

term_trends$query2_hits <- ms_term_trends$query_hits

## adjust the left y-axis scale
#scaleFactor <- max(term_trends$all_hits) / max(term_trends$query_hits)
coeff <- 3100

plot_1 <- ggplot(term_trends, aes(x=year)) +
  geom_line( aes(y=query_hits), size=0.8, color=color_blue) + 
  geom_line( aes(y=query2_hits), size=0.8, color=color_orange) + 
  geom_line( aes(y=all_hits / coeff), size=0.8, color=color_green) +
  
  scale_y_continuous(name = "Number of articles with non-inclusive terms",
                     sec.axis = sec_axis(~.*coeff, name="Total number of articles in Europe PMC")) + 
  theme_cowplot(12) +
  labs(x="Year published")

print(plot_1)
save_plot(paste0(project_path,"figures/","Figure_1.pdf"), plot_1, base_height=6, base_width =8)


## Query for terms: master TFs or master regulators

query3 <- '("master TFs" OR "Master transcription factor" OR "master regulator" OR "master TF") NOT (SRC:PPR)'
print(query3)
mtf_term_trends <- epmc_hits_trend(query3, period= year_cutoff:2020, synonym = FALSE)
term_trends$query3_hits <- mtf_term_trends$query_hits
query4 <- '("slave") NOT (SRC:PPR)'
print(query4)
s_term_trends <- epmc_hits_trend(query4, period= year_cutoff:2020, synonym = FALSE)
mtf_term_trends$query4_hits <- s_term_trends$query_hits
term_trends$query4_hits <- s_term_trends$query_hits
coeff <- 250

plot_S1 <- ggplot(mtf_term_trends, aes(x=year)) +
  geom_line(aes(y=query_hits), size=0.8, color=color_blue) + 
  geom_line(aes(y=query4_hits), size=0.8, color=color_orange) + 
  geom_line(aes(y=all_hits / coeff), size=0.8, color=color_green) +
  
  scale_y_continuous(name = "Number of articles with non-inclusive terms",
                     sec.axis = sec_axis(~.*coeff, name="Total number of articles in Europe PMC")) + 
  theme_cowplot(12) +
  #theme(legend.justification=c(0,1), legend.position=c(0.05, 0.95)) +
  labs(x="Year published") 

print(plot_S1)
save_plot(paste0(project_path,"figures/","Figure_S1.pdf"), plot_S1, base_height=6, base_width =8)

colnames(term_trends) <- c('year','total_hits_in_epmc','terms_blacklist_whitelist','terms_master_slave','terms_master_tfs','term_slave')
write.table(term_trends[c("year","total_hits_in_epmc","terms_blacklist_whitelist","terms_master_slave")],paste0(project_path,"data/","Figure_1.tsv"), quote = FALSE, row.names = FALSE, sep = "\t")
write.table(term_trends[c("year","total_hits_in_epmc","terms_master_tfs","term_slave")],paste0(project_path,"data/","Figure_S1.tsv"), quote = FALSE, row.names = FALSE, sep = "\t")


################  Count articles per journal title ####################

## for terms blacklist and whitelist
query_bw <- '(blacklist OR blacklisted OR “black-listed” OR “black-list” OR blacklisting OR whitelist OR whitelisted OR “white-listed” OR “white-list” OR whitelisting) AND (FIRST_PDATE:[2000-01-01 TO 2020-12-31]) NOT (SRC:PPR)'
terms_all_bw <- europepmc::epmc_search(query_bw, limit=4000)
terms_all_bw <- terms_all_bw %>% mutate_if(is.character, 
                                                 str_replace_all, pattern = "Br Med J", replacement = "BMJ")

journal_hits_bw <- as.data.frame(sort(table(terms_all_bw$journalTitle), decreasing=TRUE)[1:100])
colnames(journal_hits_bw) <- c('journal_title','no_of_articles')
write.table(journal_hits_bw,paste0(project_path,"data/","hits_per_journal_blacklist_whitelist.tsv"), quote = FALSE, row.names = FALSE, sep = "\t")

terms_all_bw$groupYear <- terms_all_bw$pubYear
terms_all_bw$groupYear[terms_all_bw$pubYear > 1999 & terms_all_bw$pubYear <  2011] <- "2000-2010"

top_journals_bw = names(sort(table(terms_all_bw$journalTitle),decreasing=TRUE)[1:50])
plot_S2a <- ggplot(terms_all_bw[which(terms_all_bw$journalTitle %in% as.vector(top_journals_bw)),], aes(journalTitle, fill = factor(groupYear))) + geom_bar() + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(x = "Name of Journals", y = "Articles with terms blacklist or whitelist ", fill="Year published") + color_scale_fill +
  scale_x_discrete(limits = rev(top_journals_bw)) + theme_cowplot(12) + coord_flip() + theme(legend.justification=c(1,0), legend.position=c(0.95, 0.05)) 
plot_S2a

print(query_bw)
print(paste("No of articles:",nrow(terms_all_bw)))
print(paste("No of Journals:",length(unique(terms_all_bw$journalTitle))))

## for terms master and slave
query_ms <- '("master" AND "slave") AND (FIRST_PDATE:[2000-01-01 TO 2020-12-31]) NOT (SRC:PPR)'
terms_all_ms <- europepmc::epmc_search(query_ms, limit=4000)
terms_all_ms <- terms_all_ms %>% mutate_if(is.character, 
                                           str_replace_all, pattern = "Br Med J", replacement = "BMJ")

journal_hits <- as.data.frame(sort(table(terms_all_ms$journalTitle), decreasing=TRUE)[1:100])
colnames(journal_hits) <- c('journal_title','no_of_articles')
write.table(journal_hits,paste0(project_path,"data/","hits_per_journal_master_slave.tsv"), quote = FALSE, row.names = FALSE, sep = "\t")

terms_all_ms$groupYear <- terms_all_ms$pubYear
terms_all_ms$groupYear[terms_all_ms$pubYear > 1999 & terms_all_ms$pubYear <  2011] <- "2000-2010"

top_journals_ms = names(sort(table(terms_all_ms$journalTitle),decreasing=TRUE)[1:50])

plot_S2b <- ggplot(terms_all_ms[which(terms_all_ms$journalTitle %in% as.vector(top_journals_ms)),], aes(journalTitle, fill = factor(groupYear))) + geom_bar() + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(x = "Name of Journals", y = "Articles with terms master and slave ", fill="Year published") + color_scale_fill +
  scale_x_discrete(limits = rev(top_journals_ms)) + theme_cowplot(12) + coord_flip() + theme(legend.justification=c(1,0), legend.position=c(0.95, 0.05)) 
plot_S2b
print(query_ms)
print(paste("No of articles:",nrow(terms_all_ms)))
print(paste("No of Journals:",length(unique(terms_all_ms$journalTitle))))

plot_S2_grid <- plot_grid(plot_S2a, plot_S2b, ncol = 2, labels=c('A','B'), rel_widths = c(1, 1))
save_plot(paste0(project_path,"figures/","Figure_S2.pdf"), plot_S2_grid, base_height=10, base_width =11)

################  ========== ####################