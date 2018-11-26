

## load data and packages
library(dplyr)
library(tm)
library(wordcloud)
library(data.table)
library(ggplot2)

## theme function
theme_new <- function(base_size = 24, base_family = "Helvetica"){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid = element_blank(),   
      panel.border = element_rect(fill = NA, colour = "white", size=1),
      panel.background = element_rect(fill = "white", colour = "black"), 
      strip.background = element_rect(fill = NA),
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black")
    )
}


## read the data
dtm_briefs = fread('/Users/raj2/Dropbox/endrew_amicusbriefs/output/endrew_amicus_dtm.csv')

## separate
new_colnames = make.unique(colnames(dtm_briefs))
colnames(dtm_briefs) = new_colnames
dtm_briefs_parents = dtm_briefs %>%
                  filter(who_filed_for == 'parent')

dtm_briefs_district = dtm_briefs %>%
  filter(who_filed_for == 'district')

## find colsums and sort by frequency
dtm_briefs_colsums_parents = colSums(dtm_briefs_parents[,setdiff(colnames(dtm_briefs_parents),
                                                          c('org',
                                                            'who_filed_for'))])

dtm_briefs_sorted_p = data.frame(terms = names(sort(dtm_briefs_colsums_parents, decreasing = TRUE)),
                                freq = sort(dtm_briefs_colsums_parents, decreasing = TRUE)) %>%
                      mutate(source = 'parents',
                             prop = freq/sum((dtm_briefs_colsums_parents)))

head(dtm_briefs_sorted_p)

## find colsums and sort by frequency
dtm_briefs_colsums_district = colSums(dtm_briefs_district[,setdiff(colnames(dtm_briefs_district),
                                                                 c('org',
                                                                   'who_filed_for'))])

dtm_briefs_sorted_d = data.frame(terms = names(sort(dtm_briefs_colsums_district, decreasing = TRUE)),
                                 freq = sort(dtm_briefs_colsums_district, decreasing = TRUE)) %>%
                      mutate(source = 'district',
                             prop = freq/sum((dtm_briefs_colsums_district)))
                  

## add and then do ranks/scatterplot
dtm_briefs_both = rbind.data.frame(dtm_briefs_sorted_p, dtm_briefs_sorted_d) %>%
                arrange(desc(prop)) %>%
                dplyr::select(-freq) %>%
                dcast(terms ~ source, value.var = 'prop') %>%
                mutate(average_both = (parents + district)/2) %>%
                arrange(desc(average_both))

dtm_briefs_top100 = dtm_briefs_both %>%
              slice(2:100) %>%
              mutate(text_size = average_both/2)

head(dtm_briefs_both)

## Three plots

### plot one: blank
ggplot(dtm_briefs_top100, aes(x = district, y = parents)) +
  #geom_point(color = 'white') +
  #geom_text(aes(label=terms, 
       #         size = text_size,
        #        color = 'white'),
         #   alpha = 0.8, 
          #  hjust=1, vjust=1) +
  theme_new() +
  guides(size = FALSE,
         color = FALSE) +
  geom_abline(intercept = 0, slope = 1,
              linetype = 'dashed',
              color = 'red') +
  xlab('Briefs in favor of the district') +
  ylab('Briefs in favor of the parents') +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = 'black'))

ggsave('/Users/raj2/Dropbox/endrew_amicusbriefs/output/blank_amicus.pdf',
       plot = last_plot(),
       device = 'pdf',
       width = 12, 
       height = 8)


### plot two: all words
ggplot(dtm_briefs_top100, aes(x = district, y = parents)) +
  geom_point(color = 'white') +
  geom_text(aes(label=terms),
          alpha = 0.8, 
          hjust=1, vjust=1, 
          size = 6) +
  theme_new() +
  guides(size = FALSE,
         color = FALSE) +
  geom_abline(intercept = 0, slope = 1,
              linetype = 'dashed',
              color = 'red') +
  xlab('Briefs in favor of the district') +
  ylab('Briefs in favor of the parents') +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = 'black'))

ggsave('/Users/raj2/Dropbox/endrew_amicusbriefs/output/amicus_allwords.pdf',
       plot = last_plot(),
       device = 'pdf',
       width = 12, 
       height = 8)

## focus on lower ranked words
dtm_briefs_ratio = dtm_briefs_both %>%
  mutate(text_size = average_both/2,
         ratio_district = district/parents) %>%
  filter(!terms %in% c('http', 'whether',
                       'would',
                       'must',
                       'also',
                       'use', 'et', 
                       'html', 'al', 'cation', 'tri', 'pet', 'rep', 
                       'br')) %>%
  filter(district !=0 & parents !=0) %>%
  arrange(desc(ratio_district)) %>%
  mutate(ratio_district_rank = dense_rank(desc(ratio_district)),
         ratio_parent_rank = dense_rank(ratio_district),
         topword_parent = ifelse(ratio_parent_rank < 20, 1, 0),
         topword_district = ifelse(ratio_district_rank < 15, 1, 0),
         similar_both = ifelse(abs(ratio_district-1) < 0.03, 1, 0),
         category_ratio = ifelse(topword_district == 1, 'Over-represented in briefs favoring district',
                                 ifelse(topword_parent == 1, 
                                  'Over-represented in briefs favoring parents', 
                                  ifelse(similar_both == 1,
                                 'Similar representation', 'Other')))) %>%
  mutate(term_ordered = factor(terms, unique(terms))) %>%
  dplyr::select(term_ordered, ratio_district,
                category_ratio) %>%
  filter(category_ratio != 'Other')

write.csv(dtm_briefs_ratio, '../endrew_amicus_visualize/data_todisplay/dtm_ratio.csv',
          row.names = FALSE)

## do filtering based on slider in shiny
## reorganize 
theme_ratio <- function(base_size = 16, base_family = "Helvetica"){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid = element_blank(),   
      panel.border = element_rect(fill = NA, colour = "white", size=1),
      panel.background = element_rect(fill = "white", colour = "black"), 
      strip.background = element_rect(fill = NA),
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black")
    )
}


ggplot(dtm_briefs_ratio %>% filter(topword_district == 1), 
  aes(x = term_ordered, y = ratio_district)) +
  geom_bar(stat = 'identity', fill = 'wheat4', alpha = 0.3) +
  geom_hline(yintercept = 1, linetype = 'dashed', color = 'red') +
  coord_flip() +
  xlab('') + 
  theme_ratio() +
  ylab('Representation of word in briefs supporting district\nrelative to briefs supporting parents\n(> 1 = over-represented in district;\n< 1 = over-represented in parents)')

ggplot(dtm_briefs_lower, aes(x = district, y = parents)) +
  geom_point(color = 'white') +
  geom_text(aes(label=terms),
            alpha = 0.7, 
            hjust=1, vjust=1,
            size = 6) +
  theme_new() +
  guides(size = FALSE,
         color = FALSE) +
  geom_abline(intercept = 0, slope = 1,
              linetype = 'dashed',
              color = 'red') +
  xlab('Briefs in favor of the district') +
  ylab('Briefs in favor of the parents') +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = 'black')) 

ggsave('/Users/raj2/Dropbox/endrew_amicusbriefs/output/amicus_zoom.pdf',
       plot = last_plot(),
       device = 'pdf',
       width = 12, 
       height = 8)

