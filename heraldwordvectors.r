library(wordVectors)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggdendro)#for making my dendrogram more useful
#dataset is pages 3 to 9 of the New York Herald between 1870-1875 so it is extremely large, around 9-10 million words per year of text. So I decreased vectors to 35 and window to 5, no matter how much I troubleshot my vscode and rstudio crashed at more than 45 vectors
#In essence, I am interested in associations with a specific set of politically charged words in the 1870s, and whether or not words had changing sentiments or definitions. The sentiment is more related to the corpus itself, it is a newspaper so if there is a change in a word, it implies that the readers of the newspaper would integrate the change in their social understand. Either the newspaper is reflecting changing social attitudes, creating them, or both.  
#It must be stated that I am not speaking in absolute terms nor am I making sweeping claims with the data. I am just attempting to string together observations.

if (!file.exists("C:/Users/anton/Desktop/ocr_texts/herald1875.txt")) prep_word2vec(origin = "C:/Users/anton/Desktop/ocr_texts/herald1875.txt", 
destination = "C:/Users/anton/Desktop/ocr_texts/cleanherald1875.txt", lowercase = T, bundle_ngrams = 1)
#prepping text for processing, I am doing this for each year just by changing the year, so 1870,1871 etc


prep_word2vec(origin = "C:/Users/anton/Desktop/ocr_texts/herald1875.txt", 
destination = "C:/Users/anton/Desktop/ocr_texts/cleanherald1875.txt", lowercase = T, bundle_ngrams = 1)
#no need for if since i'm jusing one text file

if (!file.exists("C:/Users/anton/Desktop/ocr_texts/cleanherald1875.bin")) {
    model <- train_word2vec("C:/Users/anton/Desktop/ocr_texts/cleanherald1875.txt", "C:/Users/anton/Desktop/ocr_texts/cleanherald1875.bin" , vectors = 45, threads = 2, window = 5, iter = 2, negative_samples = 0)
} else {
    model <- read.vectors("C:/Users/anton/Desktop/ocr_texts/cleanherald1875.bin")
}
#made models for each year of cleaned text so moving on to loading all the years for a temporal analysis

years <- 1870:1875
models <- lapply(years, function(y) {
  path <- paste0("C:/Users/anton/Desktop/heraldtext/cleanherald", y, ".bin")
  read.vectors(path)
})
names(models) <- years
#writing a quick lapply to get all the models in one variable and making it into a list for easy access
#I can run this each time in order to load the models, or use models[["1870"]] to get specific model for specific year

#first thing I want to do is see in general how a word semantically shifts over time so I need to calculate cosine similarity for each year in comparison to the preceeding year
#I want to do republican first because scholars often point to the crumbling republican party in the 1870s as a potential reason Reconstruction lost support in the north.


cosineSimilarity(models[["1870"]][["republican"]], models[["1871"]][["republican"]])
cosineSimilarity(models[["1871"]][["republican"]], models[["1872"]][["republican"]])
cosineSimilarity(models[["1872"]][["republican"]], models[["1873"]][["republican"]])
cosineSimilarity(models[["1873"]][["republican"]], models[["1874"]][["republican"]])
cosineSimilarity(models[["1874"]][["republican"]], models[["1875"]][["republican"]])
#holy moly, these results are absolutely fascinating. There is a very clear shift(drift) in the context of the word republican over time.
#1870-1871 has a relatively high correlation which makes sense, this wasn't exactly a turbulent year but 1871-1872 there is a sharp drop in cosinesimilarity. This was the year leading up to an election year and liberal republican Horace Greeley was supported by democratic party.
#So republican would certainly have a turbulent shift in usage, it's being associated with Ulysses Grant, a republican endorsed by republican party, and Horace Greeley, a liberal republican supported by democratic party.
#1872-1873 similarity does not change but is still extremely low which means the shift that happened in 1872 most likely prompted a concrete shift, the election of 1872 shifted how the word republican was used or referenced in the New York Herald.
#1873-1874 is most telling, this was the year of the Panic of 1873 and the rise of the Democrats in the house of representatives. The republicans reconstruction platform was already crumbling and they floundered on a response to the panic, their supporting of the inflation bill followed by Grant's devasting veto demolished the republican platform.
#1874-1875 has the lowest similarity score which could denote the more drastic shift in the mention or usage of the word in the papers as residual effects of the failed inflation bill and the democrats gaining control of the house in 1874.
#While these results can't tell me exactly why the word changed, the change by year lines up with historical events. 


#Of course, I want to see if reconstruction was used differently or changed semantically as that could align with a shift in attitudes towards it.
cosineSimilarity(models[["1870"]][["reconstruction"]], models[["1871"]][["reconstruction"]])
cosineSimilarity(models[["1871"]][["reconstruction"]], models[["1872"]][["reconstruction"]])
cosineSimilarity(models[["1872"]][["reconstruction"]], models[["1873"]][["reconstruction"]])
cosineSimilarity(models[["1873"]][["reconstruction"]], models[["1874"]][["reconstruction"]])
cosineSimilarity(models[["1874"]][["reconstruction"]], models[["1875"]][["reconstruction"]])
#These results are far more stable and less linear. From 1870-1871 there is a shift in context, the negative cosinesim points to dissimilarity however it is relatively close to 0 which indicates very little change. 
#1871-1872 show a great change from the previous years and indicate a strong similarity in usage between 1871-1872, this makes sense because it was leading up to and including a presidential election.
#So reconstruction was probably used in some political rhetoric and that would flatten its usage.
#1872-1873 and 1873-1874 show somewhat low cosinesim however they are still positive which indicates correlation. 1873-1874 having a major shift from 1872-1873 could most likely be due to the Panic of 1873.
#However I do think it's important to point out that the cosinesim value is still technically positive and taking all the values as a set, it is almost in the middle of all the cosinesims. I would expect to see a negative value between 1872 and 1873 due to the Panic and furthermore, I would expect to see greater change between 1873-1874.
#These observations seem to point to the fact that the word reconstruction did not change much from year to year, this coupled with the relatively low frequency of the word indicates the word probably wasn't that important to the New York Herald.


years <- 1870:1875
cosine_republican <- c(.4229654, .02685359, .03089122, -.030891227, -0.04731644)
cosine_reconstruction <- c(-0.06913874, 0.3248846, 0.1915977, 0.03036662, 0.1130118)

#remove 1870 because it's the baseline year, won't work otherwise because no year to compare to 1870
years_compared <- years[-1]


ggplot() +
  geom_line(aes(x = years_compared, y = cosine_republican, color = "Republican"), size = 1.2) +
  geom_point(aes(x = years_compared, y = cosine_republican, color = "Republican"), size = 2.5) +
  #adding point chart to actually see the individual values 
  geom_line(aes(x = years_compared, y = cosine_reconstruction, color = "Reconstruction"), size = 1.2) +
  geom_point(aes(x = years_compared, y = cosine_reconstruction, color = "Reconstruction"), size = 2.5) +
  labs(
    title = "Semantic Drift of 'Republican' and 'Reconstruction' (1870–1875)",
    x = "Year",
    y = "Cosine Similarity to Previous Year",
    color = "Keyword"
  ) +
  scale_color_manual(values = c("Republican" = "blue", "Reconstruction" = "red")) +
  theme_minimal()

  #For this visualization, if the line is moving up the similarity to the previous year increased if it moves down it decreased but if it moved down and remained above 0 there is still similarity.
  #as a comparison, republican is seemingly being redefined since the value drops year to year and even moves negative. Reconstruction flunctuates but the usage/context remains relatively similar year to year compared to republican.
  #In essence, republican appears to be being redefined while Reconstruction appears to be relatively stable. This aligns with my thesis that Reconstruction had already began losing support prior to the Panic of 1873, as early as 1870



close.reconstruction <- lapply(1870:1875, function(year) {
  combined <- models[[as.character(year)]] %>% closest_to("reconstruction", 20)
  combined$year <- year 
  return(combined)
})
#this is simply combining the reconstruction results for all the years and assigning a year column to it
close.words.recon <- do.call(rbind, close.reconstruction)
#this binds the df together
?reorder
?coord_flip


ggplot(close.words.recon, aes(x = reorder(word, `similarity to "reconstruction"`), 
y = `similarity to "reconstruction"`)) +
  geom_col() + 
  coord_flip() +
  #coord flip to increase readability
  facet_wrap(~ year, scales = "free_y") +
  #facet wrap because there's multiple years
  labs(
    title = "Top Words Closest to 'Reconstruction' by Year",
    x = "Word",
    y = "Cosine Similarity"
  ) +
  theme_minimal()

#Comparing the top words closest to Reconstruction offers a rich ground for analysis. In 1870 the top closest word is retrenchment.
#Scholars typically point to the Panic of 1873 as a northern shift away from Reconstruction, especially in using economics as a justification. However, here retrenchment is closest to reconstruction in 1870.
#It must be stated that this graphic is showing closest_to and not nearest_to so retrenchment was not necessarily used alongside reconstruction. This still seems to hint at a failing support for reconstruction as early as 1870.
#In 1872 model you have words like abolition, emancipation, and even plunderers identified as closest to. I would point to the election for this set of words because the words are very political in nature. Plunderers especially points to some rhetorical narrative when viewed in the context of the 1870s.
#1873 is fascinating but not entirely coherent. It's mostly religious words, perhaps implying that Reconstruction was used metaphorically or symbolically? Maybe following the Panic Reconstruction was being as metaphorical rhetoric instead of specific events being covered.
#The 1874 and 1875 models have words that hint at a contestation of reconstruction, you have words like usupration and inflationists(which is highly contextual) in 1874 and in 1875 you have words like sovereignty, suppressing, violations, and enforcement.  
#This aligns with the general historical consensus, post 1873, especially after 1874 you see push back against Reconstruction and its policies. However the nuance here is that in 1870 and 1871 there are hints of a push back.
#words like retrenchment, indemnification, and iniquitous being identified as closest to Reconstruction years before the Panic and the elections of 1874 hint that support for Reconstruction was failing as early as 1870.

cosineSimilarity(models[["1870"]][["republican"]], models[["1871"]][["republican"]])
cosineSimilarity(models[["1871"]][["republican"]], models[["1872"]][["republican"]])
cosineSimilarity(models[["1872"]][["republican"]], models[["1873"]][["republican"]])
cosineSimilarity(models[["1873"]][["republican"]], models[["1874"]][["republican"]])
cosineSimilarity(models[["1874"]][["republican"]], models[["1875"]][["republican"]])

close.repub.minus.dem <- lapply(1870:1875, function(year) {
  model <- models[[as.character(year)]]
  #combining all the years of models into one
  
   diff<- model[["republican"]] - model[["democrat"]]
#here i am taking all the model years of republican and democrat and subtracting democrat from republican in an effort to glean any uniqueness to republican.
combined <- nearest_to(model, diff)

   df <- data.frame(
    word = names(combined),
    similarity = unname(combined),
    year = year,
    stringsAsFactors = FALSE
  )
  return(df)
})
close.repub.minus.dem <- do.call(rbind, close.repub.minus.dem)
#this code mirrors the section of code above, I'm just joining models together by year.
x11()
ggplot(close.repub.minus.dem, aes(x = reorder(word, similarity), 
y = similarity)) +
  geom_col() + 
  coord_flip() +
  #coord flip to increase readability
  facet_wrap(~ year, scales = "free_y") +
  #facet wrap because there's multiple years
  labs(
    title = "Top Words After 'Republican' Minus 'Democrat'",
    x = "Word",
    y = "Cosine Similarity"
  ) +
  theme_minimal()
    
#The results of subtracting modelled democrat from modelled republican is fascinating because of what it points to. In 1870 the words administrative, interests, affairs, and progress imply that what made republicans distinct from democrats was the centrality of governance. 
#This makes sense when you consider that, out of the 6 years of the data set, republicans were at their strongest in 1870, or at the least they were stronger than democrats in 1870. 
#The use of the word papal perhaps hints at a religious dimension to the republicans, as distinct from democrats, or perhaps it is a reference to the rigid structure or hierarchy of the republicans. 
#Perhaps debates, assembling, and councils in 1875 reflect the fractured state of the republican party in 1875. If these words reflect what is purely republican, minus democrats, these words are historically coherent together. 




models[["1873"]]  %>% closest_to("slavery", 10)
models[["1873"]]  %>% closest_to("democrat", 10)
models[["1873"]]  %>% closest_to("republican", 10)
models[["1873"]]  %>% closest_to("reconstruction", 10)



#testing some searches, debating on turning these into table and comparing years





slavery<- models[["1873"]]  %>% closest_to("slavery", 25)
dem <- models[["1873"]] %>% closest_to("democrat", 25)
repub <- models[["1873"]]  %>% closest_to("republican", 25)
recon <- models[["1873"]] %>% closest_to("reconstruction", 25)



ggplot(close.words.recon, aes(x = reorder(word, `similarity to "reconstruction"`), 
y = `similarity to "reconstruction"`)) +
  geom_col() + 
  coord_flip() +
  #coord flip to increase readability
  facet_wrap(~ year, scales = "free_y") +
  #facet wrap because there's multiple years
  labs(
    title = "Top Words Closest to 'Reconstruction' by Year",
    x = "Word",
    y = "Cosine Similarity"
  ) +
  theme_minimal()



recon_words <- recon$word
slavery_words <- slavery$word
repub_words <- repub$word
dem_words <- dem$word
#only way I could figure out how to simply extract all the words in order to combine them into one list
all_words <- unique(c(recon_words, slavery_words, repub_words, dem_words))
#combining into list
word_vecs <- models[["1873"]][all_words, average = FALSE]
#getting word vectors for every word to plot relationships


dist_matrix <- dist(word_vecs, method = "euclidean")
#prepping for plotting, computing distance between words
hc <- hclust(dist_matrix, method = "ward.D")
#turns relationships into a hierarchy for dendogram

x11()
plot(hc, main = "Word Dendrogram - 1873", xlab = "", sub = "")
#It was cool to figure out how to do this but sadly the words are random and not attached to anything so the dendrogram only shows all of the words' relationships with each other, seperated from their origin word. So the analysis is relatively limited.


compare.rr<- models[["1873"]][c("republican", "reconstruction"), average = F]
repubrecon <- models[["1873"]][1:3000, ] %>% cosineSimilarity(compare.rr)
repubrecon[20:30, ]
x11()
high.sim<- repubrecon[rank(-apply(repubrecon, 1, max)) < 75, ]
high.sim %>%
    prcomp() %>%
    biplot(main = "Fifty words in a \n projection of Reconstruction and Republican")
  
#So this plot is equal parts bizarre and interesting. It's very difficult to dive into the nuances in this plot. Firstly there are no words between the two that are so much more strongly associated with republican that it only appears right next to republican.
#The most republican associated words are Tammany, politicians, federal, and leaders. Tammany could potentially be connected to Reconstruction but since it is a very specific event over corruption in northern politics, it makes sense that it's more associated with republican.
#Politicians, federal, and leaders also make sense with republican, even in 1873 when the republican party began its floundering. 
#Success and irish are the outliers in this plot and are seemingly incoherent. Most scholars would not associate sucess with Reconstruction in 1873 nor would they state that the North viewed it as a success. Perhaps the word holds 0 correlation to republican and a very slight correlation to republican so its association with Reconstruction is dramatically amplified.
#This makes slightly more sense, 1873 somewhat marks the beginning of the republican party losing their platform, so success surely would not be associated with them. 
#Irish is a bit harder to pin down. The only explanation I have is that the irish during the 1870s were involved in labor strikes, and discussions of labor were absolutely associated with Reconstruction. 
#Popular is also more associated with Reconstruction here. I believe popular and success being more associated with Reconstruction is more about the words having 0 correlation to Republican. Once again, 1873 was a turning point for the republican party and perhaps this is reflecting that. 