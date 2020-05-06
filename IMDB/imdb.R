path='C:/Users/Uttiya Maji/Desktop/movie_metadata.csv'
imdb=read.csv(path)
str(imdb)
imdb=imdb[c('director_name','duration','actor_1_name','gross','genres','movie_title','plot_keywords','language',
            'country','budget','title_year','imdb_score','movie_facebook_likes')]
names(imdb)

# genres
str(imdb$genres)
imdb$genres=as.character(imdb$genres)
library(stringr)
genres=str_split(imdb$genres,"\\|") #list
genres_matrix=str_split_fixed(imdb$genres,"\\|",n=8) #matrix
table(unlist(genres))
# music and musical should coerce
genres=lapply(genres,str_replace_all,'Musical','Music')
genres_matrix=matrix((apply(genres_matrix,1,str_replace,'Musical','Music')),byrow=T,ncol=8)

genre_extractor=function(genre){
   unlist(lapply(lapply(genres,str_detect,genre),sum))
}

Action=genre_extractor('Action')
Adventure=genre_extractor('Adventure')
Animation=genre_extractor('Animation')
Biography=genre_extractor('Biography')
Comedy=genre_extractor('Comedy')
Crime=genre_extractor('Crime')
Documentary=genre_extractor('Documentary')
Drama=genre_extractor('Drama')
Family=genre_extractor('Family')
Fantasy=genre_extractor('Fantasy')
Film_Noir=genre_extractor('Film-Noir')
Game_Show=genre_extractor('Game-Show')
History=genre_extractor('History')
Horror=genre_extractor('Horror')
Music=genre_extractor('Music')
Mystery=genre_extractor('Mystery')
News=genre_extractor('News')
Reality_TV=genre_extractor('Reality-TV')
Romance=genre_extractor('Romance')
Sci_Fi=genre_extractor('Sci-Fi')
Short=genre_extractor('Short')
Sport=genre_extractor('Sport')
Thriller=genre_extractor('Thriller')
War=genre_extractor('War')
Western=genre_extractor('Western')

genres.df=data.frame(Action,Adventure,Animation,Biography,Comedy,Crime,
                     Documentary,Drama,Family,Fantasy,Film_Noir,Game_Show,
                     History,Horror,Music,Mystery,News,Reality_TV,Romance,
                     Sci_Fi,Short,Sport,Thriller,War,Western)


genres.df=cbind(genres.df,rating=imdb$imdb_score)

library(tm)
genres.vsource=VectorSource(genres)
genres.vcorpus=VCorpus(genres.vsource)
genres.dtm=DocumentTermMatrix(genres.vcorpus)
genres.m=as.matrix(genres.dtm)

genres_freq=colSums(genres.m)
barplot(genres_freq)

imdb$plot_keywords[1:10]

keywords=str_split(imdb$plot_keywords,'\\|')
keywords.vsource=VectorSource(keywords)
keywords.vcorpus=VCorpus(keywords.vsource)
keywords.dtm=DocumentTermMatrix(keywords.vcorpus)
keywords.m=as.matrix(keywords.dtm)

genre_freq=colSums(genres.m)
barplot(genre_freq,las=2)
genre_freq.df=data.frame(genre=names(genre_freq),freq=unname(genre_freq))
genre_freq.df=genre_freq.df[order(genre_freq,decreasing = T),]



kword_freq=colSums(keywords.m)
barplot(kword_freq)





