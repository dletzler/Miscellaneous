
library(pdftools)

#Step 1: Import individual pdf pages

gr.pdf<-tolower(pdf_text("GR_good.pdf"))

#Step 2: Clean--eliminate dashes in words going over end of page, carriage returns, and irrelevant puncutation
gr.pdf.fixdash<-gsub("-\r\n", "", gr.pdf)
gr.pdf.nocar<-gsub("\r?\n|\r", " ", gr.pdf.fixdash)
gr.pdf.good<-gsub("[^[:alnum:][:space:]\u2019]", " ", gr.pdf.nocar)

#Step 3: Tokenize and remove spaces
gr.pdf.list<-lapply(gr.pdf.good, strsplit, " ")

remove.space<-function(x){
  y<-x[[1]][x[[1]]!=""]
  return(y)
}

gr.pdf.vec<-lapply(gr.pdf.list, remove.space)

#Step 4: Create word frequencies
gr.concord.2<-data.frame(table(unlist(gr.pdf.vec)))
gr.concord.2<-gr.concord.2[order(gr.concord.2$Freq, decreasing =T),]
gr.concord.2$Percent<-100*gr.concord.2$Freq/sum(gr.concord.2$Freq)
gr.concord.2$Var1<-as.character(gr.concord.2$Var1)

#Step 5: Generate page references
gr.concord.2$Locations<-0
num<-1:length(gr.pdf.vec)
for (i in 1:nrow(gr.concord.2)){
  vec<-rep(0, length(gr.pdf.vec))
  for (j in 1:length(gr.pdf.vec)){
    vec[j]<-gr.concord.2$Var1[i] %in% gr.pdf.vec[[j]]
  }
  loc<-num[vec==1]
  gr.concord.2$Locations[i]<-paste(loc, collapse=", ")
}

#Step 5: Spruce up
names(gr.concord.2)[1]<-"Word"
write.csv(gr.concord.2, "gr_concordance.csv")
