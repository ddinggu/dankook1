bb <- read.csv('./llastdata.csv', )
bb <- bb[,-1]

#-------단과대별 평균 대출기간-------

bb <- bb[bb$ckdl !=-1,] 
book <- subset(bb,as.numeric(substr(bb$borrow,1,4))>=2016) #borrow이 2016년 전 인 데이터는 제외
mm <- aggregate(ckdl~university,bb,mean)

# barplot(mm$ckdl)

#-----최근 1년 전체 top10-----
popular <- subset(bb,as.numeric(substr(bb$borrow,1,4))>=2019)
# View(head((sort((table((popular$bookname))),decreasing=T)),10))

sorted_yearly <- head((sort((table((popular$bookname))), decreasing=T)), 10)
yearly_top_10 <- as.data.frame(sorted_yearly)

names(yearly_top_10)[1] <- "bookname"  # DB 컬럼값을 위해 name 변경

#----역대 top10-----
# View(head((sort((table((bb$bookname))),decreasing=T)),10))

sorted_historic <- head((sort((table((bb$bookname))), decreasing=T)), 10)
historic_top_10 <- as.data.frame(sorted_historic)

names(historic_top_10)[1] <- "bookname" # DB 컬럼값을 위해 name 변경



#----단국대 학생들의 영역별 top10----
install.packages("arules")
library(arules)

# colnames(bb)
list <- split(bb$bookname,bb[,c(1,3)])
tr <- as(list,"transactions")

#사회과학
gg <- subset(bb,category=="사회과학",select = c(bookname,category))
head((sort((table(gg$bookname)),decreasing=T)),10)

gg.rules<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="맨큐의 경제학"))
inspect(sort(gg.rules, by = "lift")[1:3])

gg.rules2<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="어떻게 살 것인가"))
inspect(sort(gg.rules2, by = "lift")[1:3])

gg.rules3<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="국가란 무엇인가 / 개정신판[2판]"))
inspect(sort(gg.rules3, by = "lift")[1:3])

gg.rules4<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="총, 균, 쇠 : 무기·병균·금속은 인류의 운명을 어떻게 바꿨는가 / 제2판."))
inspect(sort(gg.rules4, by = "lift")[1:3])

gg.rules5<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="(클라우스 슈밥의) 제4차 산업혁명"))
inspect(sort(gg.rules5, by = "lift")[1:3])

gg.rules6<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="국제경제학 : 이론과 정책"))
inspect(sort(gg.rules6, by = "lift")[1:3])

gg.rules7<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="총, 균, 쇠 : 무기, 병균, 금속은 인류의 운명을 어떻게 바꿨는가 / 3판."))
inspect(sort(gg.rules7, by = "lift")[1:3])

gg.rules8<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="미시경제학"))
inspect(sort(gg.rules8, by = "lift")[1:3])

gg.rules9<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="(맨큐의 경제학) 연습문제풀이"))
inspect(sort(gg.rules9, by = "lift")[1:3])

gg.rules10<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="거시경제학"))
inspect(sort(gg.rules10, by = "lift")[1:3])


#종교

jj <- subset(bb,category=="종교",select = c(bookname,category))
View(head((sort((table(jj$bookname)),decreasing=T)),10))

jj.rules<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="멈추면, 비로소 보이는 것들 : 혜민 스님과 함께하는 내 마음 다시보기"))
inspect(sort(jj.rules, by = "lift")[1:3])

jj.rules2<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="만들어진 신"))
inspect(sort(jj.rules2, by = "lift")[1:3])

jj.rules3<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="완벽하지 않은 것들에 대한 사랑"))
inspect(sort(jj.rules3, by = "lift")[1:3])

jj.rules4<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="북유럽 신화"))
inspect(sort(jj.rules4, by = "lift")[1:3])

jj.rules5<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="북유럽 신화 여행 : 인간보다 더 인간적인 신들의 이야기"))
inspect(sort(jj.rules5, by = "lift")[1:3])

jj.rules6<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="나는 왜 기독교인이 아닌가"))
inspect(sort(jj.rules6, by = "lift")[1:3])

jj.rules7<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="신통기"))
inspect(sort(jj.rules7, by = "lift")[1:3])

jj.rules8<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="신화의 세계"))
inspect(sort(jj.rules8, by = "lift")[1:3])

jj.rules9<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="(벽화로 보는)이집트 신화"))
inspect(sort(jj.rules9, by = "lift")[1:3])

jj.rules10<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="석굴암:그 이념과 미학"))
inspect(sort(jj.rules10, by = "lift")[1:3])

#철학
hh <- subset(bb,category=="철학",select = c(bookname,category))
View(head((sort((table(hh$bookname)),decreasing=T)),10))

hh.rules<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="미움받을 용기 : 자유롭고 행복한 삶을 위한 아들러의 가르침"))
inspect(sort(hh.rules, by = "lift")[1:2])

hh.rules2<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="자존감 수업"))
inspect(sort(hh.rules2, by = "lift")[1:3])

hh.rules3<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="Justice : 정의란 무엇인가"))
inspect(sort(hh.rules3, by = "lift")[1:3])

hh.rules4<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="정의란 무엇인가"))
inspect(sort(hh.rules4, by = "lift")[1:3])

hh.rules5<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="철학 한 스푼 : [K에게 띄우는 편지]"))
inspect(sort(hh.rules5, by = "lift")[1:3])

hh.rules6<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="돈으로 살 수 없는 것들"))
inspect(sort(hh.rules6, by = "lift")[1:3])

hh.rules7<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="그릿 : IQ, 재능, 환경을 뛰어넘는 열정적 끈기의 힘"))
inspect(sort(hh.rules7, by = "lift")[1:3])

hh.rules8<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="(나를 바꾸는 심리학의 지혜)프레임 / 2판"))
inspect(sort(hh.rules8, by = "lift")[1:3])

hh.rules9<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="생각의 탄생 : 다빈치에서 파인먼까지 창조성을 빛낸 사람들의 13가지 생각도구"))
inspect(sort(hh.rules9, by = "lift")[1:3])

hh.rules10<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="(빅터 프랭클의)죽음의 수용소에서 : 당신이 가진 최고의, 그리고 최후의 자유는 바로 선택할 수 있는 자유이다"))
inspect(sort(hh.rules10, by = "lift")[1:3])

#역사
ht <- subset(bb,category=="역사",select = c(bookname,category))
View(head((sort((table(ht$bookname)),decreasing=T)),10))

ht.rules<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="사피엔스"))
inspect(sort(ht.rules, by = "lift")[1:3])

ht.rules2<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="박시백의 조선왕조실록 : 대하역사만화"))
inspect(sort(ht.rules2, by = "lift")[1:3])

ht.rules3<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="역사란 무엇인가"))
inspect(sort(ht.rules3, by = "lift")[1:3])

ht.rules4<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="(설민석의) 조선왕조실록 : 대한민국이 선택한 역사 이야기"))
inspect(sort(ht.rules4, by = "lift")[1:3])

ht.rules5<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="(한눈에 읽는)파노라마 한국사"))
inspect(sort(ht.rules5, by = "lift")[1:3])

ht.rules6<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="격동의 서양 20세기사"))
inspect(sort(ht.rules6, by = "lift")[1:3])

ht.rules7<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="(새로쓴)서양사 총론"))
inspect(sort(ht.rules7, by = "lift")[1:3])

ht.rules8<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="나의 한국 현대사 : 1959-2014, 55년의 기록"))
inspect(sort(ht.rules8, by = "lift")[1:3])

ht.rules9<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="(다시찾는)우리역사. 1-3 / 전면개정판"))
inspect(sort(ht.rules9, by = "lift")[1:3])

ht.rules10<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="호모 데우스 : 미래의 역사"))
inspect(sort(ht.rules10, by = "lift")[1:3])

#공학(기술과학 예술 총류)
gh <- subset(bb,category %in% c("기술과학","예술","총류"),select = c(bookname,category))
View(head((sort((table(gh$bookname)),decreasing=T)),10))

gh.rules<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="지적 대화를 위한 넓고 얕은 지식 : 철학, 과학, 예술, 종교, 신비 편"))

gh.rules2<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="지적 대화를 위한 넓고 얕은 지식 : 역사, 경제, 정치, 사회, 윤리 편"))

gh.rules3<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="미분적분학"))

gh.rules4<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="(Kreyszig) 공업수학"))

gh.rules5<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="유체역학"))

gh.rules6<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="경영과학"))
gh.rules7<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="재료과학과 공학"))


gh.rules8<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="반도체 소자공학"))

gh.rules9<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="조직이론과 설계"))

gh.rules10<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="미분적분학 / 증보판[2판]"))

inspect(sort(gh.rules, by = "lift")[1:3])
inspect(sort(gh.rules2, by = "lift")[1:2])
inspect(sort(gh.rules3, by = "lift")[1:3])
inspect(sort(gh.rules4, by = "lift")[1:3])
inspect(sort(gh.rules5, by = "lift")[1:3])
inspect(sort(gh.rules6, by = "lift")[1:3])
inspect(sort(gh.rules7, by = "lift")[1:3])
inspect(sort(gh.rules8, by = "lift")[1:3])
inspect(sort(gh.rules9, by = "lift")[1:3])
inspect(sort(gh.rules10, by = "lift")[1:3])

#문학(문학 언어 자연과학)
mh <- subset(bb,category %in% c("언어","문학","자연과학"),select = c(bookname,category))
View(head((sort((table(mh$bookname)),decreasing=T)),10))

mh.rules<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="나미야 잡화점의 기적 : 히가시노 게이고 장편소설"))

mh.rules2<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="언어의 온도"))

mh.rules3<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="82년생 김지영 : 조남주 장편소설"))

mh.rules4<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="7년의 밤 : 정유정 장편소설"))

mh.rules5<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="왜 나는 너를 사랑하는가 / 개정판."))

mh.rules6<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="살인자의 기억법 : 김영하 장편소설"))
mh.rules7<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="미생 : 아직 살아 있지 못한 자"))


mh.rules8<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="스물아홉 생일,1년 후 죽기로 결심했다"))

mh.rules9<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="1Q84"))

mh.rules10<-apriori(tr,parameter = list(supp = 0.01, conf = 0.8,minlen=2),appearance = list(lhs="채식주의자 : 한강 연작소설"))


inspect(sort(mh.rules2, by = "lift")[1:3])
inspect(sort(mh.rules3, by = "lift")[1:3])
inspect(sort(mh.rules4, by = "lift")[1:3])
inspect(sort(mh.rules5, by = "lift")[1:3])
inspect(sort(mh.rules6, by = "lift")[1:3])
inspect(sort(mh.rules7, by = "lift")[1:3])
inspect(sort(mh.rules8, by = "lift")[1:3])
inspect(sort(mh.rules9, by = "lift")[1:3])
inspect(sort(mh.rules10, by = "lift")[1:3])