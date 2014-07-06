import List
t s=last$6:[i|i<-[0..5],isSuffixOf(words"lios liala etr etra initis inites"!!i)s]
n="NO"
c x|any(>5)x=n
c[_]="YES"
c(x:y)|any((/=odd x).odd)y=n
c x=g$map(`div`2)x
g(0:x)=g x
g(1:2:x)=g(1:x)
g[1]="YES"
g _=n
main=interact$c.map t.words
