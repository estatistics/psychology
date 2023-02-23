
dag <- model2network("[location][quality][cost|location:quality][nopeople|location:cost]")
plot(dag)

quality.values <- factor(c("Good", "Normal", "Bad"))
location.values <- factor(c("Good", "Bad"))
cost.values <- factor(c("High", "Low"))
nopeople.values <- factor(c("High", "Low"))



quality.prob <- array(c(0.3, 0.5, 0.2), dim = 3, dimnames = list(quality = quality.values))
location.prob <- array(c(0.6, 0.4), dim = 2, dimnames = list(location = location.values))
cost.prob <- array(c(0.8, 0.2, 0.6, 0.4, 0.1, 0.9, 0.6, 0.4, 0.6, 0.4, 0.05, 0.95), dim = c(2, 3, 2), dimnames = 
                     list(cost = cost.values, quality = quality.values, location = location.values))
nopeople.prob <- array(c(0.6, 0.4, 0.8, 0.2, 0.1, 0.9, 0.6, 0.4), dim = c(2, 2, 2), dimnames = 
                         list( nopeople = nopeople.values, cost = cost.values, location = location.values))

condProbTable <- list(location = location.prob, quality = quality.prob, cost = cost.prob,  nopeople = nopeople.prob)
bn <- custom.fit(dag, condProbTable)

graphviz.plot(bn)






asia.dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")

lv = c("yes", "no")

A.prob = array(c(0.01, 0.99), dim = 2, dimnames = list(A = lv))
S.prob = array(c(0.01, 0.99), dim = 2, dimnames = list(A = lv))

T.prob = array(c(0.05, 0.95, 0.01, 0.99), dim = c(2, 2),
               dimnames = list(T = lv, A = lv))

L.prob = array(c(0.1, 0.9, 0.01, 0.99), dim = c(2, 2),
               dimnames = list(L = lv, S = lv))
B.prob = array(c(0.6, 0.4, 0.3, 0.7), dim = c(2, 2),
               dimnames = list(B = lv, S = lv))

D.prob = array(c(0.9, 0.1, 0.7, 0.3, 0.8, 0.2, 0.1, 0.9), dim = c(2, 2, 2),
               dimnames = list(D = lv, B = lv, E = lv))

E.prob = array(c(1, 0, 1, 0, 1, 0, 0, 1), dim = c(2, 2, 2),
               dimnames = list(E = lv, T = lv, L = lv))

X.prob = array(c(0.98, 0.02, 0.05, 0.95), dim = c(2, 2),
               dimnames = list(X = lv, E = lv))
cpt = list(A = A.prob, S = S.prob, T = T.prob, L = L.prob, B = B.prob,
           D = D.prob, E = E.prob, X = X.prob)
bn = custom.fit(asia.dag, cpt)

graphviz.plot(bn)
graphviz.chart(bn)









g_graph<-list(~asia, 
              ~tub | asia,
              ~smoke, 
              ~lung | smoke, 
              ~bronc | smoke, 
              ~either | lung : tub, 
              ~xray | either,
              ~dysp | bronc : either)

chestdag<-dagList(g_graph)


yn   <- c("yes", "no")
a    <- cptable(~asia, values = c(1,99), levels = yn)
t.a  <- cptable(~tub + asia, values = c(5, 95, 1, 99), levels = yn)
s    <- cptable(~smoke, value = c(5, 5), levels = yn)
l.s  <- cptable(~lung + smoke, values = c(1,9,1,99), levels = yn)
b.s  <- cptable(~bronc + smoke, values = c(6, 4, 3, 7), levels = yn)
e.lt <- cptable(~either + lung + tub, values = c(1, 0, 1, 0, 1, 0, 0, 1), levels = yn)
x.e  <- cptable(~xray + either, values = c(8, 2, 5, 95), levels = yn)
d.be <- cptable(~dysp + bronc + either, values = c(9, 1, 7, 3, 8, 2, 1, 9), levels = yn)

plist <- compileCPT(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))


grn1 <- grain(plist)
summary(grn1)
grn1c <- compile(grn1, propagate = TRUE)

summary(grn1c)

tmg <- triangulate(moralize(grn1$dag))
rip(tmg)$cliques
grn1c.ev <- setFinding(grn1c, nodes=c("asia", "dysp"), states=c("yes","yes"))
querygrain(grn1c.ev, nodes=c("lung","bronc"), type="marginal")

grn2c.ev <- setFinding(grn1c, nodes=c("asia","dysp","smoke"), 
                       states=c("yes","yes","yes"))
querygrain(grn2c.ev, nodes=c("lung","bronc"), type="marginal")


plist
grn1

plot(grn1)
plot.grain(grn1c)


graphviz.plot(grn1c)

graphviz.chart(grn1c)

condProbTable       <- list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be)
x_model             <- model2network("[X1][H1|X1]")
bn_model            <- custom.fit(g_graph, condProbTable, debug = T)




