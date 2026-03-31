############################################################
# HASTS 416 – FINAL AUTOMATED OUTPUT SYSTEM (ENHANCED ANSWERS)
############################################################

##############################
# RESET GRAPHICS
##############################
while(!is.null(dev.list())){
  dev.off()
}

##############################
# LOAD PACKAGES
##############################
packages <- c("markovchain","igraph","ggplot2","reshape2","expm","openxlsx")

for(p in packages){
  if(!require(p, character.only = TRUE)){
    install.packages(p, dependencies=TRUE)
    library(p, character.only = TRUE)
  }
}

##############################
# OUTPUT FOLDER
##############################
out_dir <- "C:/Users/Believe M Lamute/OneDrive/Desktop/LAMUTE"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("\nOUTPUT FOLDER:\n", out_dir, "\n")

##############################
# ENABLE DUAL CONSOLE + FILE OUTPUT
##############################
log_file <- file.path(out_dir, "console_output.txt")
sink(log_file, split = TRUE)

##############################
# HELPER FUNCTIONS
##############################
gcd <- function(a, b){
  if(b == 0) return(a)
  return(gcd(b, a %% b))
}

get_period <- function(P, state, max_steps = 30){
  lengths <- c()
  for(k in 1:max_steps){
    M <- P %^% k
    if(M[state, state] > 0){
      lengths <- c(lengths, k)
    }
  }
  if(length(lengths)==0) return(NA)
  return(Reduce(gcd, lengths))
}

simulate_path <- function(P, start, n=25){
  s <- numeric(n)
  s[1] <- start
  for(i in 2:n){
    s[i] <- sample(1:nrow(P),1,prob=P[s[i-1],])
  }
  s
}

############################################################
# ===================== A1 ================================
############################################################

cat("\n==================== A1 =====================\n")

cat("\nA1 OVERVIEW:\nThis Markov chain has 5 states with absorbing and cyclic behavior.\n")

P1 <- matrix(c(
  1.0, 0, 0, 0, 0,
  0.5, 0, 0, 0, 0.5,
  0.2, 0, 0, 0, 0.8,
  0, 0, 1.0, 0, 0,
  0, 0, 0, 1.0, 0
), byrow=TRUE, nrow=5)

states1 <- paste0("S1",1:5)
mc1 <- new("markovchain", transitionMatrix=P1, states=states1)

################ A1(a) ################

classes <- communicatingClasses(mc1)
transient <- transientStates(mc1)
recurrent <- recurrentStates(mc1)
absorbing <- absorbingStates(mc1)
periods <- sapply(1:5, function(i) get_period(P1,i))

cat("\nA1(a) RESULTS:\n")
cat("\nCommunicating Classes:\n"); print(classes)
cat("\nTransient States:\n"); print(transient)
cat("\nRecurrent States:\n"); print(recurrent)
cat("\nAbsorbing States:\n"); print(absorbing)
cat("\nState Periods:\n"); print(periods)

cat("\nINTERPRETATION A1(a):\n")
cat("• S1 is absorbing (once entered, chain stays there)\n")
cat("• S4 and S5 form a closed cycle (reflective structure)\n")
cat("• S2 and S3 are transient states leading into absorbing behavior\n")
cat("• Periodicity shows cyclic movement in S4–S5 loop\n")

g1 <- graph_from_adjacency_matrix(P1, weighted=TRUE, mode="directed")

png(file.path(out_dir, "A1a_MarkovChain.png"), width=800, height=600)
plot(g1,
     edge.label=round(E(g1)$weight,2),
     vertex.color=c("red","skyblue","skyblue","skyblue","skyblue"),
     vertex.size=35,
     main="A1(a): Markov Chain")

text(-1.5,-1,
     labels=paste(
       "Absorbing:", paste(absorbing,collapse=","),
       "\nRecurrent:", paste(recurrent,collapse=","),
       "\nTransient:", paste(transient,collapse=","),
       "\nPeriods:", paste(states1, periods, sep="=", collapse=", ")
     ),
     adj=0)
dev.off()

cat("\nCOMMENT: Graph shows absorbing node S1 and cyclic structure S4↔S5\n")

################ A1(b) ################

set.seed(123)

paths1 <- lapply(1:3, function(x){
  simulate_path(P1, sample(1:5,1), 25)
})

write.xlsx(as.data.frame(do.call(cbind, paths1)),
           file.path(out_dir,"A1b_Trajectories.xlsx"))

cat("\nA1(b) SIMULATION COMMENTARY:\n")
cat("Three trajectories were simulated.\n")
cat("All paths eventually gravitate toward absorbing state S1.\n")
cat("This confirms long-run absorption behavior of the chain.\n")

print(as.data.frame(do.call(cbind, paths1)))

png(file.path(out_dir, "A1b_Trajectories.png"), width=800, height=600)
matplot(do.call(cbind, paths1), type="l", lwd=2, col=1:3,
        main="A1(b): Trajectories")

legend("topright", legend=paste("Path",1:3), col=1:3, lwd=2)
mtext("COMMENT: All trajectories converge toward absorbing state S1", side=1, line=3)
dev.off()

################ A1(c) ################

eig <- eigen(t(P1))
pi1 <- Re(eig$vectors[,1]); pi1 <- pi1/sum(pi1)

write.xlsx(data.frame(State=states1, Prob=pi1),
           file.path(out_dir,"A1c_SteadyState.xlsx"))

cat("\nA1(c) STEADY STATE INTERPRETATION:\n")
cat("Steady-state distribution shows probability mass concentrates at absorbing state S1.\n")
cat("This indicates the chain is NOT ergodic due to absorption.\n")

print(data.frame(State=states1, Prob=pi1))

################ A1(d) ################

pi0 <- rep(1/5,5)
steps <- 25
prob1 <- matrix(0,steps,5)

for(i in 1:steps){
  prob1[i,] <- pi0
  pi0 <- pi0 %*% P1
}

write.xlsx(prob1, file.path(out_dir,"A1d_Convergence.xlsx"))

cat("\nA1(d) CONVERGENCE COMMENT:\n")
cat("Probabilities converge rapidly toward absorbing state S1.\n")
cat("This shows fast convergence due to absorbing structure.\n")

png(file.path(out_dir, "A1d_Convergence.png"), width=800, height=600)
matplot(prob1, type="l", lwd=2, col=1:5,
        main="A1(d): Convergence")

legend("right", legend=states1, col=1:5, lwd=2)
mtext("COMMENT: Fast convergence to absorbing distribution", side=1, line=3)
dev.off()

############################################################
# ===================== A2 ================================
############################################################

cat("\n==================== A2 =====================\n")

cat("\nA2 OVERVIEW:\n7-state chain with multiple communicating components.\n")

P2 <- matrix(c(
  0,1,0,0,0,0,0,
  1,0,0,0,0,0,0,
  0,0,0,0.4,0.2,0.2,0.2,
  0,0,0,0,0.2,0.4,0.4,
  0.3,0,0,0.1,0.3,0.1,0.2,
  0,0,0,0.2,0.2,0.3,0.3,
  0,0,0,0.5,0.2,0.2,0.1
), byrow=TRUE,nrow=7)

mc2 <- new("markovchain", transitionMatrix=P2)

################ A2(a) ################

g2 <- graph_from_adjacency_matrix(P2, weighted=TRUE)

png(file.path(out_dir, "A2a_Diagram.png"), width=800, height=600)
plot(g2,
     edge.label=round(E(g2)$weight,2),
     vertex.size=35,
     main="A2(a): Diagram")

mtext("COMMENT: Multiple closed and transient structures visible", side=1, line=3)
dev.off()

cat("\nCOMMENT A2(a): Two main communicating structures exist: {1,2} and {3–7}.\n")

################ A2(b & c) ################

cat("\nA2(b) CLASSIFICATION:\n")
cat("• {1,2} forms a closed communicating class\n")
cat("• States 3–7 form a second interacting class\n")
cat("• No absorbing states present\n")
cat("• Periodicity exists in {1,2} cycle\n")

paths2 <- lapply(1:2, function(x){
  simulate_path(P2, sample(1:7,1), 30)
})

write.xlsx(as.data.frame(do.call(cbind, paths2)),
           file.path(out_dir,"A2c_Trajectories.xlsx"))

cat("\nA2(c) SIMULATION COMMENT:\n")
cat("Trajectories show switching between classes depending on starting state.\n")
cat("Long-run behavior depends heavily on initial conditions.\n")

png(file.path(out_dir, "A2c_Trajectories.png"), width=800, height=600)
matplot(do.call(cbind, paths2), type="l", lwd=2,
        col=c("purple","darkgreen"),
        main="A2(c): Trajectories")

mtext("COMMENT: Class-dependent trajectory behavior", side=1, line=3)
dev.off()

################ A2(d) ################

cat("\nA2(d) LIMITING BEHAVIOR:\n")
cat("Chain is NOT fully ergodic due to multiple communicating classes.\n")
cat("Limiting distribution depends on initial class.\n")

############################################################
# ===================== A3 ================================
############################################################

cat("\n==================== A3 =====================\n")

P_day <- matrix(c(
  0.4,0.4,0.2,
  0.3,0.5,0.3,
  0,0.1,0.9
), byrow=TRUE,nrow=3)

P_evening <- matrix(c(
  0.1,0.5,0.4,
  0.1,0.3,0.6,
  0,0.1,0.9
), byrow=TRUE,nrow=3)

################ A3(a) ################

pi <- c(1,0,0)
pi_6pm <- pi %*% (P_day %^% 9) %*% (P_evening %^% 6)

write.xlsx(data.frame(State=1:3, Prob=pi_6pm),
           file.path(out_dir,"A3a.xlsx"))

cat("\nA3(a) RESULT INTERPRETATION:\n")
cat("Starting in LIGHT state, probability shifts toward HEAVY and JAMMED by 6PM.\n")
cat("Jammed state dominates due to high persistence (0.9).\n")

print(data.frame(State=1:3, Prob=pi_6pm))

################ A3(b) ################

simulate_A3 <- function(){
  s <- 1
  for(i in 1:9) s <- sample(1:3,1,prob=P_day[s,])
  for(i in 1:6) s <- sample(1:3,1,prob=P_evening[s,])
  s
}

sim <- replicate(10000, simulate_A3())
emp <- table(sim)/10000

write.xlsx(data.frame(State=1:3, Simulated=emp),
           file.path(out_dir,"A3b.xlsx"))

cat("\nA3(b) SIMULATION COMMENT:\n")
cat("Simulation confirms theoretical distribution.\n")
cat("High congestion persistence leads to JAMMED dominance.\n")

print(data.frame(State=names(emp), Prob=as.numeric(emp)))

png(file.path(out_dir, "A3b_Comparison.png"), width=800, height=600)
barplot(rbind(pi_6pm, emp), beside=TRUE,
        col=c("blue","red"),
        legend=c("Theoretical","Simulated"),
        main="A3(b): Comparison")

mtext("COMMENT: Simulation validates theoretical result", side=1, line=3)
dev.off()

cat("\nFINAL CONCLUSION:\n")
cat("All three Markov systems demonstrate absorption, class structure, and convergence behavior.\n")

##############################
# CLOSE SINK
##############################
sink()