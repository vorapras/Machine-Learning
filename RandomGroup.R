names <-c("Bukovsky, Markus Olaf", "Chatterjee, Deepaboli", "Chen, Jiahui", "Cheng, Wenyu", "Craven, Georgia", "Fang, Qin", "Geiecke, Friedrich Christian", "Guan, Jingyu", "Huang, Xiao", 
      "Ishida, Sahoko", "Jin, Yi", "Jin, Zuben", "Jordan Sierra, Diego Tomas", "Kuhlen, Nikolas", "Langholz, Andreas", "Long, Huilin", "Mandl, Maximilian Michael", "Mishra, Dibya Deepta",
      "Nakatudde, Nambassa Tatiagna", "Patel, Amrish", "Peng, Yili", "Rathmann, Justus Maximilian Karl", "Serra-Burriel, Feliu", "Shi, Jialu", "Voraprasertsak, Thanachai", "Wang, Jiayong",
      "Yong, Zijun", "Yu, Xintian", "Zhang, Yijing")
length(names)
id <-c(1:length(names))
###################################################################################################################
#####Assign 5 groups with 4 members and 3 groups with 3 members
# Set seed number
set.seed(03022017)
size <-sample(c(rep(4,5), rep(3,3)))
group <-list()
Group <-list()
id.left <-NULL
group[[1]] <-sample(id,size=size[1])
Group[[1]] <-names[group[[1]]]

id.left <-c(id.left,group[[1]])
group[[2]] <-sample(id[-id.left],size=size[2])
Group[[2]] <-names[group[[2]]]

id.left <-c(id.left,group[[2]])
group[[3]] <-sample(id[-id.left],size=size[3])
Group[[3]] <-names[group[[3]]]

id.left <-c(id.left,group[[3]])
group[[4]] <-sample(id[-id.left],size=size[4])
Group[[4]] <-names[group[[4]]]

id.left <-c(id.left,group[[4]])
group[[5]] <-sample(id[-id.left],size=size[5])
Group[[5]] <-names[group[[5]]]

id.left <-c(id.left,group[[5]])
group[[6]] <-sample(id[-id.left],size=size[6])
Group[[6]] <-names[group[[6]]]

id.left <-c(id.left,group[[6]])
group[[7]] <-sample(id[-id.left],size=size[7])
Group[[7]] <-names[group[[7]]]

id.left <-c(id.left,group[[7]])
group[[8]] <-sample(id[-id.left],size=size[8])
Group[[8]] <-names[group[[8]]]

Group

####################################################################################################################
#####Assign 6 groups with 4 members and 1 group with 5 members
# Set seed number
set.seed(03022017)
size <-sample(c(rep(4,6), rep(5,1)))
group <-list()
Group <-list()
id.left <-NULL
group[[1]] <-sample(id,size=size[1])
Group[[1]] <-names[group[[1]]]

id.left <-c(id.left,group[[1]])
group[[2]] <-sample(id[-id.left],size=size[2])
Group[[2]] <-names[group[[2]]]

id.left <-c(id.left,group[[2]])
group[[3]] <-sample(id[-id.left],size=size[3])
Group[[3]] <-names[group[[3]]]

id.left <-c(id.left,group[[3]])
group[[4]] <-sample(id[-id.left],size=size[4])
Group[[4]] <-names[group[[4]]]

id.left <-c(id.left,group[[4]])
group[[5]] <-sample(id[-id.left],size=size[5])
Group[[5]] <-names[group[[5]]]

id.left <-c(id.left,group[[5]])
group[[6]] <-sample(id[-id.left],size=size[6])
Group[[6]] <-names[group[[6]]]

id.left <-c(id.left,group[[6]])
group[[7]] <-sample(id[-id.left],size=size[7])
Group[[7]] <-names[group[[7]]]

Group