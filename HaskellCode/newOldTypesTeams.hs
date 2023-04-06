type Team = String
type Goals = Int
type Match = ((Team, Goals), (Team, Goals))

homeTeam :: Match -> Team
homeTeam ((hteam, hpoints),(oteam,opoints)) = hteam

totalGoals :: Match -> Goals
totalGoals ((hteam, hpoints),(oteam,opoints)) = hpoints+opoints

numu :: Match
numu = (("Newcastle",4), ("Manchester Utd", -100))