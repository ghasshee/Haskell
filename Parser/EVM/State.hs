module State where 


data State s a = State (a -> (s,a)) 

runstate st a = undefined  



