transpose :: [[Integer]]->[[Integer]]
transpose ([]:_) = []
transpose x =  map head x : transpose (map tail x)
