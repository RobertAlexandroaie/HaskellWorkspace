maxim[h] = h
maxim(h:t) = maxim2 h (maxim t)

maxim2 x y = if x>y then x else y