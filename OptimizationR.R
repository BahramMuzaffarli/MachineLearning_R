# Numerical accuracy
x = c()
x[1] = 0.1
for (i in 2 : 101)
{
  x[i] = x[i-1] + 0.1
  #x[i] = round(x[i],1)
}

options(digits = 22)
x[101]
x[101] == 10.1

options(digits = 18)
options(scipen = 0)

x = seq(0, 10, by = 0.1)
x[101] == 10





# Taylor series approx of log f-ction
xx = seq(0.1, 4, by = 0.1)
yy = log(xx)

x = 1
hh = seq(-0.9, 3, by = 0.1)
x+hh
TS1 = log(x) + x^-1 * hh

y.min = min(c(yy, TS1))
y.max = max(c(yy, TS1))

plot(xx, yy, type = "l",
     xlab = "Grid for x",
     ylab = "Value of the function",
     main = "The logarithmin function",
     ylim = c(y.min, y.max))
grid()

lines(xx, TS1, col = "blue")




#
f = function(x){
  3*x^2
}
df= function(x,f,del=10^-4){
  #calculates a derivative oof f in x
  return((f(x+del)-f(x-del))/(2*del))
}

df(-2,f)
df(-1,f)
df(0,f)
df(1,f)
df(2,f)



df(c(-2,-1,0,1,2), f)






#Directional Derivative

f = function(x1, x2){
  x1^2+x2^2
}
xx = yy =seq(-2,2,by=0.25)
zz = outer(xx,yy,f)
persp(xx,yy,zz)
persp(xx,yy,zz,phi=30)
contour(xx,yy,zz)
persp(xx,yy,zz)


