real function Fun_xy(x,y)
implicit none
real::x,y,xy,x3,y3
xy=x*y
Fun_xy=xy
end function Fun_xy

program ODE
implicit none
integer::i,xii,xff,n
real::x,y,Fun_xy,slope,step,xi,xf,k1,k2,x3,y3
write(*,*)"enter the step size"
read(*,*)step
write(*,*)"initial value of y"
read(*,*)y
write(*,*)"initial  and final value of x "
read(*,*)xi,xf

open(1,file="DEtest1.dat",status='replace')
n=10
do i=0,n


k1=Fun_xy(x,y)
k2=Fun_xy(x+(step/2),y+(step/2))

y=y+(step*k2)
x=x+step
write(*,*)x,y
write(1,10)x,y
10 format(f7.4,3x,f7.5)
end do
rewind(1)
	close(1)
end program

