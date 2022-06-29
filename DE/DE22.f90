real function Fun_xy(x,y)
implicit none
real::x,y,xy
xy=x*y
Fun_xy=xy
end function Fun_xy

program ODE
implicit none
integer::i,xii,xff
real::x,y,Fun_xy,slope,step,xi,xf
write(*,*)"enter the step size"
read(*,*)step
write(*,*)"initial value of y"
read(*,*)y
write(*,*)"initial  and final value of x "
read(*,*)xi,xf
xii=xi/step
xff=xf/step
slope=Fun_xy(xi,y)


open(1,file="DE05.dat",status='new')

do i=xii,xff

x=i*step

y=y+((step)*slope)
slope=Fun_xy(x,y)

write(*,*)x,y,slope
write(1,10)x,y
10 format(f7.4,3x,f7.5)
end do
rewind(1)
	close(1)
end program

