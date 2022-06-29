real function Fun_tv(t,v)
implicit none
real::t,v,tv,t3,v3
tv=v
Fun_tv=tv
end function Fun_tv

real function Fun_txv(t,x,v)
implicit none
real::t,x,v,txv,t3,x3,v3
txv=-x
Fun_txv=txv
end function Fun_txv

program rk2
implicit none
integer::i,tii,tff,n
real::t,v,Fun_tv,slope,step,ti,tf,k1,k2,t3,v3,x,l1,l2,Fun_txv,e
write(*,*)"enter the step size"
read(*,*)step
write(*,*)"initial value of x"
read(*,*)x
write(*,*)"initial value of v"
read(*,*)v
write(*,*)"final value of t "
read(*,*)tf
e=0.5*(x*x+v*v)

open(1,file="rk2x.dat",status='replace')
open(2,file="rk2v.dat",status='replace')
open(3,file="rk2e.dat",status='replace')
write(1,10)t,x
write(2,20)t,v
write(3,30)t,e
n=(tf/step)-step
do i=0,200
k1=Fun_tv(t,v)
l1=Fun_txv(t,x,v)
k2=Fun_tv(t+(step/2),v+(step*(l1/2)))
l2=Fun_txv(t+(step/2),x+step*(k1/2),v+step*(l1/2))
x=x+(step*k2)
v=v+(step*l2)
e=0.5*(x*x+v*v)
t=t+step
write(*,*)t,x,v,e
write(1,10)t,x
write(2,20)t,v
write(3,30)t,e
10 format(f7.4,3x,f7.5)
20 format(f7.4,3x,f7.5)
30 format(f7.4,3x,f7.5)
end do
rewind(1)
	close(1)
rewind(2)
	close(2)
rewind(3)
	close(3)

end program

