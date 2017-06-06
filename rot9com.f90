! introduce globe_data
! caution, this program may need some changes
subroutine rot9com(sachead1,sachead2)
use sacio
use globe_data, only : signcc,sigrot,nsmpl
implicit none
type(sac_head) :: sachead1,sachead2
real,parameter :: rad=atan2(1.0,1.0)*4/180
real,dimension(4,4) :: trans
real :: az,baz,sbaz,saz,caz,cbaz
real :: a,b,P,lp

a=(90-sachead2%stla)*rad
b=(90-sachead1%stla)*rad
P=abs(sachead1%stlo-sachead2%stlo)*rad
lp=acos(cos(b)*cos(a)+sin(a)*sin(b)*cos(P))
saz =a*sin(P)/lp
sbaz=b*sin(P)/lp
caz =(cos(a)-cos(b)*cos(lp))/(sin(b)*sin(lp))
cbaz=(cos(b)-cos(a)*cos(lp))/(sin(a)*sin(lp))

trans(1,1)= saz*sbaz
trans(1,2)= saz*cbaz
trans(1,3)=-caz*sbaz
trans(1,4)=-caz*cbaz

trans(2,1)=-saz*cbaz
trans(2,2)= saz*sbaz
trans(2,3)= caz*cbaz
trans(2,4)=-caz*sbaz

trans(3,1)= caz*sbaz
trans(3,2)= caz*cbaz
trans(3,3)= saz*sbaz
trans(3,4)= saz*cbaz

trans(4,1)=-caz*cbaz
trans(4,2)= caz*sbaz
trans(4,3)=-saz*cbaz
trans(4,4)= saz*sbaz
!TT
sigrot(1:nsmpl,3,3)= signcc(1:nsmpl,2,2)*trans(1,1)+signcc(1:nsmpl,2,3)*trans(1,2)&
                   +signcc(1:nsmpl,3,2)*trans(1,3)+signcc(1:nsmpl,3,3)*trans(1,4)
!TR
sigrot(1:nsmpl,3,2)= signcc(1:nsmpl,2,2)*trans(2,1)+signcc(1:nsmpl,2,3)*trans(2,2)&
                   +signcc(1:nsmpl,3,2)*trans(2,3)+signcc(1:nsmpl,3,3)*trans(2,4)
!RT
sigrot(1:nsmpl,2,3)= signcc(1:nsmpl,2,2)*trans(3,1)+signcc(1:nsmpl,2,3)*trans(3,2)&
                   +signcc(1:nsmpl,3,2)*trans(3,3)+signcc(1:nsmpl,3,3)*trans(3,4)
!RR
sigrot(1:nsmpl,2,2)= signcc(1:nsmpl,2,2)*trans(4,1)+signcc(1:nsmpl,2,3)*trans(4,2)&
                   +signcc(1:nsmpl,3,2)*trans(4,3)+signcc(1:nsmpl,3,3)*trans(4,4)
!ZR
sigrot(1:nsmpl,1,2)= signcc(1:nsmpl,1,2)*cbaz - signcc(1:nsmpl,1,3)*sbaz
!RZ
sigrot(1:nsmpl,2,1)=-signcc(1:nsmpl,2,1)* caz - signcc(1:nsmpl,3,1)* saz
!ZT
sigrot(1:nsmpl,1,3)=-signcc(1:nsmpl,1,2)*sbaz - signcc(1:nsmpl,1,3)*cbaz
!TZ
sigrot(1:nsmpl,3,1)=-signcc(1:nsmpl,2,1)* saz + signcc(1:nsmpl,3,1)* caz
sigrot(1:nsmpl,1,1)= signcc(1:nsmpl,1,1)
return
end subroutine
