! 2017/06/05 --introduce globe_data
! npts is the length of sig1 and sig2
subroutine do_ncc(ic1,ic2)
use globe_data,only: sig,signcc,npt2,npts,sig1,sig2,nmax,plan,FFTW_BACKWARD,FFTW_ESTIMATE
integer i,nft2
real aa
complex(8),dimension(nmax) :: s
aa=-1.0
do i=1,npts
   sig(i)=dcmplx(aa)*dcmplx(sig1(i,ic1))*dcmplx(conjg(sig2(i,ic2)))
   aa=-aa
enddo
call dfftw_plan_dft_1d(plan,npts,sig,s,FFTW_BACKWARD, FFTW_ESTIMATE)
call dfftw_execute(plan)
call dfftw_destroy_plan(plan)
nft2=npts/2-npt2
do i=1,2*npt2+1
   signcc(2*npt2+2-i,ic1,ic2)=-real(dreal(s(nft2+i)))
enddo
return
end subroutine
