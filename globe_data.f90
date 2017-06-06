module globe_data
integer,parameter :: nmax=4000000,nstmax=2000
integer,parameter :: FFTW_ESTIMATE=64,FFTW_MEASURE=1
integer,parameter :: FFTW_FORWARD=-1,FFTW_BACKWARD=1
complex,dimension(nmax,3) :: sig1,sig2
complex(8),dimension(nmax)   :: sig
real,dimension(nmax,3,3)  :: signcc,sigrot
integer :: npts,ncom,npt2,nsmpl
integer(8) :: plan
real :: dt
end module
