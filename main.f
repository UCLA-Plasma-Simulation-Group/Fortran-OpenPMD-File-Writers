      program openpmdio
      
      use parallel_class
      use hdf5io_class
      use mpi
      
      implicit none
      
      type(parallel),target :: p
      class(parallel), pointer :: pp => null()
      type(hdf5file) :: file
      real, dimension(:,:), allocatable :: arr
      real, dimension(:,:,:), allocatable :: arr3d
      real, dimension(:,:), allocatable :: part
      integer :: nx = 256, ny = 256, nz = 256, noff, nyp
      integer :: i, j, k, ierr = 0
      integer :: iter
      integer :: N = 10000
      
! Initialize MPI environment
      call p%new()
           
      pp => p

      if (mod(ny,pp%getnvp()) /= 0) then
         print *, 'The number of processor cannot divde ny=', ny
         call p%del()
         call exit
      else
         nyp = ny/pp%getnvp()
      end if
      
      noff = pp%getidproc()*nyp

! Allocate the data array for data output
      allocate(arr(nx,nyp))
      allocate(arr3d(nx,nyp,nz))
      allocate(part(2,N))

! Loop for iterations
      do iter = 0, 5  

! Initialize the file data for 2D mesh Ex2d
         call file%new(iter=iter,&
                      &axisLabels=(/'x','z'/),&
                      &gridSpacing=(/1.0,1.0/),&
                      &gridGlobalOffset=(/0.0d0,0.0d0/),&
                      &gridUnitSI = 1.0d0, &
                      &position=(/0.0,0.0/),&
                      &unitDimension=(/1.d0,1.d0,-3.d0,-1.d0,0.d0,0.d0,0.d0/),&
                      &records='Ex2d')
   
! Prepare the data for Ex2d             
         do i = 1, nx
         do j = 1, nyp
            arr(i,j) = exp(-real(i-100-iter*10)**2/800) * exp(-real(j+noff-128)**2/200)
         end do
         end do

! Write Ex2d   
         call pwfield(pp,file,arr(:,:),(/nx,ny/),(/nx,nyp/),(/0,noff/),ierr)
         
! Initialize the file data for 3D mesh rho3d
         call file%new(iter=iter,&
                      &gridUnitSI=1.0d0, &
                      &axisLabels=(/'x','y','z'/),&
                      &gridSpacing=(/1.0,1.0,1.0/),&
                      &gridGlobalOffset=(/0.0d0,0.0d0,0.0d0/),&
                      &position=(/0.0,0.0,0.0/),&
                      &unitDimension=(/-3.d0,0.d0,1.d0,1.d0,0.d0,0.d0,0.d0/),&
                      &records='rho3d')
   
! Prepare the data for rho3d            
         do i = 1, nx
         do j = 1, nyp
         do k = 1, nz
            arr3d(i,j,k) = exp(-real(i-100-iter*10)**2/200)*exp(-real(j+noff-128)**2/800)&
            &*exp(-real(k-128)**2/200)
         end do
         end do
         end do

! Write rho3d  
         call pwfield(pp,file,arr3d(:,:,:),(/nx,ny,nz/),(/nx,nyp,nz/),(/0,noff,0/),ierr)
                  

! Prepare the data for particles
         call random_number(part)
! part(1,:) is the x position         
         part(1,:) = part(1,:) * 256

! Initialize the file data for particle postion x
         call file%new(iter=iter,&
                      &particleName='electron',&
                      &unitDimension=(/1.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0/),&
                      &records='position',&
                      &component='x')

! Write position x
         call pwpart(pp,file,part(1,:),N,ierr)

! Initialize the file data for particle momentum x
         call file%new(iter=iter,&
                      &particleName='electron',&
                      &unitDimension=(/1.d0,1.d0,-1.d0,0.d0,0.d0,0.d0,0.d0/),&
                      &records='momentum',&
                      &component='x')

! Write momentum x
         call pwpart(pp,file,part(2,:),N,ierr)

! Initialize the file data for particle postionOffset x
         call file%new(iter=iter,&
                      &particleName='electron',&
                      &unitDimension=(/1.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0/),&
                      &records='positionOffset',&
                      &component='x')

! Write positionOffset x
         call pwpart(pp,file,0.0,ierr)

! Initialize the file data for particle mass
         call file%new(iter=iter,&
                      &particleName='electron',&
                      &unitDimension=(/0.d0,1.d0,0.d0,0.d0,0.d0,0.d0,0.d0/),&
                      &records='mass',&
                      &component='')

! Write particle mass
         call pwpart(pp,file,1.0,ierr)
      
      end do

! Terminate MPI environment
      call p%del()
      
      end program 

