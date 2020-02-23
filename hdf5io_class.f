! hdf5io module for OpenPMD
! update: 11/03/2016

      module hdf5io_class

      use parallel_class
      use HDF5
      use mpi
         
      implicit none
      
      private
      
      public :: hdf5file, pwfield, pwpart
      
      integer, parameter :: dp = kind(1.d0)

      type hdf5file
         
         private
         
         character(len=100) :: openPMD = '1.0.0'
         integer :: openPMDextension = 0
         integer :: iter = 0
         character(len=100) :: base = 'data/'
         character(len=8) :: chiter = '00000000'
         character(len=100) :: basePath = '/data/%T/'
         character(len=100) :: meshesPath = 'fields/'
         character(len=100) :: particlesPath = 'particles/'
         character(len=100) :: iterationEncoding = 'fileBased'
         character(len=100) :: iterationFormat = 'data%T.h5'
         character(len=100) :: filenamebase = 'data'
         character(len=100) :: filename = 'data00000000.h5'
         real :: time = 0.0
         real :: dt = 1.0
         real(kind=dp) :: timeUnitSI = 1.0
         character(len=100) :: records = 'Ex'
         character(len=100) :: component = ''
         character(len=100) :: geometry = 'cartesian'
         character(len=100) :: geometryParameters = 'cartesian'
         character(len=100) :: dataOrder = 'F'
         character(len=10), dimension(:), pointer :: axisLabels => null()
         real, dimension(:), pointer :: gridSpacing => null()
         real(kind=dp), dimension(:), pointer :: gridGlobalOffset => null()
         real(kind=dp) :: gridUnitSI = 1.0
         real, dimension(:), pointer :: position => null()         
         character(len=100) :: particleName = 'particle'
         real(kind=dp) :: unitSI = 1.0
         real(kind=dp), dimension(7) :: unitDimension = (/0.,0.,0.,0.,0.,0.,0./)
         real :: timeOffset = 0.0
         character(len=100) :: filepath = './'
         
         contains
         
         generic :: new => init_hdf5file
         procedure, private :: init_hdf5file
         
      end type

      interface add_h5_atribute
         module procedure add_h5_atribute_str
         module procedure add_h5_atribute_v1_str
         module procedure add_h5_atribute_single
         module procedure add_h5_atribute_v1_single
         module procedure add_h5_atribute_int
         module procedure add_h5_atribute_v1_int
         module procedure add_h5_atribute_double         
         module procedure add_h5_atribute_v1_double         
      end interface
      
      interface pwfield
        module procedure pwfield_2d
        module procedure pwfield_3d
      end interface

      interface pwpart
        module procedure pwpart_array
        module procedure pwpart_const
      end interface
      
      contains
      
      subroutine init_hdf5file(this,openPMD, openPMDextension, iter, base,&
      &basePath, meshesPath, particlesPath, iterationEncoding, iterationFormat,&
      &filenamebase, time, dt, timeUnitSI, records, component, geometry,&
      &geometryParameters, dataOrder, axisLabels, gridSpacing, gridGlobalOffset,&
      &gridUnitSI, position, particleName, unitSI, unitDimension, timeOffset,filepath)

         implicit none
         
         class(hdf5file), intent(inout) :: this
         character(len=*), intent(in), optional :: openPMD
         integer, intent(in), optional :: openPMDextension
         integer, intent(in), optional :: iter
         character(len=*), intent(in), optional :: base
         character(len=*), intent(in), optional :: basePath
         character(len=*), intent(in), optional :: meshesPath
         character(len=*), intent(in), optional :: particlesPath
         character(len=*), intent(in), optional :: iterationEncoding
         character(len=*), intent(in), optional :: iterationFormat
         character(len=*), intent(in), optional :: filenamebase
         real, intent(in), optional :: time
         real, intent(in), optional :: dt
         real(kind=dp), intent(in), optional :: timeUnitSI
         character(len=*), intent(in), optional :: records
         character(len=*), intent(in), optional :: component
         character(len=*), intent(in), optional :: geometry
         character(len=*), intent(in), optional :: geometryParameters
         character(len=*), intent(in), optional :: dataOrder
         character(len=*), dimension(:), intent(in), optional :: axisLabels
         real, dimension(:), intent(in), optional :: gridSpacing
         real(kind=dp), dimension(:), intent(in), optional :: gridGlobalOffset
         real(kind=dp), intent(in), optional :: gridUnitSI
         real, dimension(:), intent(in), optional :: position
         character(len=*), intent(in), optional :: particleName
         real(kind=dp), intent(in), optional :: unitSI
         real(kind=dp), dimension(7), intent(in), optional :: unitDimension
         real, intent(in), optional :: timeOffset
         character(len=*), intent(in), optional :: filepath         
! local data
         character(len=8) :: chiter
         
         if (present(openPMD)) then
            this%openPMD = trim(openPMD)
         end if

         if (present(openPMDextension)) then
            this%openPMDextension = openPMDextension
         end if

         if (present(iter)) then
            this%iter = iter
         end if
         
         call str(this%iter,chiter,8)
         this%chiter = trim(chiter)
          
         if (present(base)) then
            this%base = trim(base)
         end if
         
         if (present(meshesPath)) then
            this%meshesPath = trim(meshesPath)
         end if

         if (present(particlesPath)) then
            this%particlesPath = trim(particlesPath)
         end if
         
         if (present(iterationEncoding)) then
            this%iterationEncoding = trim(iterationEncoding)
         end if

         if (present(filenamebase)) then
            this%filenamebase = trim(filenamebase)
         end if
         
         this%iterationFormat = trim(this%filenamebase)//'%T.h5'
         this%filename = trim(this%filenamebase)//trim(this%chiter)//'.h5'
         
         if (present(time)) then
            this%time = time
         end if

         if (present(dt)) then
            this%dt = dt
         end if

         if (present(timeUnitSI)) then
            this%timeUnitSI = timeUnitSI
         else
            write(*,*) 'timeUnitSI = 1'
            this%timeUnitSI = 1.0
         end if

         if (present(records)) then
            this%records = trim(records)
         end if

         if (present(component)) then
            this%component = component
         end if

         if (present(geometry)) then
            this%geometry = trim(geometry)
         end if

         if (present(geometryParameters)) then
            this%geometryParameters = trim(geometryParameters)
         end if

         if (present(dataOrder)) then
            this%dataOrder = trim(dataOrder)
         end if

         if (present(axisLabels)) then
            if(associated(this%axisLabels)) deallocate(this%axisLabels)
            allocate(this%axisLabels(size(axisLabels)))
            this%axisLabels = axisLabels
         end if
   
         if (present(gridSpacing)) then
            if(associated(this%gridSpacing)) deallocate(this%gridSpacing)
            allocate(this%gridSpacing(size(gridSpacing)))
            this%gridSpacing = gridSpacing
         end if

         if (present(gridGlobalOffset)) then
            if(associated(this%gridGlobalOffset)) deallocate(this%gridGlobalOffset)
            allocate(this%gridGlobalOffset(size(gridGlobalOffset)))
            this%gridGlobalOffset = gridGlobalOffset
         end if

         if (present(gridUnitSI)) then
            this%gridUnitSI = gridUnitSI
         else
            write(*,*) 'gridUnitSI=1'
            this%gridUnitSI = 1.0d0
         end if

         if (present(position)) then
            if(associated(this%position)) deallocate(this%position)
            allocate(this%position(size(position)))
            this%position = position
         end if

         if (present(particleName)) then
            this%particleName = trim(particleName)
         end if

         if (present(unitSI)) then
            this%unitSI = unitSI
         else
            this%unitSI = 1.0d0
         end if

         if (present(unitDimension)) then
            this%unitDimension = unitDimension
         end if

         if (present(timeOffset)) then
            this%timeOffset = timeOffset
         end if

         if (present(filepath)) then
            this%filepath = trim(filepath)
         end if

         this%filename = trim(this%filepath)//trim(this%filename)
         
         
      end subroutine init_hdf5file
!
      subroutine add_h5_atribute_str(objID, name, attribute)
        
         implicit none
          
         integer(hid_t), intent(in) :: objID
         character(len=*), intent(in) :: name
         character(len=*), intent(in) :: attribute
! local data          
         integer(hid_t) :: dataspaceID, typeID, attrID
         integer(hsize_t), dimension(1) :: dims
         integer(hsize_t) :: size
         integer :: ierr
          
         dims(1) = 1
         call h5screate_simple_f(0, dims, dataspaceID, ierr) 
         call h5tcopy_f(H5T_NATIVE_CHARACTER, typeID, ierr)
          
         size = len(trim(attribute))
         call h5tset_size_f(typeID, size, ierr)
          
         call h5acreate_f(objID, name, typeID, dataspaceID, attrID, ierr)
         call h5awrite_f(attrID, typeID, trim(attribute), dims, ierr)
         call h5aclose_f(attrID, ierr)
         call h5tclose_f(typeID, ierr)
         call h5sclose_f(dataspaceID, ierr)
        
      end subroutine add_h5_atribute_str        
!
      subroutine add_h5_atribute_v1_str(objID, name, attribute)
        
         implicit none
          
         integer(hid_t), intent(in) :: objID
         character(len=*), intent(in) :: name
         character(len=*), dimension(:), intent(in) :: attribute
! local data          
         integer(hid_t) :: dataspaceID, typeID, attrID
         integer(hsize_t), dimension(1) :: dims
         integer(hsize_t) :: maxlen
         integer :: i, ierr
          
         dims(1) = size(attribute)
         call h5screate_simple_f(1, dims, dataspaceID, ierr) 
          
         maxlen = len(attribute(1))

         call h5tcopy_f(H5T_NATIVE_CHARACTER, typeID, ierr)
         call h5tset_size_f(typeID, maxlen, ierr)
         
         call h5acreate_f(objID, name, typeID, dataspaceID, attrID, ierr)
         call h5awrite_f(attrID, typeID, attribute, dims, ierr)
         call h5aclose_f(attrID, ierr)
         call h5tclose_f(typeID, ierr)
         call h5sclose_f(dataspaceID, ierr)
        
      end subroutine add_h5_atribute_v1_str
!
      subroutine add_h5_atribute_single(objID, name, attribute)
        
         implicit none
          
         integer(hid_t), intent(in) :: objID
         character(len=*), intent(in) :: name
         real, intent(in) :: attribute
         
         integer(hid_t) :: dataspaceID, attrID
         integer(hid_t) :: d_float 
         integer(hsize_t), dimension(1) :: dims
         integer :: ierr
          
         d_float = detect_precision()
         dims(1) = 1
         call h5screate_simple_f(0, dims, dataspaceID, ierr) 
         call h5acreate_f(objID, name, d_float, dataspaceID, attrID, ierr)
         call h5awrite_f(attrID, d_float, attribute, dims, ierr)
         call h5aclose_f(attrID, ierr)
         call h5sclose_f(dataspaceID, ierr)
        
      end subroutine add_h5_atribute_single
!
      subroutine add_h5_atribute_v1_single(objID, name, attribute)
        
         implicit none
          
         integer(hid_t), intent(in) :: objID
         character(len=*), intent(in) :: name
         real, dimension(:), intent(in) :: attribute
          
         integer(hid_t) :: dataspaceID, attrID
         integer(hid_t) :: d_float
         integer(hsize_t), dimension(1) :: dims
         integer :: ierr
          
         d_float = detect_precision()
         dims(1) = size(attribute)
         call h5screate_simple_f(1, dims, dataspaceID, ierr) 
         call h5acreate_f(objID, name, d_float, dataspaceID, attrID, ierr)
         call h5awrite_f(attrID, d_float, attribute, dims, ierr)
         call h5aclose_f(attrID, ierr)
         call h5sclose_f(dataspaceID, ierr)
        
      end subroutine add_h5_atribute_v1_single
!
      subroutine add_h5_atribute_int(objID, name, attribute)

         implicit none
          
         integer(hid_t), intent(in) :: objID
         character(len=*), intent(in) :: name
         integer, intent(in) :: attribute
          
         integer(hid_t) :: dataspaceID, attrID
         integer(hsize_t), dimension(1) :: dims
         integer :: ierr
          
         dims(1) = 1
         call h5screate_simple_f(0, dims, dataspaceID, ierr) 
         call h5acreate_f(objID, name, H5T_NATIVE_INTEGER, dataspaceID,&
         &attrID, ierr)
         call h5awrite_f(attrID, H5T_NATIVE_INTEGER, attribute, dims, ierr)
         call h5aclose_f(attrID, ierr)
         call h5sclose_f(dataspaceID, ierr)
        
      end subroutine add_h5_atribute_int
!
      subroutine add_h5_atribute_v1_int(objID, name, attribute)
        
         implicit none
          
         integer(hid_t), intent(in) :: objID
         character(len=*), intent(in) :: name
         integer, dimension(:), intent(in) :: attribute
          
         integer(hid_t) :: dataspaceID, attrID
         integer(hsize_t), dimension(1) :: dims
         integer :: ierr
          
         dims(1) = size(attribute)
         call h5screate_simple_f(1, dims, dataspaceID, ierr) 
         call h5acreate_f(objID, name, H5T_NATIVE_INTEGER, dataspaceID,&
         &attrID, ierr)
         call h5awrite_f(attrID, H5T_NATIVE_INTEGER, attribute, dims, ierr)
         call h5aclose_f(attrID, ierr)
         call h5sclose_f(dataspaceID, ierr)
        
      end subroutine add_h5_atribute_v1_int
!      
      subroutine add_h5_atribute_double(objID, name, attribute)

         implicit none
          
         integer(hid_t), intent(in) :: objID
         character(len=*), intent(in) :: name
         real(kind=dp), intent(in) :: attribute
          
         integer(hid_t) :: dataspaceID, attrID
         integer(hsize_t), dimension(1) :: dims
         integer :: ierr
          
         dims(1) = 1
         call h5screate_simple_f(0, dims, dataspaceID, ierr) 
         call h5acreate_f(objID, name, H5T_NATIVE_DOUBLE, dataspaceID,&
         &attrID, ierr)
         call h5awrite_f(attrID, H5T_NATIVE_DOUBLE, attribute, dims, ierr)
         call h5aclose_f(attrID, ierr)
         call h5sclose_f(dataspaceID, ierr)
        
      end subroutine add_h5_atribute_double
!
      subroutine add_h5_atribute_v1_double(objID, name, attribute)
        
         implicit none
          
         integer(hid_t), intent(in) :: objID
         character(len=*), intent(in) :: name
         real(kind=dp), dimension(:), intent(in) :: attribute
          
         integer(hid_t) :: dataspaceID, attrID
         integer(hsize_t), dimension(1) :: dims
         integer :: ierr
          
         dims(1) = size(attribute)
         call h5screate_simple_f(1, dims, dataspaceID, ierr) 
         call h5acreate_f(objID, name, H5T_NATIVE_DOUBLE, dataspaceID,&
         &attrID, ierr)
         call h5awrite_f(attrID, H5T_NATIVE_DOUBLE, attribute, dims, ierr)
         call h5aclose_f(attrID, ierr)
         call h5sclose_f(dataspaceID, ierr)
        
      end subroutine add_h5_atribute_v1_double
!      
      subroutine createfile(pp,file,ierr)

         implicit none
        
         class(parallel), intent(in), pointer :: pp
         class(hdf5file), intent(in) :: file
         integer, intent(inout) :: ierr
! local data
         integer(hid_t) :: file_id, rootID, baseid, iterid
         integer(hid_t) :: flplID, xferID
         integer :: info
         logical :: fexist
         
         inquire(FILE=trim(file%filename), EXIST=fexist)
         call MPI_BARRIER(pp%getlworld(),ierr)
         if(fexist) return         
         
         call h5open_f(ierr)
         call h5pcreate_f(H5P_FILE_ACCESS_F, flplID, ierr)         
         call h5pcreate_f(H5P_DATASET_XFER_F, xferID, ierr)  
         info = MPI_INFO_NULL
         call h5pset_fapl_mpio_f(flplID, pp%getlworld(), info, ierr)
         call h5pset_dxpl_mpio_f(xferID, H5FD_MPIO_COLLECTIVE_F, ierr)    
         call h5fcreate_f(trim(file%filename),H5F_ACC_TRUNC_F,file_id,ierr,&
         &access_prp=flplID) 
         call h5gopen_f(file_id, '/', rootID, ierr)
         call add_h5_atribute(rootID, 'openPMD', trim(file%openPMD)) 
         call add_h5_atribute(rootID, 'openPMDextension', file%openPMDextension) 
         call add_h5_atribute(rootID, 'basePath', trim(file%basePath))
         call add_h5_atribute(rootID, 'meshesPath', trim(file%meshesPath)) 
         call add_h5_atribute(rootID, 'particlesPath', trim(file%particlesPath))
         call add_h5_atribute(rootID, 'iterationEncoding', trim(file%iterationEncoding)) 
         call add_h5_atribute(rootID, 'iterationFormat', trim(file%iterationFormat))

         call h5gcreate_f(rootID, trim(file%base), baseid, ierr) 
         call h5gcreate_f(baseid, trim(file%chiter), iterid, ierr) 
         call add_h5_atribute(iterid, 'dt', file%dt) 
         call add_h5_atribute(iterid, 'time', file%time) 
         call add_h5_atribute(iterid, 'timeUnitSI', file%timeUnitSI) 


         call h5gclose_f(iterid, ierr) 
         call h5gclose_f(baseid, ierr) 
         call h5gclose_f(rootID, ierr)
         call h5pclose_f(xferID, ierr)
         call h5pclose_f(flplID, ierr)
         call h5fclose_f(file_id, ierr)
         call h5close_f(ierr)

      end subroutine createfile
!      
      subroutine pwfield_3d(pp,file,fd,gs,ls,noff,ierr)

         implicit none
        
         class(parallel), intent(in), pointer :: pp
         class(hdf5file), intent(in) :: file
         real, dimension(:,:,:), intent(in) :: fd
         integer, dimension(3), intent(in) :: gs, ls
         integer, dimension(3), intent(in) :: noff
         integer, intent(inout) :: ierr
! local data
         integer(hid_t) :: treal,flplID, xferID, dcplID, memspaceID 
         integer(hid_t) :: file_id, rootID, meshid, dset_id, dspace_id, iterid
         integer(hsize_t), dimension(3) :: start
         integer(hsize_t), dimension(3) :: gsize, lsize
         integer(hsize_t), dimension(3) :: lnoff
         integer :: info
         logical :: gexist
	 

	 real(kind=dp), dimension(3) :: local_zeros=(/ 0.0 , 0.0, 0.0 /)
    real(kind=dp), dimension(3) :: local_ones =(/ 1.0, 1.0, 1.0 /)


                  
         ierr = 0
         gsize = gs
         lsize = ls
         lnoff = noff
         
         call createfile(pp,file,ierr)
         call h5open_f(ierr)
         treal = detect_precision()
         call h5pcreate_f(H5P_FILE_ACCESS_F, flplID, ierr)         
         call h5pcreate_f(H5P_DATASET_CREATE_F, dcplID, ierr)         
         call h5pcreate_f(H5P_DATASET_XFER_F, xferID, ierr)  
         info = MPI_INFO_NULL
         call h5pset_fapl_mpio_f(flplID, pp%getlworld(), info, ierr)
         call h5pset_dxpl_mpio_f(xferID, H5FD_MPIO_COLLECTIVE_F, ierr)    
      
         call h5fopen_f(trim(file%filename),H5F_ACC_RDWR_F,file_id,ierr,&
         &access_prp=flplID) 
         call h5screate_simple_f(3, gsize, dspace_id, ierr)
         call h5screate_simple_f(3, lsize, memspaceID, ierr)
         call h5gopen_f(file_id, '/', rootID, ierr)
         call h5gopen_f(rootID, trim(file%base)//trim(file%chiter), iterid, ierr)
         call h5lexists_f(iterid, trim(file%meshesPath), gexist, ierr)
         if (gexist) then
            call h5gopen_f(iterid, trim(file%meshesPath), meshid, ierr)
         else
            call h5gcreate_f(iterid, trim(file%meshesPath), meshid, ierr)
         end if
         call h5dcreate_f(meshid, trim(file%records), treal, dspace_id, dset_id,&
         &ierr, dcplID)

         start(1) = lnoff(1)
         start(2) = lnoff(2)
         start(3) = lnoff(3)
   
         call h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, start, lsize,&
         &ierr)

         call h5dwrite_f(dset_id, treal, fd(1:lsize(1),1:lsize(2),1:lsize(3)),&
         &lsize, ierr, memspaceID, dspace_id, xfer_prp=xferID)

         call add_h5_atribute(dset_id, 'axisLabels', file%axisLabels) 
         call add_h5_atribute(dset_id, 'dataOrder', trim(file%dataOrder)) 
         call add_h5_atribute(dset_id, 'geometry', trim(file%geometry)) 
         call add_h5_atribute(dset_id, 'geometryParameters', trim(file%geometryParameters)) 
         call add_h5_atribute(dset_id, 'gridSpacing', file%gridSpacing) 
         call add_h5_atribute(dset_id, 'gridUnitSI', file%gridUnitSI) 
         call add_h5_atribute(dset_id, 'timeOffset', file%timeOffset) 
         call add_h5_atribute(dset_id, 'unitDimension', file%unitDimension) 
         if (associated(file%gridGlobalOffset)) then 
	     call add_h5_atribute(dset_id, 'gridGlobalOffset', file%gridGlobalOffset) 
	 else
	     call add_h5_atribute(dset_id, 'gridGlobalOffset', local_zeros )
	 endif
         if (associated(file%position)) then 
	     call add_h5_atribute(dset_id, 'position', file%position) 
	 else
	     call add_h5_atribute(dset_id, 'position', local_zeros )
	 endif

         call add_h5_atribute(dset_id, 'unitSI', file%unitSI) 

         call h5sclose_f(memspaceID, ierr)
         call h5sclose_f(dspace_id, ierr)
         call h5pclose_f(xferID, ierr)
         call h5pclose_f(dcplID, ierr)
         call h5pclose_f(flplID, ierr)
         call h5gclose_f(rootID, ierr)
         call h5dclose_f(dset_id, ierr)
         call h5gclose_f(meshid, ierr)
         call h5gclose_f(iterid, ierr)
         call h5fclose_f(file_id, ierr)
         call h5close_f(ierr)
               
      end subroutine pwfield_3d
!
      subroutine pwfield_2d(pp,file,fd,gs,ls,noff,ierr)

         implicit none
        
         class(parallel), intent(in), pointer :: pp
         class(hdf5file), intent(in) :: file
         real, dimension(:,:), intent(in) :: fd
         integer, dimension(2), intent(in) :: gs, ls
         integer, dimension(2), intent(in) :: noff
         integer, intent(inout) :: ierr
! local data
         integer(hid_t) :: treal,flplID, xferID, dcplID, memspaceID
         integer(hid_t) :: file_id, rootID, meshid, dset_id, dspace_id, iterid
         integer(hsize_t), dimension(2) :: start
         integer(hsize_t), dimension(2) :: gsize, lsize
         integer(hsize_t), dimension(2) :: lnoff
         integer :: info
         logical :: gexist
	 real(kind=dp), dimension(2) :: local_zeros=(/ 0.0, 0.0 /)
    real(kind=dp), dimension(2) :: local_ones= (/ 1.0d0 1.0d0  /)
                  
         ierr = 0
         gsize = gs
         lsize = ls
         lnoff = noff
                  
         call createfile(pp,file,ierr)
         call h5open_f(ierr)
         treal = detect_precision()
         call h5pcreate_f(H5P_FILE_ACCESS_F, flplID, ierr)         
         call h5pcreate_f(H5P_DATASET_CREATE_F, dcplID, ierr)         
         call h5pcreate_f(H5P_DATASET_XFER_F, xferID, ierr)  
         info = MPI_INFO_NULL
         call h5pset_fapl_mpio_f(flplID, pp%getlworld(), info, ierr)
         call h5pset_dxpl_mpio_f(xferID, H5FD_MPIO_COLLECTIVE_F, ierr)    
      
         call h5fopen_f(trim(file%filename),H5F_ACC_RDWR_F,file_id,ierr,&
         &access_prp=flplID) 
         call h5screate_simple_f(2, gsize, dspace_id, ierr)
         call h5screate_simple_f(2, lsize, memspaceID, ierr)
         call h5gopen_f(file_id, '/', rootID, ierr)
         call h5gopen_f(rootID, trim(file%base)//trim(file%chiter), iterid, ierr)
         call h5lexists_f(iterid, trim(file%meshesPath), gexist, ierr)
         if (gexist) then
            call h5gopen_f(iterid, trim(file%meshesPath), meshid, ierr)
         else
            call h5gcreate_f(iterid, trim(file%meshesPath), meshid, ierr)
         end if
         call h5dcreate_f(meshid, trim(file%records), treal, dspace_id, dset_id,&
         &ierr, dcplID)

         start(1) = lnoff(1)
         start(2) = lnoff(2)
   
         call h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, start, lsize,&
         &ierr)

         call h5dwrite_f(dset_id, treal, fd(1:lsize(1),1:lsize(2)),&
         &lsize, ierr, memspaceID, dspace_id, xfer_prp=xferID)

         call add_h5_atribute(dset_id, 'axisLabels', file%axisLabels) 
         call add_h5_atribute(dset_id, 'dataOrder', trim(file%dataOrder)) 
         call add_h5_atribute(dset_id, 'geometry', trim(file%geometry)) 
         call add_h5_atribute(dset_id, 'geometryParameters', trim(file%geometryParameters)) 
         call add_h5_atribute(dset_id, 'gridSpacing', file%gridSpacing)
         write(*,*)'gridUnitSI=',file%gridUnitSI
         call add_h5_atribute(dset_id, 'gridUnitSI', file%gridUnitSI) 
         call add_h5_atribute(dset_id, 'timeOffset', file%timeOffset) 
         call add_h5_atribute(dset_id, 'unitDimension', file%unitDimension) 
         if (associated(file%gridGlobalOffset)) then 
	     call add_h5_atribute(dset_id, 'gridGlobalOffset', file%gridGlobalOffset) 
	 else
	     call add_h5_atribute(dset_id, 'gridGlobalOffset', local_zeros)
	 endif
	     
         if (associated(file%position)) then 
	     call add_h5_atribute(dset_id, 'position', file%position) 
	 else
	     call add_h5_atribute(dset_id, 'position', local_zeros)
	 endif
         call add_h5_atribute(dset_id, 'unitSI', file%unitSI) 


         call h5sclose_f(memspaceID, ierr)
         call h5sclose_f(dspace_id, ierr)
         call h5pclose_f(xferID, ierr)
         call h5pclose_f(dcplID, ierr)
         call h5pclose_f(flplID, ierr)
         call h5gclose_f(rootID, ierr)
         call h5gclose_f(iterID, ierr)
         call h5dclose_f(dset_id, ierr)
         call h5gclose_f(meshid, ierr)
         call h5fclose_f(file_id, ierr)
         call h5close_f(ierr)
               
      end subroutine pwfield_2d
!
      subroutine pwpart_array(pp,file,part,npp,ierr)
     
         implicit none

         class(parallel), intent(in), pointer :: pp
         class(hdf5file), intent(in) :: file
         real, dimension(:), intent(in) :: part
         integer, intent(in) :: npp
         integer, intent(inout) :: ierr
! local data
         integer :: tnpp, tp, i, j
         integer :: color, pgrp, pid, pnvp
         integer(hsize_t), dimension(1) :: ldim
         integer, dimension(:), pointer :: np
         integer, dimension(:,:), pointer:: dims
         real, dimension(:), pointer :: buff
         integer(hsize_t), dimension(1) :: start,maxdim
         integer(hid_t) :: treal
         integer(hid_t) :: flplID, xferID
         integer(hid_t) :: file_id, rootID, partid, specid, recid, iterid
         integer(hid_t) :: dset_id, dspace_id, memspaceID
         integer :: info
         integer, dimension(10) :: istat
         logical :: gexist
         
         ierr = 0
         ldim(1) = 1

         call createfile(pp,file,ierr)
         call h5open_f(ierr)
         
         treal = detect_precision()
         
         tnpp = npp
         tp = 0
         call MPI_ALLREDUCE(tnpp,tp,1,MPI_INTEGER,MPI_SUM,pp%getlworld(),ierr)

         if (tnpp > 0) then 
            color = 1
         else
            color = MPI_UNDEFINED
         end if
         call MPI_COMM_SPLIT(pp%getlworld(), color, 0, pgrp, ierr)

         if (tnpp > 0) then
            call MPI_COMM_RANK(pgrp, pid, ierr)
            call MPI_COMM_SIZE(pgrp, pnvp, ierr)
            allocate(np(pnvp), dims(2,pnvp))
            call MPI_ALLGATHER(tnpp, 1, MPI_INTEGER, np, 1, MPI_INTEGER,&
            &pgrp, ierr)
            dims(1, 1) = 1
            dims(2, 1) = np(1) 
            do i = 2, pnvp
               dims(1,i) = dims(2,i-1) + 1
               dims(2,i) = dims(1,i) + np(i) - 1
            enddo
            allocate(buff(tnpp))

            call h5pcreate_f(H5P_FILE_ACCESS_F, flplID, ierr)         
            call h5pcreate_f(H5P_DATASET_XFER_F, xferID, ierr)  
            info = MPI_INFO_NULL
            call h5pset_fapl_mpio_f(flplID, pgrp, info, ierr)
            call h5pset_dxpl_mpio_f(xferID, H5FD_MPIO_COLLECTIVE_F, ierr) 
            call h5fopen_f(trim(file%filename), H5F_ACC_RDWR_F, file_id, ierr,&
            &access_prp=flplID) 
            call h5gopen_f(file_id, '/', rootID, ierr)
            call h5gopen_f(rootID, trim(file%base)//trim(file%chiter), iterid, ierr)
            call h5lexists_f(iterid, trim(file%particlesPath), gexist, ierr)
            if (gexist) then
               call h5gopen_f(iterid, trim(file%particlesPath), partid, ierr)
            else
               call h5gcreate_f(iterid, trim(file%particlesPath), partid, ierr)
            end if

            call h5lexists_f(partid, trim(file%particleName), gexist, ierr)
            if (gexist) then
               call h5gopen_f(partid, trim(file%particleName), specid, ierr)
            else
               call h5gcreate_f(partid, trim(file%particleName), specid, ierr)
            end if

            call h5lexists_f(specid, trim(file%records), gexist, ierr)
            if (gexist) then
               call h5gopen_f(specid, trim(file%records), recid, ierr)
            else
               call h5gcreate_f(specid, trim(file%records), recid, ierr)
               call add_h5_atribute(recid, 'timeOffset', file%timeOffset) 
               call add_h5_atribute(recid, 'unitDimension', file%unitDimension) 
            end if

            buff(1:tnpp) = part(1:tnpp)
            ldim(1) = tp
            call h5screate_simple_f(1, ldim, dspace_id, ierr)
            call h5dcreate_f(recid, trim(file%component), treal, dspace_id, dset_id,&
            &ierr)
            ldim(1) = tnpp
            call h5screate_simple_f(1, ldim, memspaceID, ierr)
            start = dims(1,pid+1) - 1
            call h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F,start,&
            &ldim, ierr)
            call h5dwrite_f(dset_id, treal, buff, ldim, ierr, memspaceID,&
            &dspace_id, xfer_prp=xferID)

            call add_h5_atribute(dset_id, 'unitSI', file%unitSI) 

            call h5sclose_f(memspaceID, ierr)
            call h5sclose_f(dspace_id, ierr)
            call h5dclose_f(dset_id, ierr)

            call h5pclose_f(xferID, ierr)
            call h5pclose_f(flplID, ierr)
            call h5gclose_f(partid, ierr)
            call h5gclose_f(specid, ierr)
            call h5gclose_f(iterid, ierr)
            call h5gclose_f(recid, ierr)
            call h5gclose_f(rootID, ierr)
            call h5fclose_f(file_id, ierr)
            deallocate(np,dims,buff)
         end if            

         if (pgrp /= MPI_COMM_NULL) then
            call MPI_COMM_FREE(pgrp, ierr)
         end if

         call h5close_f(ierr)
      end subroutine pwpart_array
!
      subroutine pwpart_const(pp,file,value,ierr)
     
         implicit none

         class(parallel), intent(in), pointer :: pp
         class(hdf5file), intent(in) :: file
         real, intent(in) :: value
         integer, intent(inout) :: ierr
! local data
         integer(hsize_t), dimension(1) :: ldim
         integer(hid_t) :: treal
         integer(hid_t) :: flplID, xferID
         integer(hid_t) :: file_id, rootID, partid, specid, recid, iterid, compid
         integer(hid_t) :: dspace_id, memspaceID
         integer :: info
         logical :: gexist
               
         ierr = 0
         ldim(1) = 1

         call createfile(pp,file,ierr)
         call h5open_f(ierr)
         
         treal = detect_precision()
         
         call h5pcreate_f(H5P_FILE_ACCESS_F, flplID, ierr)         
         call h5pcreate_f(H5P_DATASET_XFER_F, xferID, ierr)  
         info = MPI_INFO_NULL
         call h5pset_fapl_mpio_f(flplID, pp%getlworld(), info, ierr)
         call h5pset_dxpl_mpio_f(xferID, H5FD_MPIO_COLLECTIVE_F, ierr)    
         call h5fopen_f(trim(file%filename), H5F_ACC_RDWR_F, file_id, ierr,&
         &access_prp=flplID) 

         call h5gopen_f(file_id, '/', rootID, ierr)
         call h5gopen_f(rootID, trim(file%base)//trim(file%chiter), iterid, ierr)
         call h5lexists_f(iterid, trim(file%particlesPath), gexist, ierr)
         if (gexist) then
            call h5gopen_f(iterid, trim(file%particlesPath), partid, ierr)
         else
            call h5gcreate_f(iterid, trim(file%particlesPath), partid, ierr)
         end if
         call h5lexists_f(partid, trim(file%particleName), gexist, ierr)
         if (gexist) then
            call h5gopen_f(partid, trim(file%particleName), specid, ierr)
         else
            call h5gcreate_f(partid, trim(file%particleName), specid, ierr)
         end if
         
         call h5lexists_f(specid, trim(file%records), gexist, ierr)
         if (gexist) then
            call h5gopen_f(specid, trim(file%records), recid, ierr)
         else
            call h5gcreate_f(specid, trim(file%records), recid, ierr)
            call add_h5_atribute(recid, 'timeOffset', file%timeOffset) 
            call add_h5_atribute(recid, 'unitDimension', file%unitDimension) 
         end if
         
         if(len(trim(file%component)) > 0) then
            call h5gcreate_f(recid, trim(file%component), compid, ierr)
            call add_h5_atribute(compid, 'unitSI', file%unitSI) 
            call add_h5_atribute(compid, 'value', value) 
            call add_h5_atribute(compid, 'shape', 1) 
            call h5gclose_f(compid, ierr)
         else
            call add_h5_atribute(recid, 'unitSI', file%unitSI) 
            call add_h5_atribute(recid, 'value', value) 
            call add_h5_atribute(recid, 'shape', 1) 
         end if
         
         call h5pclose_f(xferID, ierr)
         call h5pclose_f(flplID, ierr)
         call h5gclose_f(iterid, ierr)
         call h5gclose_f(partid, ierr)
         call h5gclose_f(specid, ierr)
         call h5gclose_f(recid, ierr)
         call h5gclose_f(rootID, ierr)
         call h5fclose_f(file_id, ierr)
         call h5close_f(ierr)

      end subroutine pwpart_const
!
      subroutine str(int_in,string,ndigits)

      implicit none
      integer, intent(in) :: int_in, ndigits
      character(len=*), intent(inout) :: string
      
! local variables      
      integer  ::  izero, i, nd, m
      character(len=20)  :: chindx 
      
      m = 1 
      izero =  ichar('0')
      if (ndigits > 20) then 
         nd = 20
      else
         nd = ndigits
      endif
      if (nd > len(string)) then 
         nd = len(string)
      endif

      chindx = ''
      do i = nd, 1, -1 
         m = 10**(i-1)
         chindx = trim(chindx) // char(  izero + mod( int_in/m , 10 ) ) 
      enddo 
      string = trim(chindx)

      end subroutine str    
!
      function detect_precision()
         integer(hid_t) :: detect_precision
! local data
         real :: small

         small = 1.0e-12
         small = 1.0 + small
         if (small>1.0) then 
            detect_precision = H5T_NATIVE_DOUBLE 
         else
            detect_precision = H5T_NATIVE_REAL
         endif       

      end function detect_precision     
!      
      end module hdf5io_class

