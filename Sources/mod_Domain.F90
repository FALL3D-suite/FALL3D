!***********************************************************************
!>
!> Module for domain decomposition and MPI interprocessor data exchange.
!> @details
!> The code is compiled in parallel if the macro WITH_MPI is defined and
!> in serial otherwise.
!> @note
!> Module public variables are defined only after calling domain_decompose (mandatory)
!> @author
!> Arnau Folch
!>
!**********************************************************************
MODULE Domain
  use KindType
  use Parallel
  implicit none
  save
  !
  !    LIST OF PUBLIC VARIABLES defined when calling domain_decompose (mandatory)
  !
  integer(ip) :: COMM_GRID            !< MPI grid communicator
  integer(ip) :: COMM_GRIDZ           !< MPI grid communicator along z (used for parallel sums along z)
  integer(ip) :: COMM_GRIDY           !< MPI grid communicator along y (used for parallel sums along y)
  integer(ip) :: COMM_GRIDX           !< MPI grid communicator along x (used for parallel sums along x)
  integer(ip) :: my_grid_coord(3)     !< grid coordinates of mype_model
  integer(ip) :: my_W_proc            !< my W (left)   processor id
  integer(ip) :: my_E_proc            !< my E (right)  processor id
  integer(ip) :: my_N_proc            !< my N (top)    processor id
  integer(ip) :: my_S_proc            !< my S (bottom) processor id
  integer(ip) :: my_U_proc            !< my U (up)     processor id
  integer(ip) :: my_D_proc            !< my D (down)   processor id
  integer(ip) :: my_ips               !< my start mass point local index along i
  integer(ip) :: my_jps               !< my start mass point local index along j
  integer(ip) :: my_kps               !< my start mass point local index along k
  integer(ip) :: my_ipe               !< my end   mass point local index along i
  integer(ip) :: my_jpe               !< my end   mass point local index along j
  integer(ip) :: my_kpe               !< my end   mass point local index along k
  integer(ip) :: my_ips_2h            !< my start mass point local index along i with 2 halo
  integer(ip) :: my_jps_2h            !< my start mass point local index along j with 2 halo
  integer(ip) :: my_kps_2h            !< my start mass point local index along k with 2 halo
  integer(ip) :: my_ipe_2h            !< my end   mass point local index along i with 2 halo
  integer(ip) :: my_jpe_2h            !< my end   mass point local index along j with 2 halo
  integer(ip) :: my_kpe_2h            !< my end   mass point local index along k with 2 halo
  integer(ip) :: my_ibs               !< my start boundary   local index along i
  integer(ip) :: my_jbs               !< my start boundary   local index along j
  integer(ip) :: my_kbs               !< my start boundary   local index along k
  integer(ip) :: my_ibe               !< my end   boundary   local index along i
  integer(ip) :: my_jbe               !< my end   boundary   local index along j
  integer(ip) :: my_kbe               !< my end   boundary   local index along k
  integer(ip) :: my_ibs_1h            !< my start boundary   local index along i with 1 halo
  integer(ip) :: my_jbs_1h            !< my start boundary   local index along j with 1 halo
  integer(ip) :: my_kbs_1h            !< my start boundary   local index along k with 1 halo
  integer(ip) :: my_ibe_1h            !< my end   boundary   local index along i with 1 halo
  integer(ip) :: my_jbe_1h            !< my end   boundary   local index along j with 1 halo
  integer(ip) :: my_kbe_1h            !< my end   boundary   local index along k with 1 halo
  !
  integer(ip) :: gl_npx               !< global number of mass points along x
  integer(ip) :: gl_npy               !< global number of mass points along y
  integer(ip) :: gl_npz               !< global number of mass points along z
  integer(ip) :: gl_nbx               !< global number of boundaries  along x
  integer(ip) :: gl_nby               !< global number of boundaries  along y
  integer(ip) :: gl_nbz               !< global number of boundaries  along z
  !
  integer(ip), allocatable :: gl_ips   (:)   !< gl_ips   (npes_model) : value of my_ips    for all processors
  integer(ip), allocatable :: gl_jps   (:)   !< gl_jps   (npes_model) : value of my_jps    for all processors
  integer(ip), allocatable :: gl_kps   (:)   !< gl_kps   (npes_model) : value of my_kps    for all processors
  integer(ip), allocatable :: gl_ipe   (:)   !< gl_ipe   (npes_model) : value of my_ipe    for all processors
  integer(ip), allocatable :: gl_jpe   (:)   !< gl_jpe   (npes_model) : value of my_jpe    for all processors
  integer(ip), allocatable :: gl_kpe   (:)   !< gl_kpe   (npes_model) : value of my_kpe    for all processors
  integer(ip), allocatable :: gl_ips_2h(:)   !< gl_ips_2h(npes_model) : value of my_ips_2h for all processors
  integer(ip), allocatable :: gl_jps_2h(:)   !< gl_jps_2h(npes_model) : value of my_jps_2h for all processors
  integer(ip), allocatable :: gl_kps_2h(:)   !< gl_kps_2h(npes_model) : value of my_kps_2h for all processors
  integer(ip), allocatable :: gl_ipe_2h(:)   !< gl_ipe_2h(npes_model) : value of my_ipe_2h for all processors
  integer(ip), allocatable :: gl_jpe_2h(:)   !< gl_jpe_2h(npes_model) : value of my_jpe_2h for all processors
  integer(ip), allocatable :: gl_kpe_2h(:)   !< gl_kpe_2h(npes_model) : value of my_kpe_2h for all processors
  integer(ip), allocatable :: gl_ibs   (:)   !< gl_ibs   (npes_model) : value of my_ibs    for all processors
  integer(ip), allocatable :: gl_jbs   (:)   !< gl_jbs   (npes_model) : value of my_jbs    for all processors
  integer(ip), allocatable :: gl_kbs   (:)   !< gl_kbs   (npes_model) : value of my_kbs    for all processors
  integer(ip), allocatable :: gl_ibe   (:)   !< gl_ibe   (npes_model) : value of my_ibe    for all processors
  integer(ip), allocatable :: gl_jbe   (:)   !< gl_jbe   (npes_model) : value of my_jbe    for all processors
  integer(ip), allocatable :: gl_kbe   (:)   !< gl_kbe   (npes_model) : value of my_kbe    for all processors
  integer(ip), allocatable :: gl_ibs_1h(:)   !< gl_ibs_1h(npes_model) : value of my_ibs_1h for all processors
  integer(ip), allocatable :: gl_jbs_1h(:)   !< gl_jbs_1h(npes_model) : value of my_jbs_1h for all processors
  integer(ip), allocatable :: gl_kbs_1h(:)   !< gl_kbs_1h(npes_model) : value of my_kbs_1h for all processors
  integer(ip), allocatable :: gl_ibe_1h(:)   !< gl_ibe_1h(npes_model) : value of my_ibe_1h for all processors
  integer(ip), allocatable :: gl_jbe_1h(:)   !< gl_jbe_1h(npes_model) : value of my_jbe_1h for all processors
  integer(ip), allocatable :: gl_kbe_1h(:)   !< gl_kbe_1h(npes_model) : value of my_kbe_1h for all processors
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: domain_decompose
  PUBLIC :: domain_swap_mass_points_2halo_first
  PUBLIC :: domain_swap_mass_points_2halo_x
  PUBLIC :: domain_swap_mass_points_2halo_2Dx
  PUBLIC :: domain_swap_mass_points_2halo_y
  PUBLIC :: domain_swap_mass_points_2halo_2Dy
  PUBLIC :: domain_swap_mass_points_2halo_z
  PUBLIC :: domain_swap_mass_points_2halo_1Dx
  PUBLIC :: domain_swap_mass_points_2halo_1Dy
  PUBLIC :: domain_swap_mass_points_2halo_1Dz
  PUBLIC :: domain_swap_mass_points_2halo_corners_x
  PUBLIC :: domain_swap_mass_points_2halo_corners_y
  PUBLIC :: domain_swap_mass_points_2halo_corners_z
  PUBLIC :: domain_swap_velo_points_1halo_x
  PUBLIC :: domain_swap_velo_points_1halo_y
  PUBLIC :: domain_swap_velo_points_1halo_reshaped_y
  PUBLIC :: domain_swap_velo_points_1halo_z
  PUBLIC :: domain_gather_mass_points_2halo
  PUBLIC :: domain_gather_mass_points_2halo_2D
  PUBLIC :: domain_gather_corner_points_0halo
  PUBLIC :: domain_gather_corner_points_0halo_2D
  PUBLIC :: domain_scatter_mass_points_2halo
  PUBLIC :: domain_scatter_corner_points_0halo
  PUBLIC :: domain_scatter_corner_points_0halo_2D
  !
CONTAINS
  !
  !-------------------------------
  !    subroutine domain_decompose
  !-------------------------------
  !
  !>   @brief
  !>   Performs a 3D domain decomposition and assigns loal indexes and the rest of module public variables
  !
  subroutine domain_decompose(np,mproc,periods,MY_ERR)
    implicit none
    !
    !>   @param np(3)      Number of grid mass points along each direction
    !>   @param mproc(3)   Number of processors along each direction
    !>   @param periods(3) Periodicity of boundaries
    !>   @param MY_ERR     Error handler
    !
    integer(ip),        intent(IN)    :: np     (3)
    integer(ip),        intent(IN)    :: mproc  (3)
    logical,            intent(IN)    :: periods(3)
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip) :: npx_min_proc,npx_left
    integer(ip) :: npy_min_proc,npy_left
    integer(ip) :: npz_min_proc,npz_left
    integer(ip) :: grid_coord
    integer(ip) :: ips,ipe,jps,jpe,kps,kpe

#if defined WITH_MPI
    integer(ip) :: color,rank
#endif
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'domain_decompose'
    MY_ERR%message = ' '
    !
    !*** Store global dimensions
    !
    gl_npx = np(1)
    gl_npy = np(2)
    gl_npz = np(3)
    gl_nbx = gl_npx + 1
    gl_nby = gl_npy + 1
    gl_nbz = gl_npz + 1
    !
#if defined WITH_MPI
    !
    !*** Get Cartesian mesh communicator COMM_GRID
    !
    call MPI_CART_CREATE(COMM_MODEL,3_ip,mproc,periods,.false.,COMM_GRID,MY_ERR%flag)
    !
    !*** Get my_grid_coord(3), i.e. the cartesian grid coods of each processor
    !
    call MPI_CART_COORDS(COMM_GRID,mype_model,3_ip,my_grid_coord,MY_ERR%flag)
    !
    !*** Get processor neighbours
    !
    call MPI_CART_SHIFT(COMM_GRID, 0_ip, 1_ip, my_W_proc, my_E_proc, MY_ERR%flag)
    call MPI_CART_SHIFT(COMM_GRID, 1_ip, 1_ip, my_S_proc, my_N_proc, MY_ERR%flag)
    call MPI_CART_SHIFT(COMM_GRID, 2_ip, 1_ip, my_D_proc, my_U_proc, MY_ERR%flag)
    !
    my_W_proc = max(my_W_proc,-1)
    my_E_proc = max(my_E_proc,-1)
    my_S_proc = max(my_S_proc,-1)
    my_N_proc = max(my_N_proc,-1)
    my_U_proc = max(my_U_proc,-1)
    my_D_proc = max(my_D_proc,-1)
    !
    !*** Computes the (minimum) number of points per processor along
    !*** each direction and checks. Note that each processor must have at least
    !*** 4 points along each direction to prevent overlap of left and right halos
    !
    npx_min_proc =  gl_npx / mproc(1)
    npx_left     =  gl_npx - npx_min_proc*mproc(1)
    if(mproc(1).gt.1) then
       if(npx_min_proc.lt.4) then
          MY_ERR%flag    = 1
          MY_ERR%message = 'too many processors along x direction'
          return
       end if
    end if
    !
    npy_min_proc =  gl_npy / mproc(2)
    npy_left     =  gl_npy - npy_min_proc*mproc(2)
    if(mproc(2).gt.1) then
       if(npy_min_proc.lt.4) then
          MY_ERR%flag    = 1
          MY_ERR%message = 'too many processors along y direction'
          return
       end if
    end if
    !
    npz_min_proc =  gl_npz / mproc(3)
    npz_left     =  gl_npz - npz_min_proc*mproc(3)
    if(mproc(3).gt.1) then
       if(npz_min_proc.lt.4) then
          MY_ERR%flag    = 1
          MY_ERR%message = 'too many processors along z direction'
          return
       end if
    end if
    !
    !*** Groups of processors for integration across one dimension (parallel summ)
    !
    color = my_grid_coord(1)*mproc(2) + my_grid_coord(2)
    rank  = my_grid_coord(3)
    CALL MPI_COMM_SPLIT(COMM_GRID, color, rank, COMM_GRIDZ, MY_ERR%flag )
    !
    color = my_grid_coord(1)*mproc(3) + my_grid_coord(3)
    rank  = my_grid_coord(2)
    CALL MPI_COMM_SPLIT(COMM_GRID, color, rank, COMM_GRIDY, MY_ERR%flag )
    !
    color = my_grid_coord(2)*mproc(3) + my_grid_coord(3)
    rank  = my_grid_coord(1)
    CALL MPI_COMM_SPLIT(COMM_GRID, color, rank, COMM_GRIDX, MY_ERR%flag )
    !
#else
    !
    COMM_GRID  = 0
    COMM_GRIDZ = 0
    COMM_GRIDY = 0
    COMM_GRIDX = 0
    my_grid_coord(1) = 0
    my_grid_coord(2) = 0
    my_grid_coord(3) = 0
    my_W_proc    = -1        ! always boundary
    my_E_proc    = -1
    my_S_proc    = -1
    my_N_proc    = -1
    my_U_proc    = -1
    my_D_proc    = -1
    npx_min_proc = np(1)
    npy_min_proc = np(2)
    npz_min_proc = np(3)
    npx_left     = 0
    npy_left     = 0
    npz_left     = 0
    !
#endif
    !
    !*** Get processor local index ranges for boundaries and mass points.
    !*** Note that the remaining cells are distributed among processors to
    !*** prevent work load unbalance
    !
    ips    = 1
    my_ips = ips                                         ! first  processor along x
    do grid_coord = 1,mproc(1)-1                         ! second processor along x onwards
       ips = ips + npx_min_proc
       if(npx_left.gt.(grid_coord-1)) ips = ips + 1
       !
       if(grid_coord.eq.my_grid_coord(1)) my_ips = ips
    end do
    !
    ipe = npx_min_proc
    if(npx_left.gt.0) ipe = ipe + 1
    my_ipe = ipe                                         ! first  processor along x
    do grid_coord = 1,mproc(1)-1                         ! second processor along x onwards
       ipe = ipe + npx_min_proc
       if(npx_left.gt.grid_coord) ipe = ipe + 1
       !
       if(grid_coord.eq.my_grid_coord(1)) my_ipe = ipe
    end do
    my_ibs    = my_ips
    my_ibe    = my_ipe + 1
    my_ips_2h = my_ips - 2   ! 2 halo for mass points
    my_ipe_2h = my_ipe + 2
    my_ibs_1h = my_ibs - 1   ! 1 halo for boundaries
    my_ibe_1h = my_ibe + 1
    !
    !
    jps    = 1
    my_jps = jps                                         ! first  processor along y
    do grid_coord = 1,mproc(2)-1                         ! second processor along y onwards
       jps = jps + npy_min_proc
       if(npy_left.gt.(grid_coord-1)) jps = jps + 1
       !
       if(grid_coord.eq.my_grid_coord(2)) my_jps = jps
    end do
    !
    jpe = npy_min_proc
    if(npy_left.gt.0) jpe = jpe + 1
    my_jpe = jpe                                         ! first  processor along y
    do grid_coord = 1,mproc(2)-1                         ! second processor along y onwards
       jpe = jpe + npy_min_proc
       if(npy_left.gt.grid_coord) jpe = jpe + 1
       !
       if(grid_coord.eq.my_grid_coord(2)) my_jpe = jpe
    end do
    my_jbs    = my_jps
    my_jbe    = my_jpe + 1
    my_jps_2h = my_jps - 2   ! 2 halo for mass points
    my_jpe_2h = my_jpe + 2
    my_jbs_1h = my_jbs - 1   ! 1 halo for boundaries
    my_jbe_1h = my_jbe + 1
    !
    !
    kps    = 1
    my_kps = kps                                         ! first  processor along z
    do grid_coord = 1,mproc(3)-1                         ! second processor along z onwards
       kps = kps + npz_min_proc
       if(npz_left.gt.(grid_coord-1)) kps = kps + 1
       !
       if(grid_coord.eq.my_grid_coord(3)) my_kps = kps
    end do
    !
    kpe = npz_min_proc
    if(npz_left.gt.0) kpe = kpe + 1
    my_kpe = kpe                                         ! first  processor along z
    do grid_coord = 1,mproc(3)-1                         ! second processor along z onwards
       kpe = kpe + npz_min_proc
       if(npz_left.gt.grid_coord) kpe = kpe + 1
       !
       if(grid_coord.eq.my_grid_coord(3)) my_kpe = kpe
    end do
    my_kbs    = my_kps
    my_kbe    = my_kpe + 1
    my_kps_2h = my_kps - 2   ! 2 halo for mass points
    my_kpe_2h = my_kpe + 2
    my_kbs_1h = my_kbs - 1   ! 1 halo for boundaries
    my_kbe_1h = my_kbe + 1
    !
    !*** Finally, store information about the range of each procesor in
    !*** the global grid. Note that all the processors have this info
    !
    allocate (gl_ips   (0:npes_model-1))
    allocate (gl_ipe   (0:npes_model-1))
    allocate (gl_jps   (0:npes_model-1))
    allocate (gl_jpe   (0:npes_model-1))
    allocate (gl_kps   (0:npes_model-1))
    allocate (gl_kpe   (0:npes_model-1))
    allocate (gl_ips_2h(0:npes_model-1))
    allocate (gl_ipe_2h(0:npes_model-1))
    allocate (gl_jps_2h(0:npes_model-1))
    allocate (gl_jpe_2h(0:npes_model-1))
    allocate (gl_kps_2h(0:npes_model-1))
    allocate (gl_kpe_2h(0:npes_model-1))
    allocate (gl_ibs   (0:npes_model-1))
    allocate (gl_ibe   (0:npes_model-1))
    allocate (gl_jbs   (0:npes_model-1))
    allocate (gl_jbe   (0:npes_model-1))
    allocate (gl_kbs   (0:npes_model-1))
    allocate (gl_kbe   (0:npes_model-1))
    allocate (gl_ibs_1h(0:npes_model-1))
    allocate (gl_ibe_1h(0:npes_model-1))
    allocate (gl_jbs_1h(0:npes_model-1))
    allocate (gl_jbe_1h(0:npes_model-1))
    allocate (gl_kbs_1h(0:npes_model-1))
    allocate (gl_kbe_1h(0:npes_model-1))
    !
    gl_ips   (:) = 0
    gl_ipe   (:) = 0
    gl_jps   (:) = 0
    gl_jpe   (:) = 0
    gl_kps   (:) = 0
    gl_kpe   (:) = 0
    gl_ips_2h(:) = 0
    gl_ipe_2h(:) = 0
    gl_jps_2h(:) = 0
    gl_jpe_2h(:) = 0
    gl_kps_2h(:) = 0
    gl_kpe_2h(:) = 0
    gl_ibs   (:) = 0
    gl_ibe   (:) = 0
    gl_jbs   (:) = 0
    gl_jbe   (:) = 0
    gl_kbs   (:) = 0
    gl_kbe   (:) = 0
    gl_ibs_1h(:) = 0
    gl_ibe_1h(:) = 0
    gl_jbs_1h(:) = 0
    gl_jbe_1h(:) = 0
    gl_kbs_1h(:) = 0
    gl_kbe_1h(:) = 0
    !
    gl_ips   (mype_model) = my_ips
    gl_ipe   (mype_model) = my_ipe
    gl_jps   (mype_model) = my_jps
    gl_jpe   (mype_model) = my_jpe
    gl_kps   (mype_model) = my_kps
    gl_kpe   (mype_model) = my_kpe
    gl_ips_2h(mype_model) = my_ips_2h
    gl_ipe_2h(mype_model) = my_ipe_2h
    gl_jps_2h(mype_model) = my_jps_2h
    gl_jpe_2h(mype_model) = my_jpe_2h
    gl_kps_2h(mype_model) = my_kps_2h
    gl_kpe_2h(mype_model) = my_kpe_2h
    gl_ibs   (mype_model) = my_ibs
    gl_ibe   (mype_model) = my_ibe
    gl_jbs   (mype_model) = my_jbs
    gl_jbe   (mype_model) = my_jbe
    gl_kbs   (mype_model) = my_kbs
    gl_kbe   (mype_model) = my_kbe
    gl_ibs_1h(mype_model) = my_ibs_1h
    gl_ibe_1h(mype_model) = my_ibe_1h
    gl_jbs_1h(mype_model) = my_jbs_1h
    gl_jbe_1h(mype_model) = my_jbe_1h
    gl_kbs_1h(mype_model) = my_kbs_1h
    gl_kbe_1h(mype_model) = my_kbe_1h
    !
    call parallel_sum(gl_ips   ,COMM_MODEL)
    call parallel_sum(gl_ipe   ,COMM_MODEL)
    call parallel_sum(gl_jps   ,COMM_MODEL)
    call parallel_sum(gl_jpe   ,COMM_MODEL)
    call parallel_sum(gl_kps   ,COMM_MODEL)
    call parallel_sum(gl_kpe   ,COMM_MODEL)
    call parallel_sum(gl_ips_2h,COMM_MODEL)
    call parallel_sum(gl_ipe_2h,COMM_MODEL)
    call parallel_sum(gl_jps_2h,COMM_MODEL)
    call parallel_sum(gl_jpe_2h,COMM_MODEL)
    call parallel_sum(gl_kps_2h,COMM_MODEL)
    call parallel_sum(gl_kpe_2h,COMM_MODEL)
    call parallel_sum(gl_ibs   ,COMM_MODEL)
    call parallel_sum(gl_ibe   ,COMM_MODEL)
    call parallel_sum(gl_jbs   ,COMM_MODEL)
    call parallel_sum(gl_jbe   ,COMM_MODEL)
    call parallel_sum(gl_kbs   ,COMM_MODEL)
    call parallel_sum(gl_kbe   ,COMM_MODEL)
    call parallel_sum(gl_ibs_1h,COMM_MODEL)
    call parallel_sum(gl_ibe_1h,COMM_MODEL)
    call parallel_sum(gl_jbs_1h,COMM_MODEL)
    call parallel_sum(gl_jbe_1h,COMM_MODEL)
    call parallel_sum(gl_kbs_1h,COMM_MODEL)
    call parallel_sum(gl_kbe_1h,COMM_MODEL)
    !
    return
  end subroutine domain_decompose
  !
  !----------------------------------------------
  !    subroutine domain_swap_mass_points_2halo_x
  !-----------------------------------------------
  !
  !>   @brief
  !>   Performs swaping of 2 halos for mass points along x
  !
  subroutine domain_swap_mass_points_2halo_x ( my_c )
    implicit none
    !
    !>   @param my_c  my scalar variable (at mass points) to be swaped
    !
    real(rp), intent(INOUT) :: my_c( my_ips_2h:my_ipe_2h, my_jps_2h:my_jpe_2h, my_kps_2h:my_kpe_2h)
    !
#if defined WITH_MPI
    !
    integer(ip)           :: dimx,dimy,dimz,dim
    integer(ip)           :: ishand,irhand
    real(rp), allocatable :: work_send (:,:,:)
    real(rp), allocatable :: work_reciv(:,:,:)
    !
    !*** Initializations
    !
    dimx = 2
    dimy = my_jpe-my_jps+1
    dimz = my_kpe-my_kps+1
    dim  = dimx*dimy*dimz
    allocate(work_send (dimx,dimy,dimz))
    allocate(work_reciv(dimx,dimy,dimz))
    !
    !*** Send data to the next processor
    !
    if( my_E_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_E_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_W_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ips:my_ips+1,my_jps:my_jpe,my_kps:my_kpe)
       call parallel_isend( work_send(1,1,1), dim, my_W_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_E_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ipe+1:my_ipe+2,my_jps:my_jpe,my_kps:my_kpe) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_W_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the previous processor
    !
    if( my_W_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_W_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_E_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ipe-1:my_ipe,my_jps:my_jpe,my_kps:my_kpe)
       call parallel_isend( work_send(1,1,1), dim, my_E_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_W_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips-2:my_ips-1,my_jps:my_jpe,my_kps:my_kpe) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_E_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Deallocate
    !
    deallocate(work_send )
    deallocate(work_reciv)
    !
#endif
    !
    return
  end subroutine domain_swap_mass_points_2halo_x
  !
  !----------------------------------------------
  !    subroutine domain_swap_mass_points_2halo_first
  !-----------------------------------------------
  !
  !>   @brief
  !>   Performs swaping of 2 halos for mass points along fastest dimension
  !
  subroutine domain_swap_mass_points_2halo_first ( my_c, dim1ps, dim1pe, &
       dim2ps, dim2pe, dim3ps, dim3pe, S_proc, E_proc )
    implicit none
    !
    !>   @param my_c    my scalar variable (at mass points) to be swaped
    !>   @param 1ps    start index of variable along first (fastest) dimension
    !>   @param 1pe    end index of variable along first (fastest) dimension
    !>   @param 2ps    start index of variable along second dimension
    !>   @param 2pe    end index of variable along second dimension
    !>   @param 3ps    start index of variable along third (slowest) dimension
    !>   @param 3pe    end index of variable along third (slowest) dimension
    !>   @param S_proc  previous processor ID
    !>   @param E_proc  next processor ID
    !
    integer(ip), intent(IN) :: dim1ps,dim1pe
    integer(ip), intent(IN) :: dim2ps,dim2pe
    integer(ip), intent(IN) :: dim3ps,dim3pe
    integer(ip), intent(IN) :: S_proc,E_proc
    real(rp), intent(INOUT) :: my_c( dim1ps-2:dim1pe+2,dim2ps-2:dim2pe+2, dim3ps-2:dim3pe+2)
    !
#if defined WITH_MPI
    !
    integer(ip)           :: dimx,dimy,dimz,dim
    integer(ip)           :: ishand,irhand
    real(rp), allocatable :: work_send (:,:,:)
    real(rp), allocatable :: work_reciv(:,:,:)
    !
    !*** Initializations
    !
    dimx = 2
    dimy = dim2pe-dim2ps+1
    dimz = dim3pe-dim3ps+1
    dim  = dimx*dimy*dimz
    allocate(work_send (dimx,dimy,dimz))
    allocate(work_reciv(dimx,dimy,dimz))
    !
    !*** Send data to the next processor
    !
    if( E_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, E_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( S_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(dim1ps:dim1ps+1,dim2ps:dim2pe,dim3ps:dim3pe)
       call parallel_isend( work_send(1,1,1), dim, S_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( E_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(dim1pe+1:dim1pe+2,dim2ps:dim2pe,dim3ps:dim3pe) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( S_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the previous processor
    !
    if( S_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, S_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( E_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(dim1pe-1:dim1pe,dim2ps:dim2pe,dim3ps:dim3pe)
       call parallel_isend( work_send(1,1,1), dim, E_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( S_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(dim1ps-2:dim1ps-1,dim2ps:dim2pe,dim3ps:dim3pe) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( E_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Deallocate
    !
    deallocate(work_send )
    deallocate(work_reciv)
    !
#endif
    !
    return
  end subroutine domain_swap_mass_points_2halo_first
  !
  !----------------------------------------------
  !    subroutine domain_swap_mass_points_2halo_2Dx
  !-----------------------------------------------
  !
  !>   @brief
  !>   Performs swaping of 2 halos for mass points along x
  !
  subroutine domain_swap_mass_points_2halo_2Dx ( my_c )
    implicit none
    !
    !>   @param my_c  my scalar variable (at mass points) to be swaped
    !
    real(rp), intent(INOUT) :: my_c( my_ips_2h:my_ipe_2h, my_jps_2h:my_jpe_2h)
    !
#if defined WITH_MPI
    !
    integer(ip)           :: dimx,dimy,dim
    integer(ip)           :: ishand,irhand
    real(rp), allocatable :: work_send (:,:)
    real(rp), allocatable :: work_reciv(:,:)
    !
    !*** Initializations
    !
    dimx = 2
    dimy = my_jpe-my_jps+1
    dim  = dimx*dimy
    allocate(work_send (dimx,dimy))
    allocate(work_reciv(dimx,dimy))
    !
    !*** Send data to the next processor
    !
    if( my_E_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1), dim, my_E_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_W_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy) = my_c(my_ips:my_ips+1,my_jps:my_jpe)
       call parallel_isend( work_send(1,1), dim, my_W_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_E_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ipe+1:my_ipe+2,my_jps:my_jpe) = work_reciv(1:dimx,1:dimy)
    end if
    if( my_W_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the previous processor
    !
    if( my_W_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1), dim, my_W_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_E_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy) = my_c(my_ipe-1:my_ipe,my_jps:my_jpe)
       call parallel_isend( work_send(1,1), dim, my_E_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_W_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips-2:my_ips-1,my_jps:my_jpe) = work_reciv(1:dimx,1:dimy)
    end if
    if( my_E_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Deallocate
    !
    deallocate(work_send )
    deallocate(work_reciv)
    !
#endif
    !
    return
  end subroutine domain_swap_mass_points_2halo_2Dx
  !
  !----------------------------------------------
  !    subroutine domain_swap_mass_points_2halo_y
  !----------------------------------------------
  !
  !>   @brief
  !>   Performs swaping of 2 halos for mass points along y
  !
  subroutine domain_swap_mass_points_2halo_y( my_c )
    implicit none
    !
    !>   @param my_c  my scalar variable (at mass points) to be swaped
    !
    real(rp), intent(INOUT) :: my_c( my_ips_2h:my_ipe_2h, my_jps_2h:my_jpe_2h, my_kps_2h:my_kpe_2h)
    !
#if defined WITH_MPI
    !
    integer(ip)           :: dimx,dimy,dimz,dim
    integer(ip)           :: ishand,irhand
    real(rp), allocatable :: work_send (:,:,:)
    real(rp), allocatable :: work_reciv(:,:,:)
    !
    !*** Initializations
    !
    dimx = my_ipe-my_ips+1
    dimy = 2
    dimz = my_kpe-my_kps+1
    dim  = dimx*dimy*dimz
    allocate(work_send (dimx,dimy,dimz))
    allocate(work_reciv(dimx,dimy,dimz))
    !
    !*** Send data to the next processor
    !
    if( my_N_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_N_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_S_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ips:my_ipe,my_jps:my_jps+1,my_kps:my_kpe)
       call parallel_isend( work_send(1,1,1), dim, my_S_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_N_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips:my_ipe,my_jpe+1:my_jpe+2,my_kps:my_kpe) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_S_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the previous processor
    !
    if( my_S_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_S_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_N_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ips:my_ipe,my_jpe-1:my_jpe,my_kps:my_kpe)
       call parallel_isend( work_send(1,1,1), dim, my_N_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_S_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips:my_ipe,my_jps-2:my_jps-1,my_kps:my_kpe) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_N_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Deallocate
    !
    deallocate(work_send )
    deallocate(work_reciv)
    !
#endif
    !
    return
  end subroutine domain_swap_mass_points_2halo_y
  !
  !----------------------------------------------
  !    subroutine domain_swap_mass_points_2halo_2Dy
  !----------------------------------------------
  !
  !>   @brief
  !>   Performs swaping of 2 halos for mass points along y
  !
  subroutine domain_swap_mass_points_2halo_2Dy( my_c )
    implicit none
    !
    !>   @param my_c  my scalar variable (at mass points) to be swaped
    !
    real(rp), intent(INOUT) :: my_c( my_ips_2h:my_ipe_2h, my_jps_2h:my_jpe_2h)
    !
#if defined WITH_MPI
    !
    integer(ip)           :: dimx,dimy,dim
    integer(ip)           :: ishand,irhand
    real(rp), allocatable :: work_send (:,:)
    real(rp), allocatable :: work_reciv(:,:)
    !
    !*** Initializations
    !
    dimx = my_ipe-my_ips+1
    dimy = 2
    dim  = dimx*dimy
    allocate(work_send (dimx,dimy))
    allocate(work_reciv(dimx,dimy))
    !
    !*** Send data to the next processor
    !
    if( my_N_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1), dim, my_N_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_S_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy) = my_c(my_ips:my_ipe,my_jps:my_jps+1)
       call parallel_isend( work_send(1,1), dim, my_S_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_N_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips:my_ipe,my_jpe+1:my_jpe+2) = work_reciv(1:dimx,1:dimy)
    end if
    if( my_S_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the previous processor
    !
    if( my_S_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1), dim, my_S_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_N_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy) = my_c(my_ips:my_ipe,my_jpe-1:my_jpe)
       call parallel_isend( work_send(1,1), dim, my_N_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_S_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips:my_ipe,my_jps-2:my_jps-1) = work_reciv(1:dimx,1:dimy)
    end if
    if( my_N_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Deallocate
    !
    deallocate(work_send )
    deallocate(work_reciv)
    !
#endif
    !
    return
  end subroutine domain_swap_mass_points_2halo_2Dy
  !
  !----------------------------------------------
  !    subroutine domain_swap_mass_points_2halo_z
  !----------------------------------------------
  !
  !>   @brief
  !>   Performs swaping of 2 halos for mass points along z
  !
  subroutine domain_swap_mass_points_2halo_z( my_c )
    implicit none
    !
    !>   @param my_c  my scalar variable (at mass points) to be swaped
    !
    real(rp), intent(INOUT) :: my_c( my_ips_2h:my_ipe_2h, my_jps_2h:my_jpe_2h, my_kps_2h:my_kpe_2h)
    !
#if defined WITH_MPI
    !
    integer(ip)           :: dimx,dimy,dimz,dim
    integer(ip)           :: ishand,irhand
    real(rp), allocatable :: work_send (:,:,:)
    real(rp), allocatable :: work_reciv(:,:,:)
    !
    !*** Initializations
    !
    dimx = my_ipe-my_ips+1
    dimy = my_jpe-my_jps+1
    dimz = 2
    dim  = dimx*dimy*dimz
    allocate(work_send (dimx,dimy,dimz))
    allocate(work_reciv(dimx,dimy,dimz))
    !
    !*** Send data to the next processor
    !
    if( my_U_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_U_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_D_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ips:my_ipe,my_jps:my_jpe,my_kps:my_kps+1)
       call parallel_isend( work_send(1,1,1), dim, my_D_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_U_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips:my_ipe,my_jps:my_jpe,my_kpe+1:my_kpe+2) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_D_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the previous processor
    !
    if( my_D_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_D_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_U_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ips:my_ipe,my_jps:my_jpe,my_kpe-1:my_kpe)
       call parallel_isend( work_send(1,1,1), dim, my_U_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_D_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips:my_ipe,my_jps:my_jpe,my_kps-2:my_kps-1) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_U_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Deallocate
    !
    deallocate(work_send )
    deallocate(work_reciv)
    !
#endif
    !
    return
  end subroutine domain_swap_mass_points_2halo_z
  !
  !---------------------------------------
  !    subroutine domain_swap_mass_points_2halo_1Dx
  !---------------------------------------
  !
  !>   @brief
  !>   Performs swaping of 2 halos for 1D vector along x
  !
  subroutine domain_swap_mass_points_2halo_1Dx( my_c )
    implicit none
    !
    !>   @param my_c  my scalar variable (at mass points) to be swaped
    !
    real(rp), intent(INOUT) :: my_c( my_ips_2h:my_ipe_2h )
    !
#if defined WITH_MPI
    !
    integer(ip) :: dim
    integer(ip) :: ishand,irhand
    real(rp)    :: work_send (2)
    real(rp)    :: work_reciv(2)
    !
    !*** Initializations
    !
    dim = 2
    !
    !*** Send data to the next processor
    !
    if( my_E_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1), dim, my_E_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_W_proc.ne.-1 ) then
       work_send(1:2) = my_c(my_ips:my_ips+1)
       call parallel_isend( work_send(1), dim, my_W_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_E_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ipe+1:my_ipe+2) = work_reciv(1:dim)
    end if
    if( my_W_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the previous processor
    !
    if( my_W_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1), dim, my_W_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_E_proc.ne.-1 ) then
       work_send(1:dim) = my_c(my_ipe-1:my_ipe)
       call parallel_isend( work_send(1), dim, my_E_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_W_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips-2:my_ips-1) = work_reciv(1:dim)
    end if
    if( my_E_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
#endif
    !
    return
  end subroutine domain_swap_mass_points_2halo_1Dx
  !
  !---------------------------------------
  !    subroutine domain_swap_mass_points_2halo_1Dy
  !---------------------------------------
  !
  !>   @brief
  !>   Performs swaping of 2 halos for 1D vector along y
  !
  subroutine domain_swap_mass_points_2halo_1Dy( my_c )
    implicit none
    !
    !>   @param my_c  my scalar variable (at mass points) to be swaped
    !
    real(rp), intent(INOUT) :: my_c( my_jps_2h:my_jpe_2h)
    !
#if defined WITH_MPI
    !
    integer(ip)  :: dim
    integer(ip)  :: ishand,irhand
    real(rp)     :: work_send (2)
    real(rp)     :: work_reciv(2)
    !
    !*** Initializations
    !
    dim = 2
    !
    !*** Send data to the next processor
    !
    if( my_N_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1), dim, my_N_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_S_proc.ne.-1 ) then
       work_send(1:dim) = my_c(my_jps:my_jps+1)
       call parallel_isend( work_send(1), dim, my_S_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_N_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_jpe+1:my_jpe+2) = work_reciv(1:dim)
    end if
    if( my_S_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the previous processor
    !
    if( my_S_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1), dim, my_S_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_N_proc.ne.-1 ) then
       work_send(1:dim) = my_c(my_jpe-1:my_jpe)
       call parallel_isend( work_send(1), dim, my_N_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_S_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_jps-2:my_jps-1) = work_reciv(1:dim)
    end if
    if( my_N_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
#endif
    !
    return
  end subroutine domain_swap_mass_points_2halo_1Dy
  !
  !---------------------------------------
  !    subroutine domain_swap_mass_points_2halo_1Dz
  !---------------------------------------
  !
  !>   @brief
  !>   Performs swaping of 2 halos for 1D vector along z
  !
  subroutine domain_swap_mass_points_2halo_1Dz( my_c )
    implicit none
    !
    !>   @param my_c  my scalar variable (at mass points) to be swaped
    !
    real(rp), intent(INOUT) :: my_c(my_kps_2h:my_kpe_2h)
    !
#if defined WITH_MPI
    !
    integer(ip) :: dim
    integer(ip) :: ishand,irhand
    real(rp)    :: work_send (2)
    real(rp)    :: work_reciv(2)
    !
    !*** Initializations
    !
    dim = 2
    !
    !*** Send data to the next processor
    !
    if( my_U_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1), dim, my_U_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_D_proc.ne.-1 ) then
       work_send(1:dim) = my_c(my_kps:my_kps+1)
       call parallel_isend( work_send(1), dim, my_D_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_U_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_kpe+1:my_kpe+2) = work_reciv(1:dim)
    end if
    if( my_D_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the previous processor
    !
    if( my_D_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1), dim, my_D_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_U_proc.ne.-1 ) then
       work_send(1:dim) = my_c(my_kpe-1:my_kpe)
       call parallel_isend( work_send(1), dim, my_U_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_D_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_kps-2:my_kps-1) = work_reciv(1:dim)
    end if
    if( my_U_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
#endif
    !
    return
  end subroutine domain_swap_mass_points_2halo_1Dz
  !
  !----------------------------------------------
  !    subroutine domain_swap_mass_points_2halo_corners_x
  !----------------------------------------------
  !
  !>   @brief
  !>   Performs swaping of 2 halo corners along x.
  !>
  !>   @details
  !>   This is actually needed only for postprocess operations before converting
  !> mass point variables to corner variables
  !
  subroutine domain_swap_mass_points_2halo_corners_x( my_c )
    implicit none
    !
    !>   @param my_c  my scalar variable (at mass points) to be swaped
    !
    real(rp), intent(INOUT) :: my_c( my_ips_2h:my_ipe_2h, my_jps_2h:my_jpe_2h, my_kps_2h:my_kpe_2h)
    !
#if defined WITH_MPI
    !
    integer(ip)           :: dimx,dimy,dimz,dim
    integer(ip)           :: ishand,irhand
    real(rp), allocatable :: work_send (:,:,:)
    real(rp), allocatable :: work_reciv(:,:,:)
    !
    !*** Initializations
    !
    dimx = my_ipe_2h-my_ips_2h+1
    dimy = 2
    dimz = 2
    dim  = dimx*dimy*dimz
    allocate(work_send (dimx,dimy,dimz))
    allocate(work_reciv(dimx,dimy,dimz))
    !
    !*** Send data to the next processor (UN corner)
    !
    if( my_N_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_N_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_S_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ips_2h:my_ipe_2h,my_jps:my_jps+1,my_kpe+1:my_kpe+2)
       call parallel_isend( work_send(1,1,1), dim, my_S_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_N_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips_2h:my_ipe_2h,my_jpe+1:my_jpe+2,my_kpe+1:my_kpe+2) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_S_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    if( my_U_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_U_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_D_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ips_2h:my_ipe_2h,my_jpe+1:my_jpe+2,my_kps:my_kps+1)
       call parallel_isend( work_send(1,1,1), dim, my_D_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_U_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips_2h:my_ipe_2h,my_jpe+1:my_jpe+2,my_kpe+1:my_kpe+2) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_D_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the next processor (DN corner)
    !
    if( my_N_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_N_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_S_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ips_2h:my_ipe_2h,my_jps:my_jps+1,my_kps-2:my_kps-1)
       call parallel_isend( work_send(1,1,1), dim, my_S_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_N_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips_2h:my_ipe_2h,my_jpe+1:my_jpe+2,my_kps-2:my_kps-1) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_S_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    if( my_D_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_D_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_U_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ips_2h:my_ipe_2h,my_jpe+1:my_jpe+2,my_kpe-1:my_kpe)
       call parallel_isend( work_send(1,1,1), dim, my_U_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_D_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips_2h:my_ipe_2h,my_jpe+1:my_jpe+2,my_kps-2:my_kps-1) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_U_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the previous processor (US corner)
    !
    if( my_S_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_S_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_N_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ips_2h:my_ipe_2h,my_jpe-1:my_jpe,my_kpe+1:my_kpe+2)
       call parallel_isend( work_send(1,1,1), dim, my_N_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_S_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips_2h:my_ipe_2h,my_jps-2:my_jps-1,my_kpe+1:my_kpe+2) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_N_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    if( my_U_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_U_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_D_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ips_2h:my_ipe_2h,my_jps-2:my_jps-1,my_kps:my_kps+1)
       call parallel_isend( work_send(1,1,1), dim, my_D_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_U_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips_2h:my_ipe_2h,my_jps-2:my_jps-1,my_kpe+1:my_kpe+2) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_D_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the previous processor (DS corner)
    !
    if( my_S_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_S_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_N_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ips_2h:my_ipe_2h,my_jpe-1:my_jpe,my_kps-2:my_kps-1)
       call parallel_isend( work_send(1,1,1), dim, my_N_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_S_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips_2h:my_ipe_2h,my_jps-2:my_jps-1,my_kps-2:my_kps-1) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_N_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    if( my_D_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_D_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_U_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ips_2h:my_ipe_2h,my_jps-2:my_jps-1,my_kpe-1:my_kpe)
       call parallel_isend( work_send(1,1,1), dim, my_U_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_D_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips_2h:my_ipe_2h,my_jps-2:my_jps-1,my_kps-2:my_kps-1) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_U_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Deallocate
    !
    deallocate(work_send )
    deallocate(work_reciv)
    !
#endif
    !
    return
  end subroutine domain_swap_mass_points_2halo_corners_x
  !
  !----------------------------------------------
  !    subroutine domain_swap_mass_points_2halo_corners_y
  !----------------------------------------------
  !
  !>  @brief
  !>  Performs swaping of 2 halo corners along y.
  !>
  !>  @details
  !>  This is actually needed only for postprocess operations before converting
  !>  mass point variables to corner variables
  !
  subroutine domain_swap_mass_points_2halo_corners_y( my_c )
    implicit none
    !
    !>  @param my_c  my scalar variable (at mass points) to be swaped
    !
    real(rp), intent(INOUT) :: my_c( my_ips_2h:my_ipe_2h, my_jps_2h:my_jpe_2h, my_kps_2h:my_kpe_2h)
    !
#if defined WITH_MPI
    !
    integer(ip)           :: dimx,dimy,dimz,dim
    integer(ip)           :: ishand,irhand
    real(rp), allocatable :: work_send (:,:,:)
    real(rp), allocatable :: work_reciv(:,:,:)
    !
    !*** Initializations
    !
    dimx = 2
    dimy = my_jpe_2h-my_jps_2h+1
    dimz = 2
    dim  = dimx*dimy*dimz
    allocate(work_send (dimx,dimy,dimz))
    allocate(work_reciv(dimx,dimy,dimz))
    !
    !*** Send data to the next processor (UE corner)
    !
    if( my_U_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_U_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_D_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ipe+1:my_ipe+2,my_jps_2h:my_jpe_2h,my_kps:my_kps+1)
       call parallel_isend( work_send(1,1,1), dim, my_D_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_U_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ipe+1:my_ipe+2,my_jps_2h:my_jpe_2h,my_kpe+1:my_kpe+2) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_D_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    if( my_E_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_E_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_W_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ips:my_ips+1,my_jps_2h:my_jpe_2h,my_kpe+1:my_kpe+2)
       call parallel_isend( work_send(1,1,1), dim, my_W_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_E_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ipe+1:my_ipe+2,my_jps_2h:my_jpe_2h,my_kpe+1:my_kpe+2) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_W_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the next processor (UW corner)
    !
    if( my_U_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_U_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_D_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ips-2:my_ips-1,my_jps_2h:my_jpe_2h,my_kps:my_kps+1)
       call parallel_isend( work_send(1,1,1), dim, my_D_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_U_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips-2:my_ips-1,my_jps_2h:my_jpe_2h,my_kpe+1:my_kpe+2) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_D_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    if( my_W_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_W_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_E_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ipe-1:my_ipe,my_jps_2h:my_jpe_2h,my_kpe+1:my_kpe+2)
       call parallel_isend( work_send(1,1,1), dim, my_E_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_W_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips-2:my_ips-1,my_jps_2h:my_jpe_2h,my_kpe+1:my_kpe+2) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_E_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the previous processor (DE corner)
    !
    if( my_D_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_D_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_U_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ipe+1:my_ipe+2,my_jps_2h:my_jpe_2h,my_kpe-1:my_kpe)
       call parallel_isend( work_send(1,1,1), dim, my_U_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_D_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ipe+1:my_ipe+2,my_jps_2h:my_jpe_2h,my_kps-2:my_kps-1) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_U_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    if( my_E_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_E_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_W_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ips:my_ips+1,my_jps_2h:my_jpe_2h,my_kps-2:my_kps-1)
       call parallel_isend( work_send(1,1,1), dim, my_W_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_E_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ipe+1:my_ipe+2,my_jps_2h:my_jpe_2h,my_kps-2:my_kps-1) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_W_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the previous processor (DW corner)
    !
    if( my_D_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_D_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_U_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ips-2:my_ips-1,my_jps_2h:my_jpe_2h,my_kpe-1:my_kpe)
       call parallel_isend( work_send(1,1,1), dim, my_U_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_D_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips-2:my_ips-1,my_jps_2h:my_jpe_2h,my_kps-2:my_kps-1) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_U_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    if( my_W_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_W_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_E_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ipe-1:my_ipe,my_jps_2h:my_jpe_2h,my_kps-2:my_kps-1)
       call parallel_isend( work_send(1,1,1), dim, my_E_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_W_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips-2:my_ips-1,my_jps_2h:my_jpe_2h,my_kps-2:my_kps-1) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_E_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Deallocate
    !
    deallocate(work_send )
    deallocate(work_reciv)
    !
#endif
    !
    return
  end subroutine domain_swap_mass_points_2halo_corners_y
  !
  !----------------------------------------------
  !    subroutine domain_swap_mass_points_2halo_corners_z
  !-----------------------------------------------
  !
  !>   @brief
  !>   Performs swaping of 2 halo corners along z.

  !>   @details
  !>   This is actually needed only for postprocess operations before converting
  !>   mass point variables to corner variables
  !
  subroutine domain_swap_mass_points_2halo_corners_z ( my_c )
    implicit none
    !
    !>   @param my_c  my scalar variable (at mass points) to be swaped
    !
    real(rp), intent(INOUT) :: my_c( my_ips_2h:my_ipe_2h, my_jps_2h:my_jpe_2h, my_kps_2h:my_kpe_2h)
    !
#if defined WITH_MPI
    !
    integer(ip)           :: dimx,dimy,dimz,dim
    integer(ip)           :: ishand,irhand
    real(rp), allocatable :: work_send (:,:,:)
    real(rp), allocatable :: work_reciv(:,:,:)
    !
    !*** Initializations
    !
    dimx = 2
    dimy = 2
    dimz = my_kpe_2h-my_kps_2h+1
    dim  = dimx*dimy*dimz
    allocate(work_send (dimx,dimy,dimz))
    allocate(work_reciv(dimx,dimy,dimz))
    !
    !*** Send data to the next processor (NE corner)
    !
    if( my_E_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_E_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_W_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ips:my_ips+1,my_jpe+1:my_jpe+2,my_kps_2h:my_kpe_2h)
       call parallel_isend( work_send(1,1,1), dim, my_W_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_E_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ipe+1:my_ipe+2,my_jpe+1:my_jpe+2,my_kps_2h:my_kpe_2h) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_W_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    if( my_N_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_N_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_S_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ipe+1:my_ipe+2,my_jps:my_jps+1,my_kps_2h:my_kpe_2h)
       call parallel_isend( work_send(1,1,1), dim, my_S_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_N_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ipe+1:my_ipe+2,my_jpe+1:my_jpe+2,my_kps_2h:my_kpe_2h) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_S_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the next processor (SE corner)
    !
    if( my_E_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_E_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_W_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ips:my_ips+1,my_jps-2:my_jps-1,my_kps_2h:my_kpe_2h)
       call parallel_isend( work_send(1,1,1), dim, my_W_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_E_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ipe+1:my_ipe+2,my_jps-2:my_jps-1,my_kps_2h:my_kpe_2h) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_W_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    if( my_S_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_S_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_N_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ipe+1:my_ipe+2,my_jpe-1:my_jpe,my_kps_2h:my_kpe_2h)
       call parallel_isend( work_send(1,1,1), dim, my_N_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_S_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ipe+1:my_ipe+2,my_jps-2:my_jps-1,my_kps_2h:my_kpe_2h) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_N_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the previous processor (NW corner)
    !
    if( my_W_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_W_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_E_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ipe-1:my_ipe,my_jpe+1:my_jpe+2,my_kps_2h:my_kpe_2h)
       call parallel_isend( work_send(1,1,1), dim, my_E_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_W_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips-2:my_ips-1,my_jpe+1:my_jpe+2,my_kps_2h:my_kpe_2h) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_E_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    if( my_N_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_N_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_S_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ips-2:my_ips-1,my_jps:my_jps+1,my_kps_2h:my_kpe_2h)
       call parallel_isend( work_send(1,1,1), dim, my_S_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_N_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips-2:my_ips-1,my_jpe+1:my_jpe+2,my_kps_2h:my_kpe_2h) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_S_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the previous processor (SW corner)
    !
    if( my_W_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_W_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_E_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ipe-1:my_ipe,my_jps-2:my_jps-1,my_kps_2h:my_kpe_2h)
       call parallel_isend( work_send(1,1,1), dim, my_E_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_W_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips-2:my_ips-1,my_jps-2:my_jps-1,my_kps_2h:my_kpe_2h) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_E_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    if( my_S_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_S_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_N_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,1:dimz) = my_c(my_ips-2:my_ips-1,my_jpe-1:my_jpe,my_kps_2h:my_kpe_2h)
       call parallel_isend( work_send(1,1,1), dim, my_N_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_S_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_c(my_ips-2:my_ips-1,my_jps-2:my_jps-1,my_kps_2h:my_kpe_2h) = work_reciv(1:dimx,1:dimy,1:dimz)
    end if
    if( my_N_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Deallocate
    !
    deallocate(work_send )
    deallocate(work_reciv)
    !
#endif
    !
    return
  end subroutine domain_swap_mass_points_2halo_corners_z
  !
  !----------------------------------------------
  !    subroutine domain_swap_velo_points_1halo_x
  !----------------------------------------------
  !
  !>   @brief
  !>   Performs swaping of 1 halo for velocity points along x
  !
  subroutine domain_swap_velo_points_1halo_x( my_u )
    implicit none
    !
    !>   @param my_u  u-wind velocity at my processor cell boundaries (with 1 halo)
    !
    real(rp), intent(INOUT) :: my_u(my_ibs_1h:my_ibe_1h,my_jps:my_jpe,my_kps:my_kpe)
    !
#if defined WITH_MPI
    !
    integer(ip)           :: dimx,dimy,dimz,dim
    integer(ip)           :: ishand,irhand
    real(rp), allocatable :: work_send (:,:,:)
    real(rp), allocatable :: work_reciv(:,:,:)
    !
    !*** Initializations
    !
    dimx = 1
    dimy = my_jpe-my_jps+1
    dimz = my_kpe-my_kps+1
    dim  = dimx*dimy*dimz
    allocate(work_send (dimx,dimy,dimz))
    allocate(work_reciv(dimx,dimy,dimz))
    !
    !*** Send data to the next processor
    !
    if( my_E_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_E_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_W_proc.ne.-1 ) then
       work_send(dimx,1:dimy,1:dimz) = my_u(my_ibs+1,my_jps:my_jpe,my_kps:my_kpe)
       call parallel_isend( work_send(1,1,1), dim, my_W_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_E_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_u(my_ibe+1,my_jps:my_jpe,my_kps:my_kpe) = work_reciv(dimx,1:dimy,1:dimz)
    end if
    if( my_W_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the previous processor
    !
    if( my_W_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_W_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_E_proc.ne.-1 ) then
       work_send(dimx,1:dimy,1:dimz) = my_u(my_ibe-1,my_jps:my_jpe,my_kps:my_kpe)
       call parallel_isend( work_send(1,1,1), dim, my_E_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_W_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_u(my_ibs-1,my_jps:my_jpe,my_kps:my_kpe) = work_reciv(dimx,1:dimy,1:dimz)
    end if
    if( my_E_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Deallocate
    !
    deallocate(work_send )
    deallocate(work_reciv)
    !
#endif
    !
    return
  end subroutine domain_swap_velo_points_1halo_x
  !
  !----------------------------------------------
  !    subroutine domain_swap_velo_points_1halo_y
  !----------------------------------------------
  !
  !>   @brief
  !>   Performs swaping of 1 halo for velocity points along y
  !
  subroutine domain_swap_velo_points_1halo_y( my_v )
    implicit none
    !
    !>   @param my_v  v-wind velocity at my processor cell boundaries (with 1 halo)
    !
    real(rp), intent(INOUT) :: my_v(my_ips:my_ipe,my_jbs_1h:my_jbe_1h,my_kps:my_kpe)
    !
#if defined WITH_MPI
    !
    integer(ip)           :: dimx,dimy,dimz,dim
    integer(ip)           :: ishand,irhand
    real(rp), allocatable :: work_send (:,:,:)
    real(rp), allocatable :: work_reciv(:,:,:)
    !
    !*** Initializations
    !
    dimx = my_ipe-my_ips+1
    dimy = 1
    dimz = my_kpe-my_kps+1
    dim  = dimx*dimy*dimz
    allocate(work_send (dimx,dimy,dimz))
    allocate(work_reciv(dimx,dimy,dimz))
    !
    !*** Send data to the next processor
    !
    if( my_N_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_N_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_S_proc.ne.-1 ) then
       work_send(1:dimx,dimy,1:dimz) = my_v(my_ips:my_ipe,my_jbs+1,my_kps:my_kpe)
       call parallel_isend( work_send(1,1,1), dim, my_S_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_N_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_v(my_ips:my_ipe,my_jbe+1,my_kps:my_kpe) = work_reciv(1:dimx,dimy,1:dimz)
    end if
    if( my_S_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the previous processor
    !
    if( my_S_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_S_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_N_proc.ne.-1 ) then
       work_send(1:dimx,dimy,1:dimz) = my_v(my_ips:my_ipe,my_jbe-1,my_kps:my_kpe)
       call parallel_isend( work_send(1,1,1), dim, my_N_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_S_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_v(my_ips:my_ipe,my_jbs-1,my_kps:my_kpe) = work_reciv(1:dimx,dimy,1:dimz)
    end if
    if( my_N_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Deallocate
    !
    deallocate(work_send )
    deallocate(work_reciv)
    !
#endif
    !
    return
  end subroutine domain_swap_velo_points_1halo_y
  !
  !----------------------------------------------
  !    subroutine domain_swap_velo_points_1halo_y
  !----------------------------------------------
  !
  !>   @brief
  !>   Performs swaping of 1 halo for velocity points along y, taking into
  !    account that component has been reshaped.
  !
  subroutine domain_swap_velo_points_1halo_reshaped_y( my_v )
    implicit none
    !
    !>   @param my_v  v-wind velocity at my processor cell boundaries (with 1 halo)
    !
    real(rp), intent(INOUT) :: my_v(my_jbs_1h:my_jbe_1h,my_ips:my_ipe,my_kps:my_kpe)
    !
#if defined WITH_MPI
    !
    integer(ip)           :: dimx,dimy,dimz,dim
    integer(ip)           :: ishand,irhand
    real(rp), allocatable :: work_send (:,:,:)
    real(rp), allocatable :: work_reciv(:,:,:)
    !
    !*** Initializations
    !
    dimx = my_ipe-my_ips+1
    dimy = 1
    dimz = my_kpe-my_kps+1
    dim  = dimx*dimy*dimz
    allocate(work_send (dimy,dimx,dimz))
    allocate(work_reciv(dimy,dimx,dimz))
    !
    !*** Send data to the next processor
    !
    if( my_N_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_N_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_S_proc.ne.-1 ) then
       work_send(dimy,1:dimx,1:dimz) = my_v(my_jbs+1,my_ips:my_ipe,my_kps:my_kpe)
       call parallel_isend( work_send(1,1,1), dim, my_S_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_N_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_v(my_jbe+1,my_ips:my_ipe,my_kps:my_kpe) = work_reciv(dimy,1:dimx,1:dimz)
    end if
    if( my_S_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the previous processor
    !
    if( my_S_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_S_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_N_proc.ne.-1 ) then
       work_send(dimy,1:dimx,1:dimz) = my_v(my_jbe-1,my_ips:my_ipe,my_kps:my_kpe)
       call parallel_isend( work_send(1,1,1), dim, my_N_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_S_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_v(my_jbs-1,my_ips:my_ipe,my_kps:my_kpe) = work_reciv(dimy,1:dimx,1:dimz)
    end if
    if( my_N_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Deallocate
    !
    deallocate(work_send )
    deallocate(work_reciv)
    !
#endif
    !
    return
  end subroutine domain_swap_velo_points_1halo_reshaped_y
  !
  !----------------------------------------------
  !    subroutine domain_swap_velo_points_1halo_z
  !----------------------------------------------
  !
  !>   @brief
  !>   Performs swaping of 1 halo for velocity points along z
  !
  subroutine domain_swap_velo_points_1halo_z( my_w )
    implicit none
    !
    !>   @param my_w  w-wind velocity at my processor cell boundaries (with 1 halo)
    !
    real(rp), intent(INOUT) :: my_w(my_ips:my_ipe,my_jps:my_jpe,my_kbs_1h:my_kbe_1h)
    !
#if defined WITH_MPI
    !
    integer(ip)           :: dimx,dimy,dimz,dim
    integer(ip)           :: ishand,irhand
    real(rp), allocatable :: work_send (:,:,:)
    real(rp), allocatable :: work_reciv(:,:,:)
    !
    !*** Initializations
    !
    dimx = my_ipe-my_ips+1
    dimy = my_jpe-my_jps+1
    dimz = 1
    dim  = dimx*dimy*dimz
    allocate(work_send (dimx,dimy,dimz))
    allocate(work_reciv(dimx,dimy,dimz))
    !
    !*** Send data to the next processor
    !
    if( my_U_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_U_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_D_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,dimz) = my_w(my_ips:my_ipe,my_jps:my_jpe,my_kbs+1)
       call parallel_isend( work_send(1,1,1), dim, my_D_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_U_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_w(my_ips:my_ipe,my_jps:my_jpe,my_kbe+1) = work_reciv(1:dimx,1:dimy,dimz)
    end if
    if( my_D_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Send data to the previous processor
    !
    if( my_D_proc.ne.-1 ) then
       call parallel_irecv( work_reciv(1,1,1), dim, my_D_proc, 0_ip, irhand, COMM_MODEL )
    end if
    if( my_U_proc.ne.-1 ) then
       work_send(1:dimx,1:dimy,dimz) = my_w(my_ips:my_ipe,my_jps:my_jpe,my_kbe-1)
       call parallel_isend( work_send(1,1,1), dim, my_U_proc, 0_ip, ishand, COMM_MODEL )
    end if
    if( my_D_proc.ne.-1 ) then
       call parallel_wait( irhand )
       my_w(my_ips:my_ipe,my_jps:my_jpe,my_kbs-1) = work_reciv(1:dimx,1:dimy,dimz)
    end if
    if( my_U_proc.ne.-1 ) then
       call parallel_wait( ishand )
    end if
    !
    !*** Deallocate
    !
    deallocate(work_send )
    deallocate(work_reciv)
    !
#endif
    !
    return
  end subroutine domain_swap_velo_points_1halo_z
  !
  !----------------------------------------------
  !    subroutine domain_gather_mass_points_2halo
  !----------------------------------------------
  !
  !>   @brief
  !>   Gathers a local my_c array to Master c_global(-1:nx+2,-1:ny+2,-1:nz+2) array
  !
  subroutine domain_gather_mass_points_2halo(c_global,nx,ny,nz,my_c)
    implicit none
    !
    !>   @param c_global   Global array (in Master processor only)
    !>   @param nx         For Master, number of global mass points along x. Not used by other processors (use c_global(1,1,1))
    !>   @param ny         For Master, number of global mass points along y. Not used by other processors (use c_global(1,1,1))
    !>   @param nz         For Master, number of global mass points along z. Not used by other processors (use c_global(1,1,1))
    !>   @param my_c       my scalar variable (at mass points) to be gathered
    !
    integer(ip), intent(IN)    :: nx
    integer(ip), intent(IN)    :: ny
    integer(ip), intent(IN)    :: nz
    real(rp),    intent(INOUT) :: c_global(-1:nx+2,-1:ny+2,-1:nz+2)
    real(rp),    intent(IN)    :: my_c( my_ips_2h:my_ipe_2h, my_jps_2h:my_jpe_2h, my_kps_2h:my_kpe_2h)
    integer(ip)                :: ips,ipe,jps,jpe,kps,kpe
    !
#if defined WITH_MPI
    !
    integer(ip)              :: iproc
    integer(ip)              :: dimx,dimy,dimz,dim
    integer(ip)              :: ishand,irhand
    real(rp),    allocatable :: work_send (:,:,:)
    real(rp),    allocatable :: work_reciv(:,:,:)
    !
    !*** Master recieves data from all processors
    !
    if(master_model) then
       !
       !*** Initialize
       !
       c_global(:,:,:) = 0.0_rp
       !
       do iproc = 1,npes_model-1  ! exclude Master (0) from gather
          !
          if(gl_ips(iproc).eq.1) then
             ips = gl_ips_2h(iproc)
          else
             ips = gl_ips(iproc)
          end if
          if(gl_ipe(iproc).eq.nx) then
             ipe = gl_ipe_2h(iproc)
          else
             ipe = gl_ipe(iproc)
          end if
          !
          if(gl_jps(iproc).eq.1) then
             jps = gl_jps_2h(iproc)
          else
             jps = gl_jps(iproc)
          end if
          if(gl_jpe(iproc).eq.ny) then
             jpe = gl_jpe_2h(iproc)
          else
             jpe = gl_jpe(iproc)
          end if
          !
          if(gl_kps(iproc).eq.1) then
             kps = gl_kps_2h(iproc)
          else
             kps = gl_kps(iproc)
          end if
          if(gl_kpe(iproc).eq.nz) then
             kpe = gl_kpe_2h(iproc)
          else
             kpe = gl_kpe(iproc)
          end if
          !
          dimx = ipe - ips + 1
          dimy = jpe - jps + 1
          dimz = kpe - kps + 1
          dim  = dimx*dimy*dimz
          allocate(work_reciv (dimx,dimy,dimz))
          !
          call parallel_irecv( work_reciv(1,1,1), dim, iproc, 1_ip, irhand, COMM_MODEL )
          call parallel_wait( irhand )
          c_global(ips:ipe,jps:jpe,kps:kpe) = work_reciv(1:dimx,1:dimy,1:dimz)
          deallocate(work_reciv)
       end do
    else
       !
       if(my_ips.eq.1) then
          ips = my_ips_2h
       else
          ips = my_ips
       end if
       if(my_ipe.eq.nx) then
          ipe = my_ipe_2h
       else
          ipe = my_ipe
       end if
       !
       if(my_jps.eq.1) then
          jps = my_jps_2h
       else
          jps = my_jps
       end if
       if(my_jpe.eq.ny) then
          jpe = my_jpe_2h
       else
          jpe = my_jpe
       end if
       !
       if(my_kps.eq.1) then
          kps = my_kps_2h
       else
          kps = my_kps
       end if
       if(my_kpe.eq.nz) then
          kpe = my_kpe_2h
       else
          kpe = my_kpe
       end if
       !
       dimx = ipe - ips + 1
       dimy = jpe - jps + 1
       dimz = kpe - kps + 1
       dim  = dimx*dimy*dimz
       allocate(work_send(dimx,dimy,dimz))
       !
       work_send(1:dimx,1:dimy,1:dimz) = my_c(ips:ipe,jps:jpe,kps:kpe)
       call parallel_isend( work_send(1,1,1), dim, 0_ip, 1_ip, ishand, COMM_MODEL )
       call parallel_wait(ishand)
       deallocate(work_send)
    end if
    !
#endif
    !
    !*** Master assigns to himself
    !
    if(master_model) then
       !
       if(my_ips.eq.1) then
          ips = my_ips_2h
       else
          ips = my_ips
       end if
       if(my_ipe.eq.nx) then
          ipe = my_ipe_2h
       else
          ipe = my_ipe
       end if
       !
       if(my_jps.eq.1) then
          jps = my_jps_2h
       else
          jps = my_jps
       end if
       if(my_jpe.eq.ny) then
          jpe = my_jpe_2h
       else
          jpe = my_jpe
       end if
       !
       if(my_kps.eq.1) then
          kps = my_kps_2h
       else
          kps = my_kps
       end if
       if(my_kpe.eq.nz) then
          kpe = my_kpe_2h
       else
          kpe = my_kpe
       end if
       !
       c_global(ips:ipe,jps:jpe,kps:kpe) = my_c(ips:ipe,jps:jpe,kps:kpe)
    end if
    !
    return
  end subroutine domain_gather_mass_points_2halo
  !
  !------------------------------------------------
  !    subroutine domain_gather_mass_points_2halo_2D
  !-------------------------------------------------
  !
  !>   @brief
  !>   Gathers a local my_c array to Master c_global(-1:nx+2,-1:ny+2) array
  !
  subroutine domain_gather_mass_points_2halo_2D(c_global,nx,ny,my_c)
    implicit none
    !
    !>   @param c_global   Global array (in Master processor only)
    !>   @param nx         For Master, number of global mass points along x. Not used by other processors (use c_global(1,1))
    !>   @param ny         For Master, number of global mass points along y. Not used by other processors (use c_global(1,1))
    !>   @param my_c       my scalar variable (at mass points) to be gathered
    !
    integer(ip), intent(IN)    :: nx
    integer(ip), intent(IN)    :: ny
    real(rp),    intent(INOUT) :: c_global(-1:nx+2,-1:ny+2)
    real(rp),    intent(IN)    :: my_c( my_ips_2h:my_ipe_2h, my_jps_2h:my_jpe_2h)
    integer(ip)                :: ips,ipe,jps,jpe
    !
#if defined WITH_MPI
    !
    integer(ip)              :: iproc
    integer(ip)              :: dimx,dimy,dim
    integer(ip)              :: ishand,irhand
    real(rp),    allocatable :: work_send (:,:)
    real(rp),    allocatable :: work_reciv(:,:)
    !
    !*** Master recieves data from all processors
    !
    if(master_model) then
       !
       !*** Initialize
       !
       c_global(:,:) = 0.0_rp
       !
       do iproc = 1,npes_model-1  ! exclude Master (0) from gather
          !
          if(gl_ips(iproc).eq.1) then
             ips = gl_ips_2h(iproc)
          else
             ips = gl_ips(iproc)
          end if
          if(gl_ipe(iproc).eq.nx) then
             ipe = gl_ipe_2h(iproc)
          else
             ipe = gl_ipe(iproc)
          end if
          !
          if(gl_jps(iproc).eq.1) then
             jps = gl_jps_2h(iproc)
          else
             jps = gl_jps(iproc)
          end if
          if(gl_jpe(iproc).eq.ny) then
             jpe = gl_jpe_2h(iproc)
          else
             jpe = gl_jpe(iproc)
          end if
          !
          dimx = ipe - ips + 1
          dimy = jpe - jps + 1
          dim  = dimx*dimy
          allocate(work_reciv (dimx,dimy))
          !
          call parallel_irecv( work_reciv(1,1), dim, iproc, 1_ip, irhand, COMM_MODEL )
          call parallel_wait( irhand )
          c_global(ips:ipe,jps:jpe) = work_reciv(1:dimx,1:dimy)
          deallocate(work_reciv)
       end do
    else
       !
       if(my_ips.eq.1) then
          ips = my_ips_2h
       else
          ips = my_ips
       end if
       if(my_ipe.eq.nx) then
          ipe = my_ipe_2h
       else
          ipe = my_ipe
       end if
       !
       if(my_jps.eq.1) then
          jps = my_jps_2h
       else
          jps = my_jps
       end if
       if(my_jpe.eq.ny) then
          jpe = my_jpe_2h
       else
          jpe = my_jpe
       end if
       !
       dimx = ipe - ips + 1
       dimy = jpe - jps + 1
       dim  = dimx*dimy
       allocate(work_send(dimx,dimy))
       !
       work_send(1:dimx,1:dimy) = my_c(ips:ipe,jps:jpe)
       call parallel_isend( work_send(1,1), dim, 0_ip, 1_ip, ishand, COMM_MODEL )
       call parallel_wait(ishand)
       deallocate(work_send)
    end if
    !
#endif
    !
    !*** Master assigns to himself
    !
    if(master_model) then
       !
       if(my_ips.eq.1) then
          ips = my_ips_2h
       else
          ips = my_ips
       end if
       if(my_ipe.eq.nx) then
          ipe = my_ipe_2h
       else
          ipe = my_ipe
       end if
       !
       if(my_jps.eq.1) then
          jps = my_jps_2h
       else
          jps = my_jps
       end if
       if(my_jpe.eq.ny) then
          jpe = my_jpe_2h
       else
          jpe = my_jpe
       end if
       !
       c_global(ips:ipe,jps:jpe) = my_c(ips:ipe,jps:jpe)
    end if
    !
    return
  end subroutine domain_gather_mass_points_2halo_2D
  !
  !-----------------------------------------------
  !    subroutine domain_scatter_mass_points_2halo
  !-----------------------------------------------
  !
  !>   @brief
  !>   Distributes a Master global array c_global(-1:nx+2,-1:ny+2,-1:nz+2) to local array my_c
  !
  subroutine domain_scatter_mass_points_2halo(c_global,nx,ny,nz,my_c)
    implicit none
    !
    !>   @param c_global   Global array (in Master processor only)
    !>   @param nx         For Master, number of global mass points along x. Not used by other processors (use c_global(1,1,1))
    !>   @param ny         For Master, number of global mass points along y. Not used by other processors (use c_global(1,1,1))
    !>   @param nz         For Master, number of global mass points along z. Not used by other processors (use c_global(1,1,1))
    !>   @param my_c       my scattererd scalar variable (at mass points)
    !
    integer(ip), intent(IN)    :: nx
    integer(ip), intent(IN)    :: ny
    integer(ip), intent(IN)    :: nz
    real(rp),    intent(IN)    :: c_global(-1:nx+2,-1:ny+2,-1:nz+2)
    real(rp),    intent(INOUT) :: my_c( my_ips_2h:my_ipe_2h, my_jps_2h:my_jpe_2h, my_kps_2h:my_kpe_2h)
    !
#if defined WITH_MPI
    !
    integer(ip)              :: iproc
    integer(ip)              :: dimx,dimy,dimz,dim
    integer(ip)              :: ishand,irhand
    real(rp),    allocatable :: work_send (:,:,:)
    real(rp),    allocatable :: work_reciv(:,:,:)
    !
    !*** Master send data to all processors
    !
    if(master_model) then
       do iproc = 1,npes_model-1  ! exclude Master (0) from scatter
          !
          dimx = gl_ipe_2h(iproc)-gl_ips_2h(iproc) + 1
          dimy = gl_jpe_2h(iproc)-gl_jps_2h(iproc) + 1
          dimz = gl_kpe_2h(iproc)-gl_kps_2h(iproc) + 1
          dim  = dimx*dimy*dimz
          allocate(work_send (dimx,dimy,dimz))
          !
          work_send(1:dimx,1:dimy,1:dimz) = c_global(gl_ips_2h(iproc):gl_ipe_2h(iproc), &
               gl_jps_2h(iproc):gl_jpe_2h(iproc), &
               gl_kps_2h(iproc):gl_kpe_2h(iproc))
          call parallel_isend( work_send(1,1,1), dim, iproc, 1_ip, ishand, COMM_MODEL )
          call parallel_wait(ishand)
          deallocate(work_send)
       end do
    else
       dimx = my_ipe_2h-my_ips_2h+1
       dimy = my_jpe_2h-my_jps_2h+1
       dimz = my_kpe_2h-my_kps_2h+1
       dim  = dimx*dimy*dimz
       allocate(work_reciv(dimx,dimy,dimz))
       !
       call parallel_irecv( work_reciv(1,1,1), dim, 0_ip, 1_ip, irhand, COMM_MODEL )
       call parallel_wait( irhand )
       my_c(my_ips_2h:my_ipe_2h, my_jps_2h:my_jpe_2h, my_kps_2h:my_kpe_2h) = work_reciv(1:dimx,1:dimy,1:dimz)
       deallocate(work_reciv)
    end if
    !
#endif
    !
    !*** Master assigns to himself
    !
    if(master_model) then
       my_c    (my_ips_2h:my_ipe_2h, my_jps_2h:my_jpe_2h, my_kps_2h:my_kpe_2h) = &
            c_global(my_ips_2h:my_ipe_2h, my_jps_2h:my_jpe_2h, my_kps_2h:my_kpe_2h)
    end if
    !
    return
  end subroutine domain_scatter_mass_points_2halo
  !
  !--------------------------------------------------
  !    subroutine domain_scatter_mass_points_2halo_2D
  !--------------------------------------------------
  !
  !>   @brief
  !>   Distributes a Master global array c_global(-1:nx+2,-1:ny+2) to local array my_c
  !
  subroutine domain_scatter_mass_points_2halo_2D(c_global,nx,ny,my_c)
    implicit none
    !
    !>   @param c_global   Global array (in Master processor only)
    !>   @param nx         For Master, number of global mass points along x. Not used by other processors (use c_global(1,1,1))
    !>   @param ny         For Master, number of global mass points along y. Not used by other processors (use c_global(1,1,1))
    !>   @param my_c       my scattererd scalar variable (at mass points)
    !
    integer(ip), intent(IN)    :: nx
    integer(ip), intent(IN)    :: ny
    real(rp),    intent(IN)    :: c_global(-1:nx+2,-1:ny+2)
    real(rp),    intent(INOUT) :: my_c( my_ips_2h:my_ipe_2h, my_jps_2h:my_jpe_2h)
    !
#if defined WITH_MPI
    !
    integer(ip)              :: iproc
    integer(ip)              :: dimx,dimy,dim
    integer(ip)              :: ishand,irhand
    real(rp),    allocatable :: work_send (:,:)
    real(rp),    allocatable :: work_reciv(:,:)
    !
    !*** Master send data to all processors
    !
    if(master_model) then
       do iproc = 1,npes_model-1  ! exclude Master (0) from scatter
          !
          dimx = gl_ipe_2h(iproc)-gl_ips_2h(iproc) + 1
          dimy = gl_jpe_2h(iproc)-gl_jps_2h(iproc) + 1
          dim  = dimx*dimy
          allocate(work_send (dimx,dimy))
          !
          work_send(1:dimx,1:dimy) = c_global(gl_ips_2h(iproc):gl_ipe_2h(iproc), &
               gl_jps_2h(iproc):gl_jpe_2h(iproc))
          call parallel_isend( work_send(1,1), dim, iproc, 1_ip, ishand, COMM_MODEL )
          call parallel_wait(ishand)
          deallocate(work_send)
       end do
    else
       dimx = my_ipe_2h-my_ips_2h+1
       dimy = my_jpe_2h-my_jps_2h+1
       dim  = dimx*dimy
       allocate(work_reciv(dimx,dimy))
       !
       call parallel_irecv( work_reciv(1,1), dim, 0_ip, 1_ip, irhand, COMM_MODEL )
       call parallel_wait( irhand )
       my_c(my_ips_2h:my_ipe_2h, my_jps_2h:my_jpe_2h) = work_reciv(1:dimx,1:dimy)
       deallocate(work_reciv)
    end if
    !
#endif
    !
    !*** Master assigns to himself
    !
    if(master_model) then
       my_c    (my_ips_2h:my_ipe_2h, my_jps_2h:my_jpe_2h) = &
            c_global(my_ips_2h:my_ipe_2h, my_jps_2h:my_jpe_2h)
    end if
    !
    return
  end subroutine domain_scatter_mass_points_2halo_2D
  !
  !------------------------------------------------
  !    subroutine domain_gather_corner_points_0halo
  !------------------------------------------------
  !
  !>   @brief
  !>   Gathers a local my_c array to Master c_global(1:nx,1:ny,1:nz) array
  !
  subroutine domain_gather_corner_points_0halo(c_global,nx,ny,nz,my_c)
    implicit none
    !
    !>   @param c_global   Global array (in Master processor only)
    !>   @param nx         For Master, number of global corner points (boundaries) along x. Not used by other processors (use c_global(1,1,1))
    !>   @param ny         For Master, number of global corner points (boundaries) along y. Not used by other processors (use c_global(1,1,1))
    !>   @param nz         For Master, number of global corner points (boundaries) along z. Not used by other processors (use c_global(1,1,1))
    !>   @param my_c       my scalar variable (at corner points) to be gathered
    !
    integer(ip), intent(IN)    :: nx
    integer(ip), intent(IN)    :: ny
    integer(ip), intent(IN)    :: nz
    real(rp),    intent(INOUT) :: c_global( 1:nx, 1:ny, 1:nz)
    real(rp),    intent(IN)    :: my_c( my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe)
    integer(ip)                :: ibs,ibe,jbs,jbe,kbs,kbe
    !
#if defined WITH_MPI
    !
    integer(ip)              :: iproc
    integer(ip)              :: dimx,dimy,dimz,dim
    integer(ip)              :: ishand,irhand
    real(rp),    allocatable :: work_send (:,:,:)
    real(rp),    allocatable :: work_reciv(:,:,:)
    !
    !*** Master recieves data from all processors
    !
    if(master_model) then
       !
       !*** Initialize
       !
       c_global(:,:,:) = 0.0_rp
       !
       do iproc = 1,npes_model-1  ! exclude Master (0) from gather
          !
          ibs = gl_ibs(iproc)
          ibe = gl_ibe(iproc)
          jbs = gl_jbs(iproc)
          jbe = gl_jbe(iproc)
          kbs = gl_kbs(iproc)
          kbe = gl_kbe(iproc)
          !
          dimx = ibe - ibs + 1
          dimy = jbe - jbs + 1
          dimz = kbe - kbs + 1
          dim  = dimx*dimy*dimz
          allocate(work_reciv (dimx,dimy,dimz))
          !
          call parallel_irecv( work_reciv(1,1,1), dim, iproc, 1_ip, irhand, COMM_MODEL )
          call parallel_wait( irhand )
          c_global(ibs:ibe,jbs:jbe,kbs:kbe) = work_reciv(1:dimx,1:dimy,1:dimz)
          deallocate(work_reciv)
       end do
    else
       !
       ibs = my_ibs
       ibe = my_ibe
       jbs = my_jbs
       jbe = my_jbe
       kbs = my_kbs
       kbe = my_kbe
       !
       dimx = ibe - ibs + 1
       dimy = jbe - jbs + 1
       dimz = kbe - kbs + 1
       dim  = dimx*dimy*dimz
       allocate(work_send(dimx,dimy,dimz))
       !
       work_send(1:dimx,1:dimy,1:dimz) = my_c(ibs:ibe,jbs:jbe,kbs:kbe)
       call parallel_isend( work_send(1,1,1), dim, 0_ip, 1_ip, ishand, COMM_MODEL )
       call parallel_wait(ishand)
       deallocate(work_send)
    end if
    !
#endif
    !
    !*** Master assigns to himself
    !
    if(master_model) then
       !
       ibs = my_ibs
       ibe = my_ibe
       jbs = my_jbs
       jbe = my_jbe
       kbs = my_kbs
       kbe = my_kbe
       !
       c_global(ibs:ibe,jbs:jbe,kbs:kbe) = my_c(ibs:ibe,jbs:jbe,kbs:kbe)
    end if
    !
    return
  end subroutine domain_gather_corner_points_0halo
  !
  !---------------------------------------------------
  !    subroutine domain_gather_corner_points_0halo_2D
  !---------------------------------------------------
  !
  !>   @brief
  !>   Gathers a local my_c array to Master c_global(1:nx,1:ny) array
  !
  subroutine domain_gather_corner_points_0halo_2D(c_global,nx,ny,my_c)
    implicit none
    !
    !>   @param c_global   Global array (in Master processor only)
    !>   @param nx         For Master, number of global corner points (boundaries) along x. Not used by other processors (use c_global(1,1))
    !>   @param ny         For Master, number of global corner points (boundaries) along y. Not used by other processors (use c_global(1,1))
    !>   @param my_c       my scalar variable (at corner points) to be gathered
    !
    integer(ip), intent(IN)    :: nx
    integer(ip), intent(IN)    :: ny
    real(rp),    intent(INOUT) :: c_global( 1:nx, 1:ny)
    real(rp),    intent(IN)    :: my_c( my_ibs:my_ibe, my_jbs:my_jbe)
    integer(ip)                :: ibs,ibe,jbs,jbe
    !
#if defined WITH_MPI
    !
    integer(ip)              :: iproc
    integer(ip)              :: dimx,dimy,dim
    integer(ip)              :: ishand,irhand
    real(rp),    allocatable :: work_send (:,:)
    real(rp),    allocatable :: work_reciv(:,:)
    !
    !*** Master recieves data from all processors
    !
    if(master_model) then
       !
       !*** Initialize
       !
       c_global(:,:) = 0.0_rp
       !
       do iproc = 1,npes_model-1  ! exclude Master (0) from gather
          !
          ibs = gl_ibs(iproc)
          ibe = gl_ibe(iproc)
          jbs = gl_jbs(iproc)
          jbe = gl_jbe(iproc)
          !
          dimx = ibe - ibs + 1
          dimy = jbe - jbs + 1
          dim  = dimx*dimy
          allocate(work_reciv (dimx,dimy))
          !
          call parallel_irecv( work_reciv(1,1), dim, iproc, 1_ip, irhand, COMM_MODEL )
          call parallel_wait( irhand )
          c_global(ibs:ibe,jbs:jbe) = work_reciv(1:dimx,1:dimy)
          deallocate(work_reciv)
       end do
    else
       !
       ibs = my_ibs
       ibe = my_ibe
       jbs = my_jbs
       jbe = my_jbe
       !
       dimx = ibe - ibs + 1
       dimy = jbe - jbs + 1
       dim  = dimx*dimy
       allocate(work_send(dimx,dimy))
       !
       work_send(1:dimx,1:dimy) = my_c(ibs:ibe,jbs:jbe)
       call parallel_isend( work_send(1,1), dim, 0_ip, 1_ip, ishand, COMM_MODEL )
       call parallel_wait(ishand)
       deallocate(work_send)
    end if
    !
#endif
    !
    !*** Master assigns to himself
    !
    if(master_model) then
       !
       ibs = my_ibs
       ibe = my_ibe
       jbs = my_jbs
       jbe = my_jbe
       !
       c_global(ibs:ibe,jbs:jbe) = my_c(ibs:ibe,jbs:jbe)
    end if
    !
    return
  end subroutine domain_gather_corner_points_0halo_2D
  !
  !-------------------------------------------------
  !    subroutine domain_scatter_corner_points_0halo
  !-------------------------------------------------
  !
  !>   @brief
  !>   Distributes a Master global array c_global(1:nx,1:ny,1:nz) to local array my_c
  !
  subroutine domain_scatter_corner_points_0halo(c_global,nx,ny,nz,my_c)
    implicit none
    !
    !>   @param c_global   Global array (in Master processor only)
    !>   @param nx         For Master, number of global corner points (boundaries) along x. Not used by other processors (use c_global(1,1,1))
    !>   @param ny         For Master, number of global corner points (boundaries) along y. Not used by other processors (use c_global(1,1,1))
    !>   @param nz         For Master, number of global corner points (boundaries) along z. Not used by other processors (use c_global(1,1,1))
    !>   @param my_c       my scalar variable (at corner points) to be gathered
    !
    integer(ip), intent(IN)    :: nx
    integer(ip), intent(IN)    :: ny
    integer(ip), intent(IN)    :: nz
    real(rp),    intent(IN)    :: c_global(1:nx,1:ny,1:nz)
    real(rp),    intent(INOUT) :: my_c( my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe)
    !
#if defined WITH_MPI
    !
    integer(ip)              :: iproc
    integer(ip)              :: dimx,dimy,dimz,dim
    integer(ip)              :: ishand,irhand
    real(rp),    allocatable :: work_send (:,:,:)
    real(rp),    allocatable :: work_reciv(:,:,:)
    !
    !*** Master send data to all processors
    !
    if(master_model) then
       do iproc = 1,npes_model-1  ! exclude Master (0) from scatter
          !
          dimx = gl_ibe(iproc)-gl_ibs(iproc) + 1
          dimy = gl_jbe(iproc)-gl_jbs(iproc) + 1
          dimz = gl_kbe(iproc)-gl_kbs(iproc) + 1
          dim  = dimx*dimy*dimz
          allocate(work_send (dimx,dimy,dimz))
          !
          work_send(1:dimx,1:dimy,1:dimz) = c_global(gl_ibs(iproc):gl_ibe(iproc), &
               gl_jbs(iproc):gl_jbe(iproc), &
               gl_kbs(iproc):gl_kbe(iproc))
          call parallel_isend( work_send(1,1,1), dim, iproc, 1_ip, ishand, COMM_MODEL )
          call parallel_wait(ishand)
          deallocate(work_send)
       end do
    else
       dimx = my_ibe-my_ibs+1
       dimy = my_jbe-my_jbs+1
       dimz = my_kbe-my_kbs+1
       dim  = dimx*dimy*dimz
       allocate(work_reciv(dimx,dimy,dimz))
       !
       call parallel_irecv( work_reciv(1,1,1), dim, 0_ip, 1_ip, irhand, COMM_MODEL )
       call parallel_wait( irhand )
       my_c(my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe) = work_reciv(1:dimx,1:dimy,1:dimz)
       deallocate(work_reciv)
    end if
    !
#endif
    !
    !*** Master assigns to himself
    !
    if(master_model) then
       my_c    (my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe) = &
            c_global(my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe)
    end if
    !
    return
  end subroutine domain_scatter_corner_points_0halo
  !
  !----------------------------------------------------
  !    subroutine domain_scatter_corner_points_0halo_2d
  !----------------------------------------------------
  !
  !>   @brief
  !>   Distributes a Master global array c_global(1:nx,1:ny) to local array my_c
  !
  subroutine domain_scatter_corner_points_0halo_2d(c_global,nx,ny,my_c)
    implicit none
    !
    !>   @param c_global   Global array (in Master processor only)
    !>   @param nx         For Master, number of global corner points (boundaries) along x. Not used by other processors (use c_global(1,1,1))
    !>   @param ny         For Master, number of global corner points (boundaries) along y. Not used by other processors (use c_global(1,1,1))
    !>   @param my_c       my scalar variable (at corner points) to be gathered
    !
    integer(ip), intent(IN)    :: nx
    integer(ip), intent(IN)    :: ny
    real(rp),    intent(IN)    :: c_global(1:nx,1:ny)
    real(rp),    intent(INOUT) :: my_c( my_ibs:my_ibe, my_jbs:my_jbe)
    !
#if defined WITH_MPI
    !
    integer(ip)              :: iproc
    integer(ip)              :: dimx,dimy,dim
    integer(ip)              :: ishand,irhand
    real(rp),    allocatable :: work_send (:,:)
    real(rp),    allocatable :: work_reciv(:,:)
    !
    !*** Master send data to all processors
    !
    if(master_model) then
       do iproc = 1,npes_model-1  ! exclude Master (0) from scatter
          !
          dimx = gl_ibe(iproc)-gl_ibs(iproc) + 1
          dimy = gl_jbe(iproc)-gl_jbs(iproc) + 1
          dim  = dimx*dimy
          allocate(work_send (dimx,dimy))
          !
          work_send(1:dimx,1:dimy) = c_global(gl_ibs(iproc):gl_ibe(iproc), &
               gl_jbs(iproc):gl_jbe(iproc))
          call parallel_isend( work_send(1,1), dim, iproc, 1_ip, ishand, COMM_MODEL )
          call parallel_wait(ishand)
          deallocate(work_send)
       end do
    else
       dimx = my_ibe-my_ibs+1
       dimy = my_jbe-my_jbs+1
       dim  = dimx*dimy
       allocate(work_reciv(dimx,dimy))
       !
       call parallel_irecv( work_reciv(1,1), dim, 0_ip, 1_ip, irhand, COMM_MODEL )
       call parallel_wait( irhand )
       my_c(my_ibs:my_ibe, my_jbs:my_jbe) = work_reciv(1:dimx,1:dimy)
       deallocate(work_reciv)
    end if
    !
#endif
    !
    !*** Master assigns to himself
    !
    if(master_model) then
       my_c    (my_ibs:my_ibe, my_jbs:my_jbe) = &
            c_global(my_ibs:my_ibe, my_jbs:my_jbe)
    end if
    !
    return
  end subroutine domain_scatter_corner_points_0halo_2d
  !
  !
  !
END MODULE Domain
