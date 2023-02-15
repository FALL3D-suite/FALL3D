!******************************************************************
!>
!>   Module to solve the 1D ADS equation using the Kurganov scheme
!>   combined with an explicit RK4 for time integration
!>   @author
!>   Arnau Folch
!>
!******************************************************************
MODULE ADS
  use KindType
  use Domain
  implicit none
  !
  !>   type ADS_CB: control block for ADS solver
  !
  type ADS_CB
     !
     real(rp), allocatable :: u(:,:,:)
     real(rp), allocatable :: v(:,:,:)
     real(rp), allocatable :: w(:,:,:)
     real(rp), allocatable :: raw_c (:)
     real(rp), allocatable :: new_c (:)
     real(rp), allocatable :: c0 (:)
     !
     procedure(bconditions), pointer, nopass :: apply_bconditions
     procedure(fluxlimiter), pointer, nopass :: flux_limiter
     !
     integer(ip) :: time_marching
     real(rp)    :: bvalue(2)
     !
     ! Temporary arrays for KT_RHS
     !
     real(rp), allocatable :: raw_r (:)
     real(rp), allocatable :: raw_c_l (:)
     real(rp), allocatable :: raw_c_r (:)
     !
  end type ADS_CB
  !
  !    LIST OF PRIVATE VARIABLES
  !
  type(ADS_CB), PRIVATE, TARGET :: CB    !> Variables related to ADS solver
  !
  integer(ip),  PRIVATE, parameter :: TIME_MARCHING_EULER = 1
  integer(ip),  PRIVATE, parameter :: TIME_MARCHING_RK    = 2
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: ADS_initialize
  PUBLIC :: ADS_get_horizontal_vcomps
  PUBLIC :: ADS_update_horizontal_vcomps
  PUBLIC :: ADS_release
  PUBLIC :: ADS_solve_along_x
  PUBLIC :: ADS_solve_along_y
  PUBLIC :: ADS_solve_along_z
  PUBLIC :: ADS_solve_1D
  !
  !    LIST OF PRIVATE ROUTINES IN THE MODULE
  !
  PRIVATE :: KT_RHS
  PRIVATE :: r_minmod
  PRIVATE :: r_sbee
  PRIVATE :: r_ospre
  PRIVATE :: freeflow
  PRIVATE :: dirichlet
  PRIVATE :: periodic
  !
  !    Interface of abstract boundary conditions function
  !
  abstract interface
     subroutine bconditions (c,mydim)
       import
       real(rp),   intent(INOUT) :: c  (my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h,my_kps_2h:my_kpe_2h)
       integer(ip),intent(IN   ) :: mydim
     end subroutine bconditions
  end interface

  !
  !    Interface of flux limiter function
  !
  abstract interface
     function fluxlimiter (c,ips,ipe)
       import
       real(rp)                  :: fluxlimiter(ips-2:ipe+2)
       real(rp),   intent(IN)    :: c(ips-2:ipe+2)
       integer(ip),intent(IN)    :: ips,ipe
     end function fluxlimiter
  end interface

CONTAINS
  !
  !-----------------------------------
  !    subroutine ADS_initialize
  !-----------------------------------
  !
  !>   @brief
  !>   Allocate resources and initialize parameters for ADS solver.
  !
  subroutine ADS_initialize(limitertype,time_marching)
    implicit none
    !
    !>   @param limitertype    flux limiter flag       : MINMOD = 1, SUPERBEE = 2, OSPRE = 3
    !>   @param time_marching  time integration scheme : TIME_MARCHING_EULER = 1, TIME_MARCHING_RK  = 2
    !
    integer(ip), intent(IN)    :: limitertype
    integer(ip), intent(IN)    :: time_marching
    integer(ip)                :: gsize
    !
    !*** Temporary velocity for current time
    !
    allocate(CB%u(my_ibs_1h:my_ibe_1h,my_jps   :my_jpe   ,my_kps   :my_kpe   ))
    allocate(CB%v(my_jbs_1h:my_jbe_1h,my_ips   :my_ipe   ,my_kps   :my_kpe   ))
    allocate(CB%w(my_kbs_1h:my_kbe_1h,my_ips   :my_ipe   ,my_jps   :my_jpe   ))
    !
    !*** Configure boundary conditions
    !
    CB%apply_bconditions => freeflow
    CB%bvalue(:) = 0.0_rp
    !
    !*** Limiter
    !
    select case(limitertype)
    case(1)
       !                         minmod
       CB%flux_limiter => r_minmod
    case(2)
       !                         superbee
       CB%flux_limiter => r_sbee
    case(3)
       !                         ospre
       CB%flux_limiter => r_ospre
    end select
    !
    !*** Time marching
    !
    CB%time_marching = time_marching
    !
    !*** Resources for KT_RHS
    !
    gsize = my_ipe - my_ips + 1
    if (gsize.lt.(my_jpe-my_jps+1)) then
       gsize = my_jpe-my_jps+1
    end if
    if (gsize.lt.(my_kpe-my_kps+1)) then
       gsize = my_kpe-my_kps+1
    end if

    allocate(CB%raw_r(gsize+4))
    allocate(CB%raw_c_r(gsize+2))
    allocate(CB%raw_c_l(gsize+2))

    gsize = (my_ipe_2h-my_ips_2h+1)*(my_jpe_2h-my_jps_2h+1)*(my_kpe_2h-my_kps_2h+1)

    allocate(CB%c0(gsize))
    allocate(CB%new_c(gsize))
    allocate(CB%raw_c(gsize))

  end subroutine ADS_initialize
  !
  !----------------------------------------
  !    subroutine ADS_get_horizontal_vcomps
  !----------------------------------------
  !
  !>   @brief
  !>   Get pointers to horizontal velocity components.
  !
  subroutine ADS_get_horizontal_vcomps(my_u,my_v)
    implicit none

    real(rp),pointer,intent(INOUT) :: my_u(:,:,:)
    real(rp),pointer,intent(INOUT) :: my_v(:,:,:)

    my_u => CB%u
    my_v => CB%v

  end subroutine ADS_get_horizontal_vcomps

  !
  !----------------------------------------
  !    subroutine ADS_get_horizontal_vcomps
  !----------------------------------------
  !
  !>   @brief
  !>   Get pointers to horizontal velocity components.
  !
  subroutine ADS_update_horizontal_vcomps(my_u1,my_u2,my_v1,my_v2,stime)
    implicit none

    real(rp),            intent(IN   ) :: my_u1 (my_ibs_1h:my_ibe_1h,my_jps   :my_jpe   ,my_kps   :my_kpe   )
    real(rp),            intent(IN   ) :: my_u2 (my_ibs_1h:my_ibe_1h,my_jps   :my_jpe   ,my_kps   :my_kpe   )
    real(rp),            intent(IN   ) :: my_v1 (my_jbs_1h:my_jbe_1h,my_ips   :my_ipe   ,my_kps   :my_kpe   )
    real(rp),            intent(IN   ) :: my_v2 (my_jbs_1h:my_jbe_1h,my_ips   :my_ipe   ,my_kps   :my_kpe   )
    real(rp),            intent(IN   ) :: stime

    CB%u(:,:,:) = (1.0_rp-stime) * my_u1(:,:,:) + stime * my_u2(:,:,:)
    CB%v(:,:,:) = (1.0_rp-stime) * my_v1(:,:,:) + stime * my_v2(:,:,:)
  end subroutine ADS_update_horizontal_vcomps

  !
  !-----------------------------------
  !    subroutine ADS_release
  !-----------------------------------
  !
  !>   @brief
  !>   Release resources used by ADS solver.
  !
  subroutine ADS_release()
    implicit none

    !
    !*** Temporary velocity for current time
    !
    deallocate(CB%u)
    deallocate(CB%v)
    deallocate(CB%w)
    deallocate(CB%raw_r)
    deallocate(CB%raw_c_r)
    deallocate(CB%raw_c_l)
    deallocate(CB%raw_c)
    deallocate(CB%new_c)
    deallocate(CB%c0)

  end subroutine ADS_release
  !
  !-----------------------------------
  !    subroutine ADS_solve_along_x
  !-----------------------------------
  !
  !>   @brief
  !>   Solves a time step dt of the 1D ADS equation along x direction.
  !
  subroutine ADS_solve_along_x(my_W_flux,my_E_flux,dt,my_c,my_k1,MY_GRID,nbins)
    implicit none
    !
    !>   @param my_W_flux increment of mass flux across the W domain boundary during dt
    !>   @param my_E_flux increment of mass flux across the E domain boundary during dt
    !>   @param dt        time increment
    !>   @param my_c      my values of scaled transported variable (concentration) at mass points
    !>   @param my_k1     my values of scaled diffusion at mass points
    !>   @param MY_GRID   grid structure
    !>   @param nbins     number of bins
    !
    integer(ip),         intent(IN   ) :: nbins
    real(rp),            intent(INOUT) :: my_W_flux
    real(rp),            intent(INOUT) :: my_E_flux
    real(rp),            intent(IN   ) :: dt
    real(rp),target,     intent(INOUT) :: my_c  (my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h,my_kps_2h:my_kpe_2h,nbins)
    real(rp),target,     intent(IN   ) :: my_k1 (my_ips_2h:my_ipe_2h,my_jps   :my_jpe   ,my_kps   :my_kpe   )
    type(ARAKAWA_C_GRID),intent(IN   ) :: MY_GRID

    integer(ip)           :: j,k,ibin
    real(rp)              :: flux(2),area
    real(rp), pointer     :: my_c_1d(:)
    real(rp), pointer     :: my_u_1d(:)
    real(rp), pointer     :: my_k_1d(:)
    real(rp), pointer     :: c0(:,:,:)
    real(rp), pointer     :: new_c(:,:,:)
    real(rp), allocatable :: ki (:)
    !
    !*** Update concentration field halos
    !
    do ibin = 1,nbins
       call domain_swap_mass_points_2halo_x (my_c(:,:,:,ibin))
    end do
    !
    !*** Apply boundaries
    !
    do ibin = 1,nbins
       call CB%apply_bconditions( my_c(:,:,:,ibin), 1_ip )
    end do
    !
    !*** Allocate memory
    !
    allocate(ki     (my_ips   :my_ipe   ))
    !
    !*** Prepare shape of temporary fields
    !
    c0(my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h, my_kps_2h:my_kpe_2h) => CB%c0(:)
    new_c(my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h, my_kps_2h:my_kpe_2h) => CB%new_c(:)
    !
    !*** Loop over bins
    !
    do ibin = 1,nbins
       !
       select case(CB%time_marching)
          !
       case(TIME_MARCHING_EULER)
          !
          !   Euler O(1)
          !
          do k = my_kps,my_kpe
             do j = my_jps,my_jpe
                !
                my_c_1d(my_ips_2h:my_ipe_2h) => my_c (my_ips_2h:my_ipe_2h,j,k,ibin)
                my_k_1d(my_ips_2h:my_ipe_2h) => my_k1(my_ips_2h:my_ipe_2h,j,k)
                my_u_1d(my_ibs_1h:my_ibe_1h) => CB%u (my_ibs_1h:my_ibe_1h,j,k)
                !
                ki = KT_RHS(my_c_1d,MY_GRID%dX1_p,MY_GRID%dX1_b,my_u_1d,my_k_1d, &
                     flux,my_ips,my_ipe,my_ibs,my_ibe)
                !
                my_c(my_ips:my_ipe,j,k,ibin) = my_c_1d(my_ips:my_ipe) + dt*ki(my_ips:my_ipe)
                !
                !*** Fluxes
                !
                area = MY_GRID%dX3_b(k)*MY_GRID%dX2_b(j)
                if(my_W_proc.eq.-1) my_W_flux = my_W_flux + flux(1) *area*dt
                if(my_E_proc.eq.-1) my_E_flux = my_E_flux + flux(2) *area*dt
                !
             end do
          end do
          !
       case(TIME_MARCHING_RK)
          !
          !   RK O(4)
          !
          do k = my_kps,my_kpe
             do j = my_jps,my_jpe
                !
                my_c_1d(my_ips_2h:my_ipe_2h) => my_c (my_ips_2h:my_ipe_2h,j,k,ibin)
                my_k_1d(my_ips_2h:my_ipe_2h) => my_k1(my_ips_2h:my_ipe_2h,j,k)
                my_u_1d(my_ibs_1h:my_ibe_1h) => CB%u (my_ibs_1h:my_ibe_1h,j,k)
                !
                !***     Time integration in the interval (t,t+dt) using a RK4 Runge-Kutta method
                !
                ki = KT_RHS(my_c_1d,MY_GRID%dX1_p,MY_GRID%dX1_b,my_u_1d,my_k_1d, &
                     flux,my_ips,my_ipe,my_ibs,my_ibe)

                ! In this first stage, we need to copy also the boundary halos.
                c0(:,j,k) = my_c_1d(:)
                c0(my_ips:my_ipe,j,k) = c0(my_ips:my_ipe,j,k) + 0.5_rp*dt*ki(my_ips:my_ipe)

                new_c(:,j,k) = my_c_1d(:)
                new_c(my_ips:my_ipe,j,k) = new_c(my_ips:my_ipe,j,k) + dt*ki(my_ips:my_ipe) / 6.0_rp

             end do
          end do
          !
          !      Successive approaches. Note that swap is necessary to update predictor halo values
          !
          call domain_swap_mass_points_2halo_first (c0(:,:,:), my_ips, my_ipe, &
               my_jps,my_jpe,my_kps,my_kpe, my_W_proc, my_E_proc)

          do k = my_kps,my_kpe
             do j = my_jps,my_jpe
                !
                my_c_1d(my_ips_2h:my_ipe_2h) => c0(my_ips_2h:my_ipe_2h,j,k)
                my_k_1d(my_ips_2h:my_ipe_2h) => my_k1(my_ips_2h:my_ipe_2h,j,k)
                my_u_1d(my_ibs_1h:my_ibe_1h) => CB%u (my_ibs_1h:my_ibe_1h,j,k)
                !
                ki = KT_RHS(my_c_1d,MY_GRID%dX1_p,MY_GRID%dX1_b,my_u_1d,my_k_1d, &
                     flux,my_ips,my_ipe,my_ibs,my_ibe)
                !
                c0(my_ips:my_ipe,j,k) = my_c(my_ips:my_ipe,j,k,ibin) + 0.5_rp*dt*ki(my_ips:my_ipe)
                new_c(my_ips:my_ipe,j,k) = new_c(my_ips:my_ipe,j,k) + dt*ki(my_ips:my_ipe) / 3.0_rp
             end do
          end do
          !
          call domain_swap_mass_points_2halo_first (c0(:,:,:), my_ips, my_ipe, &
               my_jps,my_jpe,my_kps,my_kpe, my_W_proc, my_E_proc)

          do k = my_kps,my_kpe
             do j = my_jps,my_jpe
                !
                my_c_1d(my_ips_2h:my_ipe_2h) => c0(my_ips_2h:my_ipe_2h,j,k)
                my_k_1d(my_ips_2h:my_ipe_2h) => my_k1(my_ips_2h:my_ipe_2h,j,k)
                my_u_1d(my_ibs_1h:my_ibe_1h) => CB%u (my_ibs_1h:my_ibe_1h,j,k)

                ki = KT_RHS(my_c_1d,MY_GRID%dX1_p,MY_GRID%dX1_b,my_u_1d,my_k_1d, &
                     flux,my_ips,my_ipe,my_ibs,my_ibe)
                !
                c0(my_ips:my_ipe,j,k) = my_c(my_ips:my_ipe,j,k,ibin) + dt*ki(my_ips:my_ipe)
                new_c(my_ips:my_ipe,j,k) = new_c(my_ips:my_ipe,j,k) + dt*ki(my_ips:my_ipe) / 3.0_rp
             end do
          end do

          call domain_swap_mass_points_2halo_first (c0(:,:,:), my_ips,my_ipe, &
               my_jps,my_jpe,my_kps,my_kpe, my_W_proc, my_E_proc)

          do k = my_kps,my_kpe
             do j = my_jps,my_jpe
                !
                my_c_1d(my_ips_2h:my_ipe_2h) => c0(my_ips_2h:my_ipe_2h,j,k)
                my_k_1d(my_ips_2h:my_ipe_2h) => my_k1(my_ips_2h:my_ipe_2h,j,k)
                my_u_1d(my_ibs_1h:my_ibe_1h) => CB%u (my_ibs_1h:my_ibe_1h,j,k)

                ki = KT_RHS(my_c_1d,MY_GRID%dX1_p,MY_GRID%dX1_b,my_u_1d,my_k_1d, &
                     flux,my_ips,my_ipe,my_ibs,my_ibe)
                !
                my_c(my_ips:my_ipe,j,k,ibin) = new_c(my_ips:my_ipe,j,k) + dt*ki(my_ips:my_ipe) / 6.0_rp
                !
                !*** Fluxes
                !
                area = MY_GRID%dX3_b(k)*MY_GRID%dX2_b(j)
                if(my_W_proc.eq.-1) my_W_flux = my_W_flux + flux(1) *area*dt
                if(my_E_proc.eq.-1) my_E_flux = my_E_flux + flux(2) *area*dt
                !
             end do
          end do
          !
          !
       end select
       !
    end do
    !
    deallocate(ki )
    do ibin = 1,nbins
       call domain_swap_mass_points_2halo_x (my_c(:,:,:,ibin))
    end do
    !
    return
  end subroutine ADS_solve_along_x
  !
  !
  !-----------------------------------
  !    subroutine ADS_solve_along_y
  !-----------------------------------
  !
  !>   @brief
  !>   Solves a time step dt of the 1D ADS equation along y direction.
  !
  subroutine ADS_solve_along_y(my_S_flux,my_N_flux,dt,my_c,my_k2,MY_GRID,nbins)
    implicit none
    !
    !>   @param my_S_flux increment of mass flux across the S domain boundary during dt
    !>   @param my_N_flux increment of mass flux across the N domain boundary during dt
    !>   @param dt        time increment
    !>   @param my_c      my values of scaled transported variable (concentration) at mass points
    !>   @param my_k2     my values of scaled diffusion at mass points
    !>   @param MY_GRID   grid structure
    !>   @param nbins     number of bins
    !
    integer(ip),         intent(IN   ) :: nbins
    real(rp),            intent(INOUT) :: my_S_flux
    real(rp),            intent(INOUT) :: my_N_flux
    real(rp),            intent(IN   ) :: dt
    real(rp),            intent(INOUT) :: my_c  (my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h,my_kps_2h:my_kpe_2h,nbins)
    real(rp),target     ,intent(IN   ) :: my_k2 (my_jps_2h:my_jpe_2h,my_ips   :my_ipe   ,my_kps   :my_kpe   )
    type(ARAKAWA_C_GRID),intent(IN   ) :: MY_GRID
    !
    integer(ip)           :: i,k,ibin
    real(rp)              :: flux(2),area
    real(rp), pointer     :: my_c_1d(:)
    real(rp), pointer     :: my_v_1d(:)
    real(rp), pointer     :: my_k_1d(:)
    real(rp), pointer     :: c0(:,:,:)
    real(rp), pointer     :: new_c(:,:,:)
    real(rp), pointer     :: ini_c(:,:,:)
    real(rp), allocatable :: ki (:)
    !
    !*** Update concentration field halos
    !
    do ibin = 1,nbins
       call domain_swap_mass_points_2halo_y (my_c(:,:,:,ibin))
    end do
    !
    !*** Apply boundaries
    !
    do ibin = 1, nbins
       call CB%apply_bconditions( my_c(:,:,:,ibin), 2_ip )
    end do
    !
    allocate(ki     (my_jps   :my_jpe   ))
    !
    !*** Prepare shape of temporary fields
    !
    c0(my_jps_2h:my_jpe_2h,my_ips_2h:my_ipe_2h, my_kps_2h:my_kpe_2h) => CB%c0(:)
    new_c(my_jps_2h:my_jpe_2h,my_ips_2h:my_ipe_2h, my_kps_2h:my_kpe_2h) => CB%new_c(:)
    ini_c(my_jps_2h:my_jpe_2h,my_ips_2h:my_ipe_2h, my_kps_2h:my_kpe_2h) => CB%raw_c(:)
    !
    !*** Loop over bins
    !
    do ibin = 1, nbins
       !
       select case(CB%time_marching)
          !
       case(TIME_MARCHING_EULER)
          !
          !   Euler O(1)
          !
          do k = my_kps,my_kpe
             do i = my_ips,my_ipe
                ini_c(my_jps_2h:my_jpe_2h,i,k) = my_c (i,my_jps_2h:my_jpe_2h,k,ibin)
                my_c_1d(my_jps_2h:my_jpe_2h) => ini_c (my_jps_2h:my_jpe_2h,i,k)
                my_k_1d(my_jps_2h:my_jpe_2h) => my_k2(my_jps_2h:my_jpe_2h,i,k)
                my_v_1d(my_jbs_1h:my_jbe_1h) => CB%v (my_jbs_1h:my_jbe_1h,i,k)
                !
                ki = KT_RHS(my_c_1d,MY_GRID%dX2_p,MY_GRID%dX2_b,my_v_1d,my_k_1d, &
                     flux,my_jps,my_jpe,my_jbs,my_jbe)
                !
                my_c(i,my_jps:my_jpe,k,ibin) = my_c_1d(my_jps:my_jpe) + dt*ki(my_jps:my_jpe)
                !
                !***     Fluxes
                !
                area = MY_GRID%dX3_b(k)*MY_GRID%dX1_b(i)
                if(my_S_proc.eq.-1) my_S_flux = my_S_flux + flux(1) *area*dt
                if(my_N_proc.eq.-1) my_N_flux = my_N_flux + flux(2) *area*dt
                !
             end do
          end do
          !
       case(TIME_MARCHING_RK)
          !
          !   RK O(4)
          !
          do k = my_kps,my_kpe
             do i = my_ips,my_ipe
                ini_c(my_jps_2h:my_jpe_2h,i,k) = my_c (i,my_jps_2h:my_jpe_2h,k,ibin)
                my_c_1d(my_jps_2h:my_jpe_2h) => ini_c (my_jps_2h:my_jpe_2h,i,k)
                my_k_1d(my_jps_2h:my_jpe_2h) => my_k2(my_jps_2h:my_jpe_2h,i,k)
                my_v_1d(my_jbs_1h:my_jbe_1h) => CB%v (my_jbs_1h:my_jbe_1h,i,k)
                !
                !***     Time integration in the interval (t,t+dt) using a RK4 Runge-Kutta method
                !
                ki = KT_RHS(my_c_1d,MY_GRID%dX2_p,MY_GRID%dX2_b,my_v_1d,my_k_1d, &
                     flux,my_jps,my_jpe,my_jbs,my_jbe)

                ! In this first stage, we need to copy also the boundary halos.
                c0(:,i,k) = my_c_1d(:)
                c0(my_jps:my_jpe,i,k) = c0(my_jps:my_jpe,i,k) + 0.5_rp*dt*ki(my_jps:my_jpe)

                new_c(:,i,k) = my_c_1d(:)
                new_c(my_jps:my_jpe,i,k) = new_c(my_jps:my_jpe,i,k) + dt*ki(my_jps:my_jpe) / 6.0_rp
             end do
          end do
          !
          !      Successive approaches. Note that swap is necessary to update predictor halo values
          !
          call domain_swap_mass_points_2halo_first (c0(:,:,:), my_jps, my_jpe, &
               my_ips,my_ipe,my_kps,my_kpe, my_S_proc, my_N_proc)


          do k = my_kps,my_kpe
             do i = my_ips,my_ipe
                my_c_1d(my_jps_2h:my_jpe_2h) => c0 (my_jps_2h:my_jpe_2h,i,k)
                my_k_1d(my_jps_2h:my_jpe_2h) => my_k2(my_jps_2h:my_jpe_2h,i,k)
                my_v_1d(my_jbs_1h:my_jbe_1h) => CB%v (my_jbs_1h:my_jbe_1h,i,k)
                !
                ki = KT_RHS(my_c_1d,MY_GRID%dX2_p,MY_GRID%dX2_b,my_v_1d,my_k_1d, &
                     flux,my_jps,my_jpe,my_jbs,my_jbe)
                !
                c0(my_jps:my_jpe,i,k) = ini_c(my_jps:my_jpe,i,k) + 0.5_rp*dt*ki(my_jps:my_jpe)
                new_c(my_jps:my_jpe,i,k) = new_c(my_jps:my_jpe,i,k) + dt*ki(my_jps:my_jpe) / 3.0_rp
             end do
          end do
          !
          !      Successive approaches. Note that swap is necessary to update predictor halo values
          !
          call domain_swap_mass_points_2halo_first (c0(:,:,:), my_jps, my_jpe, &
               my_ips,my_ipe,my_kps,my_kpe, my_S_proc, my_N_proc)

          do k = my_kps,my_kpe
             do i = my_ips,my_ipe
                my_c_1d(my_jps_2h:my_jpe_2h) => c0 (my_jps_2h:my_jpe_2h,i,k)
                my_k_1d(my_jps_2h:my_jpe_2h) => my_k2(my_jps_2h:my_jpe_2h,i,k)
                my_v_1d(my_jbs_1h:my_jbe_1h) => CB%v (my_jbs_1h:my_jbe_1h,i,k)

                ki = KT_RHS(my_c_1d,MY_GRID%dX2_p,MY_GRID%dX2_b,my_v_1d,my_k_1d, &
                     flux,my_jps,my_jpe,my_jbs,my_jbe)
                !
                c0(my_jps:my_jpe,i,k) = ini_c(my_jps:my_jpe,i,k) + dt*ki(my_jps:my_jpe)
                new_c(my_jps:my_jpe,i,k) = new_c(my_jps:my_jpe,i,k) + dt*ki(my_jps:my_jpe) / 3.0_rp
             end do
          end do
          !
          !      Successive approaches. Note that swap is necessary to update predictor halo values
          !
          call domain_swap_mass_points_2halo_first (c0(:,:,:), my_jps,my_jpe, &
               my_ips,my_ipe,my_kps,my_kpe, my_S_proc, my_N_proc)

          do k = my_kps,my_kpe
             do i = my_ips,my_ipe
                my_c_1d(my_jps_2h:my_jpe_2h) => c0 (my_jps_2h:my_jpe_2h,i,k)
                my_k_1d(my_jps_2h:my_jpe_2h) => my_k2(my_jps_2h:my_jpe_2h,i,k)
                my_v_1d(my_jbs_1h:my_jbe_1h) => CB%v (my_jbs_1h:my_jbe_1h,i,k)

                ki = KT_RHS(my_c_1d,MY_GRID%dX2_p,MY_GRID%dX2_b,my_v_1d,my_k_1d, &
                     flux,my_jps,my_jpe,my_jbs,my_jbe)
                !
                my_c(i,my_jps:my_jpe,k,ibin) = new_c(my_jps:my_jpe,i,k) + dt*ki(my_jps:my_jpe) / 6.0_rp
                !
                !***     Fluxes
                !
                area = MY_GRID%dX3_b(k)*MY_GRID%dX1_b(i)
                if(my_S_proc.eq.-1) my_S_flux = my_S_flux + flux(1) *area*dt
                if(my_N_proc.eq.-1) my_N_flux = my_N_flux + flux(2) *area*dt
                !
             end do
          end do
          !
          !
       end select
       !
    end do
    !
    deallocate(ki )
    do ibin = 1,nbins
       call domain_swap_mass_points_2halo_y (my_c(:,:,:,ibin))
    end do
    !
    return
  end subroutine ADS_solve_along_y
  !
  !
  !-----------------------------------
  !    subroutine ADS_solve_along_z
  !-----------------------------------
  !
  !>   @brief
  !>   Solves a time step dt of the 1D ADS equation along z direction.
  !
  subroutine ADS_solve_along_z(my_D_flux,my_U_flux,dt,my_c,my_acum,my_k3,my_w1,my_w2,stime,my_vs,MY_GRID,nbins)
    implicit none
    !
    !>   @param my_D_flux increment of mass flux across the D domain boundary during dt
    !>   @param my_U_flux increment of mass flux across the U domain boundary during dt
    !>   @param dt        time increment
    !>   @param my_c      my values of scaled transported variable (concentration) at mass points
    !>   @param my_acum   my values of ground accumulation at mass points
    !>   @param my_k3     my values of scaled diffusion at mass points
    !>   @param my_w1     my values of scaled w-velocity at boundaries, first snapshot
    !>   @param my_w2     my values of scaled w-velocity at boundaries, second snapshot
    !>   @param stime     position between snapshots
    !>   @param my_vs     settling velocity at w-boundaries
    !>   @param MY_GRID   grid structure
    !>   @param nbins     number of bins
    !
    integer(ip),         intent(IN   ) :: nbins
    real(rp),            intent(INOUT) :: my_D_flux
    real(rp),            intent(INOUT) :: my_U_flux
    real(rp),            intent(IN   ) :: dt
    real(rp),target     ,intent(INOUT) :: my_c   (my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h,my_kps_2h:my_kpe_2h,nbins)
    real(rp),            intent(INOUT) :: my_acum(my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h,nbins)
    real(rp),target     ,intent(IN   ) :: my_k3  (my_kps_2h:my_kpe_2h,my_ips   :my_ipe   ,my_jps   :my_jpe)
    real(rp),            intent(IN   ) :: my_w1  (my_kbs_1h:my_kbe_1h,my_ips   :my_ipe   ,my_jps   :my_jpe)
    real(rp),            intent(IN   ) :: my_w2  (my_kbs_1h:my_kbe_1h,my_ips   :my_ipe   ,my_jps   :my_jpe)
    real(rp),            intent(IN   ) :: stime
    real(rp),            intent(IN   ) :: my_vs  (my_ips   :my_ipe   ,my_jps   :my_jpe   ,my_kbs_1h:my_kbe_1h,nbins)
    type(ARAKAWA_C_GRID),intent(IN   ) :: MY_GRID
    !
    integer(ip)           :: i,j,ibin
    real(rp)              :: flux(2),area
    real(rp), pointer     :: my_c_1d(:)
    real(rp), pointer     :: my_w_1d(:)
    real(rp), pointer     :: my_k_1d(:)
    real(rp), pointer     :: c0(:,:,:)
    real(rp), pointer     :: new_c(:,:,:)
    real(rp), pointer     :: ini_c(:,:,:)
    real(rp), allocatable :: ki (:)

    allocate(ki     (my_kps:my_kpe))
    !
    !*** Prepare shape of temporary fields
    !
    c0(my_kps_2h:my_kpe_2h,my_ips_2h:my_ipe_2h, my_jps_2h:my_jpe_2h) => CB%c0(:)
    new_c(my_kps_2h:my_kpe_2h,my_ips_2h:my_ipe_2h, my_jps_2h:my_jpe_2h) => CB%new_c(:)
    ini_c(my_kps_2h:my_kpe_2h,my_ips_2h:my_ipe_2h, my_jps_2h:my_jpe_2h) => CB%raw_c(:)
    !
    !*** Loop over bins
    !
    do ibin = 1,nbins
       !
       call domain_swap_mass_points_2halo_z (my_c(:,:,:,ibin))
       !
       !***   Compute current velocity component
       !
       CB%w(:,:,:) = (1.0_rp-stime) * my_w1(:,:,:) + stime * my_w2(:,:,:)
       do j=my_jps,my_jpe
          do i=my_ips,my_ipe
             CB%w(:,i,j) = CB%w(:,i,j) - my_vs(i,j,:,ibin)
          end do
       end do
       !
       !***  Apply boundaries
       !
       call CB%apply_bconditions( my_c(:,:,:,ibin), 3_ip )
       !
       select case(CB%time_marching)
          !
       case(TIME_MARCHING_EULER)
          !
          !   Euler O(1)
          !
          do j = my_jps,my_jpe
             do i = my_ips,my_ipe
                ini_c(my_kps_2h:my_kpe_2h,i,j) = my_c (i,j,my_kps_2h:my_kpe_2h,ibin)
                my_c_1d(my_kps_2h:my_kpe_2h) => ini_c (my_kps_2h:my_kpe_2h,i,j)
                my_k_1d(my_kps_2h:my_kpe_2h) => my_k3(my_kps_2h:my_kpe_2h,i,j)
                my_w_1d(my_kbs_1h:my_kbe_1h) => CB%w(my_kbs_1h:my_kbe_1h,i,j)
                !
                ki = KT_RHS(my_c_1d,MY_GRID%dX3_p,MY_GRID%dX3_b,my_w_1d,my_k_1d, &
                     flux,my_kps,my_kpe,my_kbs,my_kbe)

                my_c(i,j,my_kps:my_kpe,ibin) = my_c_1d(my_kps:my_kpe) + dt*ki(my_kps:my_kpe)
                !
                !***   Fluxes
                !
                area = MY_GRID%dX2_b(j)*MY_GRID%dX1_b(i) !/(MY_GRID%Hm1_p(j)*MY_GRID%Hm2_p(j))
                if(my_D_proc.eq.-1) then
                   my_D_flux    = my_D_flux + flux(1)*area*dt              ! total mass
                   my_acum(i,j,ibin) = my_acum(i,j,ibin) + abs(flux(1))*dt ! mass per unit ara (force positive sign)
                end if
                if(my_U_proc.eq.-1) then
                   my_U_flux = my_U_flux + flux(2) *area*dt
                end if
                !
             end do
          end do
          !
       case(TIME_MARCHING_RK)
          !
          !   RK O(4)
          !
          do j = my_jps,my_jpe
             do i = my_ips,my_ipe
                ini_c(my_kps_2h:my_kpe_2h,i,j) = my_c (i,j,my_kps_2h:my_kpe_2h,ibin)
                my_c_1d(my_kps_2h:my_kpe_2h) => ini_c (my_kps_2h:my_kpe_2h,i,j)
                my_k_1d(my_kps_2h:my_kpe_2h) => my_k3(my_kps_2h:my_kpe_2h,i,j)
                my_w_1d(my_kbs_1h:my_kbe_1h) => CB%w(my_kbs_1h:my_kbe_1h,i,j)
                !
                !***     Time integration in the interval (t,t+dt) using a RK4 Runge-Kutta method
                !
                ki = KT_RHS(my_c_1d,MY_GRID%dX3_p,MY_GRID%dX3_b,my_w_1d,my_k_1d, &
                     flux,my_kps,my_kpe,my_kbs,my_kbe)

                ! In this first stage, we need to copy also the boundary halos.
                c0(:,i,j) = my_c_1d(:)
                c0(my_kps:my_kpe,i,j) = c0(my_kps:my_kpe,i,j) + 0.5_rp*dt*ki(my_kps:my_kpe)

                new_c(:,i,j) = my_c_1d(:)
                new_c(my_kps:my_kpe,i,j) = new_c(my_kps:my_kpe,i,j) + dt*ki(my_kps:my_kpe) / 6.0_rp
             end do
          end do
          !
          !      Successive approaches. Note that swap is necessary to update predictor halo values
          !
          call domain_swap_mass_points_2halo_first (c0(:,:,:), my_kps,my_kpe, &
               my_ips,my_ipe,my_jps,my_jpe, my_D_proc, my_U_proc)

          do j = my_jps,my_jpe
             do i = my_ips,my_ipe
                my_c_1d(my_kps_2h:my_kpe_2h) => c0 (my_kps_2h:my_kpe_2h,i,j)
                my_k_1d(my_kps_2h:my_kpe_2h) => my_k3(my_kps_2h:my_kpe_2h,i,j)
                my_w_1d(my_kbs_1h:my_kbe_1h) => CB%w(my_kbs_1h:my_kbe_1h,i,j)

                ki = KT_RHS(my_c_1d,MY_GRID%dX3_p,MY_GRID%dX3_b,my_w_1d,my_k_1d, &
                     flux,my_kps,my_kpe,my_kbs,my_kbe)
                !
                !
                c0(my_kps:my_kpe,i,j) = ini_c(my_kps:my_kpe,i,j) + 0.5_rp*dt*ki(my_kps:my_kpe)
                new_c(my_kps:my_kpe,i,j) = new_c(my_kps:my_kpe,i,j) + dt*ki(my_kps:my_kpe) / 3.0_rp
             end do
          end do
          !
          call domain_swap_mass_points_2halo_first (c0(:,:,:), my_kps, my_kpe, &
               my_ips,my_ipe,my_jps,my_jpe, my_D_proc, my_U_proc)

          do j = my_jps,my_jpe
             do i = my_ips,my_ipe
                my_c_1d(my_kps_2h:my_kpe_2h) => c0 (my_kps_2h:my_kpe_2h,i,j)
                my_k_1d(my_kps_2h:my_kpe_2h) => my_k3(my_kps_2h:my_kpe_2h,i,j)
                my_w_1d(my_kbs_1h:my_kbe_1h) => CB%w(my_kbs_1h:my_kbe_1h,i,j)

                ki = KT_RHS(my_c_1d,MY_GRID%dX3_p,MY_GRID%dX3_b,my_w_1d,my_k_1d, &
                     flux,my_kps,my_kpe,my_kbs,my_kbe)
                !
                c0(my_kps:my_kpe,i,j) = ini_c(my_kps:my_kpe,i,j) + dt*ki(my_kps:my_kpe)
                new_c(my_kps:my_kpe,i,j) = new_c(my_kps:my_kpe,i,j) + dt*ki(my_kps:my_kpe) / 3.0_rp
             end do
          end do
          !
          call domain_swap_mass_points_2halo_first (c0(:,:,:), my_kps,my_kpe, &
               my_ips,my_ipe,my_jps,my_jpe, my_D_proc, my_U_proc)

          do j = my_jps,my_jpe
             do i = my_ips,my_ipe
                my_c_1d(my_kps_2h:my_kpe_2h) => c0 (my_kps_2h:my_kpe_2h,i,j)
                my_k_1d(my_kps_2h:my_kpe_2h) => my_k3(my_kps_2h:my_kpe_2h,i,j)
                my_w_1d(my_kbs_1h:my_kbe_1h) => CB%w(my_kbs_1h:my_kbe_1h,i,j)

                ki = KT_RHS(my_c_1d,MY_GRID%dX3_p,MY_GRID%dX3_b,my_w_1d,my_k_1d, &
                     flux,my_kps,my_kpe,my_kbs,my_kbe)
                !
                my_c(i,j,my_kps:my_kpe,ibin) = new_c(my_kps:my_kpe,i,j) + dt*ki(my_kps:my_kpe) / 6.0_rp
                !
                !***   Fluxes
                !
                area = MY_GRID%dX2_b(j)*MY_GRID%dX1_b(i) !/(MY_GRID%Hm1_p(j)*MY_GRID%Hm2_p(j))
                if(my_D_proc.eq.-1) then
                   my_D_flux    = my_D_flux + flux(1)*area*dt              ! total mass
                   my_acum(i,j,ibin) = my_acum(i,j,ibin) + abs(flux(1))*dt ! mass per unit ara (force positive sign)
                end if
                if(my_U_proc.eq.-1) then
                   my_U_flux = my_U_flux + flux(2) *area*dt
                end if
                !
             end do
          end do
          !
          !
       end select
       !
    end do
    !
    deallocate(ki )
    do ibin = 1,nbins
       call domain_swap_mass_points_2halo_z (my_c(:,:,:,ibin))
    end do
    !
    return
  end subroutine ADS_solve_along_z
  !
  !-----------------------------------
  !    subroutine ADS_solve_1D
  !-----------------------------------
  !
  !>   @brief
  !>   Solves a time step dt of the 1D ADS equation using the Kurganov scheme on a staggered grid
  !
  subroutine ADS_solve_1D(c,dxp,dxb,u,k,dt,bvalue,flux,bcode,ips,ipe,ibs,ibe,limiter,idime)
    implicit none
    !
    !>   @param c       list of scaled transported variable (concentration) at mass points
    !>   @param dxp     distance between mass points
    !>   @param dxb     distance bewteen boundary points (cell size)
    !>   @param u       scaled velocity at boundaries
    !>   @param k       scaled diffusion at mass points
    !>   @param dt      time increment
    !>   @param bvalue  boundary value
    !>   @param flux    flux at boundaries
    !>   @param bcode   boundary code: 0 do nothing (internal node in Domain Decomposition), -1 periodic (serial only), 1 Dirichlet, 2 free flow (i.e. inflow Dirichlet, outflow zero gradient)
    !>   @param ips     my starting mass point index
    !>   @param ipe     my end mass point index
    !>   @param ibs     my starting boundary index
    !>   @param ibe     my end boundary index
    !>   @param limiter flux limiter flag: MINMOD = 1, SUPERBEE = 2, OSPRE = 3
    !>   @param idime   dimension index (1,2,3); needed for swaping
    !
    integer(ip), intent(in)    :: ips
    integer(ip), intent(in)    :: ipe
    integer(ip), intent(in)    :: ibs
    integer(ip), intent(in)    :: ibe
    integer(ip), intent(in)    :: bcode(2)
    integer(ip), intent(in)    :: limiter
    integer(ip), intent(in)    :: idime
    real   (rp), intent(inout) :: c      (ips-2:ipe+2)
    real   (rp), intent(in)    :: dxp    (ips-2:ipe+2)
    real   (rp), intent(in)    :: dxb    (ibs-1:ibe+1)
    real   (rp), intent(in)    :: u      (ibs-1:ibe+1)
    real   (rp), intent(in)    :: k      (ips-2:ipe+2)
    real   (rp), intent(in)    :: dt
    real   (rp), intent(in)    :: bvalue (2)
    real   (rp), intent(inout) :: flux   (2)
    !*** Temporary work arrays
    !
    real(rp), allocatable :: c0 (:)
    real(rp), allocatable :: k1 (:)
    real(rp), allocatable :: k2 (:)
    real(rp), allocatable :: k3 (:)
    real(rp), allocatable :: k4 (:)
    !
    !*** Allocate memory
    !
    allocate(c0 (ips-2:ipe+2))
    allocate(k1 (ips  :ipe  ))
    allocate(k2 (ips  :ipe  ))
    allocate(k3 (ips  :ipe  ))
    allocate(k4 (ips  :ipe  ))

    ! Allocation of control block resources we will use in solve 1D
    allocate(CB%raw_c_r(ipe-ips+3))
    allocate(CB%raw_c_l(ipe-ips+3))
    allocate(CB%raw_r(ipe-ips+5))

    select case(limiter)
    case(1)
       !                         minmod
       CB%flux_limiter => r_minmod
    case(2)
       !                         superbee
       CB%flux_limiter => r_sbee
    case(3)
       !                         ospre
       CB%flux_limiter => r_ospre
    end select

    !
    c0 (:) = 0.0_rp
    k1 (:) = 0.0_rp
    k2 (:) = 0.0_rp
    k3 (:) = 0.0_rp
    k4 (:) = 0.0_rp
    !
    !
    !
    select case(idime)
    case (1)
       call domain_swap_mass_points_2halo_1Dx( c )
    case (2)
       call domain_swap_mass_points_2halo_1Dy( c )
    case (3)
       call domain_swap_mass_points_2halo_1Dz( c )
    end select
    !
    !*** Impose boundary conditions at boundary ibs (left)
    !
    if(bcode(1).eq.0) then
       !
       !*** Do nothing. This is the case of internal
       !*** boundaries in domain decomposition, where
       !*** concentration values at halos (i.e. fluxes) are
       !*** already set by swapping
       !
       continue
       !
    else if(bcode(1).eq.-1) then
       !
       !*** Periodic boundary conditions (serial only)
       !
       if(u(ibs).gt.0.0_rp) then
          c(ips-1) = c(ipe)
          c(ips-2) = c(ipe-1)
          c(ipe+1) = c(ips)
          c(ipe+2) = c(ips+1)
       end if
       !
    else if(bcode(1).eq.1) then
       !
       !*** Dirichlet
       !
       c(ips-1) = 2.0_rp*bvalue(1)-c(ips)
       c(ips-2) = c(ips-1)
       !
    else if(bcode(1).eq.2) then
       !
       !*** Free flow
       !
       if(u(ibs).gt.0.0_rp) then                ! inflow (dirichlet)
          c(ips-1) = 2.0_rp*bvalue(1)-c(ips)
          c(ips-2) = c(ips-1)
       else                                     ! outflow (newman, zero gradient)
          c(ips-1) = c(ips)
          c(ips-2) = c(ips-1)
       end if
    end if
    !
    !*** Impose boundary conditions at boundary ibe (right)
    !
    if(bcode(2).eq.0) then
       !
       !*** Do nothing. This is the case of internal
       !*** boundaries in domain decomposition, where
       !*** concentration values at halos (i.e. fluxes) are
       !*** already set by swapping
       !
       continue
       !
    else if(bcode(2).eq.-1) then
       !
       !*** Periodic boundary conditions (serial only)
       !
       if(u(ibe).lt.0.0_rp) then
          c(ips-1) = c(ipe)
          c(ips-2) = c(ipe-1)
          c(ipe+1) = c(ips)
          c(ipe+2) = c(ips+1)
       end if
       !
    else if(bcode(2).eq.1) then
       !
       !*** Dirichlet
       !
       c(ipe+1) = 2.0_rp*bvalue(2)-c(ipe)
       c(ipe+2) = c(ipe+1)
       !
    else if(bcode(2).eq.2) then
       !
       !*** Free flow
       !
       if(u(ibe).lt.0.0_rp) then                ! inflow (dirichlet)
          c(ipe+1) = 2.0_rp*bvalue(2)-c(ipe)
          c(ipe+2) = c(ipe+1)
       else                                     ! outflow (newman, zero gradient)
          c(ipe+1) = c(ipe)
          c(ipe+2) = c(ipe+1)
       end if
    end if
    !
    !*** Time integration in the interval (t,t+dt) using a RK4 Runge-Kutta method
    !
    c0(ips-2:ipe+2) = c(ips-2:ipe+2)                                        ! initial guess, including halos
    k1 = KT_RHS(c0,dxp,dxb,u,k,flux,ips,ipe,ibs,ibe)
    !
    !    Successive approaches. Note that swap is necessary to update predictor halo values
    !
    c0(ips:ipe) = c(ips:ipe) + 0.5_rp*dt*k1(ips:ipe)
    select case(idime)
    case (1)
       call domain_swap_mass_points_2halo_1Dx( c0 )
    case (2)
       call domain_swap_mass_points_2halo_1Dy( c0 )
    case (3)
       call domain_swap_mass_points_2halo_1Dz( c0 )
    end select
    k2 = KT_RHS(c0,dxp,dxb,u,k,flux,ips,ipe,ibs,ibe)
    !
    c0(ips:ipe) = c(ips:ipe) + 0.5_rp*dt*k2(ips:ipe)
    select case(idime)
    case (1)
       call domain_swap_mass_points_2halo_1Dx( c0 )
    case (2)
       call domain_swap_mass_points_2halo_1Dy( c0 )
    case (3)
       call domain_swap_mass_points_2halo_1Dz( c0 )
    end select
    k3 = KT_RHS(c0,dxp,dxb,u,k,flux,ips,ipe,ibs,ibe)
    !
    c0(ips:ipe) = c(ips:ipe) + dt*k3(ips:ipe)
    select case(idime)
    case (1)
       call domain_swap_mass_points_2halo_1Dx( c0 )
    case (2)
       call domain_swap_mass_points_2halo_1Dy( c0 )
    case (3)
       call domain_swap_mass_points_2halo_1Dz( c0 )
    end select
    k4 = KT_RHS(c0,dxp,dxb,u,k,flux,ips,ipe,ibs,ibe)
    !
    c(ips:ipe) = c(ips:ipe) + dt*(       k1(ips:ipe) + &
         2.0_rp*k2(ips:ipe) + &
         2.0_rp*k3(ips:ipe) + &
         k4(ips:ipe) ) /6.0_rp
    select case(idime)
    case (1)
       call domain_swap_mass_points_2halo_1Dx( c )
    case (2)
       call domain_swap_mass_points_2halo_1Dy( c )
    case (3)
       call domain_swap_mass_points_2halo_1Dz( c )
    end select
    !
    !*** Deallocate memory
    !
    deallocate(CB%raw_r )
    deallocate(CB%raw_c_r)
    deallocate(CB%raw_c_l)
    deallocate(k1 )
    deallocate(k2 )
    deallocate(k3 )
    deallocate(k4 )
    !
    return
  end subroutine ADS_solve_1D
  !
  !
  !
  function KT_RHS(c,dxp,dxb,u,k,flux,ips,ipe,ibs,ibe)
    !******************************************************************
    !*
    !*   Computes the RHS for the Kuganov-Tadmor (KT) scheme at mass points
    !*   Note that the RHS at ghost mass poins is not computed
    !*
    !******************************************************************
    implicit none
    integer(ip), intent(in)    :: ips,ipe,ibs,ibe
    real   (rp), intent(in)    :: c      (ips-2:ipe+2)
    real   (rp), intent(in)    :: dxp    (ips-2:ipe+2)   ! dxp(i) distance between point i and i+1
    real   (rp), intent(in)    :: dxb    (ibs-1:ibe+1)   ! dxb(i) size of the cell       i
    real   (rp), intent(in)    :: u      (ibs-1:ibe+1)   ! velocity at boundaries
    real   (rp), intent(in)    :: k      (ips-2:ipe+2)   ! diffusion at mass points
    !
    !    work arrays
    !
    real   (rp), intent(inout) :: flux   (2          )   ! Flux at boundaries
    real   (rp)                :: KT_RHS (ips  :ipe  )
    !
    integer(ip)          :: i
    real   (rp)          :: F_ml,F_mr,F_pl,F_pr,F_m,F_p,P_m,P_p
    real   (rp), pointer :: r(:)
    real   (rp), pointer :: c_r(:)
    real   (rp), pointer :: c_l(:)

    r(ips-2:ipe+2) => CB%raw_r(1:ipe-ips+5)
    c_r(ips-1:ipe+1) => CB%raw_c_r(1:ipe-ips+3)
    c_l(ips-1:ipe+1) => CB%raw_c_l(1:ipe-ips+3)
    !
    !*** Computes flux limiter function
    !
    r = CB%flux_limiter(c,ips,ipe)
    !
    !*** Computes c_r (right) and cl (left) at boundaries
    !*** (i.e. at i+1/2 and i-1/2 for each cell)
    !
    do i = ips-1,ipe+1
       c_r(i) = c(i  ) - 0.5_rp*r(i  )*(c(i+1)-c(i  ))
       c_l(i) = c(i-1) + 0.5_rp*r(i-1)*(c(i  )-c(i-1))
    end do
    !
    !*** Computes RHS at mass points
    !
    do i = ips,ipe
       !
       F_ml = c_l(i  )*u(i  )   ! left  advecive flux at i-1/2
       F_mr = c_r(i  )*u(i  )   ! right advecive flux at i-1/2
       F_pl = c_l(i+1)*u(i+1)   ! left  advecive flux at i+1/2
       F_pr = c_r(i+1)*u(i+1)   ! right advecive flux at i+1/2
       !
       F_m = 0.5_rp*(F_ml+F_mr) - 0.5_rp*abs(u(i  ))*(c_r(i  )-c_l(i  ))    ! F at i-1/2
       F_p = 0.5_rp*(F_pl+F_pr) - 0.5_rp*abs(u(i+1))*(c_r(i+1)-c_l(i+1))    ! F at i+1/2
       !
       P_m = 0.5_rp*(k(i  )+k(i-1))*(c(i  )-c(i-1))/dxp(i-1)                ! P at i-1/2
       P_p = 0.5_rp*(k(i+1)+k(i  ))*(c(i+1)-c(i  ))/dxp(i  )                ! P at i+1/2
       !
       KT_RHS(i) = -1.0_rp*(F_p-F_m)/dxb(i) + (P_p-P_m)/dxb(i)
       !
       if(i.eq.ips) flux(1)=F_m+P_m
       if(i.eq.ipe) flux(2)=F_p+P_p
    end do
    !
  end function KT_RHS
  !
  !
  !
  function r_minmod(c,ips,ipe)
    !******************************************************************
    !*
    !*   Returns the minmod limiter function at all mass points
    !*   (including the 2 ghost poins)
    !*
    !******************************************************************
    implicit none
    integer(ip), intent(in)    :: ips,ipe
    real   (rp), intent(in)    :: c       (ips-2:ipe+2)
    real   (rp)                :: r_minmod(ips-2:ipe+2)
    !
    integer(ip) :: i
    real   (rp) :: r
    !
    do i = ips-1,ipe+1
       if(abs(c(i+1)-c(i)).lt.epsilon) then
          r_minmod(i) = 1.0_rp
       else
          r           = (c(i)-c(i-1))/(c(i+1)-c(i))
          !        r           = r *dxp(i)/dxp(i-1)   ! non uniform
          r_minmod(i) = max(0.0_rp,min(1.0_rp,r))
       end if
    end do
    r_minmod(ips-2) = r_minmod(ips-1)
    r_minmod(ipe+2) = r_minmod(ipe+1)
    !
  end function r_minmod
  !
  !
  !
  function r_sbee(c,ips,ipe)
    !******************************************************************
    !*
    !*   Returns the superbee flux limiter function at all mass points
    !*   (including the 2 ghost poins)
    !*
    !******************************************************************
    implicit none
    integer(ip), intent(in)    :: ips,ipe
    real   (rp), intent(in)    :: c     (ips-2:ipe+2)
    real   (rp)                :: r_sbee(ips-2:ipe+2)
    !
    integer(ip) :: i
    real   (rp) :: r
    !
    do i = ips-1,ipe+1
       if(abs(c(i+1)-c(i)).lt.epsilon) then
          if(abs(c(i)-c(i-1)).lt.epsilon) then
             r_sbee(i) = 0.0_rp
          else
             r_sbee(i) = 2.0_rp
          end if
       else
          r         = (c(i)-c(i-1))/(c(i+1)-c(i))
          !          r         = r *dxp(i)/dxp(i-1)   ! non uniform

          r_sbee(i) = max(0.0_rp,min(2.0_rp*r,1.0_rp),min(r,2.0_rp))
       end if
    end do
    r_sbee(ips-2) = r_sbee(ips-1)
    r_sbee(ipe+2) = r_sbee(ipe+1)
    !
  end function r_sbee
  !
  !
  !
  function r_ospre(c,ips,ipe)
    !******************************************************************
    !*
    !*   Returns the ospre flux limiter function at all mass points
    !*   (including the 2 ghost poins)
    !*
    !******************************************************************
    implicit none
    integer(ip), intent(in)    :: ips,ipe
    real   (rp), intent(in)    :: c      (ips-2:ipe+2)
    real   (rp)                :: r_ospre(ips-2:ipe+2)
    !
    integer(ip) :: i
    real   (rp) :: r
    !
    do i = ips-1,ipe+1
       if(abs(c(i+1)-c(i)).lt.epsilon) then
          if(abs(c(i)-c(i-1)).lt.epsilon) then
             r_ospre(i) = 0.0_rp
          else
             r_ospre(i) = 1.5_rp
          end if
       else
          r          = (c(i)-c(i-1))/(c(i+1)-c(i))
          !       r          = r *dxp(i)/dxp(i-1)   ! non uniform

          r_ospre(i) = 1.5_rp*(r*r+r)/(r*r+r+1.0_rp)
       end if
    end do
    r_ospre(ips-2) = r_ospre(ips-1)
    r_ospre(ipe+2) = r_ospre(ipe+1)
    !
  end function r_ospre
  !
  !
  !
  subroutine freeflow( c, mydim )
    !******************************************************************
    !*
    !*   Apply free flow boundary conditions
    !*
    !******************************************************************
    implicit none
    real   (rp), intent(inout) :: c  (my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h,my_kps_2h:my_kpe_2h)
    integer(ip), intent(in)    :: mydim

    integer(ip)                :: i,j,k
    ! In X dimension
    if (mydim.eq.1) then
       if (my_W_proc.eq.-1) then
          do k = my_kps,my_kpe
             do j = my_jps,my_jpe
                if (CB%u(my_ibs,j,k).gt.0.0_rp) then         ! inflow (dirichlet)
                   c(my_ips-1,j,k) = 2.0_rp*CB%bvalue(1)-c(my_ips,j,k)
                else                                         ! outflow (newman, zero gradient)
                   c(my_ips-1,j,k) = c(my_ips,j,k)
                endif
                c(my_ips-2,j,k) = c(my_ips-1,j,k)

             end do
          end do
       end if
       if (my_E_proc.eq.-1) then
          do k = my_kps,my_kpe
             do j = my_jps,my_jpe
                if (CB%u(my_ibe,j,k).lt.0.0_rp) then
                   c(my_ipe+1,j,k) = 2.0_rp*CB%bvalue(2)-c(my_ipe,j,k)
                else
                   c(my_ipe+1,j,k) = c(my_ipe,j,k)
                endif
                c(my_ipe+2,j,k) = c(my_ipe+1,j,k)

             end do
          end do
       end if
    end if
    ! In Y dimension
    if (mydim.eq.2) then
       if (my_S_proc.eq.-1) then
          do k = my_kps,my_kpe
             do i = my_ips,my_ipe
                if (CB%v(my_jbs,i,k).gt.0.0_rp) then
                   c(i,my_jps-1,k) = 2.0_rp*CB%bvalue(1)-c(i,my_jps,k)
                else
                   c(i,my_jps-1,k) = c(i,my_jps,k)
                endif
                c(i,my_jps-2,k) = c(i,my_jps-1,k)

             end do
          end do
       end if
       if (my_N_proc.eq.-1) then
          do k = my_kps,my_kpe
             do i = my_ips,my_ipe
                if (CB%v(my_jbe,i,k).lt.0.0_rp) then
                   c(i,my_jpe+1,k) = 2.0_rp*CB%bvalue(2)-c(i,my_jpe,k)
                else
                   c(i,my_jpe+1,k) = c(i,my_jpe,k)
                endif
                c(i,my_jpe+2,k) = c(i,my_jpe+1,k)

             end do
          end do
       end if
    end if
    ! In Z dimension
    if (mydim.eq.3) then
       if (my_D_proc.eq.-1) then
          do j = my_jps,my_jpe
             do i = my_ips,my_ipe
                if (CB%w(my_kbs,i,j).gt.0.0_rp) then
                   c(i,j,my_kps-1) = 2.0_rp*CB%bvalue(1)-c(i,j,my_kps)
                else
                   c(i,j,my_kps-1) = c(i,j,my_kps)
                endif
                c(i,j,my_kps-2) = c(i,j,my_kps-1)

             end do
          end do
       end if
       if (my_U_proc.eq.-1) then
          do j = my_jps,my_jpe
             do i = my_ips,my_ipe
                if (CB%w(my_kbe,i,j).lt.0.0_rp) then
                   c(i,j,my_kpe+1) = 2.0_rp*CB%bvalue(2)-c(i,j,my_kpe)
                else
                   c(i,j,my_kpe+1) = c(i,j,my_kpe)
                endif
                c(i,j,my_kpe+2) = c(i,j,my_kpe+1)

             end do
          end do
       end if
    end if

  end subroutine freeflow
  !
  !
  !
  subroutine dirichlet( c, mydim )
    !******************************************************************
    !*
    !*   Apply Dirichlet boundary conditions
    !*
    !******************************************************************
    implicit none
    real   (rp), intent(inout) :: c  (my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h,my_kps_2h:my_kpe_2h)
    integer(ip), intent(in)    :: mydim

    integer(ip)                :: i,j,k
    ! In X dimension
    if (mydim.eq.1) then
       if (my_W_proc.eq.-1) then
          do k = my_kps,my_kpe
             do j = my_jps,my_jpe
                c(my_ips-1,j,k) = 2.0_rp*CB%bvalue(1)-c(my_ips,j,k)
                c(my_ips-2,j,k) = c(my_ips-1,j,k)
             end do
          end do
       end if
       if (my_E_proc.eq.-1) then
          do k = my_kps,my_kpe
             do j = my_jps,my_jpe
                c(my_ipe+1,j,k) = 2.0_rp*CB%bvalue(2)-c(my_ipe,j,k)
                c(my_ipe+2,j,k) = c(my_ipe+1,j,k)
             end do
          end do
       end if
    end if
    ! In Y dimension
    if (mydim.eq.2) then
       if (my_S_proc.eq.-1) then
          do k = my_kps,my_kpe
             do i = my_ips,my_ipe
                c(i,my_jps-1,k) = 2.0_rp*CB%bvalue(1)-c(i,my_jps,k)
                c(i,my_jps-2,k) = c(i,my_jps-1,k)
             end do
          end do
       end if
       if (my_N_proc.eq.-1) then
          do k = my_kps,my_kpe
             do i = my_ips,my_ipe
                c(i,my_jpe+1,k) = 2.0_rp*CB%bvalue(2)-c(i,my_jpe,k)
                c(i,my_jpe+2,k) = c(i,my_jpe+1,k)
             end do
          end do
       end if
    end if
    ! In Z dimension
    if (mydim.eq.3) then
       if (my_D_proc.eq.-1) then
          do j = my_jps,my_jpe
             do i = my_ips,my_ipe
                c(i,j,my_kps-1) = 2.0_rp*CB%bvalue(1)-c(i,j,my_kps)
                c(i,j,my_kps-2) = c(i,j,my_kps-1)
             end do
          end do
       end if
       if (my_U_proc.eq.-1) then
          do j = my_jps,my_jpe
             do i = my_ips,my_ipe
                c(i,j,my_kpe+1) = 2.0_rp*CB%bvalue(2)-c(i,j,my_kpe)
                c(i,j,my_kpe+2) = c(i,j,my_kpe+1)
             end do
          end do
       end if
    end if

  end subroutine dirichlet
  !
  !
  !
  subroutine periodic( c, mydim  )
    !******************************************************************
    !*
    !*   Apply Periodic boundary conditions, JUST FOR SERIAL
    !*
    !******************************************************************
    implicit none
    real   (rp), intent(inout) :: c  (my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h,my_kps_2h:my_kpe_2h)
    integer(ip), intent(in)    :: mydim

    integer(ip)               :: i,j,k
    ! In X dimension
    if (mydim.eq.1) then
       do k = my_kps,my_kpe
          do j = my_jps,my_jpe
             c(my_ips-1,j,k) = c(my_ips,j,k)
             c(my_ips-2,j,k) = c(my_ips-1,j,k)
             c(my_ipe+1,j,k) = c(my_ipe,j,k)
             c(my_ipe+2,j,k) = c(my_ipe+1,j,k)
          end do
       end do
    end if
    ! In Y dimension
    if (mydim.eq.2) then
       do k = my_kps,my_kpe
          do i = my_ips,my_ipe
             c(i,my_jps-1,k) = c(i,my_jps,k)
             c(i,my_jps-2,k) = c(i,my_jps-1,k)
             c(i,my_jpe+1,k) = c(i,my_jpe,k)
             c(i,my_jpe+2,k) = c(i,my_jpe+1,k)
          end do
       end do
    end if
    ! In Z dimension
    if (mydim.eq.3) then
       do j = my_jps,my_jpe
          do i = my_ips,my_ipe
             c(i,j,my_kps-1) = c(i,j,my_kps)
             c(i,j,my_kps-2) = c(i,j,my_kps-1)
             c(i,j,my_kpe+1) = c(i,j,my_kpe)
             c(i,j,my_kpe+2) = c(i,j,my_kpe+1)
          end do
       end do
    end if

  end subroutine periodic
  !
  !
  !
END MODULE ADS
