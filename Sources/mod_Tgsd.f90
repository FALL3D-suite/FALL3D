!***************************************************************
!>
!> Module for procedures related to SetTgsd
!> @author
!> Arnau Folch
!>
!***************************************************************
MODULE Tgsd
  use KindType
  use InpOut
  use Parallel
  implicit none
  save
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: tgsd_get_bigaussian_params
  PUBLIC :: tgsd_read_inp_granulometry
  PUBLIC :: tgsd_bcast_inp_granulometry
  PUBLIC :: tgsd_setfrac
  PUBLIC :: tgsd_write_tgsd_granulometry
  PUBLIC :: tgsd_deallocate_TGSD
  !
  !    LIST OF PRIVATE ROUTINES IN THE MODULE
  !
  PRIVATE :: tgsd_M0_fi_gaussian
  PRIVATE :: tgsd_M0_fi_weibull
  PRIVATE :: tgsd_cumulative_weibull
  PRIVATE :: fer
  PRIVATE :: alngam
  PRIVATE :: alnorm
  PRIVATE :: gammad
  !
CONTAINS
  !
  !-----------------------------------------
  !    subroutine tgsd_get_bigaussian_params
  !-----------------------------------------
  !
  !>   @brief
  !>   Sets Bigaussian distribution parameters from column height and
  !>   magma viscosity using Costa et al. (2016)
  !
  subroutine tgsd_get_bigaussian_params(MY_TGSD,h,visco,MY_ERR)
    implicit none
    !
    !>   @param MY_TGSD   tgsd configuration parameters
    !>   @param h         column haight (in m a.v.l.)
    !>   @param visco     magma viscosity (in Pa.s)
    !>   @param MY_ERR    error handler
    !
    type(TGSD_PARAMS), intent(INOUT) :: MY_TGSD
    real(rp),          intent(IN   ) :: h
    real(rp),          intent(IN   ) :: visco
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    real(rp) :: hm
    real(rp) :: a1,b1,a2,b2,a3,b3,a4,b4
    real(rp) :: s1,s2,m1,m2,p
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'tgsd_get_bigaussian_params'
    MY_ERR%message = ' '
    !
    if(MY_TGSD%type_dist.ne.'BIGAUSSIAN') then
       MY_ERR%flag    = 1
       MY_ERR%message = 'bigaussian distirbution needed'
       return
    end if
    !
    hm = h/1000.0_rp  ! m --> km a.v.l.
    a1 = 0.67_rp
    b1 = 0.07_rp
    a2 = 0.96_rp
    b2 = 0.20_rp
    a3 = 1.62_rp
    b3 = 0.66_rp
    a4 = 1.61_rp
    b4 = 0.31_rp
    !
    !*** Coarse subpopulation
    !
    s1 = a1 + b1*hm
    m1 = a2 + b2*hm - 3.0_rp*s1
    !
    !*** Fine subpopulation
    !
    s2 = 1.46_rp
    m2 = a3*((log10(visco))**b3) + m1
    !
    p = a4*exp(-b4*log10(visco))
    !
    !*** Fill structure
    !
    MY_TGSD%fimean (1) = m1
    MY_TGSD%fimean (2) = m2
    MY_TGSD%fidisp (1) = s1
    MY_TGSD%fidisp (2) = s2
    MY_TGSD%pweight(1) = p
    MY_TGSD%pweight(2) = 1.0_rp - p
    !
    return
  end subroutine tgsd_get_bigaussian_params
  !
  !-----------------------------------------
  !    subroutine tgsd_read_inp_granulometry
  !-----------------------------------------
  !
  !>   @brief
  !>   Reads a specie_TGSD block form the input file
  !
  subroutine tgsd_read_inp_granulometry(MY_FILES,MY_TGSD,SPE_CODE,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_TGSD   tgsd configuration parameters
    !>   @param SPE_CODE  specie code
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),    intent(INOUT) :: MY_FILES
    type(TGSD_PARAMS),  intent(INOUT) :: MY_TGSD
    integer(ip),        intent(IN   ) :: SPE_CODE
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR

    !
    real(rp)              :: file_version
    real(rp)              :: work(6), visco, h_m
    character(len=s_file) :: file_inp, swork(2), sblock
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'tgsd_read_inp_granulometry'
    MY_ERR%message = ' '
    !
    file_inp = MY_FILES%file_inp
    sblock   = SPE_BLOCK(SPE_CODE)
    !
    !*** Input file version
    !
    call inpout_get_rea (file_inp, 'CODE','VERSION', file_version, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) then
       return
    elseif(file_version < MIN_REQUIRED_VERSION) then
       MY_ERR%flag    = 1
       MY_ERR%message ='Input file version deprecated. Please use 8.x file version'
       return
    end if
    !
    !*** Reads TGSD block
    !
    call inpout_get_cha (file_inp, sblock,'DISTRIBUTION', MY_TGSD%type_dist, 1, MY_ERR, .true.)
    if(MY_ERR%flag.ne.0) return
    !
    select case(MY_TGSD%type_dist)
    case('GAUSSIAN')
       !
       MY_TGSD%ng         = 1
       MY_TGSD%pweight(1) = 1.0_rp
       MY_TGSD%pweight(2) = 0.0_rp
       !
       call inpout_get_rea (file_inp, sblock,'IF_GAUSSIAN', work, 2, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       MY_TGSD%fimean(1) = work(1)
       MY_TGSD%fidisp(1) = work(2)
       !
    case('BIGAUSSIAN')
       !
       MY_TGSD%ng = 2
       !
       call inpout_get_rea (file_inp, sblock,'IF_BIGAUSSIAN', work, 5, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       MY_TGSD%fimean(1)  = work(1)
       MY_TGSD%fimean(2)  = work(2)
       MY_TGSD%fidisp(1)  = work(3)
       MY_TGSD%fidisp(2)  = work(4)
       MY_TGSD%pweight(1) = work(5)
       MY_TGSD%pweight(2) = 1.0_rp - work(5)
       !
    case('WEIBULL')
       !
       MY_TGSD%ng         = 1
       MY_TGSD%pweight(1) = 1.0_rp
       MY_TGSD%pweight(2) = 0.0_rp
       !
       call inpout_get_rea (file_inp, sblock,'IF_WEIBULL', work, 2, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       MY_TGSD%fimean(1) = work(1) ! Note: use fimean/fidisp to store the Weibull parameters scale/shape
       MY_TGSD%fidisp(1) = work(2)
       !
    case('BIWEIBULL')
       !
       MY_TGSD%ng = 2
       !
       call inpout_get_rea (file_inp, sblock,'IF_BIWEIBULL', work, 5, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       MY_TGSD%fimean(1)  = work(1)  ! Note: use fimean/fidisp to store the Weibull parameters scale/shape
       MY_TGSD%fimean(2)  = work(2)
       MY_TGSD%fidisp(1)  = work(3)
       MY_TGSD%fidisp(2)  = work(4)
       MY_TGSD%pweight(1) = work(5)
       MY_TGSD%pweight(2) = 1.0_rp - work(5)
       !
    case('ESTIMATE')
       !
       MY_TGSD%type_dist = 'BIGAUSSIAN'
       MY_TGSD%ng = 2
       !
       call inpout_get_rea (file_inp, sblock,'IF_ESTIMATE', work, 2, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       visco = work(1)
       h_m   = work(2)
       call tgsd_get_bigaussian_params(MY_TGSD,h_m,visco,MY_ERR)
       if(MY_ERR%flag.ne.0) return
       !
    case('CUSTOM')
       !
       call inpout_get_cha (file_inp, sblock,'IF_CUSTOM',  swork, 2, MY_ERR, .false.)
       MY_FILES%file_tgsd = swork(2)
       return
       !
    case default
       !
       MY_ERR%flag    = 1
       MY_ERR%message = 'Type of particle TGSD distribution not implemented'
       return
       !
    end select
    !
    call inpout_get_int (file_inp, sblock,'NUMBER_OF_BINS', MY_TGSD%nbins, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    !
    call inpout_get_rea (file_inp, sblock,'FI_RANGE', work, 2, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    MY_TGSD%fimin = work(1)
    MY_TGSD%fimax = work(2)
    if(MY_TGSD%fimin.gt.MY_TGSD%fimax) then
       MY_TGSD%fimin = work(2)
       MY_TGSD%fimax = work(1)
    end if
    !
    call inpout_get_rea (file_inp, sblock,'DENSITY_RANGE', work, 2, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    MY_TGSD%rhomin = work(1)
    MY_TGSD%rhomax = work(2)
    !
    call inpout_get_rea (file_inp, sblock,'SPHERICITY_RANGE', work, 2, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    MY_TGSD%sphemin  = work(1)
    MY_TGSD%sphemax  = work(2)
    !
    !*** Allocates memory
    !
    allocate(MY_TGSD%fc   (MY_TGSD%nbins))
    allocate(MY_TGSD%rhop (MY_TGSD%nbins))
    allocate(MY_TGSD%diam (MY_TGSD%nbins))
    allocate(MY_TGSD%fi   (MY_TGSD%nbins))
    allocate(MY_TGSD%sphe (MY_TGSD%nbins))
    allocate(MY_TGSD%psi  (MY_TGSD%nbins))
    !
    return
  end subroutine tgsd_read_inp_granulometry
  !
  !-----------------------------------------
  !    subroutine tgsd_bcast_inp_granulometry
  !-----------------------------------------
  !
  !>   @brief
  !>   Master broadcasts granulometry block from input file
  !
  subroutine tgsd_bcast_inp_granulometry(MY_TGSD,MY_ERR)
    implicit none
    !
    !>   @param MY_TGSD   tgsd configuration parameters
    !>   @param MY_ERR    error handler
    !
    type(TGSD_PARAMS), intent(INOUT) :: MY_TGSD
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'tgsd_bcast_inp_granulometry'
    MY_ERR%message = ' '
    !
    call parallel_bcast(MY_TGSD%nbins    ,1,0)
    call parallel_bcast(MY_TGSD%ng       ,1,0)
    call parallel_bcast(MY_TGSD%fimean   ,2,0)
    call parallel_bcast(MY_TGSD%fidisp   ,2,0)
    call parallel_bcast(MY_TGSD%pweight  ,2,0)
    call parallel_bcast(MY_TGSD%fimin    ,1,0)
    call parallel_bcast(MY_TGSD%fimax    ,1,0)
    call parallel_bcast(MY_TGSD%rhomin   ,1,0)
    call parallel_bcast(MY_TGSD%rhomax   ,1,0)
    call parallel_bcast(MY_TGSD%sphemin  ,1,0)
    call parallel_bcast(MY_TGSD%sphemax  ,1,0)
    call parallel_bcast(MY_TGSD%type_dist,1,0)
    !
    !*** Allocates memory
    !
    if(.not.master) then
       allocate(MY_TGSD%fc   (MY_TGSD%nbins))
       allocate(MY_TGSD%rhop (MY_TGSD%nbins))
       allocate(MY_TGSD%diam (MY_TGSD%nbins))
       allocate(MY_TGSD%fi   (MY_TGSD%nbins))
       allocate(MY_TGSD%sphe (MY_TGSD%nbins))
       allocate(MY_TGSD%psi  (MY_TGSD%nbins))
    end if
    !
    return
  end subroutine tgsd_bcast_inp_granulometry
  !
  !----------------------------
  !    subroutine tgsd_setfrac
  !----------------------------
  !
  !>   @brief
  !>   Builds a granulometrtic distribution
  !
  subroutine tgsd_setfrac(MY_TGSD,MY_ERR)
    implicit none
    !
    !>   @param MY_TGSD   tgsd configuration parameters
    !>   @param MY_ERR    error handler
    !
    type(TGSD_PARAMS), intent(INOUT) :: MY_TGSD
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    integer(ip)           :: ic,ig,nc,ng
    real(rp)              :: deltafi,fimin0,fimax0
    real(rp), allocatable :: work(:,:)

    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'tgsd_setfrac'
    MY_ERR%message = ' '
    !
    nc = MY_TGSD%nbins
    ng = MY_TGSD%ng
    allocate(work(nc,ng))
    !
    !*** Compute particle diameter in mm
    !
    deltafi = (MY_TGSD%fimax-MY_TGSD%fimin)/(nc-1)
    do ic = 1,nc
       MY_TGSD%fi  (ic) = MY_TGSD%fimin + (ic-1)*deltafi
       MY_TGSD%diam(ic) = 2.0_rp**(-MY_TGSD%fi(ic))
    end do
    !
    !*** Density rhop(ic) and sphericity sphe(nc)
    !
    fimin0 = -1.0_rp ! as in Bonadonna and Phillips (2003)
    fimax0 =  6.0_rp ! as in Bonadonna and Phillips (2003)
    !
    do ic = 1,nc
       MY_TGSD%rhop(ic) = max(min((MY_TGSD%rhomax-MY_TGSD%rhomin)*(MY_TGSD%fi(ic)-fimin0)/ &
            (fimax0-fimin0)+MY_TGSD%rhomin,MY_TGSD%rhomax),MY_TGSD%rhomin)
       MY_TGSD%sphe(ic)  = (MY_TGSD%sphemax-MY_TGSD%sphemin)*(MY_TGSD%fi(ic)-MY_TGSD%fimin)/ &
            (MY_TGSD%fimax-MY_TGSD%fimin) + MY_TGSD%sphemin
    end do
    !
    !*** Builds the distribution
    !
    select case(MY_TGSD%type_dist)
    case('GAUSSIAN','BIGAUSSIAN')
       !
       !  Fraction fc(ic) (gaussian or bigaussian normalized)
       !
       do ig = 1,ng
          do ic = 1,nc
             call tgsd_M0_fi_gaussian(work(ic,ig),MY_TGSD%fi(ic),MY_TGSD%fimin,MY_TGSD%fimax,deltafi, &
                  MY_TGSD%fimean(ig),MY_TGSD%fidisp(ig))
          end do
       end do
       !
    case('WEIBULL','BIWEIBULL')
       !
       !  Note that variable fimean stores the Weibull scale factor (Lambda)
       !  and variable fidisp stores the Weibull shape factor
       !
       do ig = 1,ng
          do ic = 1,nc
             call tgsd_M0_fi_weibull(work(ic,ig),MY_TGSD%fi(ic),MY_TGSD%fimin,MY_TGSD%fimax,deltafi, &
                  MY_TGSD%fimean(ig),MY_TGSD%fidisp(ig))
          end do
       end do
       !
    end select
    !
    !*** Computes fc and blends
    !
    do ic = 1,nc
       MY_TGSD%fc(ic) = 0.0_rp
       do ig = 1,ng
          MY_TGSD%fc(ic) = MY_TGSD%fc(ic) + MY_TGSD%pweight(ig)*work(ic,ig)
       end do
    end do
    !
    !*** Finally, computes m63 (fraction of mass less than 64um or fi=4)
    !
    MY_TGSD%m63 = 0.0_rp
    do ic = 1,nc
       if(MY_TGSD%fi(ic).ge.4) MY_TGSD%m63 = MY_TGSD%m63 +  MY_TGSD%fc(ic)
    end do
    !
    return
  end subroutine tgsd_setfrac
  !
  !--------------------------------------------
  !    subroutine tgsd_write_tgsd_granulometry
  !--------------------------------------------
  !
  !>   @brief
  !>   Writes the tgsd file
  !
  subroutine tgsd_write_tgsd_granulometry(MY_FILES,MY_TGSD,SPE_NAME,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_TGSD   tgsd configuration parameters
    !>   @param SPE_NAME  specie name
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),       intent(IN   ) :: MY_FILES
    type(TGSD_PARAMS),     intent(INOUT) :: MY_TGSD
    character(len=s_name), intent(IN   ) :: SPE_NAME
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    integer(ip)           :: ic
    character(len=s_file) :: file_tgsd
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'tgsd_write_tgsd_granulometry'
    MY_ERR%message = ' '
    !
    file_tgsd = TRIM(MY_FILES%file_tgsd) //'.'//TRIM(SPE_NAME)   ! name.tgsd.specie
    !
    !*** writes the file
    !
    open(99,file=TRIM(file_tgsd) ,status='unknown',err=100)
    !
    write(99,'(i5)') MY_TGSD%nbins
    do ic = 1,MY_TGSD%nbins
       write(99,10) MY_TGSD%diam(ic),MY_TGSD%rhop(ic),MY_TGSD%sphe(ic),MY_TGSD%fc(ic)
    end do
10  format(f10.6,1x,f8.1,1x,f7.3,1x,e16.9)
    !
    close(99)
    !
    !*** Finally, write to log file
    !
    write(MY_FILES%lulog,15) TRIM(SPE_NAME),TRIM(MY_TGSD%type_dist)
15  format(/,&
         '  SPECIE ',a,/,&
         '    Distribution ',a)
    !
    select case(MY_TGSD%type_dist)
    case('GAUSSIAN')
       write(MY_FILES%lulog,20) MY_TGSD%fimean(1),MY_TGSD%fidisp(1)
20     format(&
            '    Gaussian parameters '  ,/, &
            '    Fi          : ',f5.2,/, &
            '    Sigma       : ',f5.2)
       !
    case('BIGAUSSIAN')
       write(MY_FILES%lulog,30) MY_TGSD%fimean(1), MY_TGSD%fimean(2), &
            MY_TGSD%fidisp(1), MY_TGSD%fidisp(2), &
            MY_TGSD%pweight(1),MY_TGSD%pweight(2)
30     format(&
            '    Bigaussian parameters '  ,/, &
            '    Fi 1        : ',f5.2,/, &
            '    Fi 2        : ',f5.2,/, &
            '    Sigma 1     : ',f5.2,/, &
            '    Sigma 2     : ',f5.2,/, &
            '    weight 1    : ',f5.2,/, &
            '    weight 2    : ',f5.2)
       !
    case('WEIBULL')
       write(MY_FILES%lulog,40) MY_TGSD%fimean(1),MY_TGSD%fidisp(1)
40     format(&
            '    Weibull parameters '  ,/, &
            '    Fi scale    : ',f5.2,/, &
            '    W shape     : ',f5.2)
       !
    case('BIWEIBULL')
       write(MY_FILES%lulog,50) MY_TGSD%fimean(1), MY_TGSD%fimean(2), &
            MY_TGSD%fidisp(1), MY_TGSD%fidisp(2), &
            MY_TGSD%pweight(1),MY_TGSD%pweight(2)
50     format(&
            '    Biweibull parameters '  ,/, &
            '    Fi scale 1  : ',f5.2,/, &
            '    Fi scale 2  : ',f5.2,/, &
            '    W shape 1   : ',f5.2,/, &
            '    W shape 2   : ',f5.2,/, &
            '    weight 1    : ',f5.2,/, &
            '    weight 2    : ',f5.2)
       !
    case default
       !
    end select
    !
    return
    !
100 MY_ERR%flag = 1
    MY_ERR%message ='Error opening Granulometry file '//TRIM(file_tgsd)
    !
    return
  end subroutine tgsd_write_tgsd_granulometry
  !
  !--------------------------------------------
  !    subroutine tgsd_deallocate_TGSD
  !--------------------------------------------
  !
  !>   @brief
  !>   Deallocates MY_TGSD
  !
  subroutine tgsd_deallocate_TGSD(MY_TGSD,MY_ERR)
    implicit none
    !
    !>   @param MY_TGSD   tgsd configuration parameters
    !>   @param MY_ERR    error handler
    !
    type(TGSD_PARAMS),     intent(INOUT) :: MY_TGSD
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'tgsd_deallocate_TGSD'
    MY_ERR%message = ' '
    !
    deallocate(MY_TGSD%fc  )
    deallocate(MY_TGSD%rhop)
    deallocate(MY_TGSD%diam)
    deallocate(MY_TGSD%fi  )
    deallocate(MY_TGSD%sphe)
    deallocate(MY_TGSD%psi )
    !
    return
    end subroutine tgsd_deallocate_TGSD
  !
  !
  !    PRIVATE ROUTINES
  !
  !
  subroutine tgsd_M0_fi_gaussian(fc,fi,fimin,fimax,deltafi,fimean,fidisp)
    !************************************************************************
    !*
    !*  Computes the mass for a given value of fi between fimin and fimax
    !*  (mass between fi-deltafi/2 and fi+deltafi/2) for a Gaussian distribution
    !*  The ultimate values fimin and fimax are extended to +- infinity
    !*  to ensure mass conservation.
    !*
    !*************************************************************************
    implicit none
    real(rp), intent(in)  :: fimin,fimax,deltafi,fimean,fidisp
    real(rp), intent(out) :: fc
    !
    real(rp) :: fi,fi2
    !
    !***  lower limit
    !
    if(fi <= fimin) then
       fi2 = fimin + 0.5_rp*deltafi
       fi2 = (fi2-fimean)/fidisp
       fc  = fer(fi2)
       return
    end if
    !
    !***  upper limit
    !
    if(fi.eq.fimax) then
       fi2 = fimax - 0.5_rp*deltafi
       fi2 = (fi2-fimean)/fidisp
       fc  = max(0.0_rp,1.0_rp-fer(fi2))
       return
    end if
    !
    !***  other values
    !
    fi2 = fi + 0.5_rp*deltafi
    fi2 = (fi2-fimean)/fidisp
    fc = fer(fi2)
    !
    fi2 = fi - 0.5_rp*deltafi
    fi2 = (fi2-fimean)/fidisp
    fc = max(0.0_rp,fc-fer(fi2))
    !
    return
  end subroutine tgsd_M0_fi_gaussian
  !
  !
  !
  subroutine tgsd_M0_fi_weibull(fc,fi,fimin,fimax,deltafi,scale,shp)
    !************************************************************************
    !*
    !*  Computes the mass for a given value of fi between fimin and fimax
    !*  (mass between fi-deltafi/2 and fi+deltafi/2) for a Weibull distribution.
    !*  The ultimate values fimin and fimax are extended to +- infinity
    !*  to ensure mass conservation.
    !*
    !*************************************************************************
    implicit none
    real(rp), intent(in)  :: fimin,fimax,deltafi,scale,shp
    real(rp), intent(out) :: fc
    !
    real(rp) :: fi
    !
    !***  lower limit
    !
    if(fi <= fimin) then
       fc  = tgsd_cumulative_weibull(fimin+0.5_rp*deltafi,scale,shp)
       return
    end if
    !
    !***  upper limit
    !
    if(fi >= fimax) then
       fc  = max(0.0_rp,1.0_rp-tgsd_cumulative_weibull(fimax-0.5_rp*deltafi,scale,shp))
       return
    end if
    !
    !***  other values
    !
    fc = max(0.0_rp,tgsd_cumulative_weibull(fi+0.5_rp*deltafi,scale,shp) - &
         tgsd_cumulative_weibull(fi-0.5_rp*deltafi,scale,shp))
    return
  end subroutine tgsd_M0_fi_weibull
  !
  !*****************************************************************************
  !*
  !*  fer(x) = Intergral of the Normalized Gaussian distribution
  !*  between -Infinity and x
  !*
  !*  Two versions are implemented:
  !*
  !*  a) Function fer(x) is compatible with Fortran 90 (pre F95)
  !*  b) Function fer(x) is based on the intrinsic function erf() appeared in F95
  !*
  !*  In this release, version a) is delivered for compatibility with F90
  !
  !*****************************************************************************
!!$
!!$  real(RP) function fer(x) result(fer)
!!$    !***********************************************************************
!!$    !*
!!$    !*    Computes the area below the normal distribution between
!!$    !*    -infinity and x
!!$    !*
!!$    !*   NOTE: the intrinsic function erf(x) appeared in Fortran 95
!!$    !*
!!$    !***********************************************************************
!!$    use KindType
!!$    implicit none
!!$    real(rp) ::  x
!!$    fer = 0.5_rp*(1.0_rp+erf(x/sqrt(2.0_rp)))
!!$    return
!!$  end function fer
!!$

  real(rp) function fer(x)
    !************************************************************************
    !*
    !*    Computes the area below the typified normal distribution between
    !*    -infinity and x
    !*
    !************************************************************************
    implicit none
    logical  :: go_on
    real(rp) ::  x
    real(rp) ::  t1,t2,dt
    !
    !***  Computes integral between t=0 and t=abs(x)
    !
    fer = 0.0_rp
    dt  = abs(x)/10000.0_rp
    !
    t1 = 0.0_rp
    t2 = dt
    go_on = .true.
    do while(go_on)
       fer = fer + 0.5_rp*(exp(-t1*t1/2.0_rp)+exp(-t2*t2/2.0_rp))*dt
       t1 = t2
       t2 = t2 + dt
       if(t2.ge.abs(x)) go_on = .false.
    end do
    fer = fer/sqrt(8.0_rp*atan(1.0_rp))
    !
    if(x.ge.0.0_rp) then
       fer = 0.5_rp + fer
    else
       fer = 0.5_rp - fer
    end if
    !
    return
  end function fer
  !
  !
  !
  real(rp) function tgsd_cumulative_weibull(phi,scale,shp) result(cdf)
    !************************************************************************
    !*
    !* Computes the area below the Weibull distribution in the interval
    !* between -infinity and phi
    !*
    !************************************************************************
    real(rp), intent(in) :: phi
    real(rp), intent(in) :: scale  ! Scale parameter (in terms of phi)
    real(rp), intent(in) :: shp    ! Shape parameter
    real(rp) :: x
    integer(ip) :: ierr
    !
    x = 2.0_rp**((scale-phi)*shp)/shp ! x=1/n*(2^(-phi)/2^(-scale))^n [where n=shp]
    cdf=max(0.0_rp,1.0_rp-gammad(x,1.0_rp+1.0_rp/shp,ierr)/exp(alngam(1.0_rp+1.0_rp/shp,ierr)))
    !
  end function tgsd_cumulative_weibull
  !
  !
  !
  real(rp) function alngam(xvalue,ifault)
    !
    ! ALNGAM computes the logarithm of the gamma function.
    !
    !  Modified:
    !
    !    13 January 2008
    !
    !  Author:
    !
    !    Original FORTRAN77 version by Allan Macleod.
    !    FORTRAN90 version by John Burkardt.
    !
    !  Reference:
    !
    !    Allan Macleod,
    !    Algorithm AS 245,
    !    A Robust and Reliable Algorithm for the Logarithm of the Gamma Function
    !    Applied Statistics,
    !    Volume 38, Number 2, 1989, pages 397-402.
    !
    !  Parameters:
    !
    !    Input, real (rp) XVALUE, the argument of the Gamma function.
    !
    !    Output, integer (ip) IFAULT, error flag.
    !    0, no error occurred.
    !    1, XVALUE is less than or equal to 0.
    !    2, XVALUE is too big.
    !
    !    Output, real(rp) ALNGAM, the logarithm of the gamma function of X.
    !
    implicit none
    real (rp), parameter :: alr2pi = 0.918938533204673_rp
    integer (ip) ifault
    real (rp), dimension ( 9 ) :: r1 = (/ &
         -2.66685511495_rp, &
         -24.4387534237_rp, &
         -21.9698958928_rp, &
         11.1667541262_rp, &
         3.13060547623_rp, &
         0.607771387771_rp, &
         11.9400905721_rp, &
         31.4690115749_rp, &
         15.2346874070_rp /)
    real (rp), dimension ( 9 ) :: r2 = (/ &
         -78.3359299449_rp, &
         -142.046296688_rp, &
         137.519416416_rp, &
         78.6994924154_rp, &
         4.16438922228_rp, &
         47.0668766060_rp, &
         313.399215894_rp, &
         263.505074721_rp, &
         43.3400022514_rp /)
    real (rp), dimension ( 9 ) :: r3 = (/ &
         -2.12159572323E+05_rp, &
         2.30661510616E+05_rp, &
         2.74647644705E+04_rp, &
         -4.02621119975E+04_rp, &
         -2.29660729780E+03_rp, &
         -1.16328495004E+05_rp, &
         -1.46025937511E+05_rp, &
         -2.42357409629E+04_rp, &
         -5.70691009324E+02_rp /)
    real (rp), dimension ( 5 ) :: r4 = (/ &
         0.279195317918525_rp, &
         0.4917317610505968_rp, &
         0.0692910599291889_rp, &
         3.350343815022304_rp, &
         6.012459259764103_rp /)
    real (rp) x
    real (rp) x1
    real (rp) x2
    real (rp), parameter :: xlge = 5.10E+05_rp
    real (rp), parameter :: xlgst = 1.0E+30_rp
    real (rp) xvalue
    real (rp) y

    x = xvalue
    alngam = 0.0_rp
    !
    !  Check the input.
    !
    if ( xlgst <= x ) then
       ifault = 2
       return
    end if

    if ( x <= 0.0_rp ) then
       ifault = 1
       return
    end if

    ifault = 0
    !
    !  Calculation for 0 < X < 0.5 and 0.5 <= X < 1.5 combined.
    !
    if ( x < 1.5_rp ) then

       if ( x < 0.5_rp ) then

          alngam = - log ( x )
          y = x + 1.0_rp
          !
          !  Test whether X < machine epsilon.
          !
          if ( y == 1.0_rp ) then
             return
          end if

       else

          alngam = 0.0_rp
          y = x
          x = ( x - 0.5_rp ) - 0.5_rp

       end if

       alngam = alngam + x * (((( &
            r1(5)   * y &
            + r1(4) ) * y &
            + r1(3) ) * y &
            + r1(2) ) * y &
            + r1(1) ) / (((( &
            y &
            + r1(9) ) * y &
            + r1(8) ) * y &
            + r1(7) ) * y &
            + r1(6) )

       return

    end if
    !
    !  Calculation for 1.5 <= X < 4.0.
    !
    if ( x < 4.0_rp ) then

       y = ( x - 1.0_rp ) - 1.0_rp

       alngam = y * (((( &
            r2(5)   * x &
            + r2(4) ) * x &
            + r2(3) ) * x &
            + r2(2) ) * x &
            + r2(1) ) / (((( &
            x &
            + r2(9) ) * x &
            + r2(8) ) * x &
            + r2(7) ) * x &
            + r2(6) )
       !
       !  Calculation for 4.0 <= X < 12.0.
       !
    else if ( x < 12.0_rp ) then

       alngam = (((( &
            r3(5)   * x &
            + r3(4) ) * x &
            + r3(3) ) * x &
            + r3(2) ) * x &
            + r3(1) ) / (((( &
            x &
            + r3(9) ) * x &
            + r3(8) ) * x &
            + r3(7) ) * x &
            + r3(6) )
       !
       !  Calculation for 12.0 <= X.
       !
    else

       y = log ( x )
       alngam = x * ( y - 1.0_rp ) - 0.5_rp * y + alr2pi

       if ( x <= xlge ) then

          x1 = 1.0_rp / x
          x2 = x1 * x1

          alngam = alngam + x1 * ( ( &
               r4(3)   * &
               x2 + r4(2) ) * &
               x2 + r4(1) ) / ( ( &
               x2 + r4(5) ) * &
               x2 + r4(4) )

       end if

    end if

    return
  end function alngam
  !
  !
  !
  real(rp) function alnorm(x,upper)
    !
    ! ALNORM computes the cumulative density of the standard normal distribution
    !
    !  Modified:
    !
    !    13 January 2008
    !
    !  Author:
    !
    !    Original FORTRAN77 version by David Hill.
    !    FORTRAN90 version by John Burkardt.
    !
    !  Reference:
    !
    !    David Hill,
    !    Algorithm AS 66:
    !    The Normal Integral,
    !    Applied Statistics,
    !    Volume 22, Number 3, 1973, pages 424-427.
    !
    !  Parameters:
    !
    !    Input, real (rp) X, is one endpoint of the semi-infinite interval
    !    over which the integration takes place.
    !
    !    Input, logical UPPER, determines whether the upper or lower
    !    interval is to be integrated:
    !    .TRUE.  => integrate from X to + Infinity;
    !    .FALSE. => integrate from - Infinity to X.
    !
    !    Output, real (rp) ALNORM, the integral of the standard normal
    !    distribution over the desired interval.
    !
    implicit none
    real (rp), intent(in) :: x
    logical,   intent(in) :: upper
    real (rp), parameter :: a1 = 5.75885480458_rp
    real (rp), parameter :: a2 = 2.62433121679_rp
    real (rp), parameter :: a3 = 5.92885724438_rp
    real (rp), parameter :: b1 = -29.8213557807_rp
    real (rp), parameter :: b2 = 48.6959930692_rp
    real (rp), parameter :: c1 = -0.000000038052_rp
    real (rp), parameter :: c2 = 0.000398064794_rp
    real (rp), parameter :: c3 = -0.151679116635_rp
    real (rp), parameter :: c4 = 4.8385912808_rp
    real (rp), parameter :: c5 = 0.742380924027_rp
    real (rp), parameter :: c6 = 3.99019417011_rp
    real (rp), parameter :: con = 1.28_rp
    real (rp), parameter :: d1 = 1.00000615302_rp
    real (rp), parameter :: d2 = 1.98615381364_rp
    real (rp), parameter :: d3 = 5.29330324926_rp
    real (rp), parameter :: d4 = -15.1508972451_rp
    real (rp), parameter :: d5 = 30.789933034_rp
    real (rp), parameter :: ltone = 7.0_rp
    real (rp), parameter :: p = 0.398942280444_rp
    real (rp), parameter :: q = 0.39990348504_rp
    real (rp), parameter :: r = 0.398942280385_rp
    real (rp), parameter :: utzero = 18.66_rp
    real (rp) :: y
    real (rp) :: z
    logical   :: up

    up = upper
    z = x

    if ( z < 0.0_rp ) then
       up = .not. up
       z = - z
    end if

    if ( ltone < z .and. ( ( .not. up ) .or. utzero < z ) ) then

       if ( up ) then
          alnorm = 0.0_rp
       else
          alnorm = 1.0_rp
       end if

       return

    end if

    y = 0.5_rp * z * z

    if ( z <= con ) then

       alnorm = 0.5_rp - z * ( p - q * y &
            / ( y + a1 + b1 &
            / ( y + a2 + b2 &
            / ( y + a3 ))))

    else

       alnorm = r * exp ( - y ) &
            / ( z + c1 + d1 &
            / ( z + c2 + d2 &
            / ( z + c3 + d3 &
            / ( z + c4 + d4 &
            / ( z + c5 + d5 &
            / ( z + c6 ))))))

    end if

    if ( .not. up ) then
       alnorm = 1.0_rp - alnorm
    end if

    return
  end function alnorm
  !
  !
  !
  real(rp) function gammad(x,p,ifault)
    !
    ! GAMMAD computes the Incomplete Gamma Integral
    !
    !  Auxiliary functions:
    !
    !    ALNGAM = logarithm of the gamma function,
    !    ALNORM = algorithm AS66
    !
    !  Modified:
    !
    !    20 January 2008
    !
    !  Author:
    !
    !    Original FORTRAN77 version by B Shea.
    !    FORTRAN90 version by John Burkardt.
    !
    !  Reference:
    !
    !    B Shea,
    !    Algorithm AS 239:
    !    Chi-squared and Incomplete Gamma Integral,
    !    Applied Statistics,
    !    Volume 37, Number 3, 1988, pages 466-473.
    !
    !  Parameters:
    !
    !    Input, real (rp) X, P, the parameters of the incomplete
    !    gamma ratio.  0 <= X, and 0 < P.
    !
    !    Output, integer (ip) IFAULT, error flag.
    !    0, no error.
    !    1, X < 0 or P <= 0.
    !
    !    Output, real (rp) GAMMAD, the value of the incomplete
    !    Gamma integral.
    !
    implicit none
    real (rp), intent(in) :: x,p
    integer (ip), intent(out) :: ifault
    real (rp) :: a
    real (rp) :: an
    real (rp) :: arg
    real (rp) :: b
    real (rp) :: c
    real (rp), parameter :: elimit = -88.0E0_rp
    real (rp), parameter :: oflo = 1.0E+37_rp
    real (rp), parameter :: plimit = 1000.0E+00_rp
    real (rp) :: pn1
    real (rp) :: pn2
    real (rp) :: pn3
    real (rp) :: pn4
    real (rp) :: pn5
    real (rp) :: pn6
    real (rp) :: rn
    real (rp), parameter :: tol = 1.0E-14_rp
    logical :: upper
    real (rp), parameter :: xbig = 1.0E+08_rp

    gammad = 0.0_rp
    !
    !  Check the input.
    !
    if ( x < 0.0_rp ) then
       ifault = 1
       return
    end if

    if ( p <= 0.0_rp ) then
       ifault = 1
       return
    end if

    ifault = 0

    if ( x == 0.0_rp ) then
       gammad = 0.0_rp
       return
    end if
    !
    !  If P is large, use a normal approximation.
    !
    if ( plimit < p ) then

       pn1 = 3.0_rp * sqrt ( p ) * ( ( x / p )**( 1.0_rp / 3.0_rp ) &
            + 1.0_rp / ( 9.0_rp * p ) - 1.0_rp )

       upper = .false.
       gammad = alnorm ( pn1, upper )
       return

    end if
    !
    !  If X is large set GAMMAD = 1.
    !
    if ( xbig < x ) then
       gammad = 1.0_rp
       return
    end if
    !
    !  Use Pearson's series expansion.
    !  (Note that P is not large enough to force overflow in ALNGAM).
    !  No need to test IFAULT on exit since P > 0.
    !
    if ( x <= 1.0_rp .or. x < p ) then

       arg = p * log ( x ) - x - alngam ( p + 1.0_rp, ifault )
       c = 1.0_rp
       gammad = 1.0_rp
       a = p

       do

          a = a + 1.0_rp
          c = c * x / a
          gammad = gammad + c

          if ( c <= tol ) then
             exit
          end if

       end do

       arg = arg + log ( gammad )

       if ( elimit <= arg ) then
          gammad = exp ( arg )
       else
          gammad = 0.0_rp
       end if
       !
       !  Use a continued fraction expansion.
       !
    else

       arg = p * log ( x ) - x - alngam ( p, ifault )
       a = 1.0_rp - p
       b = a + x + 1.0_rp
       c = 0.0_rp
       pn1 = 1.0_rp
       pn2 = x
       pn3 = x + 1.0_rp
       pn4 = x * b
       gammad = pn3 / pn4

       do

          a = a + 1.0_rp
          b = b + 2.0_rp
          c = c + 1.0_rp
          an = a * c
          pn5 = b * pn3 - an * pn1
          pn6 = b * pn4 - an * pn2

          if ( pn6 /= 0.0_rp ) then

             rn = pn5 / pn6

             if ( abs ( gammad - rn ) <= min ( tol, tol * rn ) ) then
                exit
             end if

             gammad = rn

          end if

          pn1 = pn3
          pn2 = pn4
          pn3 = pn5
          pn4 = pn6
          !
          !  Re-scale terms in continued fraction if terms are large.
          !
          if ( oflo <= abs ( pn5 ) ) then
             pn1 = pn1 / oflo
             pn2 = pn2 / oflo
             pn3 = pn3 / oflo
             pn4 = pn4 / oflo
          end if

       end do

       arg = arg + log ( gammad )

       if ( elimit <= arg ) then
          gammad = 1.0_rp - exp ( arg )
       else
          gammad = 1.0_rp
       end if

    end if

    return
  end function gammad
  !
  !
  !
END MODULE Tgsd
