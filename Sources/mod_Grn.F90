!***************************************************************
!>
!> Module for procedures related to tracer granulometry
!> @author
!> Arnau Folch
!>
!***************************************************************
MODULE Grn
  use KindType
  use InpOut
  use Parallel
  use Phys
  use Ensemble
  implicit none
  save
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: grn_read_inp_species
  PUBLIC :: grn_bcast_species
  PUBLIC :: grn_read_inp_aggregation
  PUBLIC :: grn_bcast_aggregation
  PUBLIC :: grn_get_granulometry
  PUBLIC :: grn_bcast_granulometry
  PUBLIC :: grn_deallocate_GRN
  PUBLIC :: grn_write_granulometry
  PUBLIC :: grn_save_granulometry
  PUBLIC :: grn_read_effective_granulometry
  PUBLIC :: grn_bcast_effective_granulometry
  PUBLIC :: grn_read_tgsd
  !
  !    LIST OF PRIVATE ROUTINES IN THE MODULE
  !
  PRIVATE :: grn_get_effective_bins
  !
CONTAINS
  !
  !-----------------------------------------
  !    subroutine  grn_read_inp_species
  !-----------------------------------------
  !
  !>   @brief
  !>   Reads and sets MY_SPE (definition of categories and specie definition)
  !
  subroutine  grn_read_inp_species(MY_FILES, MY_SPE, MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_SPE    list of parameters defining species and categories
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),      intent(INOUT) :: MY_FILES
    type(SPECIES_PARAMS), intent(INOUT) :: MY_SPE
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    character(len=s_file)  :: file_inp
    character(len=s_name)  :: word
    integer(ip)            :: ispe
    real(rp)               :: file_version,mf
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grn_read_inp_species'
    MY_ERR%message = ' '
    !
    file_inp = MY_FILES%file_inp
    !
    !*** Input file version
    !
    call inpout_get_rea (file_inp, 'CODE','VERSION', file_version, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) then
       return
    elseif(file_version < MIN_REQUIRED_VERSION) then
       MY_ERR%flag    = 1
       MY_ERR%source  = 'grn_read_inp_species'
       MY_ERR%message = 'Input file version deprecated. Please use 8.x file version '
       return
    end if
    !
    !*** First, determine the number of species
    !
    call inpout_get_cha (file_inp, 'SPECIES','TEPHRA', word, 1, MY_ERR, .true.)
    if((MY_ERR%flag.eq.0).and.(word.eq.'ON')) MY_SPE%nspe = MY_SPE%nspe + 1
    call inpout_get_cha (file_inp, 'SPECIES','DUST', word, 1, MY_ERR, .true.)
    if((MY_ERR%flag.eq.0).and.(word.eq.'ON')) MY_SPE%nspe = MY_SPE%nspe + 1
    call inpout_get_cha (file_inp, 'SPECIES','H2O', word, 1, MY_ERR, .true.)
    if((MY_ERR%flag.eq.0).and.(word.eq.'ON')) MY_SPE%nspe = MY_SPE%nspe + 1
    call inpout_get_cha (file_inp, 'SPECIES','SO2', word, 1, MY_ERR, .true.)
    if((MY_ERR%flag.eq.0).and.(word.eq.'ON')) MY_SPE%nspe = MY_SPE%nspe + 1
    call inpout_get_cha (file_inp, 'SPECIES','CS134', word, 1, MY_ERR, .true.)
    if((MY_ERR%flag.eq.0).and.(word.eq.'ON')) MY_SPE%nspe = MY_SPE%nspe + 1
    call inpout_get_cha (file_inp, 'SPECIES','CS137', word, 1, MY_ERR, .true.)
    if((MY_ERR%flag.eq.0).and.(word.eq.'ON')) MY_SPE%nspe = MY_SPE%nspe + 1
    call inpout_get_cha (file_inp, 'SPECIES','I131', word, 1, MY_ERR, .true.)
    if((MY_ERR%flag.eq.0).and.(word.eq.'ON')) MY_SPE%nspe = MY_SPE%nspe + 1
    call inpout_get_cha (file_inp, 'SPECIES','SR90', word, 1, MY_ERR, .true.)
    if((MY_ERR%flag.eq.0).and.(word.eq.'ON')) MY_SPE%nspe = MY_SPE%nspe + 1
    call inpout_get_cha (file_inp, 'SPECIES','Y90', word, 1, MY_ERR, .true.)
    if((MY_ERR%flag.eq.0).and.(word.eq.'ON')) MY_SPE%nspe = MY_SPE%nspe + 1
    !
    !*** Check errors
    !
    if(MY_SPE%nspe.eq.0) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'No specie has been defined '
       return
    end if
    !
    !*** Allocates and set values
    !
    allocate(MY_SPE%code    (MY_SPE%nspe))
    allocate(MY_SPE%category(MY_SPE%nspe))
    allocate(MY_SPE%name    (MY_SPE%nspe))
    allocate(MY_SPE%mf      (MY_SPE%nspe))
    !
    ispe = 0
    call inpout_get_cha (file_inp, 'SPECIES','TEPHRA', word, 1, MY_ERR, .true.)
    if((MY_ERR%flag.eq.0).and.(word.eq.'ON')) then
       ispe = ispe + 1
       MY_SPE%category(ispe) = CAT_PARTICLE
       MY_SPE%code    (ispe) = SPE_TEPHRA
       MY_SPE%name    (ispe) = 'tephra'
       MY_SPE%mf      (ispe) = 1.0_rp
    end if
    !
    call inpout_get_cha (file_inp, 'SPECIES','DUST', word, 1, MY_ERR, .true.)
    if((MY_ERR%flag.eq.0).and.(word.eq.'ON')) then
       ispe = ispe + 1
       MY_SPE%category(ispe) = CAT_PARTICLE
       MY_SPE%code    (ispe) = SPE_DUST
       MY_SPE%name    (ispe) = 'dust'
       MY_SPE%mf      (ispe) = 1.0_rp
    end if
    !
    call inpout_get_cha (file_inp, 'SPECIES','H2O', word, 1, MY_ERR, .true.)
    if((MY_ERR%flag.eq.0).and.(word.eq.'ON')) then
       ispe = ispe + 1
       MY_SPE%category(ispe) = CAT_AEROSOL
       MY_SPE%code    (ispe) = SPE_H2O
       MY_SPE%name    (ispe) = 'H2O'
       !
       call inpout_get_rea (file_inp, 'SPECIES','H2O', MY_SPE%mf(ispe), 1, MY_ERR)
       if(MY_ERR%flag.ne.0) then
          MY_SPE%mf(ispe) = 0.0_rp
       else
          MY_SPE%mf(ispe) = MY_SPE%mf(ispe)*1e-2_rp  !  from % to [0,1]
       end if
    end if
    !
    call inpout_get_cha (file_inp, 'SPECIES','SO2', word, 1, MY_ERR, .true.)
    if((MY_ERR%flag.eq.0).and.(word.eq.'ON')) then
       ispe = ispe + 1
       MY_SPE%category(ispe) = CAT_AEROSOL
       MY_SPE%code    (ispe) = SPE_SO2
       MY_SPE%name    (ispe) = 'SO2'
       !
       call inpout_get_rea (file_inp, 'SPECIES','SO2', MY_SPE%mf(ispe), 1, MY_ERR)
       if(MY_ERR%flag.ne.0) then
          MY_SPE%mf(ispe) = 0.0_rp
       else
          MY_SPE%mf(ispe) = MY_SPE%mf(ispe)*1e-2_rp  !  from % to [0,1]
       end if
    end if
    !
    call inpout_get_cha (file_inp, 'SPECIES','CS134', word, 1, MY_ERR, .true.)
    if((MY_ERR%flag.eq.0).and.(word.eq.'ON')) then
       ispe = ispe + 1
       MY_SPE%category(ispe) = CAT_RADIONUCLIDE
       MY_SPE%code    (ispe) = SPE_CS134
       MY_SPE%name    (ispe) = 'Cs134'
       !
       call inpout_get_rea (file_inp, 'SPECIES','CS134', MY_SPE%mf(ispe), 1, MY_ERR)
       if(MY_ERR%flag.ne.0) then
          MY_ERR%flag    = 1
          MY_ERR%message = 'Mass fraction for CS134 not specified '
          return
       else
          MY_SPE%mf(ispe) = MY_SPE%mf(ispe)*1e-2_rp  !  from % to [0,1]
       end if
    end if
    !
    call inpout_get_cha (file_inp, 'SPECIES','CS137', word, 1, MY_ERR, .true.)
    if((MY_ERR%flag.eq.0).and.(word.eq.'ON')) then
       ispe = ispe + 1
       MY_SPE%category(ispe) = CAT_RADIONUCLIDE
       MY_SPE%code    (ispe) = SPE_CS137
       MY_SPE%name    (ispe) = 'Cs137'
       !
       call inpout_get_rea (file_inp, 'SPECIES','CS137', MY_SPE%mf(ispe), 1, MY_ERR)
       if(MY_ERR%flag.ne.0) then
          MY_ERR%flag    = 1
          MY_ERR%message = 'Mass fraction for CS137 not specified '
          return
       else
          MY_SPE%mf(ispe) = MY_SPE%mf(ispe)*1e-2_rp  !  from % to [0,1]
       end if
    end if
    !
    call inpout_get_cha (file_inp, 'SPECIES','I131', word, 1, MY_ERR, .true.)
    if((MY_ERR%flag.eq.0).and.(word.eq.'ON')) then
       ispe = ispe + 1
       MY_SPE%category(ispe) = CAT_RADIONUCLIDE
       MY_SPE%code    (ispe) = SPE_I131
       MY_SPE%name    (ispe) = 'I131'
       !
       call inpout_get_rea (file_inp, 'SPECIES','I131', MY_SPE%mf(ispe), 1, MY_ERR)
       if(MY_ERR%flag.ne.0) then
          MY_ERR%flag    = 1
          MY_ERR%message = 'Mass fraction for I131 not specified '
          return
       else
          MY_SPE%mf(ispe) = MY_SPE%mf(ispe)*1e-2_rp  !  from % to [0,1]
       end if
    end if
    !
    call inpout_get_cha (file_inp, 'SPECIES','SR90', word, 1, MY_ERR, .true.)
    if((MY_ERR%flag.eq.0).and.(word.eq.'ON')) then
       ispe = ispe + 1
       MY_SPE%category(ispe) = CAT_RADIONUCLIDE
       MY_SPE%code    (ispe) = SPE_SR90
       MY_SPE%name    (ispe) = 'Sr90'
       !
       call inpout_get_rea (file_inp, 'SPECIES','SR90', MY_SPE%mf(ispe), 1, MY_ERR)
       if(MY_ERR%flag.ne.0) then
          MY_ERR%flag    = 1
          MY_ERR%message = 'Mass fraction for SR90 not specified '
          return
       else
          MY_SPE%mf(ispe) = MY_SPE%mf(ispe)*1e-2_rp  !  from % to [0,1]
       end if
    end if
    !
    call inpout_get_cha (file_inp, 'SPECIES','Y90', word, 1, MY_ERR, .true.)
    if((MY_ERR%flag.eq.0).and.(word.eq.'ON')) then
       ispe = ispe + 1
       MY_SPE%category(ispe) = CAT_RADIONUCLIDE
       MY_SPE%code    (ispe) = SPE_Y90
       MY_SPE%name    (ispe) = 'Y90'
       !
       call inpout_get_rea (file_inp, 'SPECIES','Y90', MY_SPE%mf(ispe), 1, MY_ERR)
       if(MY_ERR%flag.ne.0) then
          MY_ERR%flag    = 1
          MY_ERR%message = 'Mass fraction for Y90 not specified '
          return
       else
          MY_SPE%mf(ispe) = MY_SPE%mf(ispe)*1e-2_rp  !  from % to [0,1]
       end if
    end if
    !
    !*** Check for errors and incompatibilities
    !
    do ispe = 1,MY_SPE%nspe
       if(MY_SPE%code    (ispe).eq.SPE_TEPHRA      ) MY_SPE%exists_tephra       = .true.
       if(MY_SPE%code    (ispe).eq.SPE_DUST        ) MY_SPE%exists_dust         = .true.
       if(MY_SPE%category(ispe).eq.CAT_RADIONUCLIDE) MY_SPE%exists_radionuclide = .true.
       if(MY_SPE%category(ispe).eq.CAT_AEROSOL     ) MY_SPE%exists_aerosol      = .true.
    end do
    !
    if(MY_SPE%exists_dust) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'Sorry, dust not funtional yet. Expected for v8.1  '
       return
    end if
    !
    if(MY_SPE%exists_tephra.and.MY_SPE%exists_dust) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'Sorry, tephra and dust are incompatible  '
       return
    end if
    !
    if(MY_SPE%exists_tephra.and.MY_SPE%exists_radionuclide) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'Sorry, tephra and radionuclides are incompatible  '
       return
    end if
    !
    if(MY_SPE%exists_dust.and.MY_SPE%exists_radionuclide) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'Sorry, dust and radionuclides are incompatible  '
       return
    end if
    !
    if(MY_SPE%exists_aerosol.and.MY_SPE%exists_dust) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'Sorry, aerosols and dust are incompatible  '
       return
    end if
    !
    if(MY_SPE%exists_aerosol.and.MY_SPE%exists_radionuclide) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'Sorry, aerosols and radionuclides are incompatible  '
       return
    end if
    !
    if(MY_SPE%exists_radionuclide) then
       mf = 0.0_rp
       do ispe = 1,MY_SPE%nspe
          if(MY_SPE%category(ispe).eq.CAT_RADIONUCLIDE) then
             mf = mf + MY_SPE%mf(ispe)
          end if
       end do
       !
       if(abs(mf-1.0_rp).gt.1e-4_rp) then
         MY_ERR%flag   = 1
         MY_ERR%message = 'Mass fraction of radionuclides sum 1  '
         return
       end if
    end if
    !
    if(MY_SPE%exists_aerosol) then
       !
       if(MY_SPE%exists_tephra) then   ! magmatic aerosols
          !
          do ispe = 1,MY_SPE%nspe
            if((MY_SPE%code(ispe).eq.SPE_H2O).and.(MY_SPE%mf(ispe).eq.0.0_rp)) then
               MY_ERR%flag   = 1
               MY_ERR%message = ' Magmatic H2O percentage not specifyed'
               return
            end if
            if((MY_SPE%code(ispe).eq.SPE_SO2).and.(MY_SPE%mf(ispe).eq.0.0_rp)) then
               MY_ERR%flag   = 1
               MY_ERR%message = ' Magmatic SO2 percentage not specifyed'
               return
            end if
         end do
         !         non magmatic aerosols
       else
         !
         mf = 0.0_rp
         do ispe = 1,MY_SPE%nspe
            if(MY_SPE%category(ispe).eq.CAT_AEROSOL) then
               mf = mf + MY_SPE%mf(ispe)
            end if
         end do
         !
         if(abs(mf-1.0_rp).gt.1e-4_rp) then
            MY_ERR%flag   = 1
            MY_ERR%message = 'Mass fraction of non-magmatic aerosols must sum 1  '
            return
         end if
         !
       end if
       !
    end if
    !
    return
    end subroutine grn_read_inp_species
  !
  !-----------------------------------------
  !    subroutine grn_bcast_species
  !-----------------------------------------
  !
  !>   @brief
  !>   Master broadcasts a species structure
  !
  subroutine grn_bcast_species(MY_SPE,MY_ERR)
    implicit none
    !
    !>   @param MY_SPE    list of parameters defining species and categories
    !>   @param MY_ERR    error handler
    !
    type(SPECIES_PARAMS), intent(INOUT) :: MY_SPE
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    integer(ip)           :: i
    character(len=s_name) :: swork
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grn_bcast_species'
    MY_ERR%message = ' '
    !
    call parallel_bcast(MY_SPE%exists_tephra      ,1,0)
    call parallel_bcast(MY_SPE%exists_dust        ,1,0)
    call parallel_bcast(MY_SPE%exists_radionuclide,1,0)
    call parallel_bcast(MY_SPE%exists_aerosol     ,1,0)
    call parallel_bcast(MY_SPE%nspe,               1,0)
    !
    !*** Memory allocation
    !
    if(.not.master_model) then
       allocate(MY_SPE%code    (MY_SPE%nspe))
       allocate(MY_SPE%category(MY_SPE%nspe))
       allocate(MY_SPE%name    (MY_SPE%nspe))
       allocate(MY_SPE%mf      (MY_SPE%nspe))
    end if
    !
    call parallel_bcast(MY_SPE%code    ,MY_SPE%nspe,0)
    call parallel_bcast(MY_SPE%category,MY_SPE%nspe,0)
    call parallel_bcast(MY_SPE%mf      ,MY_SPE%nspe,0)
    !
    do i = 1,MY_SPE%nspe
       swork = MY_SPE%name(i)
       call parallel_bcast(swork,1,0)
       MY_SPE%name(i) = swork
    end do
    !
    return
  end subroutine grn_bcast_species
  !
  !-----------------------------------------
  !    subroutine grn_read_inp_aggregation
  !-----------------------------------------
  !
  !>   @brief
  !>   Reads and MY_AGR (variables related to particle aggregation)
  !
  subroutine  grn_read_inp_aggregation(MY_FILES, MY_AGR, MY_ENS, MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_AGR    list of parameters defining an aggregation model
    !>   @param MY_ENS    list of ensemble parameters
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),      intent(IN   ) :: MY_FILES
    type(AGR_PARAMS),     intent(INOUT) :: MY_AGR
    type(ENS_PARAMS),     intent(IN   ) :: MY_ENS
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    integer(ip), parameter :: nmaxbin = 100
    !
    integer(ip)            :: npar,i,j
    real(rp)               :: file_version
    real(rp)               :: work(nmaxbin)
    character(len=s_file)  :: file_inp
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grn_read_inp_aggregation'
    MY_ERR%message = ' '
    !
    file_inp = MY_FILES%file_inp
    !
    !*** Input file version
    !
    call inpout_get_rea (file_inp, 'CODE','VERSION', file_version, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) then
       return
    elseif(file_version < MIN_REQUIRED_VERSION) then
       MY_ERR%flag    = 1
       MY_ERR%source  = 'grn_read_inp_aggregation'
       MY_ERR%message = 'Input file version deprecated. Please use 8.x file version '
       return
    end if
    !
    !*** Starts reading
    !
    call inpout_get_cha (file_inp, 'PARTICLE_AGGREGATION','AGGREGATION_MODEL', MY_AGR%aggregation_model, 1, MY_ERR, .true.)
    !
    select case(MY_AGR%aggregation_model)
    case('PERCENTAGE','CORNELL')
       call inpout_get_int(file_inp,'PARTICLE_AGGREGATION','NUMBER_OF_AGGREGATE_BINS',MY_AGR%nbins_aggr, 1, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       MY_AGR%nbins_aggr  = min(MY_AGR%nbins_aggr,nmaxbin)
       MY_AGR%aggregation = .true.
    case('COSTA')
       MY_AGR%nbins_aggr  = 1
       MY_AGR%aggregation = .true.
    case('NONE')
       MY_AGR%nbins_aggr  = 0
       MY_AGR%aggregation = .false.
    case default
       MY_ERR%flag    = 1
       MY_ERR%message = 'Incorrect aggregation model'
       return
    end select
    !
    if(MY_AGR%aggregation) then
       !
       !*** Allocates
       !
       allocate(MY_AGR%diam      (MY_AGR%nbins_aggr))
       allocate(MY_AGR%rho       (MY_AGR%nbins_aggr))
       allocate(MY_AGR%percentage(MY_AGR%nbins_aggr))
       !
       call inpout_get_npar(file_inp, 'PARTICLE_AGGREGATION','DIAMETER_AGGREGATES_(MIC)', npar, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       call inpout_get_rea (file_inp, 'PARTICLE_AGGREGATION','DIAMETER_AGGREGATES_(MIC)', work, npar, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       do i = 1,MY_AGR%nbins_aggr
          j = min(i,npar)
          MY_AGR%diam(i) = work(j)
       end do
       MY_AGR%diam_aggr = work(1)  ! aggregate diameter (1 class)
       !
       call inpout_get_npar(file_inp, 'PARTICLE_AGGREGATION','DENSITY_AGGREGATES_(KGM3)', npar, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       call inpout_get_rea (file_inp, 'PARTICLE_AGGREGATION','DENSITY_AGGREGATES_(KGM3)', work, npar, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       do i = 1,MY_AGR%nbins_aggr
          j = min(i,npar)
          MY_AGR%rho(i) = work(j)
       end do
       !
       !*** If necessary, perturbate aggregate properties in ensemble runs
       !
       if(nens.gt.1) then
          do i = 1,MY_AGR%nbins_aggr
             MY_AGR%diam(i) = ensemble_perturbate_variable( ID_DIAMETER_AGGREGATES, &
                                                            MY_AGR%diam(i), MY_ENS )
             MY_AGR%rho (i) = ensemble_perturbate_variable( ID_DENSITY_AGGREGATES, &
                                                            MY_AGR%rho (i), MY_ENS )
          end do
       end if
       !
       !*** Finally, convert diameter from microns to m
       !
       do i = 1,MY_AGR%nbins_aggr
          MY_AGR%diam(i) = MY_AGR%diam(i)*1e-6_rp  ! microns to m
       end do
       MY_AGR%diam_aggr = MY_AGR%diam_aggr*1e-6_rp
       !   
       select case(MY_AGR%aggregation_model)
       case('PERCENTAGE')
          !
          call inpout_get_npar(file_inp, 'PARTICLE_AGGREGATION','PERCENTAGE_(%)', npar, MY_ERR)
          if(MY_ERR%flag.ne.0) return
          call inpout_get_rea (file_inp, 'PARTICLE_AGGREGATION','PERCENTAGE_(%)', work, npar, MY_ERR)
          if(MY_ERR%flag.ne.0) return
          do i = 1,MY_AGR%nbins_aggr
             j = min(i,npar)
             MY_AGR%percentage(i) = work(j)
             MY_AGR%percentage(i) = MY_AGR%percentage(i)*1e-2_rp   ! from % to [0,1]
          end do
          if(sum(MY_AGR%percentage(1:MY_AGR%nbins_aggr)).gt.1.0_rp) then
              MY_ERR%flag    = 1
              MY_ERR%message = 'Total percentage of aggregates higher than 100%'
              return
          end if
          !
       case('COSTA')
          !
          call inpout_get_rea (file_inp, 'PARTICLE_AGGREGATION','FRACTAL_EXPONENT', MY_AGR%Dfo, 1, MY_ERR)
          if(MY_ERR%flag.ne.0) return
          !
       end select
       !
       call inpout_get_rea (file_inp, 'PARTICLE_AGGREGATION','VSET_FACTOR', MY_AGR%vset_fac, 1, MY_ERR)
       if(MY_ERR%flag.ne.0) MY_AGR%vset_fac = 1.0_rp
       !
    end if
    !
    return
  end subroutine grn_read_inp_aggregation
  !
  !-----------------------------------------
  !    subroutine grn_bcast_aggregation
  !-----------------------------------------
  !
  !>   @brief
  !>   Master broadcasts aggregation structure
  !
  subroutine grn_bcast_aggregation(MY_AGR,MY_ERR)
    implicit none
    !
    !>   @param MY_AGR    list of parameters defining an aggregation model
    !>   @param MY_ERR    error handler
    !
    type(AGR_PARAMS),     intent(INOUT) :: MY_AGR
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grn_bcast_aggregation'
    MY_ERR%message = ' '
    !
    call parallel_bcast(MY_AGR%nbins_aggr       ,1,0)
    call parallel_bcast(MY_AGR%aggregation      ,1,0)
    call parallel_bcast(MY_AGR%aggregation_model,1,0)
    !
    if(MY_AGR%aggregation) then
       !
       if(.not.master_model) then
          allocate(MY_AGR%diam      (MY_AGR%nbins_aggr))
          allocate(MY_AGR%rho       (MY_AGR%nbins_aggr))
          allocate(MY_AGR%percentage(MY_AGR%nbins_aggr))
       end if
       !
       call parallel_bcast(MY_AGR%diam      ,MY_AGR%nbins_aggr,0)
       call parallel_bcast(MY_AGR%rho       ,MY_AGR%nbins_aggr,0)
       call parallel_bcast(MY_AGR%percentage,MY_AGR%nbins_aggr,0)
       !
       call parallel_bcast(MY_AGR%diam_aggr,1,0)
       call parallel_bcast(MY_AGR%Dfo      ,1,0)
       call parallel_bcast(MY_AGR%vset_fac ,1,0)
       !
    end if
    !
    return
  end subroutine grn_bcast_aggregation
   !
  !-----------------------------------------
  !    subroutine grn_get_granulometry
  !-----------------------------------------
  !
  !>   @brief
  !>   Reads and sets MY_GRN (granulomeric distribution depending on specie)
  !>   For particle types, it also accounts for aggregation if needed
  !
  subroutine grn_get_granulometry(MY_FILES, MY_SPE, MY_MOD, MY_GRN, MY_AGR, MY_ESP, MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_SPE    list of parameters defining species and categories
    !>   @param MY_MOD    model physics related parameters
    !>   @param MY_GRN    output granulometry parameters
    !>   @param MY_AGR    list of parameters defining an aggregation model
    !>   @param MY_ESP    list of parameters defining Eruption Source Parameters
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),      intent(INOUT) :: MY_FILES
    type(SPECIES_PARAMS), intent(IN   ) :: MY_SPE
    type(MODEL_PHYS),     intent(IN   ) :: MY_MOD
    type(BIN_PARAMS),     intent(INOUT) :: MY_GRN
    type(AGR_PARAMS),     intent(IN   ) :: MY_AGR
    type(ESP_PARAMS),     intent(IN   ) :: MY_ESP
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    integer(ip), parameter :: nmaxbin = 100
    integer(ip)            :: ispe, spe_code, cat_code, is_bin, ie_bin, ic
    integer(ip)            :: i,i1,i2,i3,i4
    real(rp)               :: file_version
    real(rp)               :: fc_old(nmaxbin)
    real(rp)               :: fc_aggr
    character(len=2)       :: ext
    character(len=s_file)  :: file_inp, file_tgsd, sblock, swork(2)
    character(len=s_name)  :: spe_name, type_dist
    !
    type(BIN_PARAMS)       :: TGSD
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grn_get_granulometry'
    MY_ERR%message = ' '
    !
    file_inp = MY_FILES%file_inp
    !
    !*** Input file version
    !
    call inpout_get_rea (file_inp, 'CODE','VERSION', file_version, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) then
       return
    elseif(file_version < MIN_REQUIRED_VERSION) then
       MY_ERR%flag    = 1
       MY_ERR%source  = 'grn_get_granulometry'
       MY_ERR%message = 'Input file version deprecated. Please use 8.x file version '
       return
    end if
    !
    !*** First determines the total number of bins in MY_GRN (all species)
    !
    do ispe = 1,MY_SPE%nspe
       !
       spe_code = MY_SPE%code(ispe)
       spe_name = MY_SPE%name(ispe)
       sblock   = SPE_BLOCK  (spe_code)
       !
       select case(MY_SPE%category(ispe))
       !
       !*** Category particles and radionuclides
       !
       case(CAT_PARTICLE,CAT_RADIONUCLIDE)
         !
         !*** Reads TGSD from a file (previously generated by task SetTgsd or directly furnished by the user) and
         !*** checks for particle aggregation
         !
         call inpout_get_cha (file_inp, sblock,'DISTRIBUTION', type_dist, 1, MY_ERR, .true.)
         if(MY_ERR%flag.ne.0) return
         if(TRIM(type_dist).eq.'CUSTOM') then
            call inpout_get_cha (file_inp, sblock,'IF_CUSTOM', swork , 2, MY_ERR, .false.)
            file_tgsd = swork(2)
            if(MY_ERR%flag.ne.0) return
         else
            file_tgsd = TRIM(MY_FILES%file_tgsd) //'.'//TRIM(spe_name)   ! name.tgsd.specie
         end if
         !
         call grn_read_tgsd(file_tgsd,TGSD,MY_ERR)
         !
         MY_GRN%nbins_par = MY_GRN%nbins_par + TGSD%nbins + MY_AGR%nbins_aggr
         !
         !*** Deallocate TGSD
         !
         call grn_deallocate_GRN(TGSD,MY_ERR)
      !
      !*** Category aerosols
      !
      case(CAT_AEROSOL)
         select case(spe_code)
         case(SPE_H2O,SPE_SO2)
            MY_GRN%nbins_gas = MY_GRN%nbins_gas + 1
         end select
      end select
      !
    end do ! ispe = 1,MY_SPE%nspe
    !
    !*** Allocates
    !
    MY_GRN%nbins     = MY_GRN%nbins_par + MY_GRN%nbins_gas
    allocate(MY_GRN%bin_effe(MY_GRN%nbins))
    allocate(MY_GRN%bin_type(MY_GRN%nbins))
    allocate(MY_GRN%bin_name(MY_GRN%nbins))
    allocate(MY_GRN%bin_cat (MY_GRN%nbins))
    allocate(MY_GRN%bin_spe (MY_GRN%nbins))
    allocate(MY_GRN%bin_fc  (MY_GRN%nbins))
    allocate(MY_GRN%bin_rho (MY_GRN%nbins))
    allocate(MY_GRN%bin_diam(MY_GRN%nbins))
    allocate(MY_GRN%bin_sphe(MY_GRN%nbins))
    allocate(MY_GRN%bin_psi (MY_GRN%nbins))
    !
    MY_GRN%bin_effe(:) = .true.
    MY_GRN%bin_name(:) = '-'
    MY_GRN%bin_type(:) = '-'
    MY_GRN%bin_cat (:) = 0
    MY_GRN%bin_spe (:) = 0
    MY_GRN%bin_fc  (:) = 0.0_rp
    MY_GRN%bin_rho (:) = 0.0_rp
    MY_GRN%bin_diam(:) = 0.0_rp
    MY_GRN%bin_sphe(:) = 0.0_rp
    MY_GRN%bin_psi (:) = 0.0_rp
    !
    !*** Fill bins for all species
    !
    is_bin = 0
    ie_bin = 0
    !
    do ispe = 1,MY_SPE%nspe
       !
       cat_code = MY_SPE%category(ispe)
       spe_code = MY_SPE%code    (ispe)
       spe_name = MY_SPE%name    (ispe)
       sblock   = SPE_BLOCK      (spe_code)
       !
       select case(cat_code)
       !
       !*** Category particles and radionuclides
       !
       case(CAT_PARTICLE,CAT_RADIONUCLIDE)
         !
         !*** Reads TGSD from a file (previously generated by task SetTgsd or directly furnished by the user)
         !
         call inpout_get_cha (file_inp, sblock,'DISTRIBUTION', type_dist, 1, MY_ERR, .true.)
         if(MY_ERR%flag.ne.0) return
         if(TRIM(type_dist).eq.'CUSTOM') then
            call inpout_get_cha (file_inp, sblock,'IF_CUSTOM', swork , 2, MY_ERR, .false.)
            file_tgsd = swork(2)
            if(MY_ERR%flag.ne.0) return
         else
            file_tgsd = TRIM(MY_FILES%file_tgsd) //'.'//TRIM(spe_name)   ! name.tgsd.specie
         end if
         !
         call grn_read_tgsd(file_tgsd,TGSD,MY_ERR)
         !
         is_bin = ie_bin + 1
         ie_bin = ie_bin + TGSD%nbins
         !
         !*** Fill particle bins
         !
         MY_GRN%bin_type(is_bin:ie_bin) = TRIM(spe_name)
         MY_GRN%bin_cat (is_bin:ie_bin) = cat_code
         MY_GRN%bin_spe (is_bin:ie_bin) = spe_code
         MY_GRN%bin_rho (is_bin:ie_bin) = TGSD%bin_rho (1:TGSD%nbins)
         MY_GRN%bin_diam(is_bin:ie_bin) = TGSD%bin_diam(1:TGSD%nbins)
         MY_GRN%bin_sphe(is_bin:ie_bin) = TGSD%bin_sphe(1:TGSD%nbins)
         MY_GRN%bin_fc  (is_bin:ie_bin) = TGSD%bin_fc  (1:TGSD%nbins)
         !
         if(cat_code.eq.CAT_RADIONUCLIDE) then
            MY_GRN%bin_fc(is_bin:ie_bin) = MY_GRN%bin_fc(is_bin:ie_bin)*MY_SPE%mf(ispe)  ! mass fraction of radionuclides must sum 1
         end if
         !
         select case(spe_code)
         case(SPE_TEPHRA)
            !
            i1 = 0
            i2 = 0
            i3 = 0
            i4 = 0
            do i = is_bin,ie_bin
               if(MY_GRN%bin_diam(i).le.64e-6_rp) then         ! < 64 um
                  i1 = i1 + 1
                  write(ext,'(i2.2)') i1
                  MY_GRN%bin_name(i) = 'fine_ash-'//ext
               else if(MY_GRN%bin_diam(i).le.2e-3_rp) then    ! < 2 mm
                  i2 = i2 + 1
                  write(ext,'(i2.2)') i2
                  MY_GRN%bin_name(i) = 'coarse_ash-'//ext
               else if(MY_GRN%bin_diam(i).le.64e-3_rp) then   ! < 64 mm
                  i3 = i3 + 1
                  write(ext,'(i2.2)') i3
                  MY_GRN%bin_name(i) = 'lapilli-'//ext
               else                                           ! > 64 mm
                 i4 = i4 + 1
                 write(ext,'(i2.2)') i4
                 MY_GRN%bin_name(i) = 'bomb-'//ext
               end if
            end do
            !
         case default
            !
            i1 = 0
            do i = is_bin,ie_bin
               i1 = i1 + 1
               write(ext,'(i2.2)') i1
               MY_GRN%bin_name(i) = TRIM(spe_name)//'-'//ext
            end do
            !
         end select
         !
         !*** Aggregation
         !
         if(MY_AGR%aggregation) then
            !
            do i = 1,MY_AGR%nbins_aggr
               MY_GRN%bin_type(ie_bin+i) = TRIM(spe_name)
               MY_GRN%bin_cat (ie_bin+i) = cat_code
               MY_GRN%bin_spe (ie_bin+i) = spe_code
               write(ext,'(i2.2)') i
               MY_GRN%bin_name(ie_bin+i) = 'aggregate-'//ext
               MY_GRN%bin_sphe(ie_bin+i) = 1.0_rp
               MY_GRN%bin_diam(ie_bin+i) = MY_AGR%diam(i)
               MY_GRN%bin_rho (ie_bin+i) = MY_AGR%rho (i)
           end do
           !
           !** If necessary computes percentage of aggregates
           !
           select case(MY_AGR%aggregation_model)
           case('PERCENTAGE')
             !
             !   Computes aggregation according to a percentage
             !   All classes below diam_aggr are reduced with
             !   a fixed user-defined percentage (that can depend on aggregate class)
             !
             MY_GRN%is_aggr         = 0
             fc_old (is_bin:ie_bin) = 0.0_rp
             do i = 1,MY_AGR%nbins_aggr
                fc_aggr = 0.0_rp
                do ic = is_bin,ie_bin
                   if(MY_GRN%bin_diam(ie_bin+i).gt.MY_GRN%bin_diam(ic)) then
                      if(MY_GRN%is_aggr.eq.0) MY_GRN%is_aggr = ic
                      fc_aggr    = fc_aggr    + MY_AGR%percentage(i)*MY_GRN%bin_fc(ic)
                      fc_old(ic) = fc_old(ic) + MY_AGR%percentage(i)*MY_GRN%bin_fc(ic)
                   end if
                end do
                MY_GRN%bin_fc(ie_bin+i) = fc_aggr
             end do
             do ic = is_bin,ie_bin
                MY_GRN%bin_fc(ic) = MY_GRN%bin_fc(ic) - fc_old(ic)
             end do
             !
           case('CORNELL')
             !
             !   Computes aggregation according to the Cornell model
             !   The aggregate classes are made of
             !       50% of particles with 3<phi<4
             !       75% of particles with 4<phi<5
             !       90% of particles with phi>5
             !
             fc_aggr         = 0.0_rp
             MY_GRN%is_aggr  = 0
             do ic = is_bin,ie_bin
                if(MY_GRN%bin_diam(ic).le.0.000125_rp.and.MY_GRN%bin_diam(ic).gt.0.0000625_rp) then ! 3<phi<4
                   if(MY_GRN%is_aggr.eq.0) MY_GRN%is_aggr = ic   ! index of the first aggregating class
                   fc_aggr = fc_aggr + 0.5_rp*MY_GRN%bin_fc(ic)
                   MY_GRN%bin_fc(ic) = 0.5_rp*MY_GRN%bin_fc(ic)
                else if(MY_GRN%bin_diam(ic).le.0.0000625_rp.and.MY_GRN%bin_diam(ic).ge.0.00003125_rp) then    ! 4<phi<5
                   if(MY_GRN%is_aggr.eq.0) MY_GRN%is_aggr = ic   ! index of the first aggregating class
                   fc_aggr = fc_aggr + 0.75_rp*MY_GRN%bin_fc(ic)
                   MY_GRN%bin_fc(ic) = 0.25_rp*MY_GRN%bin_fc(ic)
                else if(MY_GRN%bin_diam(ic).lt.0.00003125_rp) then   ! phi>5
                   if(MY_GRN%is_aggr.eq.0) MY_GRN%is_aggr = ic   ! index of the first aggregating class
                   fc_aggr = fc_aggr + 0.9_rp*MY_GRN%bin_fc(ic)
                   MY_GRN%bin_fc(ic) = 0.1_rp*MY_GRN%bin_fc(ic)
                end if
             end do
             do i = 1,MY_AGR%nbins_aggr
                MY_GRN%bin_fc(ie_bin+i) = fc_aggr/MY_AGR%nbins_aggr
             end do
             !
            !
          case('COSTA')
             !
             ! Note that in this case fc is computed later by the plume model (time dependent)
             !
             !  check that costa is only possible with plume
             !
             if(MY_ESP%source_type.ne.'PLUME') then
                MY_ERR%flag    = 1
                MY_ERR%message = 'COSTA aggregation model is only possible with the PLUME source option'
                return
             end if
             !
             MY_GRN%is_aggr = 0
             do i  = 1,MY_AGR%nbins_aggr
                do ic = is_bin,ie_bin
                   if(MY_GRN%bin_diam(ie_bin+i).gt.MY_GRN%bin_diam(ic)) then
                      if(MY_GRN%is_aggr.eq.0) MY_GRN%is_aggr = i ! index of the first aggregating class
                   end if
                end do
             end do
             !
          end select
             !
             ie_bin = ie_bin + MY_AGR%nbins_aggr ! update counter
          !
          end if ! if(MY_AGR%aggregation) then
          !
          !*** Deallocate TGSD
          !
          call grn_deallocate_GRN(TGSD,MY_ERR)
      !
      !*** Category aerosols
      !
      case(CAT_AEROSOL)
         select case(spe_code)
         case(SPE_H2O)
            !
            is_bin = ie_bin + 1
            ie_bin = ie_bin + 1
            MY_GRN%bin_type(is_bin:ie_bin) = TRIM(spe_name)
            MY_GRN%bin_name(is_bin:ie_bin) = TRIM(spe_name)
            MY_GRN%bin_cat (is_bin:ie_bin) = cat_code
            MY_GRN%bin_spe (is_bin:ie_bin) = spe_code
            MY_GRN%bin_rho (is_bin:ie_bin) = 1e3_rp
            MY_GRN%bin_diam(is_bin:ie_bin) = 1e-6_rp      ! 1 mic assumed
            MY_GRN%bin_sphe(is_bin:ie_bin) = 1.0_rp
            MY_GRN%bin_fc  (is_bin:ie_bin) = MY_SPE%mf(ispe)
            !
         case(SPE_SO2)
            !
            is_bin = ie_bin + 1
            ie_bin = ie_bin + 1
            MY_GRN%bin_type(is_bin:ie_bin) = TRIM(spe_name)
            MY_GRN%bin_name(is_bin:ie_bin) = TRIM(spe_name)
            MY_GRN%bin_cat (is_bin:ie_bin) = cat_code
            MY_GRN%bin_spe (is_bin:ie_bin) = spe_code
            MY_GRN%bin_rho (is_bin:ie_bin) = 1e3_rp
            MY_GRN%bin_diam(is_bin:ie_bin) = 1e-6_rp      ! 1 mic assumed
            MY_GRN%bin_sphe(is_bin:ie_bin) = 1.0_rp
            MY_GRN%bin_fc  (is_bin:ie_bin) = MY_SPE%mf(ispe)
            !
         end select
         !
      end select
      !
    end do ! ispe = 1,MY_SPE%nspe
    !
    !*** Computes particle shape factor
    !
    call phys_get_psi(MY_GRN%bin_psi,MY_GRN%bin_sphe,MY_GRN%bin_diam,MY_MOD%modv,MY_GRN%nbins,MY_ERR)
    !
    !*** Determinates which bins are effective
    !
    call grn_get_effective_bins(MY_FILES, MY_GRN, MY_ERR)
    !
    return
    end subroutine grn_get_granulometry
  !
  !-----------------------------------------
  !    subroutine grn_bcast_granulometry
  !-----------------------------------------
  !
  !>   @brief
  !>   Master broadcasts a granulometry structure
  !
  subroutine grn_bcast_granulometry(MY_GRN,MY_ERR)
    implicit none
    !
    !>   @param MY_GRN    output granulometry parameters
    !>   @param MY_ERR    error handler
    !
    type(BIN_PARAMS),    intent(INOUT) :: MY_GRN
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    character(s_name) :: swork
    integer(ip)       :: i
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grn_bcast_granulometry'
    MY_ERR%message = ' '
    !
    call parallel_bcast(MY_GRN%nbins    ,1,0)
    call parallel_bcast(MY_GRN%nbins_par,1,0)
    call parallel_bcast(MY_GRN%nbins_gas,1,0)
    !
    !*** Memory allocation
    !
    if(.not.master_model) then
       allocate(MY_GRN%bin_effe(MY_GRN%nbins))
       allocate(MY_GRN%bin_type(MY_GRN%nbins))
       allocate(MY_GRN%bin_name(MY_GRN%nbins))
       allocate(MY_GRN%bin_cat (MY_GRN%nbins))
       allocate(MY_GRN%bin_spe (MY_GRN%nbins))
       allocate(MY_GRN%bin_fc  (MY_GRN%nbins))
       allocate(MY_GRN%bin_rho (MY_GRN%nbins))
       allocate(MY_GRN%bin_diam(MY_GRN%nbins))
       allocate(MY_GRN%bin_sphe(MY_GRN%nbins))
       allocate(MY_GRN%bin_psi (MY_GRN%nbins))
    end if
    !
    call parallel_bcast(MY_GRN%bin_effe,MY_GRN%nbins,0)
    call parallel_bcast(MY_GRN%bin_cat ,MY_GRN%nbins,0)
    call parallel_bcast(MY_GRN%bin_spe ,MY_GRN%nbins,0)
    call parallel_bcast(MY_GRN%bin_fc  ,MY_GRN%nbins,0)
    call parallel_bcast(MY_GRN%bin_rho ,MY_GRN%nbins,0)
    call parallel_bcast(MY_GRN%bin_diam,MY_GRN%nbins,0)
    call parallel_bcast(MY_GRN%bin_sphe,MY_GRN%nbins,0)
    call parallel_bcast(MY_GRN%bin_psi ,MY_GRN%nbins,0)
    !
    do i = 1,MY_GRN%nbins
       swork = MY_GRN%bin_type(i)
       call parallel_bcast(swork,1,0)
       MY_GRN%bin_type(i) = swork
       !
       swork = MY_GRN%bin_name(i)
       call parallel_bcast(swork,1,0)
       MY_GRN%bin_name(i) = swork
    end do
    !
    return
  end subroutine grn_bcast_granulometry
  !
  !--------------------------------------------
  !    subroutine grn_deallocate_GRN
  !--------------------------------------------
  !
  !>   @brief
  !>   Deallocates a structure of type BIN_PARAMS
  !
  subroutine grn_deallocate_GRN(MY_GRN,MY_ERR)
    implicit none
    !
    !>   @param MY_GRN    list of parameters defining granulometry
    !>   @param MY_ERR    error handler
    !
    type(BIN_PARAMS),   intent(INOUT) :: MY_GRN
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grn_deallocate_GRN'
    MY_ERR%message = ' '
    !
    deallocate(MY_GRN%bin_fc  )
    deallocate(MY_GRN%bin_rho )
    deallocate(MY_GRN%bin_diam)
    deallocate(MY_GRN%bin_sphe)
    deallocate(MY_GRN%bin_psi )
    !
    return
    end subroutine grn_deallocate_GRN
  !
  !-----------------------------------------
  !    subroutine grn_read_tgsd
  !-----------------------------------------
  !
  !>   @brief
  !>   Reads a TGSD file
  !
  subroutine grn_read_tgsd(file_tgsd, TGSD, MY_ERR)
    implicit none
    !
    !>   @param file_tgsd target file
    !>   @param TGSD      strucutre to be created and filled
    !>   @param MY_ERR    error handler
    !
    character(len=s_file), intent(IN   ) :: file_tgsd
    type(BIN_PARAMS),      intent(INOUT) :: TGSD
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    integer(ip) :: i
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grn_read_tgsd'
    MY_ERR%message = ' '
    !
    open(90,file=TRIM(file_tgsd),status='old')
    read(90,*) TGSD%nbins
    !
    allocate(TGSD%bin_diam(TGSD%nbins))
    allocate(TGSD%bin_rho (TGSD%nbins))
    allocate(TGSD%bin_psi (TGSD%nbins))
    allocate(TGSD%bin_sphe(TGSD%nbins))
    allocate(TGSD%bin_fc  (TGSD%nbins))
    !
    do i = 1,TGSD%nbins
       read(90,*) TGSD%bin_diam(i),TGSD%bin_rho(i),TGSD%bin_sphe(i),TGSD%bin_fc(i)
       TGSD%bin_diam(i) = TGSD%bin_diam(i)*1e-3_rp  ! mm to m
    end do
    !
    close(90)
    !
    return
  end subroutine grn_read_tgsd
  !
  !-----------------------------------------
  !    subroutine grn_write_granulometry
  !-----------------------------------------
  !
  !>   @brief
  !>   Writes the granulometry file
  !
  subroutine grn_write_granulometry(MY_FILES, MY_GRN, MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_GRN    output granulometry
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),     intent(INOUT) :: MY_FILES
    type(BIN_PARAMS),    intent(INOUT) :: MY_GRN
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip) :: i,nbin_effe
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grn_write_granulometry'
    MY_ERR%message = ' '
    !
    !*** Computes number of effective bins
    !
    nbin_effe = 0
    do i = 1,MY_GRN%nbins
       if(MY_GRN%bin_effe(i)) nbin_effe = nbin_effe + 1
    end do
    !
    !*** Writes the file
    !
    open(90,FILE= MY_FILES%file_grn,status='unknown')
    write(90,'(4(i5,1x))') MY_GRN%nbins,nbin_effe
    do i = 1,MY_GRN%nbins
       write(90,10) MY_GRN%bin_diam(i)*1e3_rp,MY_GRN%bin_rho(i),MY_GRN%bin_sphe(i),MY_GRN%bin_fc(i), &
            MY_GRN%bin_cat(i),MY_GRN%bin_spe(i),TRIM(MY_GRN%bin_type(i)),TRIM(MY_GRN%bin_name(i)),MY_GRN%bin_effe(i)
    end do
10  format(f10.6,1x,f8.1,1x,f7.3,1x,e16.9,2x,i2,2x,i2,2x,a10,2x,a14,2x,l1)
    close(90)
    !
    return
  end subroutine grn_write_granulometry
  !
  !----------------------------------------------
  !    subroutine grn_read_effective_granulometry
  !----------------------------------------------
  !
  !>   @brief
  !>   Reads a GRN file with the effective granulometry
  !
  subroutine grn_read_effective_granulometry(MY_FILES, MY_MOD, MY_TRA, MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_MOD    model physics related parameters
    !>   @param MY_TRA    variables related to tracers
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),     intent(IN   ) :: MY_FILES
    type(MODEL_PHYS),    intent(IN   ) :: MY_MOD
    type(TRACERS),       intent(INOUT) :: MY_TRA
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip)       :: nbins,i,j,icat,ispe
    real(rp)          :: diam,rho,sphe,fc
    character(s_name) :: type,name
    logical           :: effe
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grn_read_effective_granulometry'
    MY_ERR%message = ' '
    !
    open(90,file=TRIM(MY_FILES%file_grn),status='old')
    read(90,*) nbins, MY_TRA%nbins
    !
    MY_TRA%MY_BIN%nbins = MY_TRA%nbins
    !
    allocate(MY_TRA%MY_BIN%bin_type(MY_TRA%nbins))
    allocate(MY_TRA%MY_BIN%bin_name(MY_TRA%nbins))
    allocate(MY_TRA%MY_BIN%bin_cat (MY_TRA%nbins))
    allocate(MY_TRA%MY_BIN%bin_spe (MY_TRA%nbins))
    !
    allocate(MY_TRA%MY_BIN%bin_fc  (MY_TRA%nbins))
    allocate(MY_TRA%MY_BIN%bin_rho (MY_TRA%nbins))
    allocate(MY_TRA%MY_BIN%bin_diam(MY_TRA%nbins))
    allocate(MY_TRA%MY_BIN%bin_sphe(MY_TRA%nbins))
    allocate(MY_TRA%MY_BIN%bin_psi (MY_TRA%nbins))
    !
    j = 0
    do i = 1,nbins
       read(90,*) diam,rho,sphe,fc,icat,ispe,type,name,effe
       if(effe) then
          j = j + 1
          MY_TRA%MY_BIN%bin_type(j) = type
          MY_TRA%MY_BIN%bin_name(j) = name
          MY_TRA%MY_BIN%bin_cat (j) = icat
          MY_TRA%MY_BIN%bin_spe (j) = ispe
          !
          MY_TRA%MY_BIN%bin_fc  (j) = fc
          MY_TRA%MY_BIN%bin_rho (j) = rho
          MY_TRA%MY_BIN%bin_diam(j) = diam*1e-3_rp  ! mm --> m
          MY_TRA%MY_BIN%bin_sphe(j) = sphe
       end if
    end do
    !
    close(90)
    !
    !*** Computes other variables
    !
    MY_TRA%nbins_par = 0
    MY_TRA%nbins_gas = 0
    !
    do i = 1,MY_TRA%nbins
       icat = MY_TRA%MY_BIN%bin_cat(i)
       ispe = MY_TRA%MY_BIN%bin_spe(i)
       !
       select case(icat)
       case(CAT_PARTICLE,CAT_RADIONUCLIDE)
          MY_TRA%nbins_par = MY_TRA%nbins_par + 1
       case(CAT_AEROSOL)
          MY_TRA%nbins_gas = MY_TRA%nbins_gas + 1
       end select
    end do
    !
    MY_TRA%MY_BIN%nbins_par = MY_TRA%nbins_par
    MY_TRA%MY_BIN%nbins_gas = MY_TRA%nbins_gas
    !
    !*** Computes particle shape factor
    !
    call phys_get_psi(MY_TRA%MY_BIN%bin_psi ,MY_TRA%MY_BIN%bin_sphe, &
         MY_TRA%MY_BIN%bin_diam,MY_MOD%modv,MY_TRA%MY_BIN%nbins,MY_ERR)
    !
    return
  end subroutine grn_read_effective_granulometry
  !
  !-----------------------------------------------
  !    subroutine grn_bcast_effective_granulometry
  !-----------------------------------------------
  !
  !>   @brief
  !>   Master broadcasts the tracer effective granulometry
  !
  subroutine grn_bcast_effective_granulometry(MY_TRA, MY_ERR)
    implicit none
    !
    !>   @param MY_TRA    variables related to tracers
    !>   @param MY_ERR    error handler
    !
    type(TRACERS),       intent(INOUT) :: MY_TRA
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    character(s_name) :: swork
    integer(ip)       :: i
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grn_bcast_effective_granulometry'
    MY_ERR%message = ' '
    !
    call parallel_bcast(MY_TRA%nbins    ,1,0)
    call parallel_bcast(MY_TRA%nbins_par,1,0)
    call parallel_bcast(MY_TRA%nbins_gas,1,0)
    !
    MY_TRA%MY_BIN%nbins     = MY_TRA%nbins
    MY_TRA%MY_BIN%nbins_par = MY_TRA%nbins_par
    MY_TRA%MY_BIN%nbins_gas = MY_TRA%nbins_gas
    !
    !*** Memory allocation
    !
    if(.not.master_model) then
       allocate(MY_TRA%MY_BIN%bin_type(MY_TRA%nbins))
       allocate(MY_TRA%MY_BIN%bin_name(MY_TRA%nbins))
       allocate(MY_TRA%MY_BIN%bin_cat (MY_TRA%nbins))
       allocate(MY_TRA%MY_BIN%bin_spe (MY_TRA%nbins))
       allocate(MY_TRA%MY_BIN%bin_fc  (MY_TRA%nbins))
       allocate(MY_TRA%MY_BIN%bin_rho (MY_TRA%nbins))
       allocate(MY_TRA%MY_BIN%bin_diam(MY_TRA%nbins))
       allocate(MY_TRA%MY_BIN%bin_sphe(MY_TRA%nbins))
       allocate(MY_TRA%MY_BIN%bin_psi (MY_TRA%nbins))
    end if
    !
    call parallel_bcast(MY_TRA%MY_BIN%bin_cat ,MY_TRA%nbins,0)
    call parallel_bcast(MY_TRA%MY_BIN%bin_spe ,MY_TRA%nbins,0)
    call parallel_bcast(MY_TRA%MY_BIN%bin_fc  ,MY_TRA%nbins,0)
    call parallel_bcast(MY_TRA%MY_BIN%bin_rho ,MY_TRA%nbins,0)
    call parallel_bcast(MY_TRA%MY_BIN%bin_diam,MY_TRA%nbins,0)
    call parallel_bcast(MY_TRA%MY_BIN%bin_sphe,MY_TRA%nbins,0)
    call parallel_bcast(MY_TRA%MY_BIN%bin_psi ,MY_TRA%nbins,0)
    !
    do i = 1,MY_TRA%nbins
       swork = MY_TRA%MY_BIN%bin_type(i)
       call parallel_bcast(swork,1,0)
       MY_TRA%MY_BIN%bin_type(i) = swork
       !
       swork = MY_TRA%MY_BIN%bin_name(i)
       call parallel_bcast(swork,1,0)
       MY_TRA%MY_BIN%bin_name(i) = swork
    end do
    !
    return
  end subroutine grn_bcast_effective_granulometry
  !
  !-----------------------------------------
  !    subroutine grn_save_granulometry
  !-----------------------------------------
  !
  !>   @brief
  !>   Save the effective granulometry to tracers
  !
  subroutine grn_save_granulometry(MY_GRN, MY_TRA, MY_ERR)
    implicit none
    !
    !>   @param MY_GRN    output granulometry
    !>   @param MY_TRA    variables related to tracers
    !>   @param MY_ERR    error handler
    !
    type(BIN_PARAMS),    intent(IN   ) :: MY_GRN
    type(TRACERS),       intent(INOUT) :: MY_TRA
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip) :: i,j,nbins,nbins_par,nbins_gas
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grn_save_granulometry'
    MY_ERR%message = ' '
    !
    !*** Gets the number of effective bins and its fractioning between parts and gas
    !
    nbins     = 0    ! effective bins
    nbins_par = 0
    nbins_gas = 0
    !
    do i = 1,MY_GRN%nbins
       if(MY_GRN%bin_effe(i)) then
          nbins = nbins + 1
          select case(MY_GRN%bin_spe(i))
          case(SPE_H2O,SPE_SO2)
             nbins_gas = nbins_gas + 1
          case default
             nbins_par = nbins_par + 1
          end select
       end if
    end do
    !
    !*** Saves data
    !
    MY_TRA%nbins     = nbins
    MY_TRA%nbins_par = nbins_par
    MY_TRA%nbins_gas = nbins_gas
    !
    MY_TRA%MY_BIN%nbins     = nbins
    MY_TRA%MY_BIN%nbins_par = nbins_par
    MY_TRA%MY_BIN%nbins_gas = nbins_gas
    !
    allocate(MY_TRA%MY_BIN%bin_type(nbins))
    allocate(MY_TRA%MY_BIN%bin_cat (nbins))
    allocate(MY_TRA%MY_BIN%bin_spe (nbins))
    allocate(MY_TRA%MY_BIN%bin_name(nbins))
    !
    allocate(MY_TRA%MY_BIN%bin_fc  (nbins))
    allocate(MY_TRA%MY_BIN%bin_rho (nbins))
    allocate(MY_TRA%MY_BIN%bin_diam(nbins))
    allocate(MY_TRA%MY_BIN%bin_sphe(nbins))
    allocate(MY_TRA%MY_BIN%bin_psi (nbins))
    !
    j = 0
    do i = 1,MY_GRN%nbins
       if(MY_GRN%bin_effe(i)) then
          j = j + 1
          MY_TRA%MY_BIN%bin_type(j) = MY_GRN%bin_type(i)
          MY_TRA%MY_BIN%bin_cat (j) = MY_GRN%bin_cat (i)
          MY_TRA%MY_BIN%bin_spe (j) = MY_GRN%bin_spe(i)
          MY_TRA%MY_BIN%bin_name(j) = MY_GRN%bin_name(i)
          !
          MY_TRA%MY_BIN%bin_fc  (j) = MY_GRN%bin_fc  (i)
          MY_TRA%MY_BIN%bin_rho (j) = MY_GRN%bin_rho (i)
          MY_TRA%MY_BIN%bin_diam(j) = MY_GRN%bin_diam(i)
          MY_TRA%MY_BIN%bin_sphe(j) = MY_GRN%bin_sphe(i)
          MY_TRA%MY_BIN%bin_psi (j) = MY_GRN%bin_psi (i)
       end if
    end do
    !
    return
  end subroutine grn_save_granulometry
  !
  !
  !    PRIVATE ROUTINES
  !
  !
  !
  !----------------------------------------------
  !    subroutine grn_get_effective_bins
  !---------------------------------------------
  !
  !>   @brief
  !>   Computes the effective bins
  !
  subroutine grn_get_effective_bins(MY_FILES, MY_GRN, MY_ERR)
    implicit none
    !
    !>   @param MY_FILES   list of files
    !>   @param MY_GRN     granulometry
    !>   @param MY_ERR     error handler
    !
    type(FILE_LIST),     intent(INOUT) :: MY_FILES
    type(BIN_PARAMS),    intent(INOUT) :: MY_GRN
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    logical                :: cut_off
    character(len=s_file)  :: file_inp
    character(len=s_name)  :: type_cut_off
    integer(ip)            :: i
    real(rp)               :: cut_off_value
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grn_get_effective_bins'
    MY_ERR%message = ' '
    !
    file_inp = MY_FILES%file_inp
    !
    !*** First, determine if a cuff-off of classes is required
    !
    call inpout_get_cha (file_inp, 'PARTICLE_AGGREGATION','PARTICLE_CUT_OFF', type_cut_off, 1, MY_ERR, .true.)
    select case(TRIM(type_cut_off))
    case('NONE')
       cut_off = .false.
       !
    case('FI_LARGER_THAN','FI_LOWER_THAN')
       cut_off = .true.
       call inpout_get_rea (file_inp, 'PARTICLE_AGGREGATION','PARTICLE_CUT_OFF', cut_off_value, 1, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       !
       cut_off_value = 2.0_rp**(-cut_off_value)     ! mm
       cut_off_value = 1e-3_rp*cut_off_value        ! m
       !
    case('D_(MIC)_LARGER_THAN','D_(MIC)_LOWER_THAN')
       cut_off = .true.
       call inpout_get_rea (file_inp, 'PARTICLE_AGGREGATION','PARTICLE_CUT_OFF', cut_off_value, 1, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       !
       cut_off_value = 1e-6_rp*cut_off_value        ! m
       !
    case default
       cut_off = .false.
       !
    end select
    !
    !*** Determine the cut-off
    !
    select case(TRIM(type_cut_off))
    case('FI_LARGER_THAN','D_(MIC)_LARGER_THAN')
       !
       do i = 1,MY_GRN%nbins
          if(MY_GRN%bin_diam(i).gt.cut_off_value) then
             if(MY_GRN%bin_cat(i).ne.CAT_AEROSOL) MY_GRN%bin_effe(i) = .false.
          end if
       end do
       !
    case('FI_LOWER_THAN','D_(MIC)_LOWER_THAN')
       !
       do i = 1,MY_GRN%nbins
          if(MY_GRN%bin_diam(i).lt.cut_off_value) then
             if(MY_GRN%bin_cat(i).ne.CAT_AEROSOL) MY_GRN%bin_effe(i) = .false.
          end if
       end do
       !
    end select
    !
    return
    end subroutine grn_get_effective_bins
  !
  !
  !
END MODULE Grn
