!***********************************************************************
!>
!> Module for data blocks shared by multiple TASKS
!> @author
!> Arnau Folch
!>
!**********************************************************************
  MODULE Shared
  use KindType
  implicit none
  save
  !
  !    LIST OF PUBLIC TYPES AND VARIABLES
  !
  integer(ip)          :: mproc(3) = 1   !>  number of processors along each direction (for domain decomposition)
  integer(ip)          :: nens     = 1   !>  number of members in ensemble
  !
  type(FILE_LIST)      :: MY_FILES       !>  variables related to file logic units and names
  type(ERROR_STATUS)   :: MY_ERR         !>  error handler and descriptor
  type(ARAKAWA_C_GRID) :: MY_GRID        !>  variables related to grid
  type(RUN_TIME)       :: MY_TIME        !>  variables related to run time
  type(METEOROLOGY)    :: MY_MET         !>  variables related to meteorology in MY_GRID
  type(METEO_PROFILE)  :: GL_METPROFILES !>  variables related to (global) metrorological vertical profiles
  type(SPECIES_PARAMS) :: MY_SPE         !>  variables related to categories and species
  type(TRACERS)        :: MY_TRA         !>  variables related to tracers
  type(MODEL_PHYS)     :: MY_MOD         !>  variables related to model physics and parameterizations
  type(AGR_PARAMS)     :: MY_AGR         !>  variables related to model aggregation
  type(MODEL_OUTPUT)   :: MY_OUT         !>  variables related to model output and postprocess
  type(ENS_PARAMS)     :: MY_ENS         !>  variables related to ensemble 
  !
  END MODULE Shared
