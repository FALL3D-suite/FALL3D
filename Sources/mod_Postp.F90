!***********************************************************************
!>
!> Module for postprocess operations
!> @author
!> Arnau Folch
!>
!**********************************************************************
MODULE Postp
  use KindType
  use netcdf
  use Parallel
  implicit none
  save
  !
  !    LIST OF PUBLIC VARIABLES
  !

  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: postp_read_dimension
  PUBLIC :: postp_check_dimension
  PUBLIC :: postp_interpolate_time
  PUBLIC :: postp_rank_vector
  !
  INTERFACE postp_read_variable
     MODULE PROCEDURE postp_read_variable_1d, postp_read_variable_2d, postp_read_variable_3d, postp_read_variable_4d
  END INTERFACE postp_read_variable
  PRIVATE :: postp_read_variable_1d, postp_read_variable_2d, postp_read_variable_3d, postp_read_variable_4d
  !
  INTERFACE postp_read_variable_attribute
     MODULE PROCEDURE postp_read_variable_attribute_int, postp_read_variable_attribute_rea, postp_read_variable_attribute_str
  END INTERFACE postp_read_variable_attribute
  PRIVATE :: postp_read_variable_attribute_int, postp_read_variable_attribute_rea, postp_read_variable_attribute_str
  !
CONTAINS
  !
  !-----------------------------------------
  !    subroutine postp_read_dimension
  !-----------------------------------------
  !
  !>   @brief
  !>   Reads and broadcasts a dimension from a netCDF file
  !
  subroutine postp_read_dimension(file_name, dim_name, n, MY_ERR)
    implicit none
    !
    !>   @param file_name   input file name
    !>   @param dim_name    dimension name
    !>   @param n           dimension value
    !>   @param MY_ERR    error handler
    !
    character(s_file),  intent(IN   ) :: file_name
    character(len=* ),  intent(IN   ) :: dim_name
    integer(ip),        intent(INOUT) :: n
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    character(s_file) :: cvoid
    integer(ip)       :: ncID,dimID
    integer(ip)       :: istat,mode_flag
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'postp_read_dimension'
    MY_ERR%message = ' '
    !
    if(master_model) then
       mode_flag = NF90_NOWRITE
       istat = nf90_open(TRIM(file_name), mode_flag, ncID)
       istat = nf90_inq_dimid        (ncID, dim_name, dimID)
       istat = nf90_inquire_dimension(ncID, dimID,cvoid,n)
    end if
    !
    call parallel_bcast(istat,1_ip,0_ip)
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = nf90_strerror(istat)
       return
    else
       call parallel_bcast(n,1_ip,0_ip)
    end if
    !
    if(master_model) istat = nf90_close(ncID)
    !
    return
  end subroutine postp_read_dimension
  !
  !------------------------------------
  !    subroutine postp_check_dimension
  !------------------------------------
  !
  !>   @brief
  !>   Checks a dimension across (all) processors
  !
  subroutine postp_check_dimension(n,MY_ERR)
    implicit none
    !
    !>   @param n         dimension value
    !>   @param MY_ERR    error handler
    !
    integer(ip),        intent(IN   ) :: n
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip) :: n_all(1)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'postp_check_dimension'
    MY_ERR%message = ' '
    !
    n_all(1) = n
    call parallel_sum(n_all, COMM_WORLD)
    if(n_all(1)/npes_world.ne.n) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'Inconsistent dimension across processors or ensemble members'
    end if
    return
  end subroutine postp_check_dimension
  !
  !--------------------------------------
  !    subroutine postp_interpolate_time
  !--------------------------------------
  !
  !>   @brief
  !>   Reads and broadcasts a dimension from a netCDF file
  !
  subroutine postp_interpolate_time(nt, times, time, it, s, MY_ERR)
    implicit none
    !
    !>   @param nt         dimension value
    !>   @param times      times(nt)
    !>   @param time       value at which interpolate
    !>   @param it         interpolation index
    !>   @param s          interpolation factor
    !>   @param MY_ERR     error handler
    !
    integer(ip),        intent(IN   ) :: nt
    real(rp),           intent(IN   ) :: times(nt)
    real(rp),           intent(IN   ) :: time
    integer(ip),        intent(INOUT) :: it
    real(rp),           intent(INOUT) :: s
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip)       :: i
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'postp_interpolate_time'
    MY_ERR%message = ' '
    !
    if(time.le.times(1)) then
       it = 1
       s  = 1.0_rp
       return
    else if(time.ge.times(nt)) then
       it = nt-1
       s  = 0.0_rp
       return
    else
       do i = 1,nt-1
          if( (time.ge.times(i)).and.(time.le.times(i+1)) ) then
              it = i
              s  = (times(i+1)-time)/(times(i+1)-times(i))
              cycle
          end if
       end do
       return
    end if
    !
    return
  end subroutine postp_interpolate_time
  !
  !--------------------------------
  !    subroutine postp_rank_vector
  !--------------------------------
  !
  !>   @brief
  !>   sort an array ra(1:n) into growing order using heapsort algorithm,
  !>   and considering two elements being equal if their values differ for less than "eps".
  !>   Adapted from Numerical Recipes pg. 329
  !
  subroutine postp_rank_vector(n, ra, ind, eps, MY_ERR)
    implicit none
    !
    !>   @param n       dimension value
    !>   @param ra      vector to rank (overwritten)
    !>   @param ind     index table (exchange in the index array whenever an exchange is made on the sorted data array ra)
    !>                  If on input ind(1)  = 0 then indices are initialized in the routine
    !>   @param eps     tolerance
    !>   @param MY_ERR  error handler
    !
    integer(ip),        intent(IN   ) :: n
    integer(ip),        intent(INOUT) :: ind(n)
    real(rp),           intent(INOUT) :: ra (n)
    real(rp),           intent(IN   ) :: eps
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip) :: i, ir, j, l, iind
    real(rp)    :: rra
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'postp_rank_vector'
    MY_ERR%message = ' '
    !
    !*** Initialize index array
    !
    IF (ind (1) .eq.0) then
       DO i = 1, n
          ind (i) = i
       ENDDO
    ENDIF
    ! nothing to order
    IF (n.lt.2) return
    ! initialize indices for hiring and retirement-promotion phase
    l  = n / 2 + 1
    ir = n
    !
    sorting: do
    ! still in hiring phase
    IF ( l .gt. 1 ) then
       l    = l - 1
       rra  = ra (l)
       iind = ind (l)
       ! in retirement-promotion phase.
    ELSE
       ! clear a space at the end of the array
       rra  = ra (ir)
       !
       iind = ind (ir)
       ! retire the top of the heap into it
       ra (ir) = ra (1)
       !
       ind (ir) = ind (1)
       ! decrease the size of the corporation
       ir = ir - 1
       ! done with the last promotion
       IF ( ir .eq. 1 ) then
          ! the least competent worker at all !
          ra (1)  = rra
          !
          ind (1) = iind
          exit sorting
       ENDIF
    ENDIF
    ! wheter in hiring or promotion phase, we
    i = l
    ! set up to place rra in its proper level
    j = l + l
    !
    DO while ( j .le. ir )
       IF ( j .lt. ir ) then
          ! compare to better underling
          IF ( hslt( ra (j),  ra (j + 1) ) ) then
             j = j + 1
          !else if ( .not. hslt( ra (j+1),  ra (j) ) ) then
             ! this means ra(j) == ra(j+1) within tolerance
           !  if (ind (j) .lt.ind (j + 1) ) j = j + 1
          ENDIF
       ENDIF
       ! demote rra
       IF ( hslt( rra, ra (j) ) ) then
          ra (i) = ra (j)
          ind (i) = ind (j)
          i = j
          j = j + j
       !else if ( .not. hslt ( ra(j) , rra ) ) then
          !this means rra == ra(j) within tolerance
          ! demote rra
         ! if (iind.lt.ind (j) ) then
         !    ra (i) = ra (j)
         !    ind (i) = ind (j)
         !    i = j
         !    j = j + j
         ! else
             ! set j to terminate do-while loop
         !    j = ir + 1
         ! endif
          ! this is the right place for rra
       ELSE
          ! set j to terminate do-while loop
          j = ir + 1
       ENDIF
    ENDDO
    ra (i) = rra
    ind (i) = iind

  END DO sorting
  !
  ! ascending order
  !allocate(work(n))
  !work(:) = ra(:)
  !i = 0
  !do j = n,1,-1
  !   i = i + 1
  !   ra(j) = work(i)
  !end do
  !
  return
  !
  contains
  !  internal function
  !  compare two real number and return the result

  logical function hslt( a, b )
    REAL(rp) :: a, b
    IF( abs(a-b) <  eps ) then
      hslt = .false.
    ELSE
      hslt = ( a < b )
    end if
  end function hslt
  !
  end subroutine postp_rank_vector
  !
  !
  !  PRIVATE ROUTINES
  !
  !
  subroutine postp_read_variable_1d(file_name, var_name, n1, var, MY_ERR)
    implicit none
    !
    character(s_file),  intent(IN   ) :: file_name
    character(len=* ),  intent(IN   ) :: var_name
    integer(ip),        intent(IN   ) :: n1
    real(rp),           intent(INOUT) :: var(n1)
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip)  :: ncID,varID
    integer(ip)  :: istat,mode_flag
    integer(ip)  :: start1d(1)
    integer(ip)  :: count1d(1)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'postp_read_variable_1d'
    MY_ERR%message = ' '
    !
    mode_flag = NF90_NOWRITE
    istat = nf90_open(TRIM(file_name), mode_flag, ncID)
    !
    start1d=(/1/)
    count1d=(/n1/)
    istat = nf90_inq_varid(ncID, var_name, varID)
    istat = nf90_get_var(ncID, varID, var,start=start1d,count=count1d)
    !
    istat = nf90_close(ncID)
    !
    return
  end subroutine postp_read_variable_1d
  !
  subroutine postp_read_variable_2d(file_name, var_name, n1, n2, var, MY_ERR)
    implicit none
    !
    character(s_file),  intent(IN   ) :: file_name
    character(len=* ),  intent(IN   ) :: var_name
    integer(ip),        intent(IN   ) :: n1
    integer(ip),        intent(IN   ) :: n2
    real(rp),           intent(INOUT) :: var(n1,n2)
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip)  :: ncID,varID
    integer(ip)  :: istat,mode_flag
    integer(ip)  :: start2d(2)
    integer(ip)  :: count2d(2)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'postp_read_variable_2d'
    MY_ERR%message = ' '
    !
    mode_flag = NF90_NOWRITE
    istat = nf90_open(TRIM(file_name), mode_flag, ncID)
    !
    start2d=(/1,1/)
    count2d=(/n1,n2/)
    istat = nf90_inq_varid(ncID, var_name, varID)
    istat = nf90_get_var(ncID, varID, var,start=start2d,count=count2d)
    !
    istat = nf90_close(ncID)
    !
    return
  end subroutine postp_read_variable_2d
  !
  subroutine postp_read_variable_3d(file_name, var_name, n1, n2, n3, var, MY_ERR)
    implicit none
    !
    character(s_file),  intent(IN   ) :: file_name
    character(len=* ),  intent(IN   ) :: var_name
    integer(ip),        intent(IN   ) :: n1
    integer(ip),        intent(IN   ) :: n2
    integer(ip),        intent(IN   ) :: n3
    real(rp),           intent(INOUT) :: var(n1,n2,n3)
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip)  :: ncID,varID
    integer(ip)  :: istat,mode_flag
    integer(ip)  :: start3d(3)
    integer(ip)  :: count3d(3)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'postp_read_variable_3d'
    MY_ERR%message = ' '
    !
    mode_flag = NF90_NOWRITE
    istat = nf90_open(TRIM(file_name), mode_flag, ncID)
    !
    start3d=(/1,1,1/)
    count3d=(/n1,n2,n3/)
    istat = nf90_inq_varid(ncID, var_name, varID)
    istat = nf90_get_var(ncID, varID, var,start=start3d,count=count3d)
    !
    istat = nf90_close(ncID)
    !
    return
  end subroutine postp_read_variable_3d
    !
  subroutine postp_read_variable_4d(file_name, var_name, n1, n2, n3, n4, var, MY_ERR)
    implicit none
    !
    character(s_file),  intent(IN   ) :: file_name
    character(len=* ),  intent(IN   ) :: var_name
    integer(ip),        intent(IN   ) :: n1
    integer(ip),        intent(IN   ) :: n2
    integer(ip),        intent(IN   ) :: n3
    integer(ip),        intent(IN   ) :: n4
    real(rp),           intent(INOUT) :: var(n1,n2,n3,n4)
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip)  :: ncID,varID
    integer(ip)  :: istat,mode_flag
    integer(ip)  :: start4d(4)
    integer(ip)  :: count4d(4)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'postp_read_variable_4d'
    MY_ERR%message = ' '
    !
    mode_flag = NF90_NOWRITE
    istat = nf90_open(TRIM(file_name), mode_flag, ncID)
    !
    start4d=(/1,1,1,1/)
    count4d=(/n1,n2,n3,n4/)
    istat = nf90_inq_varid(ncID, var_name, varID)
    istat = nf90_get_var(ncID, varID, var,start=start4d,count=count4d)
    !
    istat = nf90_close(ncID)
    !
    return
  end subroutine postp_read_variable_4d
  !
  subroutine postp_read_variable_attribute_int(file_name, var_name, att_name, val,  MY_ERR)
    implicit none
    !
    character(s_file),  intent(IN   ) :: file_name
    character(len=* ),  intent(IN   ) :: var_name
    character(len=* ),  intent(IN   ) :: att_name
    integer(ip),        intent(INOUT) :: val
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip)  :: ncID,varID
    integer(ip)  :: istat,mode_flag
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'postp_read_variable_attribute_int'
    MY_ERR%message = ' '
    !
    mode_flag = NF90_NOWRITE
    istat = nf90_open(TRIM(file_name), mode_flag, ncID)
    !
    istat = nf90_inq_varid(ncID, var_name, varID)
    istat = nf90_get_att  (ncID, varID, att_name, val)
    istat = nf90_close    (ncID)
    !
    return
  end subroutine postp_read_variable_attribute_int
  !
  subroutine postp_read_variable_attribute_rea(file_name, var_name, att_name, val,  MY_ERR)
    implicit none
    !
    character(s_file),  intent(IN   ) :: file_name
    character(len=* ),  intent(IN   ) :: var_name
    character(len=* ),  intent(IN   ) :: att_name
    real(rp),           intent(INOUT) :: val
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip)  :: ncID,varID
    integer(ip)  :: istat,mode_flag
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'postp_read_variable_attribute_int'
    MY_ERR%message = ' '
    !
    mode_flag = NF90_NOWRITE
    istat = nf90_open(TRIM(file_name), mode_flag, ncID)
    !
    istat = nf90_inq_varid(ncID, var_name, varID)
    istat = nf90_get_att  (ncID, varID, att_name, val)
    istat = nf90_close    (ncID)
    !
    return
  end subroutine postp_read_variable_attribute_rea
  !
  subroutine postp_read_variable_attribute_str(file_name, var_name, att_name, val,  MY_ERR)
    implicit none
    !
    character(s_file),  intent(IN   ) :: file_name
    character(len=* ),  intent(IN   ) :: var_name
    character(len=* ),  intent(IN   ) :: att_name
    character(len=* ),  intent(INOUT) :: val
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip)  :: ncID,varID
    integer(ip)  :: istat,mode_flag
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'postp_read_variable_attribute_int'
    MY_ERR%message = ' '
    !
    mode_flag = NF90_NOWRITE
    istat = nf90_open(TRIM(file_name), mode_flag, ncID)
    !
    istat = nf90_inq_varid(ncID, var_name, varID)
    istat = nf90_get_att  (ncID, varID, att_name, val)
    istat = nf90_close    (ncID)
    !
    return
  end subroutine postp_read_variable_attribute_str
  !
  END MODULE Postp
