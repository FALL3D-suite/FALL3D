!***************************************************************
!>
!> Module for time related operations
!> @author
!> Arnau Folch
!>
!***************************************************************
MODULE Time
  use KindType
  use InpOut
  use Parallel
  implicit none
  save
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: time_addtime
  PUBLIC :: time_dateformat
  PUBLIC :: time_read_inp_time
  PUBLIC :: time_bcast_inp_time
  PUBLIC :: time_real_to_date
  PUBLIC :: time_date_to_real
  PUBLIC :: time_julian_date

  !
  !    LIST OF PRIVATE ROUTINES IN THE MODULE
  !
  PRIVATE :: time_timeincr
  PRIVATE :: time_julday
  PRIVATE :: time_grday
  !
CONTAINS
  !
  !-----------------------------------------
  !    subroutine time_read_inp_time
  !-----------------------------------------
  !
  !>   @brief
  !>   Reads the time block form the input file
  !
  subroutine time_read_inp_time(MY_FILES,MY_TIME,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_TIME   run time related parameters
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),   intent(INOUT) :: MY_FILES
    type(RUN_TIME),    intent(INOUT) :: MY_TIME
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    real(rp)              :: file_version
    character(len=s_file) :: file_inp, word
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'time_read_inp_time'
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
       MY_ERR%message = 'Input file version deprecated. Please use 8.x file version'
       return
    end if
    !
    !*** Reads TIME_UTC block
    !
    call inpout_get_int (file_inp, 'TIME_UTC','YEAR', MY_TIME%start_year, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    !
    call inpout_get_int (file_inp, 'TIME_UTC','MONTH', MY_TIME%start_month, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    !
    call inpout_get_int (file_inp, 'TIME_UTC','DAY', MY_TIME%start_day, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    !
    call inpout_get_rea (file_inp, 'TIME_UTC','RUN_START_(HOURS_AFTER_00)', MY_TIME%run_start, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    MY_TIME%run_start = MY_TIME%run_start*3600.0_rp  ! h -> s
    !
    call inpout_get_rea (file_inp, 'TIME_UTC','RUN_END_(HOURS_AFTER_00)', MY_TIME%run_end, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    MY_TIME%run_end = MY_TIME%run_end*3600.0_rp  ! h -> s
    !
    call inpout_get_cha (file_inp, 'TIME_UTC','INITIAL_CONDITION',word, 1, MY_ERR, .true.)
    if(MY_ERR%flag.ne.0) return
    select case(word)
    case('RESTART')
       MY_TIME%restart   = .true.
       MY_TIME%insertion = .false.
    case('INSERTION')
       MY_TIME%restart   = .false.
       MY_TIME%insertion = .true.
    case default
       MY_TIME%restart   = .false.
       MY_TIME%insertion = .false.
    end select
    !
    if(MY_TIME%restart) then
       call inpout_get_cha (file_inp, 'TIME_UTC','RESTART_FILE',MY_FILES%file_rst, 1, MY_ERR, .false.)
       if(MY_ERR%flag.ne.0) MY_FILES%file_rst = '-'
    end if
    !
    call inpout_get_rea (file_inp, 'TIME_UTC','RUN_END_(HOURS_AFTER_00)', MY_TIME%run_end, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    MY_TIME%run_end = MY_TIME%run_end*3600.0_rp  ! h -> s
    !
    return
  end subroutine time_read_inp_time
  !
  !-----------------------------------------
  !    subroutine time_bcast_inp_time
  !-----------------------------------------
  !
  !>   @brief
  !>   Master broadcasts time block from input file
  !
  subroutine time_bcast_inp_time (MY_TIME,MY_ERR)
    implicit none
    !
    !>   @param MY_TIME   run time related parameters
    !>   @param MY_ERR    error handler
    !
    type(RUN_TIME),    intent(INOUT) :: MY_TIME
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'time_bcast_inp_time'
    MY_ERR%message = ' '
    !
    call parallel_bcast(MY_TIME%start_year  ,1,0)
    call parallel_bcast(MY_TIME%start_month ,1,0)
    call parallel_bcast(MY_TIME%start_day   ,1,0)
    call parallel_bcast(MY_TIME%run_start   ,1,0)
    call parallel_bcast(MY_TIME%run_end     ,1,0)
    call parallel_bcast(MY_TIME%restart     ,1,0)
    call parallel_bcast(MY_TIME%insertion   ,1,0)
    !
    return
  end subroutine time_bcast_inp_time
  !
  !-------------------------------
  !    subroutine time_addtime
  !-------------------------------
  !
  !>   @brief
  !>   Adds time seconds to the initial date YYYY-MM-DD-HH
  !
  subroutine time_addtime(iyr0,imo0,idy0,ihr0,iyr,imo,idy,ihr,imi,ise,time,MY_ERR)
    implicit none
    !
    !>   @param iyr0   Initial year
    !>   @param imo0   Initial month (1-12)
    !>   @param idy0   Initial day   (1-31)
    !>   @param ihr0   Initial hour  (0-23)
    !>   @param iyr    Output year
    !>   @param imo    Output month  (1-12)
    !>   @param idy    Output day    (1-31)
    !>   @param ihr    Output hour   (0-23)
    !>   @param imi    Output minute (0-59)
    !>   @param ise    Output second (0-59)
    !>   @param time   Time increment in seconds
    !>   @param MY_ERR        error handler
    !
    integer(ip), intent(IN)  :: iyr0
    integer(ip), intent(IN)  :: imo0
    integer(ip), intent(IN)  :: idy0
    integer(ip), intent(IN)  :: ihr0
    integer(ip), intent(OUT) :: iyr
    integer(ip), intent(OUT) :: imo
    integer(ip), intent(OUT) :: idy
    integer(ip), intent(OUT) :: ihr
    integer(ip), intent(OUT) :: imi
    integer(ip), intent(OUT) :: ise
    real(rp),    intent(IN)  :: time
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    integer(ip) :: nhincr,ijl
    real   (rp) :: work
    !
    !***  Initialization
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'time_addtime'
    MY_ERR%message = ' '
    !
    iyr = iyr0
    imo = imo0
    idy = idy0
    ihr = ihr0
    !
    !***  Time < 1 min
    !
    if(time < 60.0_rp) then
       imi = 0
       ise = int(time)
       return
       !
       !***  Time < 1 h
       !
    else if(time < 3600.0_rp) then
       imi = int(time/60.0_rp)
       ise = int(time)-60*imi
       return
    else
       !
       !***  Time > 1 h
       !
       nhincr = int(time/3600.0_rp)
       work   = time - nhincr*3600.0_rp
       imi    = int(work/60.0_rp)
       ise    = int(work)-60*imi
       !
       call time_julday  (iyr,imo,idy,ijl,MY_ERR)      ! Computes the Julian day ijl
       if(MY_ERR%flag.ne.0) return
       call time_timeincr(iyr,ijl,ihr,nhincr,MY_ERR)   ! Updates the  Julian day
       if(MY_ERR%flag.ne.0) return
       call time_grday   (iyr,ijl,imo,idy,MY_ERR)      ! Converts back to Gregorian
       return
    end if
    !
  end subroutine time_addtime
  !
  !-------------------------------
  !   subroutine time_dateformat
  !-------------------------------
  !
  !>   @brief
  !>   Formats a date-type string
  !
  subroutine time_dateformat(iyr,imo,idy,ihr,imi,ise,iform,str,MY_ERR)
    implicit none
    !
    !>   @param iyr      year
    !>   @param imo      month  (1-12)
    !>   @param idy      day    (1-31)
    !>   @param ihr      hour   (0-23)
    !>   @param imi      minute (0-59)
    !>   @param ise      second (0-59)
    !>   @param iform    format: flag=1 YYYY-MM-DD_HH:MM; flag=2 DDmonYYYY_HH:MM; flag=3 DD mon YYYYY at HH:MM:SS; flag=1 YYYY-MM-DD-HH-MM
    !>   @param str      output string formated as specified by iform flag
    !>   @param MY_ERR   error handler
    !
    integer(ip), intent(IN   ) :: iyr
    integer(ip), intent(IN   ) :: imo
    integer(ip), intent(IN   ) :: idy
    integer(ip), intent(IN   ) :: ihr
    integer(ip), intent(IN   ) :: imi
    integer(ip), intent(IN   ) :: ise
    integer(ip), intent(IN   ) :: iform
    character(len=*),   intent(INOUT) :: str
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip) :: str_len
    character(len=3), dimension(12) :: mo= (/'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec' /)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'time_dateformat'
    MY_ERR%message = ' '
    !
    str_len = len(str)
    str = ''
    !
    select case (iform)
    case(1)
       !
       if(str_len.lt.16) return
       str = '0000-00-00_00:00'
       write(str(1 :4 ),'(i4.4)') iyr
       write(str(6 :7 ),'(i2.2)') imo
       write(str(9 :10),'(i2.2)') idy
       write(str(12:13),'(i2.2)') ihr
       write(str(15:16),'(i2.2)') imi
       !
    case(2)
       !
       if(str_len.lt.15) return
       str = '00mon0000_00:00'
       write(str(1 :2 ),'(i2.2)') idy
       write(str(3 :5 ),'(a3)'  ) mo(imo)
       write(str(6 :9 ),'(i4.4)') iyr
       write(str(11:12),'(i2.2)') ihr
       write(str(14:15),'(i2.2)') imi
       !
    case(3)
       !
       if(str_len.lt.23) return
       str = '00 mon 0000 at 00:00:00'
       write(str(1 :2 ),'(i2.2)') idy
       write(str(4 :6 ),'(a3)'  ) mo(imo)
       write(str(8 :11),'(i4.4)') iyr
       write(str(16:17),'(i2.2)') ihr
       write(str(19:20),'(i2.2)') imi
       write(str(22:23),'(i2.2)') ise
       !
    case(4)
       !
       if(str_len.lt.16) return
       str = '0000-00-00-00-00'
       write(str(1 :4 ),'(i4.4)') iyr
       write(str(6 :7 ),'(i2.2)') imo
       write(str(9 :10),'(i2.2)') idy
       write(str(12:13),'(i2.2)') ihr
       write(str(15:16),'(i2.2)') imi
       !
    case default
       !
    end select
    !
    return
  end subroutine time_dateformat
  !
  !-------------------------------
  !   subroutine time_real_to_date
  !-------------------------------
  !
  !>   @brief
  !>   Converts a real in format YYYYMMDDHHMMSS to dates
  !
  subroutine time_real_to_date(iyr,imo,idy,ihr,imi,ise,time,MY_ERR)
    implicit none
    !
    !>   @param iyr      year
    !>   @param imo      month  (1-12)
    !>   @param idy      day    (1-31)
    !>   @param ihr      hour   (0-23)
    !>   @param imi      minute (0-59)
    !>   @param ise      second (0-59)
    !>   @param time     time in format YYYYMMDDHHMMSS
    !>   @param MY_ERR   error handler
    !
    integer(ip), intent(OUT) :: iyr
    integer(ip), intent(OUT) :: imo
    integer(ip), intent(OUT) :: idy
    integer(ip), intent(OUT) :: ihr
    integer(ip), intent(OUT) :: imi
    integer(ip), intent(OUT) :: ise
    real(rp),    intent(IN ) :: time
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    real(rp) :: t
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%message = ' '
    !
    t = time
    !
    iyr = int(t/1e10_rp)
    t   = t - iyr*1e10_rp
    imo = int(t/1e8_rp)
    t   = t - imo*1e8_rp
    idy = int(t/1e6_rp)
    t   = t - idy*1e6_rp
    ihr = int(t/1e4_rp)
    t   = t - ihr*1e4_rp
    imi = int(t/1e2_rp)
    t   = t - imi*1e2_rp
    ise = int(t)
    !
    return
  end subroutine time_real_to_date
  !
  !-------------------------------
  !   subroutine time_date_to_real
  !-------------------------------
  !
  !>   @brief
  !>   Converts date to a real in format YYYYMMDDHHMMSS
  !
  subroutine time_date_to_real(iyr,imo,idy,ihr,imi,ise,time,MY_ERR)
    implicit none
    !
    !>   @param iyr      year
    !>   @param imo      month  (1-12)
    !>   @param idy      day    (1-31)
    !>   @param ihr      hour   (0-23)
    !>   @param imi      minute (0-59)
    !>   @param ise      second (0-59)
    !>   @param time     time in format YYYYMMDDHHMMSS
    !>   @param MY_ERR   error handler
    !
    integer(ip), intent(IN ) :: iyr
    integer(ip), intent(IN ) :: imo
    integer(ip), intent(IN ) :: idy
    integer(ip), intent(IN ) :: ihr
    integer(ip), intent(IN ) :: imi
    integer(ip), intent(IN ) :: ise
    real(rp),    intent(OUT) :: time
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%message = ' '
    !
    time = 1e10_rp*iyr + 1e8_rp*imo + 1e6_rp*idy + 1e4_rp*ihr + 1e2_rp*imi + ise
    !
    return
  end subroutine time_date_to_real
  !
  !-----------------------------------------
  !    subroutine time_julian_date
  !-----------------------------------------
  !
  !>   @brief
  !>   Returns the julian date.
  !>
  !>   @details
  !>   Julian day number 0 assigned to the day starting at noon
  !>   on Monday, January 1, 4713 BC, proleptic Julian calendar (November 24, 4714 BC, in
  !>   the proleptic Gregorian calendar)
  !
  subroutine time_julian_date (yyyy, mm, dd, julian,MY_ERR)
    implicit none
    !
    !>   @param MY_ERR    error handler
    !
    integer(ip),       intent(IN )   :: yyyy, mm, dd
    integer(ip),       intent(OUT)   :: julian
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'time_julian_date'
    MY_ERR%message = ' '
    !
    julian = dd-32075+1461*(yyyy+4800+(mm-14)/12)/4 + &
         367*(mm-2-((mm-14)/12)*12)/12- &
         3*((yyyy + 4900 + (mm - 14)/12)/100)/4
    !
    return
  end subroutine time_julian_date
  !
  !
  !   PRIVATE ROUTINES
  !
  !
  subroutine time_julday(iyr,imo,iday,ijuldy,MY_ERR)
    !*********************************************************************
    !*
    !*    Computes the Julian day number from the Gregorian date
    !*    (month, day)
    !*
    !*    INPUTS:
    !*       IYR    - integer - Year
    !*       IMO    - integer - Month
    !*       IDAY   - integer - Day
    !*
    !*    OUTPUT:
    !*       IJULDY - integer - Julian day
    !*
    !* new version properly computing leap years
    !*********************************************************************
    implicit none
    !
    integer(ip),        intent(IN)    :: iyr,imo,iday
    integer(ip),        intent(OUT)   :: ijuldy
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR

    integer(ip)                :: ileap,day_month
    integer(ip), dimension(12) :: kday = (/0,31,59,90,120,151,181,212,243,273,304,334/)
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'time_julday'
    MY_ERR%message = ' '
    !
    !Check for leap year
    if(mod(iyr,4).ne.0) then
       ileap = 0
    elseif(mod(iyr,100).ne.0) then
       ileap = 1
    elseif(mod(iyr,400).ne.0) then
       ileap = 0
    else
       ileap = 1
    end if
    !
    if(imo == 4.or.imo == 6.or.imo == 9.or.imo == 11) then
       day_month = 30
    elseif(imo == 2) then
       day_month = 28 + ileap
    else
       day_month = 31
    endif
    !
    if(imo<1 .or. imo>12) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'Invalid month'
       return
    elseif(iday<1 .or. iday>day_month) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'Invalid day'
       return
    end if
    !
    !***  Compute the Julian day
    !
    ijuldy=kday(imo)+iday
    if(imo>2) ijuldy = ijuldy + ileap
    !
    return
  end subroutine time_julday
  !
  !
  subroutine time_grday(iyr,ijul,imo,iday,MY_ERR)
    !**********************************************************************
    !*
    !*    Compute the Gregorian date (month, day) from the Julian day
    !*
    !*    INPUTS:
    !*       IYR    - integer - Current year
    !*       IJUL   - integer - Current Julian day
    !*
    !*    OUTPUT:
    !*       IMO    - integer - Month
    !*       IDAY   - integer - Day
    !*
    !* new version properly computing leap years
    !**********************************************************************
    implicit none
    !
    integer(ip),        intent(IN)    :: iyr,ijul
    integer(ip),        intent(OUT)   :: imo,iday
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR

    integer(ip) :: ileap,i
    integer(ip), dimension(12,2) :: kday
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'time_grday'
    MY_ERR%message = ' '
    !
    kday(:,1)=(/31,59,90,120,151,181,212,243,273,304,334,365/)
    kday(:,2)=(/31,60,91,121,152,182,213,244,274,305,335,366/)
    !
    !Check for leap year
    if(mod(iyr,4).ne.0) then
       ileap = 1
    elseif(mod(iyr,100).ne.0) then
       ileap = 2
    elseif(mod(iyr,400).ne.0) then
       ileap = 1
    else
       ileap = 2
    end if
    !
    if( ijul < 1 .or. ijul > kday(12,ileap) ) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'Wrong computation of the gegorian day'
       return
    end if
    !
    do i=1,12
       if(ijul > kday(i,ileap)) cycle
       imo=i
       iday=ijul
       if(imo /= 1) iday=ijul-kday(imo-1,ileap)
       return
    end do
    !
    return
  end subroutine time_grday
  !
  !
  subroutine time_timeincr(iyr,ijul,ihr,nhrinc,MY_ERR)
    !**********************************************************************
    !*
    !*    Increments the time and date by "NHRINC" hours
    !*
    !*    INPUTS:
    !*       IYR    - integer - Current year
    !*       IJUL   - integer - Current Julian day
    !*       IHR    - integer - Current hour (00-23)
    !*       NHRINC - integer - Time increment (hours>0)
    !*
    !*    OUTPUT:
    !*       IYR    - integer - Updated year
    !*       IJUL   - integer - Updated Julian day
    !*       IHR    - integer - Updated hour (00-23)
    !*
    !* new version properly computing leap years
    !**********************************************************************
    implicit none
    !
    integer(ip),        intent(IN)    :: nhrinc
    integer(ip),        intent(INOUT) :: iyr,ijul,ihr
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR

    integer(ip) :: nleft,ninc,ileap
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'time_timeincr'
    MY_ERR%message = ' '
    !
    !***  Check nhrinc and ihr
    !
    if(nhrinc < 0) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'TIMEINCR invalid time increment'
       return
    elseif(ihr > 23 .or. ihr < 0) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'HOUR invalid'
       return
    end if
    !
    !***  Save increment remaining (needed if nhrinc > 8760)
    !
    nleft=nhrinc
    !
    do while(nleft>0)
       ninc  = min(nleft,8760_ip)
       nleft = nleft - ninc
       !Increment time and day
       ihr = ihr + ninc
       if(ihr<24) return
       ijul = ijul + ihr/24_ip
       ihr  = mod(ihr,24)
       !Check for leap year
       if(mod(iyr,4).ne.0) then
          ileap = 0_ip
       elseif(mod(iyr,100).ne.0) then
          ileap = 1_ip
       elseif(mod(iyr,400).ne.0) then
          ileap = 0_ip
       else
          ileap = 1_ip
       end if
       !Update year
       if(ijul>365_ip+ileap) then
          iyr  = iyr  + 1_ip
          ijul = ijul - (365_ip+ileap)
       end if
    end do
    !
    return
  end subroutine time_timeincr
  !
  !
  !
END MODULE time
