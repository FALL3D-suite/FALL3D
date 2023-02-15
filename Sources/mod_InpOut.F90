!***************************************************************
!>
!> Module for (serial) input/output operations
!> @author
!> Arnau Folch
!>
!***************************************************************
MODULE InpOut
  use KindType
  use Shared, only : mproc, nens
  implicit none
  save
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: inpout_get_npar
  PUBLIC :: inpout_get_cha
  PUBLIC :: inpout_get_int
  PUBLIC :: inpout_get_rea
  PUBLIC :: inpout_get_problemname
  PUBLIC :: inpout_get_filenames
  PUBLIC :: inpout_open_log_file
  PUBLIC :: inpout_close_log_file
  PUBLIC :: inpout_check_file
  PUBLIC :: inpout_open_file
  PUBLIC :: inpout_close_file
  PUBLIC :: inpout_print_greeting
  PUBLIC :: inpout_print_args
  PUBLIC :: inpout_print_message
  PUBLIC :: inpout_get_file_nrows
  PUBLIC :: inpout_get_file_col
  PUBLIC :: inpout_get_file_pts
  PUBLIC :: inpout_get_file_hyb
  PUBLIC :: inpout_sdecode
  PUBLIC :: inpout_decode_timeunit
  PUBLIC :: stof
  PUBLIC :: stoi1
  PUBLIC :: upcase
  !
  !    LIST OF PRIVATE ROUTINES IN THE MODULE
  !
  PRIVATE :: decod1
  PRIVATE :: stof1
  PRIVATE :: replace_char
  !
  INTERFACE inpout_get_int
     MODULE PROCEDURE inpout_get_int_single
     MODULE PROCEDURE inpout_get_int_vector
  END INTERFACE inpout_get_int
  PRIVATE :: inpout_get_int_single
  PRIVATE :: inpout_get_int_vector
  !
  INTERFACE inpout_get_rea
     MODULE PROCEDURE inpout_get_rea_single
     MODULE PROCEDURE inpout_get_rea_vector
  END INTERFACE inpout_get_rea
  PRIVATE :: inpout_get_rea_single
  PRIVATE :: inpout_get_rea_vector
  !
  !    LIST OF PRIVATE VARIABLES IN THE MODULE
  !
  integer(ip), parameter, private :: s_long  = 512    !  Generic long string lenght. Use '(a512)' to read
  integer(ip), parameter, private :: nwormax = 128
  integer(ip), parameter, private :: nparmax = 128
  !
CONTAINS
  !
  !-----------------------------------
  !    subroutine inpout_open_log_file
  !-----------------------------------
  !
  !>   @brief
  !>   Opens the log file for a given task
  !
  subroutine inpout_open_log_file(task,MY_FILES,MY_ERR)
    implicit none
    !
    !>   @param task      task flag
    !>   @param MY_FILES  list of files
    !>   @param MY_ERR    error handler
    !
    integer(ip),       intent(IN   ) :: task
    type(FILE_LIST),   intent(IN   ) :: MY_FILES
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    character(len=8)  :: sdate
    character(len=10) :: stime,stask
    character(len=24) :: str
    integer(ip)       :: lulog
    integer(ip)       :: iyr,imo,idy,ihr,imi,ise
    !
    character(len=3), dimension(12) :: mo= (/'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec' /)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'inpout_open_log_file'
    MY_ERR%message = ' '
    !
    !*** Get system date and time from system clock
    !
    call DATE_AND_TIME(sdate,stime)
    !
    iyr = int(stof(sdate(1:4),4))
    imo = int(stof(sdate(5:6),2))
    idy = int(stof(sdate(7:8),2))
    ihr = int(stof(stime(1:2),2))
    imi = int(stof(stime(3:4),2))
    ise = int(stof(stime(5:6),2))
    !
    str = '00 mon 0000 at 00:00:00'
    write(str(1 :2 ),'(i2.2)') idy
    write(str(4 :6 ),'(a3)'  ) mo(imo)
    write(str(8 :11),'(i4.4)') iyr
    write(str(16:17),'(i2.2)') ihr
    write(str(19:20),'(i2.2)') imi
    write(str(22:23),'(i2.2)') ise
    !
    MY_ERR%cpu_start_date = 'Run start date '//TRIM(str)
    !
    !*** Opens and writes the log file
    !
    lulog = MY_FILES%lulog
    !
    select case(task)
    case(TASK_SET_ENS)
       stask = 'SetEns'
    case(TASK_SET_TGSD)
       stask = 'SetTgsd'
    case(TASK_SET_DBS)
       stask = 'SetDbs'
    case(TASK_SET_SRC)
       stask = 'SetSrc'
    case(TASK_RUN_FALL3D)
       stask = 'FALL3D'
    case(TASK_POS_ENS)
       stask = 'PosEns'
    case(TASK_POS_VAL)
       stask = 'PosVal'
    end select
    !
    open (lulog,file=TRIM(MY_FILES%file_log),status='unknown')
    write(lulog,1) VERSION,stask,str,rp,  &
         mproc(1)*mproc(2)*mproc(3), &
         mproc(1),mproc(2),mproc(3)
    !
1   format('---------------------------------------------------------',/, &
         '                      FALL3D suite                       ',/, &
         '  Version : ',a,'                                        ',/, &
         '  Task    : ',a,   '                                     ',/, &
         '                                                         ',/, &
         '  Copyright (C) 2018 GNU General Public License version 3',/, &
         '  See licence for details                                ',/, &
         '---------------------------------------------------------',/, &
         '  Run start time     : ',a ,/,&
         '  Real precision     : ',i8 ,/,&
         '  Num. processors    : ',i8,/,&
         '       npx           : ',i8,/,&
         '       npy           : ',i8,/,&
         '       npz           : ',i8)
    !
    select case(task)
    case(TASK_SET_ENS)
       !
       write(lulog,2) TRIM(MY_FILES%file_inp), &
            TRIM(MY_FILES%file_log), TRIM(MY_FILES%file_ens)
2      format(/,&
            '  INPUT FILES            '  ,/, &
            '  Input          file  : ',a,/, &
            '                         '  ,/, &
            '  OUTPUT FILES           '  ,/, &
            '  Log            file  : ',a,/, &
            '  Random numbers file  : ',a)
       !
    case(TASK_SET_TGSD)
       !
       write(lulog,3) TRIM(MY_FILES%file_inp), &
            TRIM(MY_FILES%file_log)
3      format(/,&
            '  INPUT FILES          '  ,/, &
            '  Input        file  : ',a,/, &
            '                       '  ,/, &
            '  OUTPUT FILES         '  ,/, &
            '  Log          file  : ',a)
       !
    case(TASK_SET_DBS)
       !
       write(lulog,4) TRIM(MY_FILES%file_inp), &
            TRIM(MY_FILES%file_log), &
            TRIM(MY_FILES%file_dbs), &
            TRIM(MY_FILES%file_pro)
4      format(/,&
            '  INPUT FILES          '  ,/, &
            '  Input        file  : ',a,/, &
            '                       '  ,/, &
            '  OUTPUT FILES         '  ,/, &
            '  Log          file  : ',a,/, &
            '  Meteo (dbs)  file  : ',a,/, &
            '  Meteo profil file  : ',a)
       !
    case(TASK_SET_SRC)
       !
       write(lulog,5) TRIM(MY_FILES%file_inp), &
            TRIM(MY_FILES%file_dbs), &
            TRIM(MY_FILES%file_pro), &
            TRIM(MY_FILES%file_log), &
            TRIM(MY_FILES%file_grn), &
            TRIM(MY_FILES%file_src)
5      format(/,&
            '  INPUT FILES          '  ,/, &
            '  Input        file  : ',a,/, &
            '  Meteo (dbs)  file  : ',a,/, &
            '  Meteo profil file  : ',a,/, &
            '                       '  ,/, &
            '  OUTPUT FILES         '  ,/, &
            '  Log          file  : ',a,/, &
            '  Granulometry file  : ',a,/, &
            '  Source       file  : ',a)
       !
    case(TASK_RUN_FALL3D)
       !
       write(lulog,6) TRIM(MY_FILES%file_inp), &
            TRIM(MY_FILES%file_src), &
            TRIM(MY_FILES%file_grn), &
            TRIM(MY_FILES%file_dbs), &
            TRIM(MY_FILES%file_pts), &
            TRIM(MY_FILES%file_log), &
            TRIM(MY_FILES%file_res), &
            TRIM(MY_FILES%file_rst)
6      format(/,&
            '  INPUT FILES          '  ,/, &
            '  Input        file  : ',a,/, &
            '  Source       file  : ',a,/, &
            '  Granulometry file  : ',a,/, &
            '  Meteo (dbs)  file  : ',a,/, &
            '  Points       file  : ',a,/, &
            '                       '  ,/, &
            '  OUTPUT FILES         '  ,/, &
            '  Log          file  : ',a,/, &
            '  Results      file  : ',a,/, &
            '  Restart      file  : ',a)
       !
    case(TASK_POS_ENS)
       !
       write(lulog,7) TRIM(MY_FILES%file_inp), &
                      TRIM(MY_FILES%file_log), &
                      TRIM(MY_FILES%file_pos)
7      format(/,&
            '  INPUT FILES            '  ,/, &
            '  Input          file  : ',a,/, &
            '                         '  ,/, &
            '  OUTPUT FILES           '  ,/, &
            '  Log            file  : ',a,/, &
            '  Results        file  : ',a)
       !
    case(TASK_POS_VAL)
       !
       write(lulog,8) TRIM(MY_FILES%file_inp), &
                      TRIM(MY_FILES%file_log)
8      format(/,&
            '  INPUT FILES            '  ,/, &
            '  Input          file  : ',a,/, &
            '                         '  ,/, &
            '  OUTPUT FILES           '  ,/, &
            '  Log            file  : ',a)
       !
    end select
    !
    return
  end subroutine inpout_open_log_file
  !
  !-----------------------------------
  !    subroutine inpout_close_log_file
  !-----------------------------------
  !
  !>   @brief
  !>   Close the log file for a given task
  !
  subroutine inpout_close_log_file(task,MY_FILES,MY_ERR)
    implicit none
    !
    !>   @param task      task flag
    !>   @param MY_FILES  list of files
    !>   @param MY_ERR    error handler
    !
    integer(ip),       intent(IN   ) :: task
    type(FILE_LIST),   intent(IN   ) :: MY_FILES
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    character(len=10) :: stask
    character(len=8)  :: sdate
    character(len=10) :: stime
    character(len=24) :: str
    integer(ip)       :: lulog,iwarn
    integer(ip)       :: iyr,imo,idy,ihr,imi,ise
    !
    character(len=3), dimension(12) :: mo= (/'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec' /)
    !
    !*** Initializations
    !
    lulog = MY_FILES%lulog
    !
    select case(task)
    case(TASK_SET_ENS)
       stask = 'SetEns'
    case(TASK_SET_TGSD)
       stask = 'SetTgsd'
    case(TASK_SET_DBS)
       stask = 'SetDbs'
    case(TASK_SET_SRC)
       stask = 'SetSrc'
    case(TASK_RUN_FALL3D)
       stask = 'FALL3D'
    case(TASK_POS_ENS)
       stask = 'PosEns'
    case(TASK_POS_VAL)
       stask = 'PosVal'
    end select
    !
    !*** Get system date and time from system clock
    !
    call CPU_TIME     (MY_ERR%cpu_end_time)
    call DATE_AND_TIME(sdate,stime)
    !
    iyr = int(stof(sdate(1:4),4))
    imo = int(stof(sdate(5:6),2))
    idy = int(stof(sdate(7:8),2))
    ihr = int(stof(stime(1:2),2))
    imi = int(stof(stime(3:4),2))
    ise = int(stof(stime(5:6),2))
    !
    str = '00 mon 0000 at 00:00:00'
    write(str(1 :2 ),'(i2.2)') idy
    write(str(4 :6 ),'(a3)'  ) mo(imo)
    write(str(8 :11),'(i4.4)') iyr
    write(str(16:17),'(i2.2)') ihr
    write(str(19:20),'(i2.2)') imi
    write(str(22:23),'(i2.2)') ise
    !
    write(lulog,1) str,(MY_ERR%cpu_end_time-MY_ERR%cpu_start_time)
1   format(/,&
         '  End time           : ',a,/,&
         '  CPU time (s)       : ',f10.0)
    !
    !*** Warning list
    !
    write(lulog,2) MY_ERR%nwarn
2   format(2x,'Number of warnings :',i2)
    do iwarn = 1,MY_ERR%nwarn
       write(lulog,3) iwarn,TRIM(MY_ERR%warning(iwarn))
    end do
3   format(2x,'Warning number     :',i2,' : ',a)
    !
    !*** Close file
    !
    if(MY_ERR%flag.ne.0) then
       !
       write(lulog,10) TRIM(MY_ERR%message),TRIM(MY_ERR%source)
10     format(2x,'Number of errors   : 1' ,/, &
            2x,'ERROR TYPE         : ',a,/, &
            2x,'ERROR SOURCE       : ',a)
       write(lulog,11) stask
11     format(2x,'Task ',a10,'    : ends ABNORMALLY',/)
       close(lulog)
    else
       !
       write(lulog,20) stask
20     format(2x,'Number of errors   : 0',/, &
            2x,'Task ',a10,'    : ends NORMALLY',/)
       close(lulog)
    end if
    !
    return
  end subroutine inpout_close_log_file
  !
  !-----------------------------------
  !    subroutine inpout_check_file
  !-----------------------------------
  !
  !>   @brief
  !>   Check file existence
  !
  subroutine inpout_check_file(fname,MY_ERR)
    implicit none
    !
    !>   @param fname     file name
    !>   @param MY_ERR    error handler
    !
    character(len=s_file), intent(IN   ) :: fname
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    logical :: exist_file
    !
    inquire(file=TRIM(fname),exist=exist_file)
    !
    if(exist_file) then
        MY_ERR%flag    = 0
        MY_ERR%message = ' '
    else
        MY_ERR%flag    = -1
        MY_ERR%source  = 'inpout_open_file'
        MY_ERR%message = 'Error opening file '//TRIM(fname)
    end if
    !
    return
  end subroutine inpout_check_file
  !
  !-----------------------------------
  !    subroutine inpout_open_file
  !-----------------------------------
  !
  !>   @brief
  !>   Opens a file
  !
  subroutine inpout_open_file(lu,fname,MY_ERR)
    implicit none
    !
    !>   @param lu        logical unit
    !>   @param fname     file name
    !>   @param MY_ERR    error handler
    !
    integer(ip),           intent(IN   ) :: lu
    character(len=s_file), intent(IN   ) :: fname
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%message = ' '
    !
    open(lu,file=TRIM(fname),status='unknown',err=100)
    return
    !
100 MY_ERR%flag    = -1
    MY_ERR%source  = 'inpout_open_file'
    MY_ERR%message = 'error opening file '//TRIM(fname)
    return
  end subroutine inpout_open_file
  !
  !-----------------------------------
  !    subroutine inpout_close_file
  !-----------------------------------
  !
  !>   @brief
  !>   Closes a file
  !
  subroutine inpout_close_file(lu,MY_ERR)
    implicit none
    !
    !>   @param lu        logical unit
    !>   @param MY_ERR    error handler
    !
    integer(ip),           intent(IN   ) :: lu
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%message = ' '
    !
    close(lu)
    return
  end subroutine inpout_close_file
  !
  !-------------------------------------
  !    subroutine inpout_get_problemname
  !-------------------------------------
  !
  !>   @brief
  !>   Retrives problem name and path for input file
  !
  subroutine inpout_get_problemname(MY_FILES,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),   intent(INOUT) :: MY_FILES
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    logical     :: search, found
    integer(ip) :: ipos,ip2,ip1
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'inpout_get_problemname'
    MY_ERR%message = ' '
    !
    !*** search for '.'
    !
    ipos   = LEN_TRIM(MY_FILES%file_inp)
    found  = .false.
    search = .true.
    if(ipos.eq.0) search = .false.
    do while(search)
       if(MY_FILES%file_inp(ipos:ipos).eq.'.') then
          found  = .true.
          search = .false.
       else
          ipos = ipos - 1
       end if
       if(ipos.eq.0) search = .false.
    end do
    if(found) then
       ip2 = ipos
    else
       ip2 = 1
    end if
    !
    !*** search for '/'
    !
    ipos   = LEN_TRIM(MY_FILES%file_inp)
    found  = .false.
    search = .true.
    if(ipos.eq.0) search = .false.
    do while(search)
       if(MY_FILES%file_inp(ipos:ipos).eq.'/') then
          found  = .true.
          search = .false.
       else
          ipos = ipos - 1
       end if
       if(ipos.eq.0) search = .false.
    end do
    if(found) then
       ip1 = ipos
    else
       ip1 = 1
    end if
    !
    if(ip1.eq.1) then
       MY_FILES%problempath = '.'
       if(ip2.eq.1) then
          MY_FILES%problemname = MY_FILES%file_inp
       else
          MY_FILES%problemname = MY_FILES%file_inp(1:ip2-1)
       end if
    else
       MY_FILES%problempath = MY_FILES%file_inp(1:ip1-1)
       if(ip2.eq.1) then
          MY_FILES%problemname = MY_FILES%file_inp(ip1+1:LEN_TRIM(MY_FILES%file_inp))
       else
          MY_FILES%problemname = MY_FILES%file_inp(ip1+1:ip2-1)
       end if
    end if
    !
    MY_FILES%commonpath = MY_FILES%problempath
    !
    return
  end subroutine inpout_get_problemname
  !
  !-------------------------------------
  !    subroutine inpout_get_filenames
  !-------------------------------------
  !
  !>   @brief
  !>   Sets the filenames depending on the task
  !
  subroutine inpout_get_filenames(task,MY_FILES,MY_ERR)
    implicit none
    !
    !>   @param task      task flag
    !>   @param MY_FILES  list of files
    !>   @param MY_ERR    error handler
    !
    integer(ip),       intent(IN   ) :: task
    type(FILE_LIST),   intent(INOUT) :: MY_FILES
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'inpout_get_filenames'
    MY_ERR%message = ' '
    !
    select case(task)
    case(TASK_SET_ENS)
       !
       MY_FILES%file_log  = TRIM(MY_FILES%commonpath)//'/'//TRIM(MY_FILES%problemname)//'.SetEns.log'
       MY_FILES%file_ens  = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.ens'
       !
    case(TASK_SET_TGSD)
       !
       MY_FILES%file_log  = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.SetTgsd.log'
       MY_FILES%file_tgsd = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.tgsd'
       !
    case(TASK_SET_DBS)
       !
       MY_FILES%file_log  = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.SetDbs.log'
       MY_FILES%file_dbs  = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.dbs.nc'
       MY_FILES%file_met  = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.wrf.nc'      ! default value
       MY_FILES%file_pro  = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.dbs.pro'
       MY_FILES%file_hyb  = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.hyb'         ! default value
       !
    case(TASK_SET_SRC)
       !
       MY_FILES%file_log  = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.SetSrc.log'
       MY_FILES%file_tgsd = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.tgsd'
       MY_FILES%file_dbs  = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.dbs.nc'
       MY_FILES%file_pro  = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.dbs.pro'
       MY_FILES%file_grn  = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.grn'
       MY_FILES%file_src  = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.src'
       MY_FILES%file_plu  = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.plume'
       !
    case(TASK_RUN_FALL3D)
       !
       MY_FILES%file_log  = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.Fall3d.log'
       MY_FILES%file_dbs  = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.dbs.nc'
       MY_FILES%file_grn  = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.grn'
       MY_FILES%file_src  = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.src'
       MY_FILES%file_pts  = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.pts'
       MY_FILES%file_res  = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.res.nc'
       MY_FILES%file_rst  = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.YYYY-MM-DD-HH-MM.rst.nc'
       MY_FILES%file_gc   = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.gc.res'
       !
    case(TASK_POS_ENS)
       !
       MY_FILES%file_log  = TRIM(MY_FILES%commonpath)//'/'//TRIM(MY_FILES%problemname)//'.PosEns.log'
       MY_FILES%file_res  = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.res.nc'
       MY_FILES%file_pos  = TRIM(MY_FILES%commonpath)//'/'//TRIM(MY_FILES%problemname)//'.ens.nc'
       !
    case(TASK_POS_VAL)
       !
       MY_FILES%file_log  = TRIM(MY_FILES%commonpath)//'/'//TRIM(MY_FILES%problemname)//'.PosVal.log'
       !
    end select
    !
    return
  end subroutine inpout_get_filenames
  !
  !-----------------------------------
  !    subroutine inpout_print_greeting
  !-----------------------------------
  !
  !>   @brief
  !>   Prints a greeting on screen
  !
  subroutine inpout_print_greeting(npes_world, fname)
      implicit none
      !
      !>   @param npes_world    total number of procesors
      !>   @param fname         name of the input file
      !
      integer(ip),      intent(IN) :: npes_world
      character(len=*), intent(IN) :: fname
      !
      write(*,1) VERSION,TRIM(fname),npes_world,mproc,nens
1 format('-----------------------------------------------------------',/, &
         '                                                           ',/, &
         '           ______      _      _      ____  _____           ',/, &
         '          |  ____/\   | |    | |    |___ \|  __ \          ',/, &
         '          | |__ /  \  | |    | |      __) | |  | |         ',/, &
         '          |  __/ /\ \ | |    | |     |__ <| |  | |         ',/, &
         '          | | / ____ \| |____| |____ ___) | |__| |         ',/, &
         '          |_|/_/    \_\______|______|____/|_____/          ',/, &
         '                                                           ',/, &
         '                                                           ',/, &
         '                 Initializing FALL3D suite                 ',/, &
         '                                                           ',/, &
         '   Copyright: 2018 GNU General Public License version 3    ',/, &
         '                 (see licence for details)                 ',/, &
         '                                                           ',/, &
         '  Version               : ',a                               ,/, &
         '  Input File            : ',a                               ,/, &
         '  Number of processors  : ',i5.5                            ,/, &
         '  Number of sub-domains : ',i5.5,' x ',i5.5,' x ',i5.5      ,/, &
         '  Number of instances   : ',i5.5                            ,/, &
         '                                                           ',/, &
         '-----------------------------------------------------------'    &
        )
      !
      return
  end subroutine inpout_print_greeting
  !
  !-----------------------------------
  !    subroutine inpout_print_args
  !-----------------------------------
  !
  !>   @brief
  !>   Prints on screen the correct usage of call arguments
  !
  subroutine inpout_print_args
    implicit none
    !
    write(*,1) VERSION
    !
1   format('--------------------------------------------------------------------------',/, &
           '                                                                          ',/, &
           '                ______      _      _      ____  _____                     ',/, &
           '               |  ____/\   | |    | |    |___ \|  __ \                    ',/, &
           '               | |__ /  \  | |    | |      __) | |  | |                   ',/, &
           '               |  __/ /\ \ | |    | |     |__ <| |  | |                   ',/, &
           '               | | / ____ \| |____| |____ ___) | |__| |                   ',/, &
           '               |_|/_/    \_\______|______|____/|_____/                    ',/, &
           '                                                                          ',/, &
           '                                                                          ',/, &
           '       Copyright: 2018 GNU General Public License version 3               ',/, &
           '                    (see licence for details)                             ',/, &
           '                                                                          ',/, &
           '  model version: ',a,'                                                    ',/, &
           '                                                                          ',/, &
           '  usage:                                                                  ',/, &
           '   Fall3d.x Task InputFile [NPX] [NPY] [NPZ] [-nens SIZE]                 ',/, &
           '                                                                          ',/, &
           '  positional arguments:                                                   ',/, &
           '   Task (single)   : SetTgsd, SetDbs, SetSrc, Fall3d, PosEns, PosVal      ',/, &
           '   Task (multiple) : All (1-4 above)                                      ',/, &
           '   InputFile       : Parameter input file                                 ',/, &
           '                                                                          ',/, &
           '  optional arguments:                                                     ',/, &
           '   NPX        : processors (sub-domains) along x (default NPX =1 )        ',/, &
           '   NPY        : processors (sub-domains) along y (default NPY =1 )        ',/, &
           '   NPZ        : processors (sub-domains) along z (default NPZ =1 )        ',/, &
           '   -nens SIZE : ensemble size                    (default SIZE=1 )        ',/, &
           '                                                                          ',/, &
           '  note:                                                                   ',/, &
           '   For parallel runs it is required that  NPROC = NPX * NPY * NPZ * SIZE  ',/, &
           '                                                                          ',/, &
           '  examples:                                                               ',/, &
           '   1. Run SetTgsd utility                                                 ',/, &
           '   > Fall3d.x SetTgsd problemname.inp [-nens ENS_SIZE]                    ',/, &
           '                                                                          ',/, &
           '   2. Run SetDbs utility                                                  ',/, &
           '   > Fall3d.x SetDbs problemname.inp [NPX NPY NPZ -nens ENS_SIZE]         ',/, &
           '                                                                          ',/, &
           '   3. Run SetSrc utility                                                  ',/, &
           '   > Fall3d.x SetSrc problemname.inp [NPX NPY NPZ -nens ENS_SIZE]         ',/, &
           '                                                                          ',/, &
           '   4. Run Fall3d solver                                                   ',/, &
           '   > Fall3d.x Fall3d problemname.inp [NPX NPY NPZ -nens ENS_SIZE]         ',/, &
           '                                                                          ',/, &
           '   5. Run PosEns utility                                                  ',/, &
           '   > Fall3d.x PosEns problemname.inp [-nens ENS_SIZE]                     ',/, &
           '                                                                          ',/, &
           '   6. Run PosVal utility                                                  ',/, &
           '   > Fall3d.x PosVal problemname.inp [NPX NPY]                            ',/, &
           '                                                                          ',/, &
           '   7. Run tasks 1-4 above consecutively                                   ',/, &
           '   > Fall3d.x All problemname.inp [NPX NPY NPZ -nens ENS_SIZE]            ',/, &
           '                                                                          ',/, &
           '--------------------------------------------------------------------------')
    !
    return
  end subroutine inpout_print_args
  !
  !-----------------------------------
  !    subroutine inpout_print_message
  !-----------------------------------
  !
  !>   @brief
  !>   Prints on screen Error: file not found
  !
  subroutine inpout_print_message(message)
    implicit none
    !
    !>   @param message        message to be displayed
    !
    character(len=*), intent(IN) :: message
    !
    write(*,*) trim(message)
    !
    return
  end subroutine inpout_print_message
  !
  !-----------------------------------
  !    subroutine inpout_get_npar
  !-----------------------------------
  !
  !>   @brief
  !>   Gets the number of parameters associated to a line in a block
  !
  subroutine inpout_get_npar(fname,sblock,line,npar,MY_ERR)
    implicit none
    !
    !>   @param fname   name of the input file
    !>   @param sblock  block in which to search
    !>   @param line    target line in the block
    !>   @param line    target line in the block
    !>   @param npar    number of parameters
    !>   @param MY_ERR  error handler
    !
    character(len=*),  intent(IN   ) :: fname
    character(len=*),  intent(IN   ) :: sblock
    character(len=*),  intent(IN   ) :: line
    integer(ip),       intent(INOUT) :: npar
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    character(len=s_long) :: card
    character(len=s_long) :: words(nwormax)
    logical               :: linefound,blockfound
    integer(ip)           :: nword
    real(rp)              :: param(nparmax)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'inpout_get_npar'
    MY_ERR%message = ' '
    !
    words(:)(:) = ' '
    !
    !*** Opens the file
    !
    open(90,FILE=TRIM(fname),STATUS='old',ERR=101)
    !
    !*** Search the line
    !
    blockfound = .false.
    linefound  = .false.
    do while(.not.linefound)
       do while(.not.blockfound)
          read(90,'(a512)',END=102) card
          call inpout_sdecode(card,words,param,nword,npar)
          if(words(1)(1:LEN_TRIM(sblock)) == sblock(1:LEN_TRIM(sblock))) &
               blockfound=.true.
       end do
       read(90,'(a512)',END=103) card
       call inpout_sdecode(card,words,param,nword,npar)
       if(words(1)(1:LEN_TRIM(line)) == line(1:LEN_TRIM(line))) &
            linefound = .true.
    end do
    !
    !***  Successful end
    !
    close(90)
    return
    !
    !***  List of errors
    !
101 MY_ERR%flag = -1
    close(90)
    MY_ERR%message = 'inpout_get_npar : error opening the input file '//TRIM(fname)
    return
    !
102 MY_ERR%flag = -1
    close(90)
    MY_ERR%message = 'inpout_get_npar : block '//TRIM(sblock)// ' not found in the input file'
    return
    !
103 MY_ERR%flag = -1
    close(90)
    MY_ERR%message = 'inpout_get_npar : line '//TRIM(line)// ' not found in the input file'
    return
    !
  end subroutine inpout_get_npar
  !
  !-----------------------------------
  !    subroutine inpout_get_cha
  !-----------------------------------
  !
  !>   @brief
  !>   Gets nval character values associated to a line in a block
  !>   @details
  !>   Words are converted to UPPER case unless the optional argument uppercase=.false. is given
  !
  subroutine inpout_get_cha(fname,sblock,line,cvalue,nval,MY_ERR,uppercase)
    implicit none
    !
    !>   @param fname     name of the input file
    !>   @param sblock    block in which to search
    !>   @param line      target line in the block
    !>   @param cvalue    cvalue(nval) values of the nval characters (words) read
    !>   @param nval      number of characters (words) to read
    !>   @param MY_ERR    error handler
    !>   @param uppercase optional parameter (default=.true.)
    !
    integer(ip),       intent(IN   ) :: nval
    character(len=*),  intent(IN   ) :: fname
    character(len=*),  intent(IN   ) :: sblock
    character(len=*),  intent(IN   ) :: line
    character(len=*),  intent(INOUT) :: cvalue(nval)
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    logical, optional ,intent(IN   ) :: uppercase
    !
    character(len=s_long) :: card
    character(len=s_long) :: words(nwormax)
    logical               :: linefound,blockfound
    integer(ip)           :: nword,npar,ival,j
    real(rp)              :: param(nparmax)
    logical               :: to_uppercase
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'inpout_get_npar'
    MY_ERR%message = ' '
    !
    words(:)(:) = ' '
    if(present(uppercase)) then
       to_uppercase = uppercase
    else
       to_uppercase = .true.   ! default value
    end if
    !
    !***  Opens the file
    !
    open(90,FILE=TRIM(fname),STATUS='old',ERR=101)
    !
    !***  Search the line
    !
    blockfound = .false.
    linefound  = .false.
    do while(.not.linefound)
       do while(.not.blockfound)
          read(90,'(a512)',END=102) card
          call inpout_sdecode(card,words,param,nword,npar)
          if(words(1)(1:LEN_TRIM(sblock)).eq.sblock(1:LEN_TRIM(sblock))) blockfound=.true.
       end do
       read(90,'(a512)',END=103) card
       call inpout_sdecode(card,words,param,nword,npar)
       if(words(1)(1:LEN_TRIM(line)).eq.line(1:LEN_TRIM(line))) linefound = .true.
    end do
    !
    if((nword-1).lt.nval) goto 104
    !
    do ival = 1,nval
       cvalue(ival)(1:LEN_TRIM(words(ival+1))) = words(ival+1)(1:LEN_TRIM(words(ival+1)))
       !
       !***     Fill the rest with ' '
       !
       do j=LEN_TRIM(words(ival+1))+1,LEN(cvalue(ival))
          cvalue(ival)(j:j)=' '
       end do
    end do
    !
    !***  Convert to upper case
    !
    if(to_uppercase) then
       do ival = 1,nval
          call upcase(cvalue(ival))
       end do
    end if
    !
    !***  Successful end
    !
    close(90)
    return
    !
    !***  List of errors
    !
101 MY_ERR%flag = -1
    close(90)
    MY_ERR%message = 'inpout_get_cha: error opening the input file '//TRIM(fname)
    return
    !
102 MY_ERR%flag = -1
    close(90)
    MY_ERR%message = 'inpout_get_cha: block '//TRIM(sblock)// ' not found in the input file'
    return
    !
103 MY_ERR%flag = -1
    close(90)
    MY_ERR%message = 'inpout_get_cha: line '//TRIM(line)// ' not found in the input file'
    return
    !
104 MY_ERR%flag = -1
    close(90)
    MY_ERR%message = 'inpout_get_cha: too few parameters in line '//TRIM(line)
    return
    !
  end subroutine inpout_get_cha
  !
  !------------------------------------
  !    subroutine inpout_get_int_single
  !------------------------------------
  !
  !>   @brief
  !>   Gets one integer value associated to a line in a block
  !
  subroutine inpout_get_int_single(fname,sblock,line,ivalue,nval,MY_ERR)
    implicit none
    !
    !>   @param fname     name of the input file
    !>   @param sblock    block in which to search
    !>   @param line      target line in the block
    !>   @param ivalue    value of the integer read
    !>   @param nval      number of integers to read
    !>   @param MY_ERR    error handler
    !
    character(len=*),  intent(IN   ) :: fname
    character(len=*),  intent(IN   ) :: sblock
    character(len=*),  intent(IN   ) :: line
    integer(ip),       intent(INOUT) :: ivalue
    integer(ip),       intent(IN   ) :: nval
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    integer(ip) :: ivoid(1)
    !
    call inpout_get_int_vector(fname,sblock,line,ivoid,nval,MY_ERR)
    ivalue = ivoid(1)
    return
    !
  end subroutine inpout_get_int_single
  !
  !------------------------------------
  !    subroutine inpout_get_rea_single
  !------------------------------------
  !
  !>   @brief
  !>   Gets one real value associated to a line in a block
  !
  subroutine inpout_get_rea_single(fname,sblock,line,rvalue,nval,MY_ERR)
    implicit none
    !
    !>   @param fname     name of the input file
    !>   @param sblock    block in which to search
    !>   @param line      target line in the block
    !>   @param rvalue    value of the real read
    !>   @param nval      number of reals to read
    !>   @param MY_ERR    error handler
    !
    character(len=*),  intent(IN   ) :: fname
    character(len=*),  intent(IN   ) :: sblock
    character(len=*),  intent(IN   ) :: line
    real(rp),          intent(INOUT) :: rvalue
    integer(ip),       intent(IN   ) :: nval
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    real(rp) :: rvoid(1)
    !
    call inpout_get_rea_vector(fname,sblock,line,rvoid,nval,MY_ERR)
    rvalue = rvoid(1)
    return
    !
  end subroutine inpout_get_rea_single
  !
  !------------------------------------
  !    subroutine inpout_get_int_vector
  !------------------------------------
  !
  !>   @brief
  !>   Gets nval integer values associated to a line in a block
  !
  subroutine inpout_get_int_vector(fname,sblock,line,ivalue,nval,MY_ERR)
    implicit none
    !
    !>   @param fname     name of the input file
    !>   @param sblock    block in which to search
    !>   @param line      target line in the block
    !>   @param ivalue    values of the integers read
    !>   @param nval      number of integers to read
    !>   @param MY_ERR    error handler
    !
    character(len=*),  intent(IN   ) :: fname
    character(len=*),  intent(IN   ) :: sblock
    character(len=*),  intent(IN   ) :: line
    integer(ip),       intent(IN   ) :: nval
    integer(ip),       intent(INOUT) :: ivalue(nval)
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    character(len=s_long) :: card
    character(len=s_long) :: words(nwormax)
    logical               :: linefound,blockfound
    integer(ip)           :: nword,npar,ival
    real(rp)              :: param(nparmax)
    !
    !***  Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'inpout_get_int'
    MY_ERR%message = ' '
    !
    words(:)(:) = ' '
    !
    !*** Opens the file
    !
    open(90,FILE=TRIM(fname),STATUS='old',ERR=101)
    !
    !*** Search the line
    !
    blockfound = .false.
    linefound  = .false.
    do while(.not.linefound)
       do while(.not.blockfound)
          read(90,'(a512)',END=102) card
          call inpout_sdecode(card,words,param,nword,npar)
          if(words(1)(1:LEN_TRIM(sblock)).eq.sblock(1:LEN_TRIM(sblock))) blockfound=.true.
       end do
       read(90,'(a512)',END=103) card
       call inpout_sdecode(card,words,param,nword,npar)
       if(words(1)(1:LEN_TRIM(line)).eq.line(1:LEN_TRIM(line))) linefound = .true.
    end do
    !
    if(npar.lt.nval) goto 105
    !
    do ival = 1,nval
       ivalue(ival) = INT(param(ival))
    end do
    !
    !***  Successful end
    !
    close(90)
    return
    !
    !***  List of errors
    !
101 MY_ERR%flag = -1
    close(90)
    MY_ERR%message = 'get_input_int: error opening the input file '//TRIM(fname)
    return
    !
102 MY_ERR%flag = -1
    close(90)
    MY_ERR%message = 'get_input_int: block '//TRIM(sblock)// ' not found in the input file'
    return
    !
103 MY_ERR%flag = -1
    close(90)
    MY_ERR%message = 'get_input_int: line '//TRIM(line)// ' not found in the input file'
    return
    !
105 MY_ERR%flag = 1
    close(90)
    MY_ERR%message = 'get_input_int: too few parameters in line '//TRIM(line)
    return
    !
  end subroutine inpout_get_int_vector
  !
  !------------------------------------
  !    subroutine inpout_get_rea_vector
  !------------------------------------
  !
  !>   @brief
  !>   Gets nval real values associated to a line in a block
  !
  subroutine inpout_get_rea_vector(fname,sblock,line,rvalue,nval,MY_ERR)
    implicit none
    !
    !>   @param fname     name of the input file
    !>   @param sblock    block in which to search
    !>   @param line      target line in the block
    !>   @param rvalue    values of the reals read
    !>   @param nval      number of reals to read
    !>   @param MY_ERR    error handler
    !
    character(len=*),  intent(IN   ) :: fname
    character(len=*),  intent(IN   ) :: sblock
    character(len=*),  intent(IN   ) :: line
    integer(ip),       intent(IN   ) :: nval
    real(rp),          intent(INOUT) :: rvalue(nval)
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    character(len=s_long) :: card
    character(len=s_long) :: words(nwormax)
    logical               :: linefound,blockfound
    integer(ip)           :: nword,npar,ival
    real(rp)              :: param(nparmax)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'inpout_get_rea'
    MY_ERR%message = ' '
    !
    words(:)(:) = ' '
    !
    !*** Opens the file
    !
    open(90,FILE=TRIM(fname),STATUS='old',ERR=101)
    !
    !*** Search the line
    !
    blockfound = .false.
    linefound  = .false.
    do while(.not.linefound)
       do while(.not.blockfound)
          read(90,'(a512)',END=102) card
          call inpout_sdecode(card,words,param,nword,npar)
          if(words(1)(1:LEN_TRIM(sblock)).eq.sblock(1:LEN_TRIM(sblock))) blockfound=.true.
       end do
       read(90,'(a512)',END=103) card
       call inpout_sdecode(card,words,param,nword,npar)
       if(words(1)(1:LEN_TRIM(line)).eq.line(1:LEN_TRIM(line))) linefound = .true.
    end do
    !
    if(npar.lt.nval) goto 105
    !
    do ival = 1,nval
       rvalue(ival) = param(ival)
    end do
    !
    !***  Successful end
    !
    close(90)
    return
    !
101 MY_ERR%flag = -1
    close(90)
    MY_ERR%message = 'inpout_get_rea: error opening the input file '//TRIM(fname)
    return
    !
102 MY_ERR%flag = -1
    close(90)
    MY_ERR%message = 'inpout_get_rea: block '//TRIM(sblock)// ' not found in the input file'
    return
    !
103 MY_ERR%flag = -1
    close(90)
    MY_ERR%message = 'inpout_get_rea: line '//TRIM(line)// ' not found in the input file'
    return
    !
105 MY_ERR%flag = -1
    close(90)
    MY_ERR%message = 'inpout_get_rea: too few parameters in line '//TRIM(line)
    return
    !
  end subroutine inpout_get_rea_vector
  !
  !------------------------------------
  !    subroutine inpout_get_file_nrows
  !------------------------------------
  !
  !>   @brief
  !>   Gets the number of rows in a file
  !
  subroutine inpout_get_file_nrows(fname,n,MY_ERR)
    implicit none
    !
    !>   @param fname     name of the input file
    !>   @param nval      number file rows
    !>   @param MY_ERR    error handler
    !
    character(len=*),  intent(IN   ) :: fname
    integer(ip),       intent(INOUT) :: n
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    character(len=s_long) :: card
    character(len=s_long) :: words(nwormax)
    integer(ip)           :: nword,npar
    real(rp)              :: param(nparmax)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%message = ' '
    !
    open(99,FILE=TRIM(fname),status='old',err=100)
    n = 0
    do
       read(99,'(a512)',END=10) card
       call inpout_sdecode(card,words,param,nword,npar)
       if(npar.gt.0) n = n + 1
    end do
10  close(99)
    return
    !
100 MY_ERR%flag = -1
    MY_ERR%source  = 'inpout_get_file_nrows'
    MY_ERR%message = 'error opening file '//TRIM(fname)
    return
    !
  end subroutine inpout_get_file_nrows
  !
  !------------------------------------
  !    subroutine inpout_get_file_col
  !------------------------------------
  !
  !>   @brief
  !>   Gets nval values from column icol in a file
  !
  subroutine inpout_get_file_col(fname,icol,val,nval,MY_ERR)
    implicit none
    !
    !>   @param fname     name of the input file
    !>   @param icol      column to read
    !>   @param val       val(nval) returned values
    !>   @param nval      number of rows to read
    !>   @param MY_ERR    error handler
    !
    character(len=*),  intent(IN   ) :: fname
    integer(ip),       intent(IN   ) :: icol
    integer(ip),       intent(IN   ) :: nval
    real(rp),          intent(INOUT) :: val(nval)
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    integer(rp) :: ival,i
    real(rp)    :: rvoid
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%message = ' '
    !
    open(99,FILE=TRIM(fname),status='old',err=100)
    do ival = 1,nval
       read(99,*,end=101,err=101) (rvoid,i=1,icol-1), val(ival)
    end do
    close(99)
    return
    !
100 MY_ERR%flag = -1
    MY_ERR%source  = 'inpout_get_file_col'
    MY_ERR%message = 'error opening file '//TRIM(fname)
    return
101 MY_ERR%flag = -1
    MY_ERR%source  = 'inpout_get_file_col'
    MY_ERR%message = 'error reading file '//TRIM(fname)
    return
    !
  end subroutine inpout_get_file_col
  !
  !------------------------------------
  !    subroutine inpout_get_file_pts
  !------------------------------------
  !
  !>   @brief
  !>   Reads the pts file with the names and coordinates of points to track
  !
  subroutine inpout_get_file_pts(fname,MY_PTS,MY_ERR)
    implicit none
    !
    !>   @param fname     name of the pts file
    !>   @param MY_PTS    tracking points structure
    !>   @param MY_ERR    error handler
    !
    character(len=*),      intent(IN   ) :: fname
    type(TRACKING_POINTS), intent(INOUT) :: MY_PTS
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    character(len=s_name) :: cvoid
    character(len=s_long) :: card
    character(len=s_long) :: words(nwormax)
    integer(ip)           :: nword, npar
    integer(ip)           :: npts,ipts
    real(rp)              :: param(nwormax)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'inpout_get_file_pts'
    MY_ERR%message = ' '
    !
    open(99,FILE=TRIM(fname),status='old',err=100)
    npts = 0
    do
       read(99,*,end=10,err=101) cvoid    ! Read a real value
       npts = npts + 1
    end do
10  rewind(99)
    !
    !*** Allocates memory
    !
    MY_PTS%npts = npts
    allocate(MY_PTS%name_pts(npts))
    allocate(MY_PTS%xpts    (npts))
    allocate(MY_PTS%ypts    (npts))
    allocate(MY_PTS%zpts    (npts))
    !
    do ipts = 1,npts
       read(99,'(a512)',end=10,err=101) card
       call inpout_sdecode(card,words,param,nword,npar)
       if(nword.eq.0) then
          MY_ERR%flag = -1
          MY_ERR%message = 'no name for points in file '//TRIM(fname)
       else
          MY_PTS%name_pts(ipts) = words(1)(1:s_name)
       end if
       !
       if(npar.lt.2) then
          MY_ERR%flag = -1
          MY_ERR%message = 'no coordinates for points in file '//TRIM(fname)
          return
       else if(npar.eq.2) then
          MY_PTS%xpts(ipts) = param(1)
          MY_PTS%ypts(ipts) = param(2)
          MY_PTS%zpts(ipts) = -1.0_rp
       else if(npar.gt.2) then
          MY_PTS%xpts(ipts) = param(1)
          MY_PTS%ypts(ipts) = param(2)
          MY_PTS%zpts(ipts) = param(3)
       end if
    end do
    close(99)
    return
    !
100 MY_ERR%flag = -1
    MY_ERR%message = 'error opening file '//TRIM(fname)
    return
101 MY_ERR%flag = -1
    MY_ERR%message = 'error reading file '//TRIM(fname)
    return
    !
  end subroutine inpout_get_file_pts
  !
  !------------------------------------
  !    subroutine inpout_get_file_hyb
  !------------------------------------
  !
  !>   @brief
  !>   Reads the coefficient a,b for hybrid vertical coordinates
  !
  subroutine inpout_get_file_hyb(fname,nz,a,b,zreversed,MY_ERR)
    implicit none
    !
    !>   @param fname     name of the hyb file
    !>   @param nz        number of vertical levels
    !>   @param a         coefficient a(0:nz) (half levels)
    !>   @param b         coefficient b(0:nz) (half levels)
    !>   @param zreversed states if vertical levels are reversed
    !>   @param MY_ERR    error handler
    !
    character(len=*),      intent(IN   ) :: fname
    integer(ip),           intent(IN   ) :: nz
    real(rp),              intent(INOUT) :: a(0:nz)
    real(rp),              intent(INOUT) :: b(0:nz)
    logical,               intent(IN   ) :: zreversed
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    integer(ip)           :: nlevs, ilev, i
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'inpout_get_file_hyb'
    MY_ERR%message = ' '
    !
    open(99,FILE=TRIM(fname),status='old',err=100)
    nlevs = 0
    do
       read(99,*,end=10,err=101) ilev
       nlevs = nlevs + 1
    end do
10  rewind(99)
    nlevs = nlevs - 1
    !
    if(nlevs.ne.nz) then
       MY_ERR%flag    = -1
       MY_ERR%message = 'Incorrect number of full level in file '//TRIM(fname)
       return
    end if

    if(zreversed) then
       do i = nlevs,0,-1
          read(99,*,err=101) ilev, a(i), b(i)
       end do
    else
       do i = 0,nlevs
          read(99,*,err=101) ilev, a(i), b(i)
       end do
    end if
    close(99)
    return
    !
100 MY_ERR%flag = -1
    MY_ERR%message = 'error opening file '//TRIM(fname)
    return
101 MY_ERR%flag = -1
    MY_ERR%message = 'error reading file '//TRIM(fname)
    return
    !
  end subroutine inpout_get_file_hyb
  !
  !------------------------------------
  !    subroutine inpout_sdecode
  !------------------------------------
  !
  !>   @brief
  !>   Decodes a string card(s_long) into words and parameters
  !
  subroutine inpout_sdecode(card0,words,param,nword,npar)
    implicit none
    !
    !>   @param card    line to decode
    !>   @param words   list of words
    !>   @param param   list of parameters
    !>   @param nword   number of words
    !>   @param npar    number of parameters
    !
    character(len=s_long), intent(IN   ) :: card0
    character(len=s_long), intent(INOUT) :: words(nwormax)
    real(rp),              intent(INOUT) :: param(nparmax)
    integer(ip),           intent(INOUT) :: nword
    integer(ip),           intent(INOUT) :: npar
    !
    logical               :: comment_found
    character(len=s_long) :: card
    character(len=1)      :: sstring(s_long)
    integer(ip)           :: ipos,first,last,nstr,lflag,i,ii
    real(rp)              :: digit
    !
    !*** Initializations
    !
    nword   = 0
    npar    = 0
    ipos    = 0
    !
    comment_found = .false.
    do i = 1,s_long
       if(card0(i:i).eq.'!') comment_found = .true.   ! remove anything after a comment (!)
       if(comment_found) then
          card(i:i) = ' '
       else
          card(i:i) = card0(i:i)
       end if
       if(card0(i:i).eq.'|') card(i:i) = ' '          ! replace characer '|' by blank space
    end do
    !
    !*** Start decoding
    !
    do while(1.ne.0)
       !                                    ! Find first position
       ipos = ipos + 1
       if(ipos.gt.s_long) return
10     if(card(ipos:ipos).eq.' '.or.card(ipos:ipos).eq.'=') then
          ipos = ipos + 1
          if(ipos.gt.s_long) return
          go to 10
       end if
       first = ipos    ! First position found
       !
       ipos = ipos + 1
       if(ipos.gt.s_long) return
20     if(card(ipos:ipos).ne.' '.and.card(ipos:ipos).ne.'=') then
          ipos = ipos + 1
          if(ipos.gt.s_long) return
          go to 20
       end if
       last = ipos-1    ! Last position found
       !
       nstr = last-first+1

       ii = 0
       do i=first,last
          ii = ii + 1
          sstring(ii) = card(i:i)
       end do
       call decod1(sstring,nstr,lflag,digit)
       if(lflag.eq.0) then
          npar = npar + 1
          param(npar)= digit
       else if(lflag.eq.1) then
          nword = nword + 1
          words(nword)(:) = ' '
          words(nword)(1:nstr) = card(first:last)
       end if
       !
    end do
    return
  end subroutine inpout_sdecode
  !
  !-----------------------------------
  !    subroutine inpout_decode_timeunit
  !-----------------------------------
  !
  !>   @brief
  !>   Decode time units in netcdf file
  !
  subroutine inpout_decode_timeunit(timeunit_string,time_factor,time_ref,MY_ERR)
      implicit none
      !
      !>   @param timeunit_string    time units attribute from a netcdf file
      !>   @param time_factor        time factor required to convert to seconds
      !>   @param time_ref           reference time
      !>   @param MY_ERR             error handler
      !
      character(len=*),    intent(in)     :: timeunit_string
      real(rp),            intent(out)    :: time_factor
      type(DATETIME),      intent(out)    :: time_ref
      type(ERROR_STATUS),  intent(INOUT)  :: MY_ERR
      !
      integer(ip)           :: istat
      integer(ip)           :: i,istring
      character(len=s_name) :: string_tmp,string_YYYYMMDD,string_HHMMSS
      integer(ip)           :: parse_datetime(6)
      !
      MY_ERR%flag    = 0
      MY_ERR%source  = 'inpout_decode_timeunit'
      MY_ERR%message = ' '
      !
      if(timeunit_string.eq.'') then
          MY_ERR%flag    = 1
          MY_ERR%message = "not found a valid unit time"
          return
      end if
      !
      !*** Read UNITS substring in: UNITS since YYYY-MM-DD HH:MM:SS
      !
      istring    = index(timeunit_string,'since')-1
      string_tmp = adjustl(trim(timeunit_string(1:istring)))
      !
      if(istring.gt.1) then
          call upcase(string_tmp)
      else
          MY_ERR%flag    = 1
          MY_ERR%message = "Expected format for time units: UNITS since YYYY-MM-DD HH:MM:SS"
          return
      end if
      !
      select case (TRIM(string_tmp))
          case('SECONDS','SECOND')
              time_factor = 1.0_rp
          case('MINUTES','MINUTE')
              time_factor = 60.0_rp
          case('HOURS','HOUR')
              time_factor = 3600.0_rp
          case('DAYS','DAY')
              time_factor = 86400.0_rp
          case default
              MY_ERR%flag    = 1
              MY_ERR%message = 'not recognised unit time: ' // TRIM(string_tmp)
              return
      end select
      !
      !*** Read YYYY-MM-DD HH:MM:SS substring in: UNITS since YYYY-MM-DD HH:MM:SS
      !
      string_tmp = adjustl(trim(timeunit_string(istring+6:)))
      call upcase(string_tmp)
      call replace_char(string_tmp,'+.TZ')
      !
      !*** Read YYYY-MM-DD substring and update string_tmp
      !
      istring = index(string_tmp,' ')
      if(istring.gt.1) then
          string_YYYYMMDD = adjustl(trim(string_tmp(1:istring-1)))
          string_tmp = adjustl(trim(string_tmp(istring:)))
      else
          MY_ERR%flag    = 1
          MY_ERR%message = "Unable to read unit time in netcdf file"
          return
      end if
      !
      !*** Read HH:MM:SS substring 
      !
      istring = index(string_tmp,' ')
      if(istring.gt.1) then
          string_HHMMSS = adjustl(trim(string_tmp(1:istring-1)))
      else
          string_HHMMSS = ' '
      end if
      !
      parse_datetime(:) = 0 !Default value
      !
      !*** Read YYYY-MM-DD (mandatory)
      !
      call replace_char(string_YYYYMMDD,'-')
      do i=1,3
        istring = index(string_YYYYMMDD,' ')
        if(istring.le.1) then
            istat = 1
            exit
        end if
        read(string_YYYYMMDD(1:istring-1),*,iostat=istat) parse_datetime(i)
        if(istat.ne.0) exit
        string_YYYYMMDD = adjustl(trim(string_YYYYMMDD(istring:)))
      end do
      !
      if(istat.ne.0) then
        MY_ERR%flag    = 1
        MY_ERR%message = "Unable to read unit time in netcdf file"
        return
      end if
      !
      !*** Read HH:MM:SS (optional)
      !
      call replace_char(string_HHMMSS,':')
      do i=4,6
        istring = index(string_HHMMSS,' ')
        if(istring.le.1) exit
        read(string_HHMMSS(1:istring-1),*,iostat=istat) parse_datetime(i)
        if(istat.ne.0) then
            parse_datetime(i) = 0
            exit
        end if
        string_HHMMSS = adjustl(trim(string_HHMMSS(istring:)))
      end do
      !
      time_ref = DATETIME(parse_datetime(1), &
                          parse_datetime(2), &
                          parse_datetime(3), &
                          parse_datetime(4), &
                          parse_datetime(5), &
                          parse_datetime(6)  )
      !
      if(istat.ne.0) then
        write(string_tmp,10) time_ref
        call task_wriwarn(MY_ERR,"Unrecognised characters in netcdf time units. &
                                  Assuming the reference time: "//string_tmp)
      end if
      !
10 format(I4,2('-',I2.2),1x,I2.2,2(':',I2.2))
      !
      return
  end subroutine inpout_decode_timeunit
  !
  !-----------------------------------
  !    function stof
  !-----------------------------------
  !
  !>   @brief
  !>   Converts a real/integer number stored in a string into a real digit format
  !
  real(rp) function stof(string,nstr)
    implicit none
    !
    !>   @param string input string
    !>   @param nstr   lenght of string
    !
    character(len=1), intent(in) :: string(*)
    integer(ip),      intent(in) :: nstr
    !
    integer(ip) :: i,ipos,nsign,esign,nvalu
    integer(ip) :: expo,valu(s_name)
    logical     :: next
    !
    stof = 0.0_rp
    !
    !***  Sing decoding
    !
    ipos = 1
    if(ichar(string(ipos)).eq.43) then         !  + sign
       nsign = 1
       ipos  = ipos + 1
    else if(ichar(string(ipos)).eq.45) then    !  - sign
       nsign = -1
       ipos  = ipos + 1
    else                                       !  no sing (+)
       nsign = 1
       ipos  = ipos
    end if
    !
    !***  Base decoding
    !
    nvalu = 0
    next  = .true.
    do while(next)
       if((ichar(string(ipos)).eq.68 ).or. &       ! D
            (ichar(string(ipos)).eq.69 ).or. &       ! E
            (ichar(string(ipos)).eq.100).or. &       ! d
            (ichar(string(ipos)).eq.101).or. &       ! e
            (ichar(string(ipos)).eq.46 )) then       ! .
          next = .false.
       else
          nvalu = nvalu + 1
          valu(nvalu) = stof1(string(ipos))
          ipos = ipos + 1
          if(ipos.eq.(nstr+1)) then
             next = .false.
             ipos = ipos - 1
          end if
       end if
    end do
    do i = 1,nvalu
       stof = stof + valu(i)*1e1_rp**(nvalu-i)
    end do
    !
    !***  Decimal decoding
    !
    if((ichar(string(ipos)).eq.46   ).and.  &
         ipos  .ne.nstr) then
       ipos = ipos + 1
       nvalu = 0
       next  = .true.
       do while(next)
          if((ichar(string(ipos)).eq.68 ).or. &        ! D
               (ichar(string(ipos)).eq.69 ).or. &       ! E
               (ichar(string(ipos)).eq.100).or. &       ! d
               (ichar(string(ipos)).eq.101)) then      ! e
             next = .false.
          else
             nvalu = nvalu + 1
             valu(nvalu) = stof1(string(ipos))
             ipos = ipos + 1
             if(ipos.eq.(nstr+1)) then
                next = .false.
                ipos = ipos - 1
             end if
          end if
       end do
       do i = 1,nvalu
          stof = stof + valu(i)*10.0_rp**(-i)
       end do
    end if
    !
    !***  Exponent
    !
    if(((ichar(string(ipos)).eq.68 ).or. &        ! D
         (ichar(string(ipos)).eq.69 ).or. &        ! E
         (ichar(string(ipos)).eq.100).or. &        ! d
         (ichar(string(ipos)).eq.101)).and. &      ! e
         ipos  .ne.nstr) then
       ipos = ipos + 1
       if(ichar(string(ipos)).eq.43) then         !  + sign
          esign = 1
          ipos  = ipos + 1
       else if(ichar(string(ipos)).eq.45) then    !  - sign
          esign = -1
          ipos  = ipos + 1
       else                                       !  no sing (+)
          esign = 1
          ipos  = ipos
       end if
       !
       nvalu = 0
       next  = .true.
       do while(next)
          nvalu = nvalu + 1
          valu(nvalu) = stof1(string(ipos))
          ipos = ipos + 1
          if(ipos.eq.(nstr+1)) then
             next = .false.
             ipos = ipos - 1
          end if
       end do
       expo = 0
       do i = 1,nvalu
          expo = expo + int(valu(i)*1e1_rp**(nvalu-i))
       end do
       !
       if(esign.eq.1) then
          stof = stof*(10.0_rp**expo)
       else if(esign.eq.-1) then
          stof = stof/(10.0_rp**expo)
       end if
       !
    end if
    !
    stof = nsign*stof
  end function stof
  !
  !
  !    PRIVATE ROUTINES
  !
  !
  integer(ip) function stoi1(string1)
    !**************************************************************
    !*
    !*   Decodes a character*1 string
    !*
    !**************************************************************
    implicit none
    character(len=1) :: string1
    !
    stoi1 = 0
    if(string1.eq.'0') then
       stoi1 = 0
    else if(string1.eq.'1') then
       stoi1 = 1
    else if(string1.eq.'2') then
       stoi1 = 2
    else if(string1.eq.'3') then
       stoi1 = 3
    else if(string1.eq.'4') then
       stoi1 = 4
    else if(string1.eq.'5') then
       stoi1 = 5
    else if(string1.eq.'6') then
       stoi1 = 6
    else if(string1.eq.'7') then
       stoi1 = 7
    else if(string1.eq.'8') then
       stoi1 = 8
    else if(string1.eq.'9') then
       stoi1 = 9
    end if
    return
  end function stoi1
  !
  !
  !
  subroutine decod1(string,nstr,lflag,digit)
    !*******************************************************************
    !*
    !*    This subroutine decodes a single string(1:nstr)
    !*
    !*    If string(1:nstr) is a string returns lflag = 1
    !*    If string(1:nstr) is a number returns lflag = 0 and the digit
    !*
    !*******************************************************************
    implicit none
    integer(ip) :: nstr,lflag
    integer(ip) :: istr,decim
    real(rp)    :: digit
    character(len=1) :: string(s_long)
    integer(ip) :: sigflag  ! Found the sign
    integer(ip) :: dotflag  ! Found a dot
    integer(ip) :: expflag  ! Found a exponent
    !
    lflag = 0                                             ! Number by default
    istr  = 1
    sigflag = 0
    dotflag = 0
    expflag = 0
    !
    do while(istr.le.nstr)
       decim = ichar(string(istr))                           ! decimal value
       if(decim.lt.48.or.decim.gt.57) then                   ! It is not a num.
          if(decim.eq.43 .or. decim.eq.45) then
             sigflag=sigflag+1                        ! found + -
          else if(decim.eq.68 .or. decim.eq.69) then
             expflag=expflag+1                        ! found D E
          else if(decim.eq.100.or. decim.eq.101) then
             expflag=expflag+1                        ! found d e
          else if(decim.eq.46) then
             dotflag=dotflag+1                        ! found .
          else
             lflag = 1
             istr  = nstr  ! Stop scan
          end if
          if(dotflag.gt.1 .or. expflag.gt.1 .or. sigflag.gt.2) then
             lflag = 1
             istr  = nstr  ! Stop scan
          end if
       end if
       istr = istr+1
    end do
    !
    if(lflag.eq.0) digit = stof(string,nstr)               ! It's a number
    !
    return
  end subroutine decod1
  !
  !
  !
  integer(ip) function stof1(string1)
    !**************************************************************
    !*
    !*    Decodes a character*1 string
    !*
    !**************************************************************
    implicit none
    character(len=1) :: string1
    !
    if(string1.eq.'0') then
       stof1 = 0
    else if(string1.eq.'1') then
       stof1 = 1
    else if(string1.eq.'2') then
       stof1 = 2
    else if(string1.eq.'3') then
       stof1 = 3
    else if(string1.eq.'4') then
       stof1 = 4
    else if(string1.eq.'5') then
       stof1 = 5
    else if(string1.eq.'6') then
       stof1 = 6
    else if(string1.eq.'7') then
       stof1 = 7
    else if(string1.eq.'8') then
       stof1 = 8
    else if(string1.eq.'9') then
       stof1 = 9
    else
       stof1 = 0
    end if
    return
  end function stof1
  !
  !
  !
  subroutine upcase(word)
    !***********************************************************************
    !*
    !*    This routine converts word to upper case
    !*
    !***********************************************************************
    implicit none
    character(len=*) :: word
    integer(ip)      :: iposi,ioctv,item1,item2,item3
    integer(ip)      :: ilen
    !
    item1 = int(o'141')
    item2 = int(o'172')
    item3 = int(o'40')
    !
    ilen=LEN_TRIM(word)
    !
    do iposi=1,ilen                                   ! process all positions
       ioctv=ichar(word(iposi:iposi))                 ! octal value
       if(item1.le.ioctv.and.item2.ge.ioctv) then     ! it is a lower case
          ioctv=ioctv-item3                           ! equivalent upper case
          word(iposi:iposi)=char(ioctv)               ! convert it to upcase
       end if
    end do ! iposi=1,ilen
    !
    return
  end subroutine upcase
  !
  !
  !
  recursive subroutine replace_char(string,substring)
    !***********************************************************************
    !*
    !*    This routine converts word to upper case
    !*
    !***********************************************************************
    implicit none
    !
    character(len=*), intent(inout) :: string
    character(len=*), intent(in)    :: substring
    integer(ip)                     :: istring
    !
    istring = scan(string,substring)
    if(istring.gt.0) then
        string(istring:istring) = ' '
        call replace_char(string,substring)
    end if
    return
  end subroutine replace_char
  !
  !
  !
END MODULE InpOut
