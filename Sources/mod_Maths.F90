!***************************************************************
!>
!> Module for Math operations
!> @author
!> Arnau Folch
!>
!***************************************************************
MODULE Maths
  use KindType, only: ip,dp,Q1_GRID,ERROR_STATUS
  implicit none
  save
  !
  !    NOTE: This routine works always in double precision
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: maths_set_lnodsQ1
  PUBLIC :: maths_get_host_elemQ1
  !
  !    LIST OF PRIVATE ROUTINES IN THE MODULE
  !
  PRIVATE :: maths_elq1p1
  PRIVATE :: maths_newrap
  PRIVATE :: maths_mbmabt
  PRIVATE :: maths_invmtx
  PRIVATE :: maths_shafun_q1
  !
CONTAINS
  !
  !-----------------------------------------
  !    subroutine maths_set_lnodsQ1
  !-----------------------------------------
  !
  !>   @brief
  !>   Sets nodal connectivities for a Q1 2D structured mesh
  !
  subroutine maths_set_lnodsQ1(MY_MESH,MY_ERR)
    implicit none
    !
    !>   @param MY_MESH    Q1 2D mesh parameters
    !>   @param MY_ERR     error handler
    !
    type(Q1_GRID),     intent(INOUT) :: MY_MESH
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    integer(ip) :: ielem,ix,iy
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'maths_set_lnodsQ1'
    MY_ERR%message = ' '
    !
    allocate(MY_MESH%lnods(4,MY_MESH%nelem))
    !
    ielem = 0
    do iy = 1,MY_MESH%ny-1
       do ix = 1,MY_MESH%nx-1
          ielem = ielem + 1
          MY_MESH%lnods(1,ielem) = (iy-1)*MY_MESH%nx + ix
          MY_MESH%lnods(2,ielem) = (iy-1)*MY_MESH%nx + ix + 1
          MY_MESH%lnods(3,ielem) = (iy  )*MY_MESH%nx + ix + 1
          MY_MESH%lnods(4,ielem) = (iy  )*MY_MESH%nx + ix
       end do
    end do
    !
    if(ielem /= MY_MESH%nelem) then
       MY_ERR%flag    = 1
       MY_ERR%source  = 'Incorrect number of elements'
       return
    end if
    !
    return
  end subroutine maths_set_lnodsQ1
  !
  !
  !-----------------------------------------
  !    subroutine maths_get_host_elemQ1
  !-----------------------------------------
  !
  !>   @brief
  !>   Computes hosting elements and interpolation factors on a Q1 2D mesh for a given list of points
  !
  subroutine maths_get_host_elemQ1(MY_MESH,npoin,lon_po,lat_po,el_po,s_po,t_po,MY_ERR)
    implicit none
    !
    !>   @param MY_MESH    Q1 2D mesh parameters
    !>   @param npoin      number of points
    !>   @param lon_po     point x-coordinates
    !>   @param lat_po     point y-coordinates
    !>   @param el_po      point hosting element: el_po(ipoin) = ielem
    !>   @param s_po       point interpolation factor: s_po(ipoin)  = s
    !>   @param t_po       point interpolation factor: t_po(ipoin)  = t
    !>   @param MY_ERR     error handler
    !
    type(Q1_GRID),     intent(IN   ) :: MY_MESH
    integer(ip),       intent(IN   ) :: npoin
    real(dp),          intent(IN   ) :: lon_po(npoin)
    real(dp),          intent(IN   ) :: lat_po(npoin)
    integer(ip),       intent(INOUT) :: el_po (npoin)
    real(dp),          intent(INOUT) :: s_po  (npoin)
    real(dp),          intent(INOUT) :: t_po  (npoin)
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    logical     :: found
    integer(ip) :: ipoin,ielem,i
    real   (dp) :: elcod(2,4),coglo(2),coloc(2)
    real   (dp) :: lmini,lmaxi
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'maths_get_host_elemQ1'
    MY_ERR%message = ' '
    !
    lmini = 0.0_dp
    lmaxi = 1.0_dp
    !
    !*** Loop over all points
    !
    do ipoin = 1,npoin
       !
       found = .false.
       ielem = 0
       do while(.not.found)
          coglo(1) = lon_po(ipoin)
          coglo(2) = lat_po(ipoin)
          !
          ielem = ielem + 1
          if(ielem == MY_MESH%nelem+1) then
             MY_ERR%flag    = 1
             MY_ERR%message = ' point not found in meteo model domain'
             return
          end if
          do i=1,4
             elcod(1,i) = MY_MESH%coord(1,MY_MESH%lnods(i,ielem))
             elcod(2,i) = MY_MESH%coord(2,MY_MESH%lnods(i,ielem))
          end do
          !all longitudes are assumed to be in the range [-180,180).
          !if any discontinuity is detected, we convert them to [0,360)
          if(elcod(1,1).gt.elcod(1,2) .or. elcod(1,4).gt.elcod(1,3) ) then
             do i=1,4
                if (elcod(1,i).lt.0.0_dp) elcod(1,i) = elcod(1,i) + 360.0_dp
             end do
             if(coglo(1).lt.0.0_dp) coglo(1) = coglo(1) + 360.0_dp
          end if
          call maths_elq1p1(4_ip,lmini,lmaxi,elcod,coglo,coloc,found)
          !
       end do
       !
       !*** Element found
       !
       el_po(ipoin) = ielem
       s_po (ipoin) = MAX(-1.0_dp,MIN(1.0_dp,coloc(1)))
       t_po (ipoin) = MAX(-1.0_dp,MIN(1.0_dp,coloc(2)))
       !
    end do
    !
    return
  end subroutine maths_get_host_elemQ1
  !
  !
  !    PRIVATE ROUTINES
  !
  !
  subroutine maths_elq1p1(pnode,lmini,lmaxi,elcod,coglo,coloc,found)
    !**************************************************************
    !*
    !*    Check if point with global coordinates (x,y)=COGLO is inside
    !*    a triangle P1 or a quadrilateral Q1. The Q1 element is
    !*    divided into two P1 elements. Returns the local coordinates
    !*    (s,t)=COLOC
    !*
    !*    For P1 triangles we have:
    !*    x = (1-s-t)*x1 + s*x2 + t*x3
    !*    y = (1-s-t)*y1 + s*y2 + t*y3
    !*
    !*    This linear problem is solved for (s,t):
    !*         (x3-x1)(y-y1) -(y3-y1)(x-x1)
    !*    s =  ----------------------------
    !*         (x3-x1)(y2-y1)-(y3-y1)(x2-x1)
    !*
    !*         (x-x1)(y2-y1) -(y-y1)(x2-x1)
    !*    t =  ----------------------------
    !*         (x3-x1)(y2-y1)-(y3-y1)(x2-x1)
    !*
    !**************************************************************
    implicit none
    logical                  :: found
    integer(ip), intent(in)  :: pnode
    real(dp),    intent(in)  :: lmini,lmaxi,elcod(2,pnode),coglo(2)
    real(dp),    intent(out) :: coloc(2)
    !
    real(dp)                 :: deter,colo3,x2x1,y2y1,x3x1,y3y1,xx1,yy1
    !
    found = .false.
    !
    !*** P1 and Q1: Check if point is in first triangle: nodes 1-2-3
    !
    x2x1     = elcod(1,2)-elcod(1,1)
    y2y1     = elcod(2,2)-elcod(2,1)
    x3x1     = elcod(1,3)-elcod(1,1)
    y3y1     = elcod(2,3)-elcod(2,1)
    xx1      = coglo(1)  -elcod(1,1)
    yy1      = coglo(2)  -elcod(2,1)
    deter    = 1.0_dp/(x3x1*y2y1-y3y1*x2x1)
    coloc(1) = deter*(x3x1*yy1-y3y1*xx1)
    coloc(2) = deter*(y2y1*xx1-x2x1*yy1)
    !
    if(abs(coloc(1)).le.1d-8) coloc(1) = 0.0_dp
    if(abs(coloc(2)).le.1d-8) coloc(2) = 0.0_dp

    if((coloc(1)>=lmini).and.(coloc(1)<=lmaxi)) then
       if((coloc(2)>=lmini).and.(coloc(2)<=lmaxi)) then
          colo3 = 1.0_dp-coloc(1)-coloc(2)
          if(colo3>=lmini.and.colo3<=lmaxi) found = .true.
       end if
    end if

    if(pnode==4) then
       !
       ! Q1: Check if point is in second triangle: nodes 1-3-4
       !
       if(.not.found) then
          x2x1     = elcod(1,3)-elcod(1,1)
          y2y1     = elcod(2,3)-elcod(2,1)
          x3x1     = elcod(1,4)-elcod(1,1)
          y3y1     = elcod(2,4)-elcod(2,1)
          xx1      = coglo(1)  -elcod(1,1)
          yy1      = coglo(2)  -elcod(2,1)
          deter    = 1.0_dp/(x3x1*y2y1-y3y1*x2x1)
          coloc(1) = deter*(x3x1*yy1-y3y1*xx1)
          coloc(2) = deter*(y2y1*xx1-x2x1*yy1)
          !
          if(abs(coloc(1)).le.1d-8) coloc(1) = 0.0_dp
          if(abs(coloc(2)).le.1d-8) coloc(2) = 0.0_dp

          if((coloc(1)>=lmini).and.(coloc(1)<=lmaxi)) then
             if((coloc(2)>=lmini).and.(coloc(2)<=lmaxi)) then
                colo3 = 1.0_dp-coloc(1)-coloc(2)
                if(colo3>=lmini.and.colo3<=lmaxi) found = .true.
             end if
          end if
       end if
       if(found) then
          call maths_newrap(coglo,coloc,2_ip,4_ip,elcod)
       end if
       !
    end if
    !
  end subroutine maths_elq1p1
  !
  !
  !
  subroutine maths_newrap(coglo,coloc,ndime,pnode,elcod)
    !**************************************************************
    !*
    !*    Calculate the inverse transformation (x,y,z)-->(s,t,r)
    !*
    !*    Iterate for f(s_i)=x_i: ds = J^{-1}.xpoin - J^{-1}.f(s_i)
    !*                               = J^{-1}dx
    !*                            ds = s_{i+1}-s_i  (deltas)
    !*                            dx = xpoin-f(s_i) (deltax)
    !*    where the s_i's are the coordinates in the local basis and
    !*    xpoin(idime)'s the real ones.
    !*
    !**************************************************************
    implicit none
    integer(ip), intent(in)  :: ndime,pnode
    real(dp),    intent(in)  :: coglo(ndime),elcod(ndime,pnode)
    real(dp),    intent(out) :: coloc(ndime)
    !
    integer(ip)              :: inode,iiter,jdime,maxit,idime,ntens
    real(dp)                 :: shapf(4),deriv(2,4)
    real(dp)                 :: xjacm(2,2),xjaci(2,2)
    real(dp)                 :: deltx(3),delts(3),xnorm,detja
    real(dp)                 :: rnode,diame,coocg(3)
    real(dp)                 :: shacg(4),dercg(2,4)
    !
    ! Initial condition
    !
    coloc(1)=0.0_dp
    coloc(2)=0.0_dp
    coloc(ndime)=0.0_dp
    if(ndime==1) then
       ntens=1
    else if(ndime==2) then
       ntens=3
    else
       ntens=6
    end if
    call maths_shafun_q1(coloc(1),coloc(2),shapf,deriv)
    !
    ! Element diameter
    !
    do idime=1,ndime
       coocg(idime)=0.0_dp
    end do
    do inode=1,pnode
       do idime=1,ndime
          coocg(idime)=coocg(idime)+elcod(idime,inode)
       end do
    end do
    rnode=1.0_dp/real(pnode)
    do idime=1,ndime
       coocg(idime)=rnode*coocg(idime)
    end do
    call maths_shafun_q1(coocg(1),coocg(2),shacg,dercg)
    call maths_mbmabt(xjacm,elcod,dercg,ndime,ndime,pnode)
    call maths_invmtx(xjacm,xjaci,diame,ndime)
    diame=1.0_dp/(diame**(2.0_dp/real(ndime)))
    !
    ! Initialize dx=coglo-f(s_1) with s_1=(0,0,0)
    !
    do idime=1,ndime
       deltx(idime)=0.0_dp
       do inode=1,pnode
          deltx(idime)=deltx(idime)&
               +shapf(inode)*elcod(idime,inode)
       end do
    end do
    xnorm=0.0_dp
    do idime=1,ndime
       deltx(idime) = coglo(idime)-deltx(idime)
       xnorm        = xnorm+deltx(idime)*deltx(idime)
    end do
    xnorm=xnorm*diame
    iiter=0
    maxit=10
    !
    ! Iterate for f(s_i)=x_i
    !
    do while( (xnorm>1d-8).and.(iiter<=maxit) )
       iiter=iiter+1
       !
       ! Compute J
       !
       do jdime=1,ndime
          do idime=1,ndime
             xjacm(idime,jdime)=0.0_dp
             do inode=1,pnode
                xjacm(idime,jdime)=xjacm(idime,jdime)&
                     +deriv(idime,inode)*elcod(jdime,inode)
             end do
          end do
       end do
       !
       ! Compute J^{-1}
       !
       call maths_invmtx(xjacm,xjaci,detja,ndime)

       do idime=1,ndime
          delts(idime)=0.0_dp
          !
          ! Compute J^{-1}.dx
          !
          do jdime=1,ndime
             delts(idime)=delts(idime)+deltx(jdime)*xjaci(jdime,idime)
          end do
       end do
       do idime=1,ndime
          coloc(idime)=coloc(idime)+delts(idime)
       end do
       if ((coloc(1)>1d99).or.(coloc(2)>1d99).or.(coloc(ndime)>1d99)) then
          iiter=maxit+1
       else
          call maths_shafun_q1(coloc(1),coloc(2),shapf,deriv)
       end if
       !
       ! Compute f_i
       !
       do idime=1,ndime
          deltx(idime)=0.0_dp
          do inode=1,pnode
             deltx(idime)=deltx(idime)&
                  +shapf(inode)*elcod(idime,inode)
          end do
       end do
       !
       ! Compute dx=coglo-f
       !         xnorm=sum ds^2
       !
       xnorm=0.0_dp
       do idime=1,ndime
          deltx(idime)=coglo(idime)-deltx(idime)
          xnorm=xnorm+delts(idime)*delts(idime)
       end do
       xnorm=xnorm*diame
    end do
    if(xnorm>1d-8) coloc(1)=2.0_dp

  end subroutine maths_newrap
  !
  !
  !
  subroutine maths_mbmabt(a,b,c,n1,n2,n3)
    !********************************************************************
    !*
    !*  This routine evaluates the matrix product A = B Ct, where
    !*  A -> Mat(n1,n2), B -> Mat(n1,n3), C -> Mat(n2,n3)
    !*
    !********************************************************************
    implicit none
    integer(ip) :: n1,n2,n3
    integer(ip) :: i,j,k
    real(dp)    :: a(n1,n2), b(n1,n3), c(n2,n3)
    !
    do i=1,n1
       do j=1,n2
          a(i,j)=0.0_dp
          do k=1,n3
             a(i,j)=a(i,j)+b(i,k)*c(j,k)
          end do
       end do
    end do
    !
    return
  end subroutine maths_mbmabt
  !
  !
  !
  subroutine maths_invmtx(a,b,deter,nsize)
    !********************************************************************
    !
    ! This routine inverts a square matrix A -> Mat(nsize,nsize). The
    ! inverse is stored in B. Its determinant is DETER
    !
    !********************************************************************
    implicit none
    integer(ip), intent(in)  :: nsize
    real(dp),    intent(in)  :: a(nsize,*)
    real(dp),    intent(out) :: deter,b(nsize,*)
    real(dp)                 :: t1,t2,t3,t4,denom

    if(nsize==1) then
       !
       ! Inverse of a 1*1 matrix
       !
       deter=a(1,1)
       if(deter/=0.0_dp) return
       b(1,1) = 1.0_dp/a(1,1)

    else if(nsize==2) then
       !
       ! Inverse of a 2*2 matrix
       !
       deter=a(1,1)*a(2,2)-a(2,1)*a(1,2)
       if(deter==0.0_dp) return
       denom  = 1.0_dp/deter
       b(1,1) = a(2,2)*denom
       b(2,2) = a(1,1)*denom
       b(2,1) =-a(2,1)*denom
       b(1,2) =-a(1,2)*denom

    else if(nsize==3) then
       !
       ! Inverse of a 3*3 matrix
       !
       t1  = a(2,2)*a(3,3) - a(3,2)*a(2,3)
       t2  =-a(2,1)*a(3,3) + a(3,1)*a(2,3)
       t3  = a(2,1)*a(3,2) - a(3,1)*a(2,2)
       deter = a(1,1)*t1 + a(1,2)*t2 + a(1,3)*t3
       if(deter==0.0_dp) return
       denom  = 1.0_dp/deter
       b(1,1) = t1*denom
       b(2,1) = t2*denom
       b(3,1) = t3*denom
       b(2,2) = ( a(1,1)*a(3,3) - a(3,1)*a(1,3))*denom
       b(3,2) = (-a(1,1)*a(3,2) + a(1,2)*a(3,1))*denom
       b(3,3) = ( a(1,1)*a(2,2) - a(2,1)*a(1,2))*denom
       b(1,2) = (-a(1,2)*a(3,3) + a(3,2)*a(1,3))*denom
       b(1,3) = ( a(1,2)*a(2,3) - a(2,2)*a(1,3))*denom
       b(2,3) = (-a(1,1)*a(2,3) + a(2,1)*a(1,3))*denom

    else if(nsize==4) then
       !
       ! Inverse of a 4*4 matrix
       !
       t1=   a(2,2)*a(3,3)*a(4,4) + a(2,3)*a(3,4)*a(4,2)&
            +a(2,4)*a(3,2)*a(4,3) - a(2,3)*a(3,2)*a(4,4)&
            -a(2,2)*a(3,4)*a(4,3) - a(2,4)*a(3,3)*a(4,2)
       t2=  -a(2,1)*a(3,3)*a(4,4) - a(2,3)*a(3,4)*a(4,1)&
            -a(2,4)*a(3,1)*a(4,3) + a(2,4)*a(3,3)*a(4,1)&
            +a(2,3)*a(3,1)*a(4,4) + a(2,1)*a(3,4)*a(4,3)
       t3=   a(2,1)*a(3,2)*a(4,4) + a(2,2)*a(3,4)*a(4,1)&
            +a(2,4)*a(3,1)*a(4,2) - a(2,4)*a(3,2)*a(4,1)&
            -a(2,2)*a(3,1)*a(4,4) - a(2,1)*a(3,4)*a(4,2)
       t4=  -a(2,1)*a(3,2)*a(4,3) - a(2,2)*a(3,3)*a(4,1)&
            -a(2,3)*a(3,1)*a(4,2) + a(2,3)*a(3,2)*a(4,1)&
            +a(2,2)*a(3,1)*a(4,3) + a(2,1)*a(3,3)*a(4,2)
       deter= a(1,1)*t1 + a(1,2)*t2 + a(1,3)*t3 + a(1,4)*t4
       if(deter==0.0_dp) return
       denom = 1.0_dp/deter
       b(1,1) = t1*denom
       b(2,1) = t2*denom
       b(3,1) = t3*denom
       b(4,1) = t4*denom
       b(1,2) =(- a(1,2)*a(3,3)*a(4,4) - a(1,3)*a(3,4)*a(4,2)&
            &   - a(1,4)*a(3,2)*a(4,3) + a(1,3)*a(3,2)*a(4,4)&
            &   + a(1,2)*a(3,4)*a(4,3) + a(1,4)*a(3,3)*a(4,2))*denom
       b(2,2) =(  a(1,1)*a(3,3)*a(4,4) + a(1,3)*a(3,4)*a(4,1)&
            &   + a(1,4)*a(3,1)*a(4,3) - a(1,4)*a(3,3)*a(4,1)&
            &   - a(1,3)*a(3,1)*a(4,4) - a(1,1)*a(3,4)*a(4,3))*denom
       b(3,2) =(- a(1,1)*a(3,2)*a(4,4) - a(1,2)*a(3,4)*a(4,1)&
            &   - a(1,4)*a(3,1)*a(4,2) + a(1,4)*a(3,2)*a(4,1)&
            &   + a(1,2)*a(3,1)*a(4,4) + a(1,1)*a(3,4)*a(4,2))*denom
       b(4,2) =(  a(1,1)*a(3,2)*a(4,3) + a(1,2)*a(3,3)*a(4,1)&
            &   + a(1,3)*a(3,1)*a(4,2) - a(1,3)*a(3,2)*a(4,1)&
            &   - a(1,2)*a(3,1)*a(4,3) - a(1,1)*a(3,3)*a(4,2))*denom
       b(1,3) =(  a(1,2)*a(2,3)*a(4,4) + a(1,3)*a(2,4)*a(4,2)&
            &   + a(1,4)*a(2,2)*a(4,3) - a(1,3)*a(2,2)*a(4,4)&
            &   - a(1,2)*a(2,4)*a(4,3) - a(1,4)*a(2,3)*a(4,2))*denom
       b(2,3) =(- a(1,1)*a(2,3)*a(4,4) - a(1,3)*a(2,4)*a(4,1)&
            &   - a(1,4)*a(2,1)*a(4,3) + a(1,4)*a(2,3)*a(4,1)&
            &   + a(1,3)*a(2,1)*a(4,4) + a(1,1)*a(2,4)*a(4,3))*denom
       b(3,3) =(  a(1,1)*a(2,2)*a(4,4) + a(1,2)*a(2,4)*a(4,1)&
            &   + a(1,4)*a(2,1)*a(4,2) - a(1,4)*a(2,2)*a(4,1)&
            &   - a(1,2)*a(2,1)*a(4,4) - a(1,1)*a(2,4)*a(4,2))*denom
       b(4,3) =(- a(1,1)*a(2,2)*a(4,3) - a(1,2)*a(2,3)*a(4,1)&
            &   - a(1,3)*a(2,1)*a(4,2) + a(1,3)*a(2,2)*a(4,1)&
            &   + a(1,2)*a(2,1)*a(4,3) + a(1,1)*a(2,3)*a(4,2))*denom
       b(1,4) =(- a(1,2)*a(2,3)*a(3,4) - a(1,3)*a(2,4)*a(3,2)&
            &   - a(1,4)*a(2,2)*a(3,3) + a(1,4)*a(2,3)*a(3,2)&
            &   + a(1,3)*a(2,2)*a(3,4) + a(1,2)*a(2,4)*a(3,3))*denom
       b(2,4) =(  a(1,1)*a(2,3)*a(3,4) + a(1,3)*a(2,4)*a(3,1)&
            &   + a(1,4)*a(2,1)*a(3,3) - a(1,4)*a(2,3)*a(3,1)&
            &   - a(1,3)*a(2,1)*a(3,4) - a(1,1)*a(2,4)*a(3,3))*denom
       b(3,4) =(- a(1,1)*a(2,2)*a(3,4) - a(1,2)*a(2,4)*a(3,1)&
            &   - a(1,4)*a(2,1)*a(3,2) + a(1,4)*a(2,2)*a(3,1)&
            &   + a(1,2)*a(2,1)*a(3,4) + a(1,1)*a(2,4)*a(3,2))*denom
       b(4,4) =(  a(1,1)*a(2,2)*a(3,3) + a(1,2)*a(2,3)*a(3,1)&
            &   + a(1,3)*a(2,1)*a(3,2) - a(1,3)*a(2,2)*a(3,1)&
            &   - a(1,2)*a(2,1)*a(3,3) - a(1,1)*a(2,3)*a(3,2))*denom
    else
       !
       ! Inverse of a nsize*nsize matrix
       !
       !    do isize=1,nsize
       !        do jsize=1,nsize
       !           b(isize,jsize)=a(isize,jsize)
       !        enddo
       !     enddo
       !     call elsest_invert(b,nsize,nsize)
    end if

  end subroutine maths_invmtx
  !
  !
  !
  subroutine maths_shafun_q1(s,t,myshape,deriv)
    !************************************************************
    !*
    !*   Evaluates Q1 shape funcion and derivatives at (s,t)
    !*
    !************************************************************
    implicit none
    real(dp) :: s,t,myshape(4),deriv(2,4)
    real(dp) :: st
    !
    st=s*t
    myshape(1)=(1.0_dp-t-s+st)*0.25_dp                           !  4         3
    myshape(2)=(1.0_dp-t+s-st)*0.25_dp                           !
    myshape(3)=(1.0_dp+t+s+st)*0.25_dp                           !
    myshape(4)=(1.0_dp+t-s-st)*0.25_dp                           !
    deriv(1,1)=(-1.0_dp+t)*0.25_dp                               !  1         2
    deriv(1,2)=(+1.0_dp-t)*0.25_dp
    deriv(1,3)=(+1.0_dp+t)*0.25_dp
    deriv(1,4)=(-1.0_dp-t)*0.25_dp
    deriv(2,1)=(-1.0_dp+s)*0.25_dp
    deriv(2,2)=(-1.0_dp-s)*0.25_dp
    deriv(2,3)=(+1.0_dp+s)*0.25_dp
    deriv(2,4)=(+1.0_dp-s)*0.25_dp
    !
    return
  end subroutine maths_shafun_q1
  !
  !
  !
END MODULE Maths
