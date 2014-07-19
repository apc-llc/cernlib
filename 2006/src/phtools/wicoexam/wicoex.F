*
* $Id: wicoex.F,v 1.1.1.1 1996/03/22 16:42:50 mclareni Exp $
*
* $Log: wicoex.F,v $
* Revision 1.1.1.1  1996/03/22 16:42:50  mclareni
* Phtools
*
*
* This directory was created from phtools.car patch wicoexam
      PROGRAM WICOEX
C  This example makes the ray-tracing procedure for a single 
C  user defined ray within the user defined Winston Cone
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        DIMENSION X0(2),V0(3),X1(2),V1(3)
        TIN=5.7587705   ! entrance diameter of the Winston Cone
      TOUT=1.         ! exit diameter of the Winston Cone
      RR=1.49         ! ratio between internal and external 
C                         refractive indeces
c      RR=0.           ! if RR.le.1 100% reflection independent of 
C                         incidence angle
      NMAX=20         ! maximum allowed # of bounces for a single ray
        CALL WICOIN(TIN,TOUT,RR,NMAX,ELLE)
      PRINT*,' Winston Cone LENGTH =',ELLE
C-------  loop on input data
      X0(1)=2.    ! input coordinate of the user defined ray
      X0(2)=1.
      V0(1)=0.1    ! input direction of the user defined ray
      V0(2)=0.1
      V0(3)=-1.
      CALL WICO(X0,V0,X1,V1,D,N,IERR)
      IF(IERR.EQ.1)PRINT*,' ray impact point out of entrance face 
     &  - IERR=1'
      IF(IERR.EQ.2)PRINT*,' number of bounces greater than NMAX 
     &  - IERR=2'
      IF(IERR.EQ.3)PRINT*,' the ray comes back to the entrance face
     &  - IERR=3'
      IF(IERR.EQ.4)PRINT*,' ray incident out of limit angle
     &  - IERR=4'
      IF(IERR.EQ.0)THEN
        PRINT*,' photon transmitted - IERR=0'
          PRINT*,' output coordinate',X1(1),X1(2)
          PRINT*,' output direction',V1(1),V1(2),V1(3)
          PRINT*,' total path length',D
          PRINT*,' total number of bounces',N
        ENDIF
      STOP
      END
