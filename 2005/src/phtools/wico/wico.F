*
* $Id: wico.F,v 1.1.1.1 1996/03/22 16:42:49 mclareni Exp $
*
* $Log: wico.F,v $
* Revision 1.1.1.1  1996/03/22 16:42:49  mclareni
* Phtools
*
*
      SUBROUTINE WICO(X0,V0,X1,V1,D,N,IERR)
******************************************************
*                                                    *
*       NAME: WICO                                   *
*       AUTHOR: ANTONIO DI DOMENICO                  *
*       LANGUAGE: FORTRAN-77                         *
*       VERSION: 1.1                                 *
*       RELEASED: 14-JAN-1994                        *
*       REVISED: 11-APR-1994                         *
*                                                    *
******************************************************
*
*       FUNCTIONS:
*       CWICOF             
*
*       COMMON BLOCKS:
*       CWICO
*       CURDAT
*
******************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        DIMENSION X0(2),V0(3),X1(2),V1(3)
        COMMON/CWICO/TIN,TOUT,RR,ELLE,THM,STHM,A,THLIM,
     &  AINF,ASUP,PIG,EPS,MAXF,NMAX
      COMMON/CURDAT/XP,YP,ZP,CX0,CY0,CZ0
      EXTERNAL CWICOF
C--------------------------------------------------------
      ANORM=DSQRT(V0(1)**2+V0(2)**2+V0(3)**2)
      CX0=V0(1)/ANORM
      CY0=V0(2)/ANORM
      CZ0=V0(3)/ANORM
        XP=X0(1)
        YP=X0(2)
      ZP=ELLE
      N=0
      D=0
C------ check if IERR=1 ---------------------------------------
      IF(DSQRT(XP**2+YP**2).GT.TIN/2.)THEN
        WRITE(6,*)'IERR=1 - ray incident out of the entrance face'
          IERR=1
        RETURN
      ENDIF
C------ propagation through the Winston Cone -------------------
2      CONTINUE
C---- controls if the ray hits the photocathode --
      IF(CZ0.LT.0.)THEN 
        SP=-ZP/CZ0
        X1(1)=XP+CX0*SP
        X1(2)=YP+CY0*SP
        IF(DSQRT(X1(1)**2+X1(2)**2).LT.TOUT/2.)THEN
            Z1=0.
          D=D+DSQRT((XP-X1(1))**2+(YP-X1(2))**2+(ZP-Z1)**2)
            V1(1)=CX0
            V1(2)=CY0
            V1(3)=CZ0
            IERR=0
          RETURN
        ENDIF
      ELSE IF(CZ0.GT.0.)THEN
        SP=(ELLE-ZP)/CZ0
        X1(1)=XP+CX0*SP
        X1(2)=YP+CY0*SP
        IF(DSQRT(X1(1)**2+X1(2)**2).LT.TIN/2.)THEN
            Z1=ELLE
          D=D+DSQRT((XP-X1(1))**2+(YP-X1(2))**2+(ZP-Z1)**2)
            V1(1)=CX0
            V1(2)=CY0
            V1(3)=CZ0
C          WRITE(6,*)'IERR=3 - the ray comes back to the entrance face'
            IERR=3
          RETURN
        ENDIF
      ELSE IF(CZ0.EQ.0.)THEN
          IF(CX0.GT.0.)THEN
            SP=(TIN/2.-XP)/CX0
          ELSE IF(CX0.LT.0)THEN
            SP=(-TIN/2.-XP)/CX0
          ELSE IF(CX0.EQ.0.)THEN
            IF(CY0.GT.0)THEN
              SP=(TIN/2.-YP)/CY0
            ELSE IF(CY0.LT.0)THEN
              SP=(-TIN/2.-YP)/CY0
            ENDIF
          ENDIF
      ENDIF
C------ check if IERR=2 ---------------------------------------
      IF(N.GT.NMAX)THEN
        WRITE(6,*)'IERR=2 - number of bounces greater than NMAX'
          IERR=2
        RETURN
      ENDIF         
C--------------------------------------------------------
C----------- determination of the impact point position on the cone surface
      ASUP=SP
      CALL DZERO(AINF,ASUP,SST,R,EPS,MAXF,CWICOF)
      XST=XP+SST*CX0
      YST=YP+SST*CY0
      ZST=ZP+SST*CZ0
C----------- determination of the tangent plane at the impact point
      TGTH=(TOUT/2.+DSQRT(XST**2+YST**2))/ZST
      TH=DATAN(TGTH)
      XPRIME=DSQRT((DSQRT(XST**2+YST**2)+TOUT/2.)**2+ZST**2)
     &  *DSIN(TH+THM)
      ETA=DATAN(2.D0*A*XPRIME)+THM
        ZETA=DATAN2(YST,XST)
      IF(ZETA.LT.0.)ZETA=ZETA+2.*PIG
      CX1=-DSIN(ZETA)
      CY1=DCOS(ZETA)
      CZ1=0.
      CX2=DCOS(ETA)*DCOS(ZETA)
      CY2=DCOS(ETA)*DSIN(ZETA)
      CZ2=DSIN(ETA)
      CX3=DCOS(PIG/2.+ETA)*DCOS(ZETA)
      CY3=DCOS(PIG/2.+ETA)*DSIN(ZETA)
      CZ3=DSIN(PIG/2.+ETA)
      COSENA=CX0*CX1+CY0*CY1+CZ0*CZ1
      COSENB=CX0*CX2+CY0*CY2+CZ0*CZ2
      COSENC=CX0*CX3+CY0*CY3+CZ0*CZ3
      IF((PIG-DACOS(COSENC)).LE.THLIM)THEN
C        WRITE(6,*)'IERR=4 - ray incident out of limit angle'
          IERR=4
        RETURN
      ENDIF
      DETER=CX1*(CY2*CZ3-CY3*CZ2)-CY1*(CX2*CZ3-CZ2*CX3)
      CXST=COSENA*(CY2*CZ3-CY3*CZ2)-CY1*(COSENB*CZ3+CZ2*COSENC)
      CXST=CXST/DETER
      CYST=CX1*(COSENB*CZ3+CZ2*COSENC)-COSENA*(CX2*CZ3-CZ2*CX3)
      CYST=CYST/DETER
      CZST=CX1*(-CY2*COSENC-COSENB*CY3)-
     &  CY1*(-CX2*COSENC-COSENB*CX3)+COSENA*(CX2*CY3-CY2*CX3)
      CZST=CZST/DETER
C----------------------------------------------------------------------
      N=N+1
      D=D+DSQRT((XP-XST)**2+(YP-YST)**2+(ZP-ZST)**2)
      XP=XST
      YP=YST
      ZP=ZST
      CX0=CXST
      CY0=CYST
      CZ0=CZST
      GOTO 2
C-----------------------------------------------------------------
      END
