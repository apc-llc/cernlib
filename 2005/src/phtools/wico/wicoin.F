*
* $Id: wicoin.F,v 1.1.1.1 1996/03/22 16:42:49 mclareni Exp $
*
* $Log: wicoin.F,v $
* Revision 1.1.1.1  1996/03/22 16:42:49  mclareni
* Phtools
*
*
      SUBROUTINE WICOIN(UTIN,UTOUT,URR,INMAX,UELLE)
******************************************************
*
*       NAME: WICOIN
*       AUTHOR: ANTONIO DI DOMENICO
*       LANGUAGE: FORTRAN-77
*       VERSION: 1.0
*       RELEASED: 11-APR-1994
*
******************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        COMMON/CWICO/TIN,TOUT,RR,ELLE,THM,STHM,A,THLIM,
     &  AINF,ASUP,PIG,EPS,MAXF,NMAX
C---------- initialization --------------------------------
      TIN=UTIN
      TOUT=UTOUT
        IF(TOUT.GE.TIN)STOP'error - TIN smaller than TOUT'
      RR=URR
      NMAX=INMAX
      PIG=4.*DATAN(1.D0)
C--------- Winston Cone parameters calculation -------
      STHM=TOUT/TIN
      THM=DASIN(STHM)
      ELLE=TOUT*(1.+1/STHM)/(2.*DTAN(THM))
      UELLE=ELLE
      IF(RR.GT.1.)THLIM=DASIN(1./RR)
      IF(RR.LE.1.)THLIM=-1000.
      A=(1.-STHM)/TOUT/DCOS(THM)**2/2.
C------- DZERO routine initialization parameters ------------------
      EPS=1.D-15
      MAXF=100
      AINF=0.
C------ end of initialization ---------------------------------
      END
