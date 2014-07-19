*
* $Id: cwicof.F,v 1.1.1.1 1996/03/22 16:42:49 mclareni Exp $
*
* $Log: cwicof.F,v $
* Revision 1.1.1.1  1996/03/22 16:42:49  mclareni
* Phtools
*
*
      FUNCTION CWICOF(S,I)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        COMMON/CWICO/TIN,TOUT,RR,ELLE,THM,STHM,A,THLIM,
     &  AINF,ASUP,PIG,EPS,MAXF,NMAX
      COMMON/CURDAT/XP,YP,ZP,CX0,CY0,CZ0
      IF(S.EQ.AINF)THEN
        CWICOF=1.
        RETURN
      ENDIF
       IF(S.EQ.ASUP)THEN
        CWICOF=-1.
        RETURN
      ENDIF
      TGTH=(TOUT/2.+DSQRT((XP+S*CX0)**2+(YP+S*CY0)**2))/(ZP+S*CZ0)
      TH=DATAN(TGTH)
      IF(TH.LT.THM)THEN
        CWICOF=1.
        RETURN
      ELSE
        RHO=(1.+STHM)*TOUT/(1.-DCOS(TH+THM))
        CWICOF=RHO**2-(TOUT/2.+DSQRT((XP+S*CX0)**2+(YP+S*CY0)**2))**2-
     &    (ZP+S*CZ0)**2
      RETURN
      ENDIF
      END
