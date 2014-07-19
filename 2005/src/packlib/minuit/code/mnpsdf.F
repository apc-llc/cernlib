*
* $Id: mnpsdf.F,v 1.2 1996/03/15 18:02:50 james Exp $
*
* $Log: mnpsdf.F,v $
* Revision 1.2  1996/03/15 18:02:50  james
*     Modified Files:
* mnderi.F eliminate possible division by zero
* mnexcm.F suppress print on STOP when print flag=-1
*          set FVAL3 to flag if FCN already called with IFLAG=3
* mninit.F set version 96.03
* mnlims.F remove arguments, not needed
* mnmigr.F VLEN -> LENV in debug print statement
* mnparm.F move call to MNRSET to after NPAR redefined, to zero all
* mnpsdf.F eliminate possible division by zero
* mnscan.F suppress printout when print flag =-1
* mnset.F  remove arguments in call to MNLIMS
* mnsimp.F fix CSTATU so status is PROGRESS only if new minimum
* mnvert.F eliminate possible division by zero
*
* Revision 1.1.1.1  1996/03/07 14:31:31  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNPSDF
#include "minuit/d506dp.inc"
CC        calculates the eigenvalues of v to see if positive-def.
CC        if not, adds constant along diagonal to make positive.
#include "minuit/d506cm.inc"
      CHARACTER CHBUFF*12
      DIMENSION S(MNI)
      EPSMIN = 1.0E-6
      EPSPDF = MAX(EPSMIN, EPSMA2)
      DGMIN = VHMAT(1)
C                        Check if negative or zero on diagonal
      DO 200 I= 1, NPAR
      NDEX = I*(I+1)/2
      IF (VHMAT(NDEX) .LE. ZERO) THEN
          WRITE (CHBUFF(1:3),'(I3)') I
          CALL MNWARN('W',CFROM,
     +'Negative diagonal element'//CHBUFF(1:3)//' in Error Matrix')
      ENDIF
      IF (VHMAT(NDEX) .LT. DGMIN)  DGMIN = VHMAT(NDEX)
  200 CONTINUE
      IF (DGMIN .LE. ZERO) THEN
         DG = (ONE+EPSPDF) - DGMIN
         WRITE (CHBUFF,'(E12.2)') DG
         CALL MNWARN('W',CFROM,
     +     CHBUFF//' added to diagonal of error matrix')
      ELSE
         DG = ZERO
      ENDIF
C                    Store VHMAT in P, make sure diagonal pos.
      DO 213 I= 1, NPAR
      NDEX = I*(I-1)/2
      NDEXD = NDEX + I
      VHMAT(NDEXD) = VHMAT(NDEXD) + DG
      IF (VHMAT(NDEXD) .LE. ZERO)   VHMAT(NDEXD) = 1.0
      S(I) = 1.0/SQRT(VHMAT(NDEXD))
      DO 213 J= 1, I
      NDEX =  NDEX + 1
  213 P(I,J) = VHMAT(NDEX) * S(I)*S(J)
C      call eigen (p,p,maxint,npar,pstar,-npar)
      CALL MNEIG(P,MAXINT,NPAR,MAXINT,PSTAR,EPSPDF,IFAULT)
      PMIN = PSTAR(1)
      PMAX = PSTAR(1)
      DO 215 IP= 2, NPAR
      IF (PSTAR(IP) .LT. PMIN)  PMIN = PSTAR(IP)
      IF (PSTAR(IP) .GT. PMAX)  PMAX = PSTAR(IP)
  215 CONTINUE
      PMAX = MAX(ABS(PMAX), ONE)
      IF ((PMIN .LE. ZERO .AND. LWARN) .OR.  ISW(5) .GE. 2) THEN
         WRITE (ISYSWR,550)
         WRITE (ISYSWR,551) (PSTAR(IP),IP=1,NPAR)
      ENDIF
      IF (PMIN .GT. EPSPDF*PMAX)  GO TO 217
      IF (ISW(2) .EQ. 3)  ISW(2)=2
      PADD = 1.0E-3*PMAX - PMIN
      DO 216 IP= 1, NPAR
      NDEX = IP*(IP+1)/2
  216 VHMAT(NDEX) = VHMAT(NDEX) *(1.0 + PADD)
      CSTATU= 'NOT POSDEF'
      WRITE (CHBUFF,'(G12.5)') PADD
      CALL MNWARN('W',CFROM,
     +   'MATRIX FORCED POS-DEF BY ADDING '//CHBUFF//' TO DIAGONAL.')
  217 CONTINUE
C
  550 FORMAT (' EIGENVALUES OF SECOND-DERIVATIVE MATRIX:' )
  551 FORMAT (7X,6E12.4)
      RETURN
      END
