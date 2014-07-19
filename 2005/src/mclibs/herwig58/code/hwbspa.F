*
* $Id: hwbspa.F,v 1.1.1.1 1996/03/08 17:02:10 mclareni Exp $
*
* $Log: hwbspa.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:10  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWBSPA.
*CMZ :-        -26/04/91  14.26.44  by  Federico Carminati
*-- Author :    Ian Knowles
C------------------------------------------------------------------------
      SUBROUTINE HWBSPA
C------------------------------------------------------------------------
C     Given initial space-like parton INITBR with interference partner
C     INTERF and spin density DECPAR(INITBR), reconstructs time-like
C     four momenta in jet cascade and returns the spin density matrix
C     RHOPAR(INITBR). First makes a pass backwards down the space-like
C     branches and then advances evolving each time-like side branch
C     (using HWBTIM), assigning azimuthal angles according to the
C     algorithm in: I.G. Knowles, Comp. Phys. Comm. 58 (90) 271.
C
C     On input PPAR(1-5,*) contains:  On output PPAR(1-5,*) contains:
C     (P-trans,DXi,P-long,E,M)        (P-x,P-y,P-z,E,M) (if TMBR(*))
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      LOGICAL EICOR
      INTEGER JPAR,KPAR,LPAR,MPAR
      DOUBLE PRECISION HWRGEN,DMIN,PT,EIKON,EISCR,EINUM,EIDEN1,EIDEN2,
     & WT,SPIN,Z1,Z2,TR,PRMAX,CX,SX,CAZ,ROHEP(3),RMAT(3,3),ZERO2(2)
      DATA ZERO2/ZERO, ZERO/
      DATA DMIN/1.D-15/
      IF (IERROR.NE.0) RETURN
      JPAR=2
      KPAR=1
      IF (NPAR.EQ.2) THEN
         CALL HWVZRO(2,RHOPAR(1,2))
         RETURN
      ENDIF
C Generate azimuthal angle of JPAR's branching using an M-function
C     Find the daughters of JPAR, with LPAR time-like
  10  LPAR=JDAPAR(1,JPAR)
      IF (TMPAR(LPAR)) THEN
         MPAR=LPAR+1
      ELSE
         MPAR=LPAR
         LPAR=MPAR+1
      ENDIF
C Soft correlations
      CALL HWUROT(PPAR(1,JPAR), ONE,ZERO,RMAT)
      CALL HWUROF(RMAT,PPAR(1,KPAR),ROHEP)
      PT=MAX(SQRT(ROHEP(1)*ROHEP(1)+ROHEP(2)*ROHEP(2)),DMIN)
      EIKON=1.
      EICOR=AZSOFT.AND.IDPAR(LPAR).EQ.13
      IF (EICOR) THEN
         EISCR=1.-PPAR(5,MPAR)*PPAR(5,MPAR)/(MIN(PPAR(2,LPAR),
     &   PPAR(2,MPAR))*PPAR(4,MPAR)*PPAR(4,MPAR))
         EINUM=PPAR(4,KPAR)*PPAR(4,LPAR)*ABS(PPAR(2,LPAR)-PPAR(2,MPAR))
         EIDEN1=PPAR(4,KPAR)*PPAR(4,LPAR)-ROHEP(3)*PPAR(3,LPAR)
         EIDEN2=PT*ABS(PPAR(1,LPAR))
         EIKON=MAX(EISCR+EINUM/MAX(EIDEN1-EIDEN2,DMIN),ZERO)
      ENDIF
C Spin correlations
      WT=0.
      SPIN=1.
      IF (AZSPIN.AND.IDPAR(JPAR).EQ.13) THEN
         Z1=PPAR(4,JPAR)/PPAR(4,MPAR)
         Z2=1.-Z1
         IF (IDPAR(MPAR).EQ.13) THEN
            TR=Z1/Z2+Z2/Z1+Z1*Z2
         ELSEIF (IDPAR(MPAR).LT.13) THEN
            TR=(Z1*Z1+Z2*Z2)/2.
         ENDIF
         WT=Z2/(Z1*TR)
      ENDIF
C Assign the azimuthal angle
      PRMAX=(1.+ABS(WT))*EIKON
  50  CALL HWRAZM( ONE,CX,SX)
      CALL HWUROT(PPAR(1,JPAR),CX,SX,RMAT)
C Determine the angle between the branching planes
      CALL HWUROF(RMAT,PPAR(1,KPAR),ROHEP)
      CAZ=ROHEP(1)/PT
      PHIPAR(1,JPAR)=2.*CAZ*CAZ-1.
      PHIPAR(2,JPAR)=2.*CAZ*ROHEP(2)/PT
      IF (EICOR) EIKON=MAX(EISCR+EINUM/MAX(EIDEN1-EIDEN2*CAZ,DMIN),ZERO)
      IF (AZSPIN) SPIN=1.+WT*(DECPAR(1,JPAR)*PHIPAR(1,JPAR)
     &                       +DECPAR(2,JPAR)*PHIPAR(2,JPAR))
      IF (SPIN*EIKON.LT.HWRGEN(0)*PRMAX) GOTO 50
C Construct full 4-momentum of LPAR, sum P-trans of MPAR
      PPAR(2,LPAR)=0.
      PPAR(2,MPAR)=0.
      CALL HWUROB(RMAT,PPAR(1,LPAR),PPAR(1,LPAR))
      CALL HWVDIF(2,PPAR(1,2),PPAR(1,LPAR),PPAR(1,2))
C Test for end of space-like branches
      IF (JDAPAR(1,MPAR).EQ.0) GO TO 60
C     Generate new Decay matrix
      CALL HWBAZF(MPAR,JPAR,ZERO2,DECPAR(1,JPAR),
     &            PHIPAR(1,JPAR),DECPAR(1,MPAR))
C     Advance along the space-like branch
      JPAR=MPAR
      KPAR=LPAR
      GOTO 10
C Retreat along space-like line
C     Assign initial spin density matrix
  60  CALL HWVEQU(2,ZERO2,RHOPAR(1,MPAR))
      CALL HWUMAS(PPAR(1,2))
  70  IF (MPAR.EQ.2) RETURN
C Construct spin density matrix for time-like branch
      CALL HWBAZF(MPAR,JPAR,RHOPAR(1,MPAR),PHIPAR(1,JPAR),
     &                      DECPAR(1,JPAR),RHOPAR(1,LPAR))
C Evolve time-like side branch
      CALL HWBTIM(LPAR,MPAR)
C Construct spin density matrix for space-like branch
      CALL HWBAZF(MPAR,JPAR,PHIPAR(1,JPAR),RHOPAR(1,MPAR),
     &                      DECPAR(1,LPAR),RHOPAR(1,JPAR))
C Find parent and partner of MPAR
      MPAR=JPAR
      JPAR=JMOPAR(1,MPAR)
      LPAR=MPAR+1
      IF (JMOPAR(1,LPAR).NE.JPAR) LPAR=MPAR-1
      GOTO 70
      END
