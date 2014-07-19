*
* $Id: isacfl.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: isacfl.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE ISACFL
C--------------------------------------------------------------------
C-                                                                  -
C-      Trivial calorimeter simulation.                             -
C-      Assume uniform y and phi bins.                              -
C-      Fill Zebra banks  ISAC and ISCL. Needs banks ISP1 and ISV1  -
C-                                                                  -
C-                   SDP May,1986, Rev. May,1988                    -
C-                                                                  -
C--------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "izisac.inc"
#include "iziscl.inc"
#include "izisp1.inc"
#include "izisv1.inc"
#include "zebcom.inc"
      INTEGER NCY,NCPHI
      REAL YCMIN,YCMAX,PI
      PARAMETER (NCY=80,NCPHI=64)
      PARAMETER (YCMIN=-4.,YCMAX=4.,PI=3.1415938)
      INTEGER NCHGM(NCY,NCPHI)
      REAL EHAD(NCY,NCPHI),EEM(NCY,NCPHI)
      REAL CTHCAL(NCY),STHCAL(NCY),CPHCAL(NCPHI),SPHCAL(NCPHI)
      REAL ETACAL(NCY),PHICAL(NCPHI)
      REAL ETA,PHI,THX,EIP,DELPHI,DELY,ET,ETMIN
      REAL SPHI,CPHI,STH,CTH,EEMC,EHADC
      INTEGER IDABS,IY,IPHI,JETNO(NCY,NCPHI)
      INTEGER LISV1,LISP1,LISAC,LISCL,IOISAC,IOISCL,GZISAC
      LOGICAL FIRST,CEDIT
      DATA FIRST/.TRUE./
      LISAC=GZISAC()
      IF(LISAC.NE.0) GOTO 999      ! already exists
C
C          INITIALIZE E ARRAYS
      IF(FIRST) THEN
        DO 50 IPHI=1,NCPHI
        DO 50 IY=1,NCY
        NCHGM(IY,IPHI)=0
        EEM(IY,IPHI)=0.
   50   EHAD(IY,IPHI)=0.
        CALL MZFORM('ISAC','2I 4F',IOISAC)
        CALL MZFORM('ISCL','2I -F',IOISCL)
        FIRST=.FALSE.
C
C            INITIALIZE PSEUDO-CALORIMETER
C
        DELPHI=2.*PI/NCPHI
        DELY=(YCMAX-YCMIN)/NCY
C
C          CALCULATE TRIG. FUNCTIONS.
        DO 60 IPHI=1,NCPHI
        PHI=DELPHI*(IPHI-.5)
        PHICAL(IPHI)=PHI
        CPHCAL(IPHI)=COS(PHI)
        SPHCAL(IPHI)=SIN(PHI)
60      CONTINUE
        DO 70 IY=1,NCY
        ETA=DELY*(IY-.5)+YCMIN
        THX=2.*ATAN(EXP(-ETA))
        ETACAL(IY)=ETA
        CTHCAL(IY)=COS(THX)
        STHCAL(IY)=SIN(THX)
70      CONTINUE
      ENDIF
C
C            Fill calorimeter description in ISAC
C
      CALL MZBOOK(IXMAIN,LISAC,LISAE,-IZISAC,
     $            'ISAC',3,3,6,IOISAC,-1)
C
      IQ(LISAC+1)=NCPHI    ! number of phi cells
      IQ(LISAC+2)=NCY      !   "        y    "
      Q(LISAC+3)=DELPHI   ! phi cell size
      Q(LISAC+4)=DELY     !  y    "   "
      Q(LISAC+5)=YCMIN    ! minimum y
      Q(LISAC+6)=YCMAX    ! maximum y
C
C          Fill calorimeter cells
C
      LISV1=LISAE-IZISV1
C
  100 LISV1=LQ(LISV1)        ! loop over vertices
      IF(LISV1.GT.0) THEN
        LISP1=LISV1-IZISP1
C
  200   LISP1=LQ(LISP1)      ! loop over particles
        IF(LISP1.GT.0) THEN
          IDABS=IABS(IQ(LISP1+1))
          IF(IDABS.EQ.11.OR.IDABS.EQ.13.OR.IDABS.EQ.15
     $        .OR.IDABS.EQ.14..OR.IDABS.EQ.30) GO TO 200
          EIP=Q(LISP1+5)
          PHI=Q(LISP1+7)
          ETA=Q(LISP1+9)
          IF(ETA.LT.YCMIN.OR.ETA.GT.YCMAX) GO TO 200
          IY=INT((ETA-YCMIN)/DELY)+1
          IPHI=INT(PHI/DELPHI)+1
          JETNO(IY,IPHI)=LQ(LISP1-2)    ! pointer to initial parton
          IF(IDABS.EQ.10.OR.IDABS.EQ.12.OR.IDABS.EQ.110.OR.
     $        IDABS.EQ.220) THEN
            EEM(IY,IPHI)=EEM(IY,IPHI)+EIP
            IF(IDABS.EQ.10) THEN
              NCHGM(IY,IPHI)=NCHGM(IY,IPHI)+1
            ELSE
              NCHGM(IY,IPHI)=NCHGM(IY,IPHI)+10000
            ENDIF
          ELSE
            EHAD(IY,IPHI)=EHAD(IY,IPHI)+EIP
            NCHGM(IY,IPHI)=NCHGM(IY,IPHI)+10000
          ENDIF
C
        GOTO 200
        ENDIF
C
      GOTO 100
      ENDIF
C
C        Create Zebra banks ISCL (calorimeter cells)
      LISCL=0
      DO 300 IY=1,NCY
      DO 300 IPHI=1,NCPHI
      EEMC=EEM(IY,IPHI)
      EHADC=EHAD(IY,IPHI)
      STH=STHCAL(IY)
      CTH=CTHCAL(IY)
      SPHI=SPHCAL(IPHI)
      CPHI=CPHCAL(IPHI)
      ETA=ETACAL(IY)
      IF(CEDIT(EEMC,EHADC,SPHI,CPHI,STH,CTH,ETA)) THEN
        IF(LISCL.EQ.0) THEN
          CALL MZBOOK(IXMAIN,LISCL,LISAC,-IZISCL,
     $                'ISCL',2,0,10,IOISCL,-1)
        ELSE
          CALL MZBOOK(IXMAIN,LISCL,LISCL,0,
     $                'ISCL',2,0,10,IOISCL,-1)
        ENDIF
C
        LQ(LISCL-1)=JETNO(IY,IPHI)   ! pointer to initial parton
        IQ(LISCL+1)=IY+IPHI*10000
        IQ(LISCL+2)=NCHGM(IY,IPHI)   ! no. of gammas+10000*(no. of charged)
        Q(LISCL+3)=EEMC
        Q(LISCL+4)=EHADC
        Q(LISCL+5)=STH
        Q(LISCL+6)=CTH
        Q(LISCL+7)=SPHI
        Q(LISCL+8)=CPHI
        Q(LISCL+9)=PHICAL(IPHI)
        Q(LISCL+10)=ETA
      ENDIF
      NCHGM(IY,IPHI)=0
      EEM(IY,IPHI)=0.
      EHAD(IY,IPHI)=0.
  300 CONTINUE
  999 RETURN
      END
