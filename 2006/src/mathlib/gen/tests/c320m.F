*
* $Id: c320m.F,v 1.1.1.1 1996/04/01 15:01:15 mclareni Exp $
*
* $Log: c320m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:15  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C320M
C     This program tests the operation of MATHLIB subprograms
C     CELFUN and WELFUN

#include "imp64r.inc"
c     REAL RRIZET
C     Set maximum error allowed for test to be considered successful
      DIMENSION TOL(2)

#include "gen/def64.inc"
     + Z0, Z1, Z2, HF, XK2, U, V, SUK, CUK, DUK, SVK, CVK, DVK,
     + XKP2, SUKP, CUKP, DUKP, SVKP, CVKP, DVKP

      LOGICAL LTEST
#include "gen/defc64.inc"
     +           I,Z,SN,CN,DN,SNT,CNT,DNT
      COMPLEX    IS,ZS,SNS,CNS,DNS,SNTS,CNTS,DNTS
      PARAMETER ( Z0 = 0, Z1 = 1, Z2 = 2, HF = Z1/2)
#if defined(CERNLIB_CMPXDOUB)
      PARAMETER (I = (0D0,1D0))
#endif
#if !defined(CERNLIB_CMPXDOUB)
      PARAMETER (I = (0E0,1E0))
#endif
      PARAMETER (IS= (0E0,1E0))
#include "iorc.inc"
      DATA LTEST/.TRUE./
      DATA TOL/2D-5, 1D-12/


      CALL HEADER('C320',0)

         ERRMAX= 0.0E0
      JF=1
      WRITE(LOUT,'(/10X,''TEST FOR CELFUN'')')
      WRITE(LOUT,'(/9X,''Z'',12X,''XK'',7X,''SNerror'',3X,''CNerror''
     1,3X,''DNerror'')')
      DO 11 IX = -4,4
      DO 11 IY = -4,4,4
      Z=HF*(IX+I*IY)
      U=Z
      V=-I*Z
      DO 11 IK = 0,4
      XK=IK/(2*Z2)
      XK2=XK**2
      XKP2=1-XK2

      ZS=Z
      RU=U
      RV=V
      RXK=XK
      RXK2=XK2
      RXKP2=XKP2

      CALL CELFUN(ZS,RXK2,SNS,CNS,DNS)
      CALL ELFUN(RU,RXK2,RSUK,RCUK,RDUK)
      CALL ELFUN(RV,RXK2,RSVK,RCVK,RDVK)
      CALL ELFUN(RU,RXKP2,RSUKP,RCUKP,RDUKP)
      CALL ELFUN(RV,RXKP2,RSVKP,RCVKP,RDVKP)
      RD=1/(1-(RSVKP*RDUK)**2)
      SNTS=(RSUK*RDVKP+IS*RCUK*RDUK*RSVKP*RCVKP)*RD
      CNTS=(RCUK*RCVKP-IS*RSUK*RDUK*RSVKP*RDVKP)*RD
      DNTS=(RDUK*RCVKP*RDVKP-IS*RXK2*RSUK*RCUK*RSVKP)*RD
      RSNE=0
      IF(SNS .NE. 0) RSNE=ABS((SNS-SNTS)/SNS)
      RCNE=ABS((CNS-CNTS)/CNS)
      RDNE=ABS((DNS-DNTS)/DNS)
      RERRMAX=ERRMAX
      ERRMAX=MAX(RDNE,RCNE,RSNE,RERRMAX)
      WRITE(LOUT,'(1X,''('',F6.2,'' ,'',F6.2,'' )'',F10.6,3(1P,D10.1))')
     1 ZS,RXK,RSNE,RCNE,RDNE
   11 CONTINUE

#if !defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF+1)
#endif
#if defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF)
#endif
      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,D9.1)') ERRMAX
      LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
         ERRMAX= 0E0

      WRITE(LOUT,'(/9X,''Z'',12X,''XK'',7X,''SNerror'',3X,''CNerror''
     1,3X,''DNerror'')')
      DO 12 IK = 0,4
      XK=IK/(2*Z2)
      XK2=XK**2
      XKP2=1-XK2
      DO 12 IX = -4,4
      DO 12 IY = -4,4,4
      Z=HF*(IX+I*IY)
      U=Z
      V=-I*Z

      ZS=Z
      RU=U
      RV=V
      RXK=XK
      RXK2=XK2
      RXKP2=XKP2

      CALL CELFUN(ZS,RXK2,SNS,CNS,DNS)
      CALL ELFUN(RU,RXK2,RSUK,RCUK,RDUK)
      CALL ELFUN(RV,RXK2,RSVK,RCVK,RDVK)
      CALL ELFUN(RU,RXKP2,RSUKP,RCUKP,RDUKP)
      CALL ELFUN(RV,RXKP2,RSVKP,RCVKP,RDVKP)
      RD=1/(1-(RSVKP*RDUK)**2)
      SNTS=(RSUK*RDVKP+IS*RCUK*RDUK*RSVKP*RCVKP)*RD
      CNTS=(RCUK*RCVKP-IS*RSUK*RDUK*RSVKP*RDVKP)*RD
      DNTS=(RDUK*RCVKP*RDVKP-IS*RXK2*RSUK*RCUK*RSVKP)*RD
      RSNE=0
      IF(SNS .NE. 0) RSNE=ABS((SNS-SNTS)/SNS)
      RCNE=ABS((CNS-CNTS)/CNS)
      RDNE=ABS((DNS-DNTS)/DNS)
      RERRMAX=ERRMAX
      ERRMAX=MAX(RDNE,RCNE,RSNE,RERRMAX)
      WRITE(LOUT,'(1X,''('',F6.2,'' ,'',F6.2,'' )'',F10.6,3(1P,D10.1))')
     1 ZS,RXK,RSNE,RCNE,RDNE
   12 CONTINUE
#if !defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF+1)
#endif
#if defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF)
#endif
      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,D9.1)') ERRMAX
      LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
         ERRMAX= 0E0

      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
      RZ2=Z2
      CALL CELFUN(IS,RZ2,SNS,CNS,DNS)
#if defined(CERNLIB_DOUBLE)
         ERRMAX= 0D0
      JF=2
      WRITE(LOUT,'(/10X,''TEST FOR WELFUN'')')
      WRITE(LOUT,'(/9X,''Z'',12X,''XK'',7X,''SNerror'',3X,''CNerror''
     1,3X,''DNerror'')')
      DO 1 IX = -4,4
      DO 1 IY = -4,4,4
      Z=HF*(IX+I*IY)
      U=Z
      V=-I*Z
      DO 1 IK = 0,4
      XK=IK/(2*Z2)
      XK2=XK**2
      XKP2=1-XK2
      CALL WELFUN(Z,XK2,SN,CN,DN)
      CALL DELFUN(U,XK2,SUK,CUK,DUK)
      CALL DELFUN(V,XK2,SVK,CVK,DVK)
      CALL DELFUN(U,XKP2,SUKP,CUKP,DUKP)
      CALL DELFUN(V,XKP2,SVKP,CVKP,DVKP)
      D=1/(1-(SVKP*DUK)**2)
      SNT=(SUK*DVKP+I*CUK*DUK*SVKP*CVKP)*D
      CNT=(CUK*CVKP-I*SUK*DUK*SVKP*DVKP)*D
      DNT=(DUK*CVKP*DVKP-I*XK2*SUK*CUK*SVKP)*D
      SNE=0
      IF(SN .NE. 0) SNE=ABS((SN-SNT)/SN)
      CNE=ABS((CN-CNT)/CN)
      DNE=ABS((DN-DNT)/DN)
      ERRMAX=MAX(ABS(DNE),ABS(CNE),ABS(SNE),ERRMAX)
      WRITE(LOUT,'(1X,''('',F6.2,'' ,'',F6.2,'' )'',F10.6,3(1P,D10.1))')
     1 Z,XK,SNE,CNE,DNE
    1 CONTINUE
      ETOL=TOL(2)

      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,D9.1)') ERRMAX
      LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
         ERRMAX= 0D0

      WRITE(LOUT,'(/9X,''Z'',12X,''XK'',7X,''SNerror'',3X,''CNerror''
     1,3X,''DNerror'')')
      DO 2 IK = 0,4
      XK=IK/(2*Z2)
      XK2=XK**2
      XKP2=1-XK2
      DO 2 IX = -4,4
      DO 2 IY = -4,4,4
      Z=HF*(IX+I*IY)
      U=Z
      V=-I*Z
      CALL WELFUN(Z,XK2,SN,CN,DN)
      CALL DELFUN(U,XK2,SUK,CUK,DUK)
      CALL DELFUN(V,XK2,SVK,CVK,DVK)
      CALL DELFUN(U,XKP2,SUKP,CUKP,DUKP)
      CALL DELFUN(V,XKP2,SVKP,CVKP,DVKP)
      D=1/(1-(SVKP*DUK)**2)
      SNT=(SUK*DVKP+I*CUK*DUK*SVKP*CVKP)*D
      CNT=(CUK*CVKP-I*SUK*DUK*SVKP*DVKP)*D
      DNT=(DUK*CVKP*DVKP-I*XK2*SUK*CUK*SVKP)*D
      SNE=0
      IF(SN .NE. 0) SNE=ABS((SN-SNT)/SN)
      CNE=ABS((CN-CNT)/CN)
      DNE=ABS((DN-DNT)/DN)
      ERRMAX=MAX(ABS(DNE),ABS(CNE),ABS(SNE),ERRMAX)
      WRITE(LOUT,'(1X,''('',F6.2,'' ,'',F6.2,'' )'',F10.6,3(1P,D10.1))')
     1 Z,XK,SNE,CNE,DNE
    2 CONTINUE
      ETOL=TOL(2)
      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,D9.1)') ERRMAX
      LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
         ERRMAX= 0D0

      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
      CALL WELFUN(I,Z2,SN,CN,DN)

#endif

C     Check if the test was successful
       IRC=ITEST('C320',LTEST)
      CALL PAGEND('C320')
      END
