*
* $Id: gphxsd.F,v 1.1.1.1 1995/10/24 10:22:16 cernlib Exp $
*
* $Log: gphxsd.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:16  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE GPHXSD
C.
C.    ******************************************************************
C.    *                                                                *
C.    *  Debugs  PHXS bank containing x-section constants              *
C.    *                                                                *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gcjloc.inc"
#include "geant321/gcpmxz.inc"
#include "geant321/gcphxs.inc"
#include "geant321/gc10ev.inc"
      CHARACTER*20 CHTMED, CHMATE
*
* Loop over material
      DO 10 ITM=1,IQ(JTMED-2)
         JTM=LQ(JTMED-ITM)
         IF(JTM.GT.0) THEN
            IMA=Q(JTM+6)
            JMA=LQ(JMATE-IMA)
            CALL UHTOC(IQ(JTM+1),4,CHTMED,20)
            CALL UHTOC(IQ(JMA+1),4,CHMATE,20)
            WRITE(6,10000) CHTMED(:LNBLNK(CHTMED)),
     +      CHMATE(:LNBLNK(CHMATE))
            JPHOT = LQ(JMA-6)
            JPHXS=LQ(JPHOT-1)
            NZ=Q(JPHXS+1)
            NIT=Q(JPHXS+3*NZ+2)
            WRITE(6,10100) NZ
            WRITE(6,10200) (Q(JPHXS+1+IZ),Q(JPHXS+1+2*NZ+IZ),IZ=1,NZ)
            WRITE(6,10300)(IT+1,(Q(JPHXS+3*NZ+2+5*IT+IP),IP=1,5),IT=0,
     +      NIT- 1)
         ENDIF
   10 CONTINUE
10000 FORMAT(' Tracking medium : ',A,' Material : ',A)
10100 FORMAT(1X,I3,' elements in this material with ',I3,
     +       ' energy intervals')
10200 FORMAT('    Z    = ',F5.2,'    Weight   = ',F5.2)
10300 FORMAT((' Int. N. ',I2,' E= ',E11.4,' K= ',4(E11.4,',')))
      END
