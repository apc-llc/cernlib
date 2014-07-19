*
* $Id: guout.F,v 1.2 1996/02/01 14:37:26 ravndal Exp $
*
* $Log: guout.F,v $
* Revision 1.2  1996/02/01 14:37:26  ravndal
* Clean up of the repository
*
* Revision 1.1.1.1  1995/10/24 10:22:09  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE GUOUT
C.
C.    *
C.    *       User routine called at the end of each event
C.    *
C.
C
 
#include "geant321/gcflag.inc"
#include "geant321/gcscan.inc"
#include "pvolum.inc"
#include "celoss.inc"
      SAVE NID1
      DATA NID1/0/
 
      IF(ISWIT(9).NE.0) RETURN
      IF(SCANFL) THEN
         CALL GSCANO
      ELSE
         DLC = 0.
         DRC = 0.
 
         DO 10 I = 1,NL
            SEL1 (I) = SEL1 (I) + DEDL(I)
            SEL2 (I) = SEL2 (I) + DEDL(I)**2
 
            DLC = DLC + DEDL(I)
            SEL1C(I) = SEL1C(I) + DLC
            SEL2C(I) = SEL2C(I) + DLC**2
   10    CONTINUE
 
         DO 20 I = 1,NR
            SER1 (I) = SER1 (I) + DEDR(I)
            SER2 (I) = SER2 (I) + DEDR(I)**2
 
            DRC = DRC + DEDR(I)
            SER1C(I) = SER1C(I) + DRC
            SER2C(I) = SER2C(I) + DRC**2
   20    CONTINUE
 
         DO 30 IPAT = 1,3
            DO 30 NPL = 1,NL
               SNPAT1(NPL,IPAT) = SNPAT1(NPL,IPAT) + FNPAT(NPL,IPAT)
               SNPAT2(NPL,IPAT) = SNPAT2(NPL,IPAT) + FNPAT(NPL,IPAT)**
     +         2
   30    CONTINUE
 
         ETOT = 100.*IEVENT*DLC/EINTOT
         CALL HFF1( 1,NID1, ETOT,1.)
 
      END IF
 
      END
