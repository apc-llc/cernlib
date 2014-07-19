*
* $Id: guout.F,v 1.2 1996/02/22 13:24:02 ravndal Exp $
*
* $Log: guout.F,v $
* Revision 1.2  1996/02/22 13:24:02  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:22  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE GUOUT
C.    *
C.    *       User routine called at the end of each event
C.    *
#include "geant321/gcflag.inc"
#include "geant321/gckine.inc"
#include "pvolum.inc"
#include "celoss.inc"
      DIMENSION IDLK(3),NIDLK(3)
      SAVE IDLK,NID1,NID3,NIDLK
      DATA      IDLK / 6,7,8 /
      DATA NID1/0/, NID3/0/ , NIDLK/3*0/
 
      DLC = 0.
      DRC = 0.
 
      DO 2 I = 1,NLTOT
         SEL1 (I) = SEL1 (I) + DEDL(I)
         SEL2 (I) = SEL2 (I) + DEDL(I)**2
         DLC      = DLC      + DEDL(I)
         SEL1C(I) = SEL1C(I) + DLC
         SEL2C(I) = SEL2C(I) + DLC**2
   2  CONTINUE
 
      DO 3 I = 1,NRTOT
         SER1 (I) = SER1 (I) + DEDR(I)
         SER2 (I) = SER2 (I) + DEDR(I)**2
         DRC      = DRC      + DEDR(I)
         SER1C(I) = SER1C(I) + DRC
         SER2C(I) = SER2C(I) + DRC**2
   3  CONTINUE
 
      DO 14 IPAT = 1,4
      DO 14 NPL  = 1,NLTOT
         SNPAT1(NPL,IPAT) = SNPAT1(NPL,IPAT) + FNPAT(NPL,IPAT)
         SNPAT2(NPL,IPAT) = SNPAT2(NPL,IPAT) + FNPAT(NPL,IPAT)**2
   14 CONTINUE
 
      ESEEN = 100.*DLC/PKINE(3)
      CALL HFF1( 1,NID1, ESEEN,1.)
 
      ETOT = ESEEN
      DO 16 LK=1,3
         ELEAK(LK) = 100.*ELEAK(LK)/PKINE(3)
         CALL HFF1(IDLK(LK),NIDLK(LK),ELEAK(LK),1.)
         ETOT = ETOT + ELEAK(LK)
   16 CONTINUE
      CALL HFF1( 9,NID3, ETOT,1.)
 
      END
