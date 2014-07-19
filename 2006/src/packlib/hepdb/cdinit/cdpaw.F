*
* $Id: cdpaw.F,v 1.1.1.1 1996/02/28 16:24:14 mclareni Exp $
*
* $Log: cdpaw.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:14  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDPAW(JWPAW,JHBOOK,IDIV,CHNAME,JW,JWMAX,CHOPT,IRC)
 
*
*     Initialise (optionally) Zebra, HBOOK and HEPDB
*
 
      CHARACTER*(*) CHNAME
      COMMON /PAWC/  NWPAWC,IXPAWC,IHBOOK,IXHIGZ,IXKU,IFENCE(5)
     +,              LMAIN, IPAW(9989)
*
*     chopt: Z - zebra
*            P - paw
*            H - hbook
*
#include "hepdb/hdbopts.inc"
 
      IRC = 0
 
      NWPAW  = JWPAW
      NHBOOK = JHBOOK
      NW     = JW
      NWMAX  = JWMAX
*
*     Zebra?
*
      IF(IOPTZ.NE.0) THEN
         CALL MZEBRA(-1)
         IF(NHBOOK.GT.0) NHBOOK = -NHBOOK
      ENDIF
*
*     MZPAW?
*
      IF(IOPTP.NE.0) THEN
         CALL MZPAW(NWPAW,CHOPT)
         NHBOOK = 0
      ENDIF
*
*     HBOOK?
*
      IF(IOPTH.NE.0) THEN
         CALL HLIMIT(NHBOOK)
      ENDIF
 
      CALL MZDIV(IXPAWC,IDIV,CHNAME,NW,NWMAX,'LCM')
      CALL CDFRST(IDIV)
*
      END
