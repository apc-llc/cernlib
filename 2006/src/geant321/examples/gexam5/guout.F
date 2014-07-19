*
* $Id: guout.F,v 1.2 1996/02/01 15:03:14 ravndal Exp $
*
* $Log: guout.F,v $
* Revision 1.2  1996/02/01 15:03:14  ravndal
* Clean up repository
*
* Revision 1.1.1.1  1995/10/24 10:22:17  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE GUOUT
*
C.    ******************************************************************
C.    *                                                                *
C.    *       User routine called at the end of each event.            *
C.    *                                                                *
C.    ******************************************************************
C.
C.
#include "geant321/gcunit.inc"
#include "geant321/gcflag.inc"
#include "nascom.inc"
*
      SAVE ID101,ID102,ID103,ID104
      DATA ID101,ID102,ID103,ID104/4*0/
C.
C.    ------------------------------------------------------------------
C.
      ITWO=0
      IF((ED1.GT.1.E-10).AND.(ED2.GT.1.E-10))THEN
         NTWO=NTWO+1
         ITWO=1
         CALL HFF1(101,ID101,ED1,1.)
         CALL HFF1(102,ID102,ED2,1.)
         CALL HFF1(103,ID103,ETOT,1.)
         CALL HFF2(104,ID104,ED2,ED1,1.)
      ENDIF
C
      IF(IDEBUG.NE.0)THEN
         IF(ITWO.NE.0.OR.ISWIT(6).NE.0)THEN
            WRITE(LOUT,701)IEVENT,NTWO,ED1,ED2,ETOT
 701        FORMAT(' --------- SUMMARY OF EVENT NR ',I6,
     +      ' NTWO =',I5,' ----------',/
     +      ' ENERGY DEPOSIT IN D1 :',E11.3,' GEV',/
     +      ' ENERGY DEPOSIT IN D2 :',E11.3,' GEV',/
     +      ' SUM OF ENERGY DEPOSITS:',E11.3,' GEV')
*
         ENDIF
      ENDIF
  100 RETURN
*
      END
C
