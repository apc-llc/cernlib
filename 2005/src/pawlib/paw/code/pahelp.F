*
* $Id: pahelp.F,v 1.1.1.1 1996/03/01 11:38:40 mclareni Exp $
*
* $Log: pahelp.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:40  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 18/04/95  16.31.43  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAHELP
*
*.          Interface between KUIP and various HELP routines of
*.          HPLOT and HIGZ
*.
#include "hbook/hcunit.inc"
#include "paw/pcpatl.inc"
      COMMON /HILUN/ LUNIN,LUNOUT,LUNERR
*
      CALL KUHELP(LUN,CHPATL)
*
      IF(CHPATL(1:3).EQ.'SET')THEN
         LOLD=LERR
         LERR=LUN
         LOLD2=LUNOUT
         LUNOUT=LUN
         CALL HPLSET('SHOW',0.)
         LUNOUT=LOLD2
         LERR=LOLD
      ELSEIF(CHPATL(1:6).EQ.'OPTION')THEN
         LOLD=LERR
         LERR=LUN
         CALL HPLOPT('SHOW',1)
         LERR=LOLD
      ELSEIF(CHPATL(1:5).EQ.'IGSET')THEN
         LOLD=LUNOUT
         LUNOUT=LUN
         CALL IGSET('SHOW',0.)
         LUNOUT=LOLD
      ELSEIF(CHPATL(1:5).EQ.'SLIDE')THEN
#if defined(CERNLIB_SLIDE)
         LOLD=LUNOUT
         LUNOUT=LUN
         CALL SLIHLP
         LUNOUT=LOLD
#endif
      ELSEIF(CHPATL(1:4).EQ.'CALL')THEN
#if defined(CERNLIB_COMIS)
         LOLD=LERR
         LERR=LUN
         CALL PAWCSH
         LERR=LOLD
#endif
      ENDIF
*
      END

