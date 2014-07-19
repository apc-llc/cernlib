*
* $Id: pazdz.F,v 1.1.1.1 1996/03/01 11:38:44 mclareni Exp $
*
* $Log: pazdz.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:44  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  1.05/02 03/01/89  10.05.15  by  Rene Brun
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAZDZ
*
*           /ZEBRA/DZ
*           Using the ZEBRA debug package
*
#include "hbook/hcbook.inc"
#include "paw/pcpatl.inc"
      CHARACTER*8 CHOPT
      CHARACTER*4 BNAME
*
      CALL KUPATL(CHPATL,NPAR)
*
      IF(CHPATL.EQ.'SHOW')THEN
         CALL KUGETC(BNAME,NCH)
         CALL KUGETI(NUMBER)
         CALL KUGETC(CHOPT,NCH)
         LOC=LZLOC(IXPAWC,BNAME,NUMBER)
         IF(LOC.NE.0)THEN
            CALL DZSHOW('DZSHOW',IXPAWC,LOC,CHOPT,0,0,0,0)
         ENDIF
*
      ELSEIF(CHPATL.EQ.'SURV')THEN
         CALL KUGETC(BNAME,NCH)
         CALL KUGETI(NUMBER)
         LOC=LZLOC(IXPAWC,BNAME,NUMBER)
         IF(LOC.NE.0)THEN
            CALL DZSURV('DZSURV',IXPAWC,LOC)
         ENDIF
*
      ELSEIF(CHPATL.EQ.'SNAP')THEN
         CALL KUGETI(IDIV)
         CALL KUGETC(CHOPT,NCH)
         CALL DZSNAP('DZSNAP',IDIV,CHOPT)
*
      ELSEIF(CHPATL.EQ.'VERIFY')THEN
         CALL KUGETI(IDIV)
         CALL KUGETC(CHOPT,NCH)
         CALL DZVERI('DZVERI',IDIV,CHOPT)
*
      ELSEIF(CHPATL.EQ.'STORE')THEN
         CALL KUGETI(IXSTOR)
         CALL DZSTOR('DZSTOR',IXSTOR)
      ENDIF
*
      END
