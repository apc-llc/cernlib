*
* $Id: pazfz.F,v 1.1.1.1 1996/03/01 11:38:44 mclareni Exp $
*
* $Log: pazfz.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:44  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.06/18 28/02/95  13.33.32  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAZFZ
*
*            /ZEBRA/FZ
*           Operations on FZ files
*
#include "paw/pcpatl.inc"
#include "paw/pcchar.inc"
      CHARACTER*16 CHOPT
*
      CALL KUPATL(CHPATL,NPAR)
*
      IF(CHPATL.EQ.'FILE')THEN
         CALL PAWFOP
*
      ELSEIF(CHPATL.EQ.'TOALPHA')THEN
         CALL KUGETF(CHFILE,NCH)
         CALL KUOPEN(97,CHFILE,'UNKNOWN',ISTAT)
         IF(ISTAT.NE.0)RETURN
         CALL FZFILE(97,900,'OA')
         CALL RZTOFZ(97,' ')
         CALL FZENDO(97,'T')
         CALL PACLOS(97)
*
      ELSEIF(CHPATL.EQ.'FRALPHA')THEN
         CALL KUGETF(CHFILE,NCH)
         CALL KUOPEN(97,CHFILE,'VERYOLD',ISTAT)
         IF(ISTAT.NE.0)RETURN
         CALL FZFILE(97,900,'IA')
         CALL RZFRFZ(97,' ')
         CALL FZENDI(97,'T')
         CALL PACLOS(97)
*
      ELSEIF(CHPATL.EQ.'TOFZ')THEN
         CALL KUGETI(LUN)
         CALL PALUNF(LUN,2,IFREE)
         IF(IFREE.NE.2)RETURN
         CALL KUGETC(CHOPT,NCH)
         CALL RZTOFZ(LUN,CHOPT)
*
      ELSEIF(CHPATL.EQ.'FRFZ')THEN
         CALL KUGETI(LUN)
         CALL PALUNF(LUN,2,IFREE)
         IF(IFREE.NE.1)RETURN
         CALL KUGETC(CHOPT,NCH)
         CALL RZFRFZ(LUN,CHOPT)
*
      ENDIF
*
      END
