*
* $Id: pbhlun.F,v 1.1.1.1 1996/03/01 11:39:06 mclareni Exp $
*
* $Log: pbhlun.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:06  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/07 31/05/94  09.27.12  by  Rene Brun
*-- Author :    Alfred Nathaniel   07/08/92
      SUBROUTINE PBHLUN(BRCLAS,BRNAME,VARSET)
      CHARACTER*(*) BRCLAS,BRNAME,VARSET
*
*            Browser interface to return next HBOOK file
*
#include "hbook/hcdire.inc"
#include "paw/pawlun.inc"
      CHARACTER CHLUN*4
      SAVE ICHT
*-------------------------------------------------
*
      IF(BRNAME.EQ.' ') ICHT=0
 1    CONTINUE
      ICHT=ICHT+1
      IF(ICHT.LE.NCHTOP) THEN
         LUN=ICHTOP(ICHT)
*
*-- piaf or pawserv files
*
         IF (LUN .GT. 1000) THEN
            LUN = ICHLUN(ICHT)
            IF(LUN.NE.0)GO TO 2
            IF(ICHTYP(ICHT).NE.0)GO TO 1
            IF (BRCLAS.NE.'HBOOK') go to 1
            BRNAME=CHTOP(ICHT)
            VARSET='root=//'//BRNAME(:LENOCC(BRNAME))//
     +      ' file='''//HFNAME(ICHT)(:LENOCC(HFNAME(ICHT)))//''''
               RETURN
         ENDIF
*
*-- already open unit in chain
*
   2     IF (LUN.GT.200 .AND. LUN.LT.300) THEN
            LUN = LUN - 200
         ENDIF
*
         IF(LUN.LT.0 .AND. BRCLAS.EQ.'HBOOK') THEN
            BRNAME=CHTOP(ICHT)
            VARSET='root=//'//BRNAME(:LENOCC(BRNAME))//
     +         ' file='''//HFNAME(ICHT)(:LENOCC(HFNAME(ICHT)))//''''
            RETURN
*
         ELSEIF(LUN.EQ.0 .AND. BRCLAS.EQ.'PAWC') THEN
            BRNAME=CHTOP(ICHT)
            VARSET='root=//'//BRNAME(:LENOCC(BRNAME))//
     +         ' file='''//HFNAME(ICHT)(:LENOCC(HFNAME(ICHT)))//''''
            RETURN
         ELSEIF(LUN.GE.1 .AND. LUN.LE.128) THEN
            IF(LUNIT(LUN).EQ.3 .AND. BRCLAS.EQ.'HBOOK') THEN
*
*            HBOOK file (skip chain when no file connected)
*
               IF (HFNAME(ICHT) .EQ. 'Chain') GOTO 1
*
               BRNAME=CHTOP(ICHT)
               WRITE(CHLUN,'(I4)') LUN
               CALL HCLEFT(CHLUN,1,4)
               VARSET='root=//'//BRNAME(:LENOCC(BRNAME))//
     +            ' file='''//HFNAME(ICHT)(:LENOCC(HFNAME(ICHT)))//
     +            ''' unit='//CHLUN(1:4)
               RETURN
            ELSEIF(LUNIT(LUN).EQ.4 .AND. BRCLAS.EQ.'HIGZ') THEN
*
*            HIGZ file
*
               BRNAME=CHTOP(ICHT)
               WRITE(CHLUN,'(I4)') LUN
               CALL HCLEFT(CHLUN,1,4)
               VARSET='root=//'//BRNAME(:LENOCC(BRNAME))//
     +            ' file='//HFNAME(ICHT)(:LENOCC(HFNAME(ICHT)))//
     +            ' unit='//CHLUN(1:4)
               RETURN
            ENDIF
         ENDIF
         GOTO 1
      ENDIF
      BRNAME=' '
      END

