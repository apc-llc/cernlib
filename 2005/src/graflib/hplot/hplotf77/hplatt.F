*
* $Id: hplatt.F,v 1.1.1.1 1996/01/19 10:49:57 mclareni Exp $
*
* $Log: hplatt.F,v $
* Revision 1.1.1.1  1996/01/19 10:49:57  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.18/12 09/06/94  15.04.10  by  O.Couet
*-- Author :    O.Couet   08/06/94
      SUBROUTINE HPLATT(IOBJ)
*.==========>
*.
*. This routine sets the HIGZ attributes according to
*. the HPLOT attributes for a given HPLOT object:
*.
*. IOBJ is the object identifier:
*.
*. 1 = Histograms
*. 2 = Functions
*.
*..=========>
#include "hplot/hpl1.inc"
*._____________________________
*
      GOTO (10,20), IOBJ
*
   10 CALL ISLWSC(FLOAT(IHWID))
      IF(IHTYP.NE.0)THEN
         IAHTYP                = IABS(IHTYP)
         IHFAIS                = IAHTYP/1000 - 1
         IF(IHFAIS.LT.0)IHFAIS = 3
         CALL ISFAIS(IHFAIS)
         IHFASI                = MOD(IAHTYP,1000)
         IF(IHTYP.LT.0)IHFASI  = -IHFASI
         IF((IHFASI.NE.0).AND.(IHFAIS.GE.2))CALL ISFASI(IHFASI)
         CALL ISFACI(IHCOL)
         CALL ISPLCI(IHCOL)
      ELSE
         CALL ISFAIS(0)
         CALL ISFACI(IHCOL)
         CALL ISPLCI(IHCOL)
      ENDIF
      IF(IHBCOL.NE.0)CALL ISPLCI(IHBCOL)
      CALL IGSET('BORD',1.)
      GOTO 999
*
   20 CALL ISLWSC(FLOAT(IFWID))
      IF(.NOT.LOASTK.AND.IFTYP.NE.0)THEN
         IAFTYP                = IABS(IFTYP)
         IFFAIS                = IAFTYP/1000 - 1
         IF(IFFAIS.LT.0)IFFAIS = 3
         CALL ISFAIS(IFFAIS)
         IFFASI                = MOD(IAFTYP,1000)
         IF(IFTYP.LT.0)IFFASI  = -IFFASI
         IF((IFFASI.NE.0).AND.(IFFAIS.GE.2))CALL ISFASI(IFFASI)
         CALL ISFACI(IFCOL)
         CALL ISPLCI(IFCOL)
      ENDIF
      IF(IFBCOL.NE.0)THEN
         CALL ISPLCI(IFBCOL)
         CALL IGSET('BORD',1.)
      ELSE
         CALL IGSET('BORD',0.)
      ENDIF
*
  999 END
