*
* $Id: izinit.F,v 1.1.1.1 1996/02/14 13:11:10 mclareni Exp $
*
* $Log: izinit.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:10  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.06/06 08/03/89  15.00.12  by  O.Couet
*-- Author :
      SUBROUTINE IZINIT(NWORDS)
*.===========>
*.
*.   This routine initialize the IZ package :
*.
*. -Creates an HIGZ division .
*. -Initialise the links .
*. -Initialize the following parameters :
*.
*.  INSIZ : Default (NT) bank size .
*.  IISIZ : Default (I) bank size .
*.  IFSIZ : Default (F) bank size .
*.  ISSIZ : Default (C) bank size .
*.  NBPICT : Number of pictures in memory .
*.  LPICT : Current picture link . If < 0 , there is no current picture .
*.  LPICD : Displayed picture link . If < 0 , there is no displayed picture .
*.  KXXX : Flag for XXX attributes . Indicate if a primitive with XXX
*.        attributes has been called since the last call to the
*.        XXX attribute .
*.
*. _Input parameters:
*.
*.  INTEGER NWORDS : Minimal ZEBRA space in memory
*.                   (if=0 , standart memory allocation)
*.
*..==========> (O.Couet)
#include "higz/hiatnb.inc"
#include "higz/hipaw.inc"
#include "higz/hiflat.inc"
      DIMENSION IRFLAT(NBRATT),IIFLAT(NBIATT)
      EQUIVALENCE (IRFLAT(1),KMKSC),(IIFLAT(1),KPLCI)
*.______________________________________
*
*              Default bank sizes initialisation
*
      INSIZ=100
      IISIZ=500
      IFSIZ=1000
      ISSIZ=100
*
*              ZEBRA space initialisation
*
      NW=NWORDS
      IF(NW.LT.500)NW=500
      IF(NW.GT.NWPAW-1000)NW=NWPAW-1000
*
*              HIGZ division initialisation
*              Links  initialisation
*
      CALL MZDIV(IXPAWC,IXHIGZ,'HIGZ',NW,NWPAW,'P')
      CALL MZLINK(IXPAWC,'/HILINK/',LHIGZ,LWV,LAST)
*
*              Number of pictures=0
*              No current IZ picture
*              Display picture=current IZ picture
*
      NBPICT=0
      LPICT=-1
      LPICD=LPICT
*
*              Attributes flags initialisation
*
      DO 10 I=1,NBRATT
         IRFLAT(I)=0
   10 CONTINUE
      DO 20 I=1,NBIATT
         IIFLAT(I)=0
   20 CONTINUE
*
      END
#endif
