*
* $Id: ipscol.F,v 1.2 2005/10/28 12:38:14 couet Exp $
*
* $Log: ipscol.F,v $
* Revision 1.2  2005/10/28 12:38:14  couet
* - SVG driver, workstation type -779
*
* Revision 1.1.1.1  1996/02/14 13:11:07  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/09 27/09/94  17.04.43  by  O.Couet
*-- Author :
      SUBROUTINE IPSCOL(ICOL)
*.===========>
*.
*.  Set the current color
*.
*. _Input parameters:
*.
*. INTEGER ICOL : Color Index.
*.
*..==========> (O.Couet)
#include "higz/hiflag.inc"
#include "higz/hipost.inc"
#include "higz/hilut.inc"
*.______________________________________
*
      IPSCI=ICOL
      IF(IPSCI.LT.0)IPSCI=0
      IF(IPSCI.GT.NBCLUT-1)IPSCI=NBCLUT-1
      I=IPSCI+1
      IF(VFLAG)THEN
         CALL IPPSTF(5,'"rgb(')
         CALL IPKOUT(INT(REDLUT(I)*255.))
         CALL IPPSTF(1,',')
         CALL IPKOUT(INT(GRNLUT(I)*255.))
         CALL IPPSTF(1,',')
         CALL IPKOUT(INT(BLULUT(I)*255.))
         CALL IPPSTF(2,')"')
      ELSE
         IF(REDLUT(I).EQ.PSRED.AND.
     +      GRNLUT(I).EQ.PSGRN.AND.
     +      BLULUT(I).EQ.PSBLU)RETURN
         PSRED=REDLUT(I)
         PSGRN=GRNLUT(I)
         PSBLU=BLULUT(I)
         IF(.NOT.TFLAG) THEN
            IF(REDLUT(I).LE.0..AND.
     +         GRNLUT(I).LE.0..AND.
     +         BLULUT(I).LE.0.)THEN
               CALL IPPSTF(6,' black')
            ELSE
               CALL IPFOUT(REDLUT(I))
               CALL IPFOUT(GRNLUT(I))
               CALL IPFOUT(BLULUT(I))
               CALL IPPSTF(2,' c')
            ENDIF
         ENDIF
      ENDIF
*
      END
