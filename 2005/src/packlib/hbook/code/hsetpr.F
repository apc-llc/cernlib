*
* $Id: hsetpr.F,v 1.1.1.1 1996/01/16 17:07:48 mclareni Exp $
*
* $Log: hsetpr.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:48  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 05/03/89  09.22.02  by  Rene Brun
*-- Author :
      SUBROUTINE HSETPR (NAME,VALUE)
*.==========>
*.      SETS A HFIT OR HPARAM PARAMETER 'NAME' TO A SPECIFIED VALUE
*..=========> ( R.Brun ,D.Lienart )
#include "hbook/hcpar0.inc"
      CHARACTER*4 NAME,NAMPAR(9)
      DIMENSION VALPAR(9)
      EQUIVALENCE (PNEF,VALPAR(1))
      SAVE NAMPAR
      DATA NAMPAR /'PNEF','PNBF','PSEL','PFLV','PLUN','PNBX','PNCX',
     +             'FNIX','FEPS'/
*
*
      DO 10 I=1,9
         IF (NAME.EQ.NAMPAR(I)) THEN
            VALPAR(I)=VALUE
            RETURN
         ENDIF
   10 CONTINUE
      END
