*
* $Id: ipltyp.F,v 1.1.1.1 1996/02/14 13:11:06 mclareni Exp $
*
* $Log: ipltyp.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:06  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/09 27/09/94  17.04.43  by  O.Couet
*-- Author :
      SUBROUTINE IPLTYP(LTYPE)
*.===========>
*.
*.   This routine writes the commands for changing the line style
*.   to the Postscript/LaTeX metafile.
*.
*. _Input parameters:
*.
*.  INTEGER LTYPE : line type
*.                  2 = dashed
*.                  3 = dotted
*.                  4 = dash-dotted
*                   else = solid
*.
*..==========> (A.Nathaniel)
#include "higz/hiflag.inc"
#include "higz/hipost.inc"
      IF(LTYPE.EQ.IPSLT)RETURN
      IPSLT=LTYPE
      IF(TFLAG) THEN
         IF(LTYPE.EQ.2) THEN
*--- dashed line
            CALL IPPSTR(BSLASH//'let'//BSLASH//'higzline='//
     +         BSLASH//'higzdashed'//BSLASH//'higzoffs=0'//
     +         BSLASH//'higzadash=12'//BSLASH//'higzbdash=24'//
     +         BSLASH//'higzcdash=36'//BSLASH//'higzddash=48')
         ELSEIF(LTYPE.EQ.3) THEN
*--- dotted line
            CALL IPPSTR(BSLASH//'let'//BSLASH//'higzline='//
     +         BSLASH//'higzdashed'//BSLASH//'higzoffs=0'//
     +         BSLASH//'higzadash=1'//BSLASH//'higzbdash=12'//
     +         BSLASH//'higzcdash=13'//BSLASH//'higzddash=24')
         ELSEIF(LTYPE.EQ.4) THEN
*--- dash-dotted line
            CALL IPPSTR(BSLASH//'let'//BSLASH//'higzline='//
     +         BSLASH//'higzdashed'//BSLASH//'higzoffs=0'//
     +         BSLASH//'higzadash=12'//BSLASH//'higzbdash=24'//
     +         BSLASH//'higzcdash=25'//BSLASH//'higzddash=36')
         ELSEIF(LTYPE.NE.0) THEN
*--- solid line
            CALL IPPSTR(BSLASH//'let'//BSLASH//'higzline='//
     +         BSLASH//'higzsolid')
         ENDIF
      ELSE
         IF(LTYPE.EQ.2) THEN
            CALL IPPSTF(13,' [12 12] 0 sd')
         ELSEIF(LTYPE.EQ.3) THEN
            CALL IPPSTF(11,' [4 8] 0 sd')
         ELSEIF(LTYPE.EQ.4) THEN
            CALL IPPSTF(18,' [12 15 4 15] 0 sd')
         ELSE
            CALL IPPSTF(8,' [] 0 sd')
         ENDIF
      ENDIF
      END
