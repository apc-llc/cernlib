*
* $Id: hplist.F,v 1.2 1999/05/18 08:50:56 couet Exp $
*
* $Log: hplist.F,v $
* Revision 1.2  1999/05/18 08:50:56  couet
* - Protection added if NCOL is too large
*
* Revision 1.1.1.1  1996/01/19 10:50:09  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.12/01 12/03/92  11.50.17  by  O.Couet
*-- Author :    O.Couet   23/10/90
      SUBROUTINE HPLIST(LISTNB,NI,LISTI)
*.===========>
*.
*.   Define a list of attributes
*.
*. _Input parameters:
*.
*.  INTEGER LISTNB : List number
*.  INTEGER NI     : Number of elements to be define
*.  INTEGER LISTI  : List elements
*.
*.  If NI <= 0, an automatic palette is defined from 9 to
*.              NCOL (defined by IGSET NCOL). If NCOL <= 8
*.              the palette is defined from 1 to NCOL also
*.              but the color definitions are not changed.
*.  If LISTNB <= 0, the list of the defined palettes is printed.
*.
*..==========> (O.Couet)
#include "hplot/hpl14.inc"
      DIMENSION LISTI(*)
      DIMENSION LUT(8)
      DATA LUT /0,5,7,3,6,2,4,1/
*.______________________________________
*
*
*              Print the list of the Defined the palette
*
      IF(LISTNB.LE.0)THEN
         PRINT*,'The define lists are :'
         DO 10 I=1,NBLIST
            IF(LSTTYP(I).NE.0)PRINT*,I
   10    CONTINUE
         RETURN
      ENDIF
*
*             The palette LISTNB is defined automatically
*
      LN=MIN(NBLIST,LISTNB)
      IF(NI.EQ.0)THEN
         LSTTYP(LN)=1
         CALL IGQ('NCOL',RVAL)
         N=INT(RVAL)
         IF(N.LE.8)THEN
            DO 20 I=1,N
               ILIST(LN,I)=LUT(I)
   20       CONTINUE
         ELSE
            N=MAX(N-8,2)
            IF (N.GT.MXLIST) THEN
               CALL IGERR('NCOL is too large','HPLIST')
               N = MXLIST
            ENDIF
            DEC=240./N
            H=240.
            DO 30 I=1,N
               CALL IGHTOR(H,0.4,1.,R,G,B)
               CALL ISCR(1,I+7,R,G,B)
               ILIST(LN,I)=I+7
               H=H-DEC
   30       CONTINUE
         ENDIF
         NBEL(LN)=N
         RETURN
      ENDIF
*
*              Define the list
*
      N=MIN(NI,MXLIST)
      NBEL(LN)=N
      LSTTYP(LN)=1
      DO 40 I=1,N
         ILIST(LN,I)=LISTI(I)
   40 CONTINUE
*
      END
