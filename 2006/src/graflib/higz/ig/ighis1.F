*
* $Id: ighis1.F,v 1.1.1.1 1996/02/14 13:10:34 mclareni Exp $
*
* $Log: ighis1.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:34  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.07/01 13/07/89  14.34.05  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGHIS1(N,IOPTG,IOPTX,IOPTY)
*.===========>
*.
*.   Take the LOG10 of XWORK and YWORK according to the value of
*. IOPT and put it in XWORKL and YWORKL.
*.
*. _Input parameters:
*.
*.  INTEGER N : Number of channels in XWORK and in YWORK.
*.  INTEGER IOPT : IGHIST options.
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#include "higz/hihist.inc"
*.______________________________________
*
      DO 10 I=1,N
         XWORKL(I)=XWORK(I)
         YWORKL(I)=YWORK(I)
         IF(IOPTG.NE.0.AND.IOPTX.NE.0)THEN
            IF(XWORKL(I).GT.0.)THEN
               XWORKL(I)=LOG10(XWORKL(I))
            ELSE
               XWORKL(I)=RWXMIN
            ENDIF
         ENDIF
         IF(IOPTG.NE.0.AND.IOPTY.NE.0)THEN
            IF(YWORKL(I).GT.0.)THEN
               YWORKL(I)=LOG10(YWORKL(I))
            ELSE
               YWORKL(I)=RWYMIN
            ENDIF
         ENDIF

   10 CONTINUE
*
      END
