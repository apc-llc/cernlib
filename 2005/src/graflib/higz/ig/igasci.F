*
* $Id: igasci.F,v 1.1.1.1 1996/02/14 13:10:32 mclareni Exp $
*
* $Log: igasci.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:32  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.12/08 28/03/91  11.53.12  by  O.Couet
*-- Author :    O.Couet   28/03/91
      INTEGER FUNCTION IGASCI(CC)
*.===========>
*.
*. Return the ASCII code of the printable characters
*.
*..==========> (O.Couet)
      CHARACTER*1 CC
#if defined(CERNLIB_IBM)
      DIMENSION IEB(95)
      DATA IEB /
     + 64, 90,127,123, 91,108, 80,125, 77, 93, 92, 78,107, 96, 75, 97,
     +240,241,242,243,244,245,246,247,248,249,122, 94, 76,126,110,111,
     +124,193,194,195,196,197,198,199,200,201,209,210,211,212,213,214,
     +215,216,217,226,227,228,229,230,231,232,233,173,224,189, 95,109,
     +121,129,130,131,132,133,134,135,136,137,145,146,147,148,149,150,
     +151,152,153,162,163,164,165,166,167,168,169,192, 79,208,161/
#endif
*.______________________________________
*
      IC=ICHAR(CC)
#if defined(CERNLIB_IBM)
      IGASCI=32
      DO 10 I=1,95
         IF(IEB(I).EQ.IC)THEN
            IGASCI=31+I
            RETURN
         ENDIF
   10 CONTINUE
#endif
#if !defined(CERNLIB_IBM)
      IGASCI=IC
#endif
*
      END
