*
* $Id: epstat.F,v 1.2 1996/09/20 14:05:27 cernlib Exp $
*
* $Log: epstat.F,v $
* Revision 1.2  1996/09/20 14:05:27  cernlib
* clean up selection of format 2004
*
* Revision 1.1.1.1  1996/03/08 15:21:44  mclareni
* Epio
*
*
#include "epio/pilot.h"
      SUBROUTINE EPSTAT
C. PRINTS ALL ACTUAL CONTROL WORDS.
#include "epio/epiocom.inc"
      WRITE(NOUTUT,2001) NMUNIT,NWUNIT,NCONT
      WRITE(NOUTUT,2002) (LIST(I),I=1,NCONT)
      N=LIST(1)
      IF(N.LE.0)  GOTO 77777
      N=MIN0(NMUNIT,N)
      DO 1  I=1,N
      WRITE(NOUTUT,2003) I
      K=NCONT+NWUNIT*(I-1)
      WRITE(NOUTUT,2004) (LIST(K+J),J=1,NWUNIT)
    1 CONTINUE
77777 RETURN
#if !defined(CERNLIB_CDC)
2001  FORMAT(//,' ++++++++ EP FORMAT STATUS ROUTINE ++++++++',//,
     1         ' MAX. NO. OF UNITS            =',I8,/,
     2         ' NO. OF CONTROL WORDS/UNIT    =',I8,/,
     3         ' NO. OF OVERALL CONTROL WORDS =',I8)
2002  FORMAT(/ ,' OVERALL CONTROL WORDS ARE',//,
     1         ' NO. OF ACTIVE UNITS          =',I8,/,
     2         ' MAX. NO. OF ERROR PRINTS     =',I8,/,
     3         ' ACTUAL NO. OF ERROR PRINTS   =',I8,/,
     4         ' BITS / MACHINE WORD          =',I8,/,
     5         ' NO. OF 16 BIT WORDS TO PAD TO=',I8,/,
     6         ' STANDARD P.H. LENGTH         =',I8,/,
     7         ' NO. OF ACCESSIBLE CONTR.W.   =',I8,/,
     8         ' LOGICAL OUTPUT UNIT          =',I8)
2003  FORMAT(/ ,' CONTROL WORDS OF INTERNAL UNIT',I8,/)
#endif
#if defined(CERNLIB_CDC)
2001  FORMAT(//," ++++++++ EP FORMAT STATUS ROUTINE ++++++++",//,
     1         " MAX. NO. OF UNITS            =",I8,/,
     2         " NO. OF CONTROL WORDS/UNIT    =",I8,/,
     3         " NO. OF OVERALL CONTROL WORDS =",I8)
2002  FORMAT(/ ," OVERALL CONTROL WORDS ARE",//,
     1         " NO. OF ACTIVE UNITS          =",I8,/,
     2         " MAX. NO. OF ERROR PRINTS     =",I8,/,
     3         " ACTUAL NO. OF ERROR PRINTS   =",I8,/,
     4         " BITS / MACHINE WORD          =",I8,/,
     5         " NO. OF 16 BIT WORDS TO PAD TO=",I8,/,
     6         " STANDARD P.H. LENGTH         =",I8,/,
     7         " NO. OF ACCESSIBLE CONTR.W.   =",I8,/,
     8         " LOGICAL OUTPUT UNIT          =",I8)
2003  FORMAT(/ ," CONTROL WORDS OF INTERNAL UNIT",I8,/)
#endif
#if defined(CERNLIB_UNIVAC)
2004  FORMAT(1X,10I8,/,1X,10I8,/,1X,3I8,O16,I8)
#else
2004  FORMAT(1X,10I8)
#endif
      END
