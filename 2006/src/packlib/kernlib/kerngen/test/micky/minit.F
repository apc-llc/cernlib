*
* $Id: minit.F,v 1.1.1.1 1996/02/15 17:54:53 mclareni Exp $
*
* $Log: minit.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:53  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE MINIT

#include "kerngent/mkcde.inc"

      DIMENSION MBCD(47)

      DATA  MBCD / 4H    ,4H0   ,4H1   ,4H2   ,4H3   ,
     +             4H4   ,4H5   ,4H6   ,4H7   ,4H8   ,
     +             4H9   ,4HA   ,4HB   ,4HC   ,4HD   ,
     +             4HE   ,4HF   ,4HG   ,4HH   ,4HI   ,
     +             4HJ   ,4HK   ,4HL   ,4HM   ,4HN   ,
     +             4HO   ,4HP   ,4HQ   ,4HR   ,4HS   ,
     +             4HT   ,4HU   ,4HV   ,4HW   ,4HX   ,
     +             4HY   ,4HZ   ,4H+   ,4H-   ,4H=   ,
     +             4H*   ,4H/   ,4H(   ,4H)   ,4H.   ,
     +             4H,   ,4H'   /


      IQBITW = 32
      IQCHAW =  4
      ITB    =  6

      ITIMES = 0

      DO 14 J=1,47
   14 IBCD(J)=MBCD(J)

      DO 24 J=1,100
   24 INTG(J)=J

C----     ZEROV(1-5)  CONTAIN THE LEVELS OF PRECISION EXPECTED AFTER
C-        (1) NO, (2) LITTLE, (3) SOME, (4) FAIR, (5) MUCH FLOATING CALC

#if defined(CERNLIB_B60)||defined(CERNLIB_B64)
      ZEROV(1) = 1.0 E-20
      ZEROV(2) = 1.0 E-12
      ZEROV(3) = 1.0 E-8
      ZEROV(4) = 1.0 E-6
      ZEROV(5) = 1.0 E-4
#else
      ZEROV(1) = 1.0 E-10
      ZEROV(2) = 1.0 E-6
      ZEROV(3) = 1.0 E-5
      ZEROV(4) = 1.0 E-4
      ZEROV(5) = 1.0 E-3
#endif

      ZERGU = ZEROV(1)
      ZERLEV= ZEROV(1)

      NFAIPR = 4
      NEACHP = 0
      NFAILT = 0
      NFAIL  = 0
      NTEST  = 0

      WRITE (ITB,9000)
      WRITE (ITB,9001)
      CALL KERNGT (ITB)
      IF (IFLGU.EQ.0)        RETURN

      RETURN

 9000 FORMAT ('1'/' MICKY executing.')
 9001 FORMAT (1X/1X,
     + 40H  MICKY    2.71  950712  9.30           )
      END
