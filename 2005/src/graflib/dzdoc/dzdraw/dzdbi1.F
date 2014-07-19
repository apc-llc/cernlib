*
* $Id: dzdbi1.F,v 1.1.1.1 1996/03/04 16:13:10 mclareni Exp $
*
* $Log: dzdbi1.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:10  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDBI1(CTEMP,IFWID,NBITS, IQPRNT)
*.
*...DZDBI1  write headline for bitwise dump indicating the bit positions
*.
*. INPUT     : CTEMP   character array as working space
*.             IFWID   field width
*.             NBITS   number of bits
*.             IQPRNT  printing unit
*. OUTPUT    :
*.
*. CALLED    :  DZDBIT
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*.**********************************************************************
*.
      CHARACTER*(*) CTEMP
*----
      ICLINE = 10+NBITS*IFWID
      IP = 9 + IFWID
      DO 10 I=1,ICLINE
   10 CTEMP(I:I) = '-'
      WRITE(IQPRNT,10000)CTEMP(1:ICLINE)
      CTEMP(1:ICLINE) = ' '
      DO 20 I=1,NBITS
      IDIG = I/10
      IF(IDIG .NE. 0)WRITE(CTEMP(IP:IP),10100)IDIG
      IP = IP + IFWID
   20 CONTINUE
      WRITE(IQPRNT,10000)CTEMP(1:ICLINE)
      CTEMP(1:ICLINE) = ' '
      IP = 9 + IFWID
      DO 30 I=1,NBITS
      IDIG = MOD(I,10)
      WRITE(CTEMP(IP:IP),10100)IDIG
      IP = IP + IFWID
   30 CONTINUE
      WRITE(IQPRNT,10000)CTEMP(1:ICLINE)
      DO 40 I=1,ICLINE
   40 CTEMP(I:I) = '-'
      WRITE(IQPRNT,10000)CTEMP(1:ICLINE)
10000 FORMAT(' ',A)
10100 FORMAT(I1)
      END
