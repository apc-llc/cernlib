*
* $Id: dzdbit.F,v 1.1.1.1 1996/03/04 16:13:10 mclareni Exp $
*
* $Log: dzdbit.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:10  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDBIT(IXSTOR,L,IFIRST,ILAST,IFWID,IZERO)
*.
*...DZDBIT   bitwise dump of data words
*.
*. INPUT     :  IXSTOR  store index
*.              L       link to bank to be dumped
*.              IFIRST   first word to dump
*.              ILAST    last word
*.              IFWID    field width
*.              IZERO    if /=0 print also 0 values
*. COMMON    :
*. SEQUENCE  : MZCA   MZCB   QSTORE ZEBQ   ZUNIT
*. CALLS     : DZDBI1
*. CALLED    : DZDDAW
*.
*. REPORT CONDITIONS
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*.**********************************************************************
*.
*
#include "zebra/zebq.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
#include "zebra/zunit.inc"
#include "dzdchv.inc"
*
      PARAMETER (MAXLPP=10)
      CHARACTER*132 CTEMP
*----
#include "zebra/qstore.inc"
      IF(ILAST .NE. 0)THEN
            IL1 = ILAST
      ELSE
            IL1 = IQ(KQS+L-1)
      ENDIF
      IF(IFIRST .LE. 0)THEN
           IF1=1
      ELSE
           IF1=IFIRST
      ENDIF
      IF(IF1 .GT. IL1)THEN
            ISTEP = -1
      ELSE
            ISTEP = 1
      ENDIF
      IF(IFWID .GT. 3)IFWID = 3
      IF(IFWID .LT. 1)IFWID = 1
      ICLINE = 10+32*IFWID
      NL = MAXLPP + 1
      DO 20 I=IF1,IL1,ISTEP
      IF(NL .GT. MAXLPP)THEN
            CALL DZDBI1(CTEMP,IFWID,32,IQPRNT)
            NL = 0
      ENDIF
      NL = NL + 1
      IP = 9 + IFWID
      CTEMP(1:ICLINE) = ' '
      WRITE(CTEMP(1:6),10200)I,' | '
      DO 10 K=1,32
      IDIG = JBYT(IQ(KQS+L+I),K,1)
      IF(IDIG .NE. 0 .OR. IZERO .NE. 0)WRITE(CTEMP(IP:IP),10100)IDIG
      IP = IP+IFWID
   10 CONTINUE
      WRITE(IQPRNT,10000)CTEMP(1:ICLINE)
   20 CONTINUE
   30 CONTINUE
10000 FORMAT(' ',A)
10100 FORMAT(I1)
10200 FORMAT(I6,A3)
      END
