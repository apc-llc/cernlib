*
* $Id: ifput1.F,v 1.3 1996/03/12 08:29:23 cernlib Exp $
*
* $Log: ifput1.F,v $
* Revision 1.3  1996/03/12 08:29:23  cernlib
* Compile ifput1.F only on VAX and IBM.
*
* Revision 1.2  1996/03/07 17:38:16  couet
* IFALCO selection was wrong
*
* Revision 1.1.1.1  1996/02/14 13:10:31  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.14/04 23/03/92  10.37.37  by  Unknown
*-- Author :
#if defined(CERNLIB_VAX)
      SUBROUTINE IFPUT1(BOUT,IPOUT)
      LOGICAL*1 BOUT(256)
      INTEGER SYS$ASSIGN,SYS$QIOW,STAT(2)
      N=SYS$QIOW(,%VAL(ICHAN),%VAL(304),
     *%REF(STAT),,,%REF(BOUT),%VAL(IPOUT),,,,)
      RETURN
      ENTRY IFIOI
      N=SYS$ASSIGN('SYS$OUTPUT',ICHAN,,)
      END
#endif
#if defined(CERNLIB_IBM)
      SUBROUTINE IFPUT1(ICOUNT,BUFF)
      INTEGER BUFF(1)
      INTEGER*2 I1,I3,I4
      INTEGER*4 IT
      LOGICAL*1 LT(4),LBUFF(256)
      EQUIVALENCE (IT,LT(1))
      DO 10 I=1,ICOUNT
         IT=BUFF(I)
         LBUFF(I)=LT(4)
   10 CONTINUE
      I1=2
      I3=ICOUNT
      CALL TIF71(I1,LBUFF,I3,I4)
      ISTAT=I4
      END
#endif
