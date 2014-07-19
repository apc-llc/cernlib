*
* $Id: mdext.F,v 1.1.1.1 1996/02/26 17:16:48 mclareni Exp $
*
* $Log: mdext.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:48  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/02 08/07/93  18.32.13  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      FUNCTION MDEXT(LENG,IND)
      COMMON/MDPOOL/IQ(6)
      LOGICAL MDEXT
#if defined(CERNLIB_APOLLO)
      INTEGER LENMEM,L
      POINTER/LENMEM/L
      LENMEM=LENG*4
      IM=MALLOC(L)
      IF(IM.EQ.0)THEN
         MDEXT=.FALSE.
      ELSE
         MDEXT=.TRUE.
         IND=(IM-IADDR(IQ(1))+4)/4
      ENDIF
**      MDEXT=MDEXL(LENG,IND)
#endif
#if defined(CERNLIB_VAX)
      L=LENG*4
      K=LIB$GET_VM(L,IM)
      K1=K/2
      IF(K-K1*2 .EQ. 1)THEN
        MDEXT=.TRUE.
        IND=(IM-%LOC(IQ(1))+4)/4
      ELSE
        MDEXT=.FALSE.
      ENDIF
***+SELF,IF=IBM,UNIX.
#endif
#if defined(CERNLIB_IBM)
      LOGICAL MDEXL
      MDEXT=MDEXL(LENG,IND)
#endif
#if defined(CERNLIB_UNIX)
      INTEGER CSALLO
      L=LENG*4
      IM=CSALLO(L)
      IF(IM.EQ.0)THEN
         MDEXT=.FALSE.
      ELSE
         MDEXT=.TRUE.
         IND=(IM-LOCB(IQ(1))+4)/4
      ENDIF
#endif
      END
