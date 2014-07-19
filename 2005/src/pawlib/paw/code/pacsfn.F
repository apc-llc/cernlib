*
* $Id: pacsfn.F,v 1.1.1.1 1996/03/01 11:38:38 mclareni Exp $
*
* $Log: pacsfn.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:38  mclareni
* Paw
*
*
#include "paw/pilot.h"
#if defined(CERNLIB_COMIS)
*CMZ :  2.05/10 01/06/94  17.51.30  by  Alfred Nathaniel
*-- Author :    Alfred Nathaniel   01/06/94
      SUBROUTINE PACSFN(CHLINE,CHOPT,IVAL,RVAL,DVAL)
      CHARACTER*(*) CHLINE,CHOPT
      DOUBLE PRECISION DVAL
*
*--- Call a COMIS function and return its functionm value
*
      COMMON/PCCSFN/DFNVAL,IFNVAL,RFNVAL
      DOUBLE PRECISION DFNVAL

      CHARACTER CHCALL*128

      CHCALL='/FORTRAN/CALL ''='//CHOPT(:1)
      L=LENOCC(CHCALL)
      DO 10 I=1,LENOCC(CHLINE)
         IF(CHLINE(I:I).EQ.'''') THEN
            CHCALL(L+1:)=''''''
            L=L+2
         ELSE
            CHCALL(L+1:)=CHLINE(I:I)
            L=L+1
         ENDIF
 10   CONTINUE
      CHCALL(L+1:)=''''

      CALL KUEXEL(CHCALL)
      IVAL=IFNVAL
      RVAL=RFNVAL
      DVAL=DFNVAL
      END
#endif
