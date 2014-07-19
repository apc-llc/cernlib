*
* $Id: cslibr.F,v 1.1.1.1 1996/02/26 17:16:25 mclareni Exp $
*
* $Log: cslibr.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:25  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/02 08/07/93  18.21.05  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSLIBR(REC,N)
*******************************************
*                                         *
*   READ THE CURRENT RECORD INTO REC.     *
*   N IS THE OUTPUT PARAMETER: NUMBER OF  *
*   CHARACTERS READ.                      *
*                                         *
*   N =-1  INDICATES EOF.                 *
*                                         *
*******************************************
      CHARACTER*(*) REC
      COMMON /CSLBRW/ LIBIND,LIBST,LEOF,LIBRD,LIBTXT
*
#if defined(CERNLIB_VAX)
      DIMENSION ISTRD(2)
      LIBST=LBR$GET_RECORD(LIBIND,REC,ISTRD)
      IF(.NOT.LIBST) GO TO 1
      N=ISTRD(1)
      RETURN
    1 IF(LIBST.NE.LEOF) CALL LIB$SIGNAL(%VAL(LIBST))
      N=-1
#endif
      END
