*
* $Id: create.F,v 1.1.1.1 1996/02/26 17:16:38 mclareni Exp $
*
* $Log: create.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:38  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.06  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CREATE(SUB,NST,NS,COM,NCT,NC)
      CHARACTER *(*) SUB(NST),COM(NCT)
      CHARACTER*24 HEAD, END*12, COMM*28, EXT*24,CSCOM*40,CSEXT*40
      DATA HEAD  /'      SUBROUTINE CSCST'/,
     ,     END   /'      END'/,
     ,     COMM  /'      COMMON/        /I    '/,
     ,     EXT   /'      EXTERNAL         '/
     ,     CSCOM /'      CALL CSCOM(''        #'',I    )'/,
     ,     CSEXT /'      CALL CSEXT(''        #'',        )'/
  21  FORMAT(A)
      JNUM=MJSCHA(COMM(24:24))
      JNUM1=MJSCHA(CSCOM(31:31))
      WRITE(2,21)HEAD
      DO 1 I=1,NC
       COMM(14:21)=COM(I)
       JI=JNUM
       N=4
       CALL CBIS(I,JI,N)
       WRITE(2,21)COMM
   1  CONTINUE
      DO 2 I=1,NS
       EXT(16:)=SUB(I)
       WRITE(2,21)EXT
   2  CONTINUE
      DO 3 I=1,NC
        CSCOM(19:26)=COM(I)
        JI=JNUM1
        N=4
        CALL CBIS(I,JI,N)
        WRITE(2,21)CSCOM
   3  CONTINUE
      DO 4 I=1,NS
       CSEXT(19:26)=SUB(I)
       CSEXT(30:37)=SUB(I)
       WRITE(2,21)CSEXT
   4  CONTINUE
      WRITE(2,21)END
      END
