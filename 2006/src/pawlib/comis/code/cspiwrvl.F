*
* $Id: cspiwrvl.F,v 1.2 1996/04/02 22:12:49 thakulin Exp $
*
* $Log: cspiwrvl.F,v $
* Revision 1.2  1996/04/02 22:12:49  thakulin
* Remove CERNLIB_PIAF macro and check the run time variable IPIAF in
* paw/pcmode.inc instead.
*
* Revision 1.1.1.1  1996/02/26 17:16:32  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_PAW)
*CMZ :  1.18/02 14/04/94  11.52.20  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   08/04/94
      SUBROUTINE CSPIWRVL(LUN,LINE,LP,KERR)
      CHARACTER *(*)          LINE
      CHARACTER OLINE*80, NAME*32
      IPOS=1
      CALL CSNIDN(LINE(:LP),IPOS,NAME,LNAME)
      IF(NAME.EQ.'VECTOR')CALL CSNIDN(LINE(:LP),IPOS,NAME,LNAME)
      IF(LNAME.LE.0)RETURN
      OLINE='*CS*V*L*'
      LO=9
 10   OLINE(LO:)=NAME(:LNAME)
      LO=LO+LNAME+1
      CALL CSNLEX(LINE(:LP),IPOS,NAME,LNAME,LEXEM )
      IF(LNAME.LE.0)THEN
        WRITE(LUN,77,ERR=99)OLINE(:LO-1)
 77     FORMAT(A)
        RETURN
      ELSE
*-- next lexem must be ( or ,
        IF(NAME.EQ.',')THEN
          CALL CSNIDN(LINE(:LP),IPOS,NAME,LNAME)
          GO TO 10
        ELSE
*-- skip untill )
          LEVEL=1
 20       CALL CSNLEX(LINE(:LP),IPOS,NAME,LNAME,LEXEM )
          IF(NAME.EQ.')')THEN
            LEVEL=LEVEL-1
          ELSEIF(NAME.EQ.'(')THEN
            LEVEL=LEVEL+1
          ENDIF
          IF(LEVEL.GT.0)GO TO 20
          CALL CSNLEX(LINE(:LP),IPOS,NAME,LNAME,LEXEM )
          IF(LNAME.LE.0)THEN
            WRITE(LUN,77,ERR=99)OLINE(:LO-1)
            RETURN
          ELSE
*-- next lexem must be ,
            CALL CSNIDN(LINE(:LP),IPOS,NAME,LNAME)
            GO TO 10
          ENDIF
        ENDIF
      ENDIF
 99   KERR=1
      END
#endif
