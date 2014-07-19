*
* $Id: proced.F,v 1.1.1.1 1996/02/26 17:16:38 mclareni Exp $
*
* $Log: proced.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:38  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.06  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE PROCED(SUB,NST,NS,COM,NCT,NC)
      CHARACTER *(*) SUB(NST),COM(NCT)
      CHARACTER *80 REC,IDENT*8
  71  FORMAT(A)
      J=MJSCHA(IDENT)
      NS=0
      NC=0
  1   READ(1,71,END=999)REC
  2   IF(REC(1:6).NE.' ')GO TO 1
      I=INDEX(REC(7:),'COMMON')
      IF(I.NE.0) THEN
* COMMON?
        IF(REC(1:I+5).NE.' ')GO TO 1
        IS=I+6+6
  3     I1=INDEX(REC(IS:),'/')
        IF(I1.EQ.0)THEN
  4       READ(1,71,END=999)REC
          IF(REC(1:1).EQ.'*' .OR. REC(1:1).EQ.'C')GO TO 4
          IF(REC(1:5).NE.' ')GO TO 2
          IF(REC(6:6).EQ.' ')GO TO 2
           IS=7
           GO TO 3
        ELSE
          I1=IS+I1-1
          I2=INDEX(REC(I1+1:),'/')
          IF(I2.EQ.0)GO TO 1
          I2=I2+I1
          IS=MIN0(I2+1,80)
          N=NIDENT(REC(I1+1:I2-1),IDENT)
          IF(N.EQ.0)GO TO 3
        ENDIF
        CALL PUT(COM,NCT,NC,IDENT)
        GO TO 3
      ELSE
*  FUNCTION SUBROUTINE ENTRY
        I=INDEX(REC(7:),'FUNCTION')
        IF(I.NE.0)THEN
         I1=I+8+6
* TEST  FUNCTION
        ELSE
          I=INDEX(REC(7:),'SUBROUTINE')
          I1=I+10+6
          IF(I.EQ.0)THEN
            I=INDEX(REC(7:),'ENTRY')
            IF(I.EQ.0)THEN
              I=INDEX(REC(7:),'CALL')
              IF(I.EQ.0)GO TO 1
              I1=I+4+6
            ELSE
              I1=I+5+6
            ENDIF
          ENDIF
          IF(REC(1:I+5).NE.' ')GO TO 1
        ENDIF
*TEST IDENT AND PUT IT IN TABLE
        N=NIDENT(REC(I1:),IDENT)
        IF(N.EQ.0)GO TO 1
        CALL PUT(SUB,NST,NS,IDENT)
      ENDIF
      GO TO 1
 999  END
