*
* $Id: pawork.F,v 1.1.1.1 1996/03/01 11:38:43 mclareni Exp $
*
* $Log: pawork.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.00/02 11/01/93  17.32.52  by  O.Couet
*-- Author :    O.Couet   19/02/90
      SUBROUTINE PAWORK
*
*           To create/delete or set workstation parameters
*
      CHARACTER*32 CHPATL
      CHARACTER*8  CHACT,CHOPT
      DIMENSION RVAL(11),XY(2)
*
      CALL KUPATL(CHPATL,NPAR)
*
      IF(CHPATL.EQ.'WORKSTATION')THEN
         CALL KUGETI(ID)
         CALL KUGETC(CHOPT,NCH)
         CALL KUGETI(IWTYP)
*
         CALL IGQWK(0,'NBWK',RVAL)
         INOPWK=INT(RVAL(1))
*
*              Open workstations
*
         IF(INDEX(CHOPT,'O').NE.0)THEN
            IF(ID.GT.0)THEN
   10          CALL IGQWK(ID,'OPEN',ROPEN)
               IF(INT(ROPEN).NE.0)THEN
                  ID = ID+1
                  GOTO 10
               ENDIF
               CALL IOPWK(ID,0,IWTYP)
            ENDIF
         ENDIF
*
*              Close workstations
*
         IF(INDEX(CHOPT,'C').NE.0)THEN
            IF(ID.EQ.1)THEN
               PRINT*, '**** Cannot close workstation 1'
               RETURN
            ENDIF
            IF(ID.GT.0)THEN
               CALL ICLWK(ID)
            ELSEIF(ID.EQ.0)THEN
               DO 20 I=1,INOPWK
                  IF(INT(RVAL(I+1)).EQ.1)GOTO 20
                  CALL ICLWK(INT(RVAL(I+1)))
   20          CONTINUE
            ENDIF
         ENDIF
*
*              Active workstations
*
         IF(INDEX(CHOPT,'A').NE.0)THEN
            IF(ID.GT.0)THEN
               CALL IACWK(ID)
            ELSEIF(ID.EQ.0)THEN
               DO 30 I=1,INOPWK
                  CALL IGQWK(INT(RVAL(I+1)),'ACTI',RACT)
                  IF(RACT.EQ.0.)CALL IACWK(INT(RVAL(I+1)))
   30          CONTINUE
            ELSEIF(ID.LT.0)THEN
               DO 40 I=1,INOPWK
                  CALL IGQWK(INT(RVAL(I+1)),'ACTI',RACT)
                  IF(-ID.EQ.INT(RVAL(I+1)))THEN
                     IF(RACT.EQ.0.)CALL IACWK(-ID)
                  ELSE
                     IF(RACT.EQ.1.)CALL IDAWK(INT(INT(RVAL(I+1))))
                  ENDIF
   40          CONTINUE
            ENDIF
         ENDIF
*
*              Deactive workstations
*
         IF(INDEX(CHOPT,'D').NE.0)THEN
            IF(ID.GT.0)THEN
               CALL IDAWK(ID)
            ELSEIF(ID.EQ.0)THEN
               DO 50 I=1,INOPWK
                  CALL IGQWK(INT(RVAL(I+1)),'ACTI',RACT)
                  IF(RACT.EQ.1.)CALL IDAWK(INT(RVAL(I+1)))
   50          CONTINUE
            ELSEIF(ID.LT.0)THEN
               DO 60 I=1,INOPWK
                  CALL IGQWK(INT(RVAL(I+1)),'ACTI',RACT)
                  IF(-ID.EQ.INT(RVAL(I+1)))THEN
                     IF(RACT.EQ.1.)CALL IDAWK(-ID)
                  ELSE
                     IF(RACT.EQ.0.)CALL IACWK(INT((RVAL(I+1))))
                  ENDIF
   60          CONTINUE
            ENDIF
         ENDIF
*
*              List all the opened workstations
*
         IF(INDEX(CHOPT,'L').NE.0)THEN
            PRINT*, '+----+----+------+----------+-------+-------+'
            PRINT*, '| NB | ID | TYPE | ACTIVITY | XSIZE | YSIZE |'
            PRINT*, '+----+----+------+----------+-------+-------+'
            DO 70 I=1,INOPWK
               CALL IGQWK(INT(RVAL(I+1)),'ACTI',RACT)
               IF(RACT.EQ.1)THEN
                  CHACT=' ACTIVE '
               ELSE
                  CHACT='DEACTIVE'
               ENDIF
               CALL IGQWK(INT(RVAL(I+1)),'MXDS',XY)
               IT=IGIWTY(INT(RVAL(I+1)))
               WRITE(6,10000)I,INT(RVAL(I+1)),IT,CHACT,XY(1),XY(2)
   70       CONTINUE
            PRINT*, '+----+----+------+----------+-------+-------+'
         ENDIF
         RETURN
      ENDIF
*
10000 FORMAT(' | ',I2,' | ',I2,' | ',I4,' | ',A8,' | '
     +,          F5.0,' | ',F5.0,' |')
*
      END
