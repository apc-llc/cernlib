*
* $Id: dzdird.F,v 1.1.1.1 1996/03/04 16:13:16 mclareni Exp $
*
* $Log: dzdird.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:16  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDIRD(LDUNIT,CDIR,COUT,IFLAG)
 
      INTEGER LDUNIT,IFLAG
      CHARACTER*(*) CDIR,COUT
      CHARACTER*80  CLINE
 
*     simple implementation of doc
*     IFLAG = 0: print doc
*             1: return first line in COUT
 
*      WRITE(*,*)LDUNIT,IFLAG,CDIR
      IF(LDUNIT.LE.0 .AND. IFLAG.LE.0)GOTO 99
      IF(CDIR(1:2).NE.'//')GOTO 99
      LENDIR=LENOCC(CDIR)
      REWIND LDUNIT
      ISLASH = INDEX(CDIR(3:),'/')
      IF(ISLASH.EQ.0)THEN
        GOTO 99
      ELSE
        ISLASH=ISLASH+2
      ENDIF
      INDOC=0
1     CALL DZDCRD(LDUNIT,CLINE,IRC)
      IF(IRC.NE.0)GOTO 99
*1     READ(LDUNIT,'(A)',END=99)CLINE
      IF(INDOC.GT.0)THEN
         IF(CLINE(1:1).EQ.'/')THEN
           GOTO 99
         ELSE
           GOTO 5
         ENDIF
      ENDIF
      IF(CLINE(1:1).NE.'/')GOTO 1
      CALL CLTOU(CLINE)
      LENLIN=LENOCC(CLINE)
      ISLASH=LENDIR-LENLIN+1
      IF(ISLASH.LE.0)GOTO 1
      IF(INDEX(CDIR(ISLASH:LENDIR),CLINE(1:LENLIN)).EQ.0)GOTO 1
 
      INDOC=1
      IF(IFLAG.EQ.1)THEN
         COUT=' '
         CALL DZDCRD(LDUNIT,COUT,IRC)
         IF(IRC.NE.0)GOTO 99
*         READ(LDUNIT,'(A)',END=99)COUT
*         WRITE(*,*)COUT
         GOTO 99
      ENDIF
*     PRINT THE LINE
5     LENLIN=LENOCC(CLINE)
      WRITE(*,*)CLINE(1:LENLIN)
      GOTO 1
99    RETURN
      END
 
**********************************************************************
