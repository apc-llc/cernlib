*
* $Id: hmerge1.F,v 1.3 1997/02/21 12:15:42 couet Exp $
*
* $Log: hmerge1.F,v $
* Revision 1.3  1997/02/21 12:15:42  couet
* -previous commit was wrong
*
* Revision 1.2  1997/02/21 12:12:33  couet
* *** empty log message ***
*
* Revision 1.1.1.1  1996/01/16 17:08:10  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/04 07/06/94  09.07.24  by  Rene Brun
*-- Author :    Rene Brun,Fons Rademakers   26/05/94
      SUBROUTINE HMERGE1(CDIR)
*
#include "hbook/hcbook.inc"
*
      INTEGER         IQUEST
      COMMON/QUEST/   IQUEST(100)
*
      CHARACTER*(*)   CDIR
      CHARACTER*80    MDIR, RDIR
      LOGICAL         HEXIST
      INTEGER         I, NKEY, IDN, JLS, IOFSET
      INTEGER         LENOCC
*
      PRINT *, 'Initializing directory ', CDIR(1:LENOCC(CDIR))
*
      MDIR      = CDIR
      MDIR(3:6) = 'PAWC'
      RDIR      = CDIR
      RDIR(3:6) = 'BIGF'
      JLS = ICFILA('/', CDIR, 1, LENOCC(CDIR))
      IF (JLS .GT. 2) THEN
         CALL HMDIR(RDIR, ' ')
         CALL HMDIR(MDIR, 'S')
      ENDIF
      CALL HCDIR(CDIR, ' ')
*
      NKEY = 50000
      DO 20 I = 1, NKEY
         CALL RZINK(I, 0, 'S')
         IF (IQUEST(1) .NE. 0) GOTO 999
         IF (JBIT (IQUEST(14),4) .NE. 0) GOTO 20
         IDN = IQUEST(21)
         IOFSET = 100
30       CONTINUE
         IF (HEXIST(IDN+IOFSET)) THEN
            IOFSET = IOFSET + 1
            GOTO 30
         ENDIF
         CALL HRIN(IDN,999,IOFSET)
         IF (IQUEST(1) .NE. 0) THEN
            PRINT *, 'Cannot read object ', IDN
            GOTO 99
         ENDIF
         ID = IDN + IOFSET
         IF (JBIT(IQ(LCID+KBITS),4) .NE. 0) THEN
*
*         Create copy of Ntuple IDN
*
            CALL HCDIR(RDIR, ' ')
            CALL HNTDUP(ID, IDN, .FALSE., ' ', 'D')
            CALL HCDIR(CDIR, ' ')
         ENDIF
*
         CALL HDELET(ID)
*
20    CONTINUE
*
      GOTO 999
*
*         Abnormal end
*
99    CONTINUE
*
999   END
