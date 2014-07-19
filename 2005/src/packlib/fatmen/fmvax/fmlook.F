*
* $Id: fmlook.F,v 1.1.1.1 1996/03/07 15:18:23 mclareni Exp $
*
* $Log: fmlook.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:23  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMLOOK(CHJOB,CHQUE,JOB_NAME,IRC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to check if the tape is not being staged
C-
C-   Inputs  :
C-
C-     CHJOB job name (tape labes) to be verified
C-           (file name up to and excluding ".")
C-
C-     CHQUE class of queues to search
C-
C-   Outputs :
C-
C-     IRC = 0: job not found
C-     IRC > 0: entry number, coresponding to JOB_NAME
C-     IRC < 0: error
C-
C-     JOB_NAME full job name (if found)
C-
C-   Controls: none
C-
C-   Created  10-JUN-1992   Krzysztof L. Genser
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      CHARACTER*(*) CHJOB,CHQUE
C
      CHARACTER*39  JOB_NAME
      INTEGER*4     JOB_NAME_LEN
 
      INTEGER*4 LENOCC
      INTEGER*4 SYS$GETQUIW
 
C
      INTEGER*4 IRC,IRC_LEN,QSTATUS,JSTATUS
      INTEGER*4 LJOB,LQUE
C
      INTEGER*4 IDEBFA
      COMMON /FATUSE/ IDEBFA
C
      STRUCTURE /ITMLST/
      UNION
      MAP
      INTEGER*2 BUFFER_LENGTH
      INTEGER*2 ITEM_CODE
      INTEGER*4 BUFFER_ADDRESS
      INTEGER*4 RETURN_LENGTH_ADDRESS
      ENDMAP
      MAP
      INTEGER*4 END_LIST /0/
      ENDMAP
      END UNION
      END STRUCTURE
C
      STRUCTURE /IOSBLK/
      INTEGER*4 STS,ZEROED
      END STRUCTURE
C
      RECORD /ITMLST/ QUEUE_LIST(4)
      RECORD /ITMLST/ JOB_LIST(6)
      RECORD /IOSBLK/ IOSB
C
      CHARACTER*31  QUEUE_NAME
      INTEGER*4     QUEUE_NAME_LEN
      INTEGER*4     SEARCH_FLAGS,JOB_QSTATUS
C
      INCLUDE '($QUIDEF)'
      INCLUDE '($JBCMSGDEF)'
      INCLUDE '($SJCDEF)'
C
      IRC = 0
C
      LJOB = LENOCC(CHJOB)
      LQUE = LENOCC(CHQUE)
C
C ****  define the item list for queue operation
C
      QUEUE_LIST(1).BUFFER_LENGTH         = LQUE
      QUEUE_LIST(1).ITEM_CODE             = QUI$_SEARCH_NAME
      QUEUE_LIST(1).BUFFER_ADDRESS        = %LOC(CHQUE)
      QUEUE_LIST(1).RETURN_LENGTH_ADDRESS = 0
C
      QUEUE_LIST(2).BUFFER_LENGTH         = 4
      QUEUE_LIST(2).ITEM_CODE             = QUI$_SEARCH_FLAGS
      QUEUE_LIST(2).BUFFER_ADDRESS        = %LOC(SEARCH_FLAGS)
      QUEUE_LIST(2).RETURN_LENGTH_ADDRESS = 0
C
      QUEUE_LIST(3).BUFFER_LENGTH         = 31
      QUEUE_LIST(3).ITEM_CODE             = QUI$_QUEUE_NAME
      QUEUE_LIST(3).BUFFER_ADDRESS        = %LOC(QUEUE_NAME)
      QUEUE_LIST(3).RETURN_LENGTH_ADDRESS = %LOC(QUEUE_NAME_LEN)
C
      QUEUE_LIST(4).END_LIST              = 0
C
C ****  define the item list for job query
C
      JOB_LIST(1).BUFFER_LENGTH           = 4
      JOB_LIST(1).ITEM_CODE               = QUI$_SEARCH_FLAGS
      JOB_LIST(1).BUFFER_ADDRESS          = %LOC(SEARCH_FLAGS)
      JOB_LIST(1).RETURN_LENGTH_ADDRESS   = 0
C
      JOB_LIST(2).BUFFER_LENGTH           = 39
      JOB_LIST(2).ITEM_CODE               = QUI$_JOB_NAME
      JOB_LIST(2).BUFFER_ADDRESS          = %LOC(JOB_NAME)
      JOB_LIST(2).RETURN_LENGTH_ADDRESS   = %LOC(JOB_NAME_LEN)
C
      JOB_LIST(3).BUFFER_LENGTH           = 4
      JOB_LIST(3).ITEM_CODE               = QUI$_ENTRY_NUMBER
      JOB_LIST(3).BUFFER_ADDRESS          = %LOC(IRC)
      JOB_LIST(3).RETURN_LENGTH_ADDRESS   = %LOC(IRC_LEN)
C
      JOB_LIST(4).END_LIST                = 0
C
C
C ****  loop over the queues
C
      QSTATUS = SYS$GETQUIW (,%VAL(QUI$_CANCEL_OPERATION),,,,,)
 
      IF ( .NOT.QSTATUS ) THEN
         CALL LIB$SIGNAL(%VAL(QSTATUS))
         IRC = -1
      ENDIF
C
C ****  loop over generic queues first
C
      SEARCH_FLAGS = ( QUI$M_SEARCH_GENERIC .OR.
     +  QUI$M_SEARCH_WILDCARD .OR. QUI$M_SEARCH_ALL_JOBS)
C
   10 CONTINUE
C
      QSTATUS = SYS$GETQUIW
     +   (,%VAL(QUI$_DISPLAY_QUEUE),,QUEUE_LIST,IOSB,,)
C
      IF ( QSTATUS ) THEN
 
         IF ( IOSB.STS ) THEN
 
            IF (IDEBFA.GE.1) PRINT *,QUEUE_NAME(1:QUEUE_NAME_LEN)
C
C ****  loop over jobs in the queue
C
   20       CONTINUE
 
            JSTATUS = SYS$GETQUIW (,%VAL(QUI$_DISPLAY_JOB),,JOB_LIST,
     +      IOSB,,)
 
            CALL CLTOU(JOB_NAME(1:JOB_NAME_LEN))
            IF ( JSTATUS ) THEN
 
               IF ( IOSB.STS .AND. IOSB.STS .NE.QUI$M_JOB_INACCESSIBLE)
     +         THEN
 
                  IF (IDEBFA.GE.1) PRINT *,JOB_NAME(1:JOB_NAME_LEN),
     +            IRC
 
                  IF (INDEX(JOB_NAME(1:JOB_NAME_LEN),CHJOB(1:LJOB))
     +            .NE.0) THEN
                     RETURN
                  ENDIF
                  GOTO 20
               ENDIF
            ENDIF
            GOTO 10
         ENDIF
      ENDIF
C
C ****  loop over batch queues
C
      SEARCH_FLAGS = ( QUI$M_SEARCH_BATCH .OR.
     +  QUI$M_SEARCH_WILDCARD .OR. QUI$M_SEARCH_ALL_JOBS)
   30 CONTINUE
C
      QSTATUS = SYS$GETQUIW
     +   (,%VAL(QUI$_DISPLAY_QUEUE),,QUEUE_LIST,IOSB,,)
C
      IF ( QSTATUS ) THEN
 
         IF ( IOSB.STS ) THEN
 
            IF (IDEBFA.GE.1) PRINT *,QUEUE_NAME(1:QUEUE_NAME_LEN)
C
C ****  loop over jobs in the queue
C
   40       CONTINUE
 
            JSTATUS = SYS$GETQUIW (,%VAL(QUI$_DISPLAY_JOB),,JOB_LIST,
     +      IOSB,,)
 
            IF ( JSTATUS ) THEN
 
               IF ( IOSB.STS .AND. IOSB.STS .NE.QUI$M_JOB_INACCESSIBLE)
     +         THEN
 
                  IF (IDEBFA.GE.1) PRINT *,JOB_NAME(1:JOB_NAME_LEN),IRC
 
                  IF (INDEX(JOB_NAME(1:JOB_NAME_LEN),CHJOB(1:LJOB))
     +            .NE.0) THEN
                     RETURN
                  ENDIF
                  GOTO 40
               ENDIF
            ENDIF
            GOTO 30
         ENDIF
      ENDIF
 
      IRC = 0
 
      END
