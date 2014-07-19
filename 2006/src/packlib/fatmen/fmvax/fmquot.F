*
* $Id: fmquot.F,v 1.1.1.1 1996/03/07 15:18:21 mclareni Exp $
*
* $Log: fmquot.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:21  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMQUOT(USED,FREE,ALLO,OVER,IRC)
*CMZ :          13/11/91  12.06.22  by  Jamie Shiers
*-- Author :    Julian Bunn    13/11/91
*
*     Get disk quota information - blocks used,
*     allocated and overdraft. Copied from SPACE
*
*     Return codes: < 0 : error
*                   = 0 : OK
*                   > 0 : disk quotas not enabled on this volume
*
      IMPLICIT INTEGER (A-Z)
      INCLUDE '($SSDEF)'
      INCLUDE '($IODEF)'
      INCLUDE '($FIBDEF)'
      INCLUDE '($LBRDEF)'
      INCLUDE '($JPIDEF)'
      INCLUDE '($UAIDEF)'
      INCLUDE '($LNMDEF)'
      INTEGER*2 IOSB2(4)
      INTEGER*4 DESC(2),DF(2),IOSB(2)
      EQUIVALENCE (IOSB,IOSB2)
      INTEGER*2 DISK_CHAN
      INTEGER*4 QF(8)
      STRUCTURE /ITEMLIST/
        INTEGER*2 BUFLEN
        INTEGER*2 ITEMCODE
        INTEGER*4 BUFFADD
        INTEGER*4 RETLADD
      END STRUCTURE
      RECORD /ITEMLIST/ ITEM_LIST(3)
      STRUCTURE /FILEINFO/
        INTEGER*4 FIB$L_ACCTL
        INTEGER*4 FIB$L1
        INTEGER*4 FIB$L2
        INTEGER*4 FIB$L3
        INTEGER*4 FIB$L_WCC
        INTEGER*2 FIB$W_NMCTL
        INTEGER*2 FIB$W_CNTRLFUNC
        INTEGER*4 FIB$L_CNTRLVAL
      END STRUCTURE
      RECORD /FILEINFO/ FILE_INFO
 
      CHARACTER*80 DISK
      INTEGER UIC(2)
#include "fatmen/slate.inc"
 
      IRC = 0
*
*     Get current working directory
*
      CALL GETWDF(DISK)
      IF(IS(1).LE.0) THEN
         IRC = -1
         GOTO 999
      ENDIF
 
      LDISK = INDEX(DISK,':') - 1
*
*     Get current UIC
*
      ITEM_LIST(1).BUFLEN    =4
      ITEM_LIST(1).ITEMCODE  =JPI$_UIC
      ITEM_LIST(1).BUFFADD   =%LOC(UIC)
      ITEM_LIST(1).RETLADD   =%LOC(LUIC)
      ITEM_LIST(2).BUFLEN    =0
      ITEM_LIST(2).ITEMCODE  =0
      STATUS=SYS$GETJPIW(,,,ITEM_LIST,,,)
      IF(STATUS.NE.SS$_NORMAL) THEN
         IRC = -2
         GOTO 999
      ENDIF
 
      UIC(2)  = 0
*
*     Assign the disk
*
      STATUS = SYS$ASSIGN(DISK(:LDISK),DISK_CHAN,,)
      IF(.NOT.STATUS) THEN
         STATUS=LIB$SIGNAL(%VAL(STATUS))
         IRC = -3
         GOTO 999
      ENDIF
*
*     Get quota information
*
      QF(1) = 0
      QF(2) = UIC(1)
      FILE_INFO.FIB$L_ACCTL     = 0
      FILE_INFO.FIB$L1          = 0
      FILE_INFO.FIB$L2          = 0
      FILE_INFO.FIB$L3          = 0
      FILE_INFO.FIB$L_WCC       = 0
      FILE_INFO.FIB$W_NMCTL     = 0
      FILE_INFO.FIB$W_CNTRLFUNC = FIB$C_EXA_QUOTA
      FILE_INFO.FIB$L_CNTRLVAL  = 0
      DESC(1) = 28
      DESC(2) = %LOC(FILE_INFO)
      DF(1)   = 32
      DF(2)   = %LOC(QF)
*
      STATUS = SYS$QIOW(,%VAL(DISK_CHAN),%VAL(IO$_ACPCONTROL),
     +                   IOSB,,,DESC,DF,LFIELD,DF,,)
      IF(.NOT.STATUS) THEN
         STATUS = LIB$SIGNAL(%VAL(STATUS))
         IRC = -4
         GOTO 999
      ENDIF
      IF(.NOT.IOSB(1)) THEN
            IF(IOSB(1).EQ.SS$_QFNOTACT) THEN
            IRC = 1
         ELSE
            STATUS = LIB$SIGNAL(%VAL(IOSB(1)))
            IRC = -5
            GOTO 999
         ENDIF
      ENDIF
*
      USED = QF(3)
      ALLO = QF(4)
      OVER = QF(5)
      FREE = ALLO - USED
 
 999  CONTINUE
*
*     Deassign the disk
*
      STATUS = SYS$DASSGN(%VAL(DISK_CHAN))
      IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
      RETURN
      END
