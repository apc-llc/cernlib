*
* $Id: fabnet.F,v 1.1.1.1 1996/03/07 15:18:18 mclareni Exp $
*
* $Log: fabnet.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:18  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FABNET(FNAME,CHUSER,CHNODE,IFLAG,IRC)
#include "fatmen/fatsys.inc"
*
*     IFLAG = -1 : sendfile
*             -2 : mvs job (vm machines only)
*
      CHARACTER*(*) CHUSER,CHNODE,FNAME
#if defined(CERNLIB_VAXVMS)
*
*     Copy file to RDR of specified machine
*
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
      CHARACTER*80  CARD
      CHARACTER*128 FILEN
      INTEGER       SYSTEMF
 
      LFN   = LENOCC(FNAME)
      IF(IFLAG.LT.-1) THEN
         IRC = IFLAG
         RETURN
      ENDIF
 
      LUS   = LENOCC(CHUSER)
      LNO   = LENOCC(CHNODE)
 
      FILEN = CHNODE(1:LNO)//'::"RDR:<'//
     +        CHUSER(1:LUS)//'>FATMEN.RDRFILE/CARD/CR"'
      IF(IDEBFA.GE.2) PRINT *,'FABNET. sending file ',
     +   ' to ',FILEN
      OPEN(LUFZFA,FILE=FNAME(1:LFN),STATUS='OLD',
     +     FORM='FORMATTED',ACCESS='SEQUENTIAL')
      OPEN(UNIT=99,NAME=FILEN(1:LENOCC(FILEN)),STATUS='NEW',ERR=98)
      IRC = 0
1     CONTINUE
      READ(LUFZFA,'(A)',END=99) CARD
      WRITE(99,'(A)') CARD
      GOTO 1
98    IRC = 1
99    CLOSE (LUFZFA)
      CLOSE (99)
      IF ((IRC.EQ.0) .AND.(IDEBFA.GE.-3))
     +   WRITE(LPRTFA,*) 'Sent file to ',CHUSER(1:LUS),' at ',
     +   CHNODE(1:LNO)
#endif
#if defined(CERNLIB_IBMMVS)
      LFN   = LENOCC(FNAME)
      LUS   = LENOCC(CHUSER)
      LNO   = LENOCC(CHNODE)
 
      IF(IFLAG.LT.-1) THEN
         IRC = IFLAG
         RETURN
      ENDIF
 
      CALL TSOLNK('TRANSMIT '//CHNODE(1:LNO)//'.'//
     +   CHUSER(1:LUS)//' DSN('//FNAME(1:LFN)//')')
#endif
#if defined(CERNLIB_IBMVM)
      LFN   = LENOCC(FNAME)
      LUS   = LENOCC(CHUSER)
      LNO   = LENOCC(CHNODE)
 
      IF(IFLAG.EQ.-1) THEN
         CALL VMCMS('EXEC SENDFILE '//FNAME(1:LFN)//' '//
     +      CHUSER(1:LUS)//' AT '//CHNODE(1:LNO),IRC)
      ELSE
         CALL VMCMS('EXEC FATSUB   '//FNAME(1:LFN)//' '//
     +      CHUSER(1:LUS)//' AT '//CHNODE(1:LNO),IRC)
      ENDIF
#endif
      END
