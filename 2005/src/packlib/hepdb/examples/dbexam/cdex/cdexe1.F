*
* $Id: cdexe1.F,v 1.1.1.1 1996/02/28 16:23:39 mclareni Exp $
*
* $Log: cdexe1.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:39  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDEXE1(INSTR,IFEXT,IFEXE)
*      ------------------------*****-*****-
*
*    Subroutine for CD/hepdb calls
*
*      -- passed instruction --
      CHARACTER*(*) INSTR
*
*       -- local variables --
        CHARACTER LINE*80 , FNAME*8 , PREF*8
*
      DATA IFINIT/0/
*
*
      IF(IFINIT.LE.0) THEN
*         ... initialization...
        IFINIT=1
      ENDIF
*
*       -- IFEXE: if  0 -no command was executed yet
*                 if  1 -a  command was executed, don't try more
*                 if -1 -a HELP command is being executed
      IF(IFEXE.EQ.-1) THEN
c***                                1         2         3         4
c***                       1234567890123456789012345678901234567890
CC        CALL CDX_MESS(' -------------------------------- CDxxxx-'
CC     +              //'--------------------------------------')
*
      ENDIF
      IF(IFEXE.NE.0) RETURN
*
*
      LINE=INSTR
      CALL CLTOU(LINE)
*
      JPAK=0
      IFTX=0
*
   12 CONTINUE
      CALL TEXINS(LINE,JPAK,IFTX,FNAME,NUMB,FNUM)
      LFN=INDEX(FNAME,' ')-1
      IF(LFN.LE.0) LFN=LEN(FNAME)
      NXL=JBYT(IFTX,1,2)
*
      IF(IFEXE.EQ.-1.OR.IFSHEQC(FNAME(:LFN),'HELP').GT.0) THEN
        IFEXE=-1
CC      to change...
*
      ELSE IF(FNAME.EQ.'CDPREF') THEN
        IFEXE=1
   21   CONTINUE
        CALL TEXINS(LINE,JPAK,IFTX,PREF,NUMB,FNUM)
        NXL=JBYT(IFTX,1,2)
        IF(PREF.NE.' ') CALL CXPREF(PREF)
        IF(NXL.GT.0) GOTO 21
*
      ELSE
*        = = = = =  it is a foreign instruction = = = = =
      ENDIF
*
   99 CONTINUE
      END
