*
* $Id: cdex.F,v 1.1.1.1 1996/02/28 16:23:39 mclareni Exp $
*
* $Log: cdex.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:39  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      PROGRAM CDEX
*     ------------
*----------------------------------------------*
*    Test program for the CD (HEPDB) package   *
*----------------------------------------------*
*
      PARAMETER (MWPAWC=100000)
      COMMON /PAWC/ IPAWC(MWPAWC)
*
* (ICDXDIV,LCDXDIV)
#include "cdxdiv.inc"
* (LUNINP,LUNOUT,LUNPRO,LUNCMD)
#include "cdxluns.inc"
#include "cdxfils.inc"
#include "yeaaas.inc"
*
      CHARACTER*8 FNAME,PRFX,ANSW
      CHARACTER INPL*80, LINE*80, CODE*11, PRF*8, CH2*2, CH*1
      CHARACTER IFLN(20)*80, IFANSW*12
      CHARACTER*60 FILNAME , FILNM(10)
      LOGICAL   OK
      CHARACTER MESL*78
*
      INTEGER IFNX , LPRF
*
      CALL CDX_MESS(' ')
      CALL CDX_MESS('---- CDEX start ----')
*
      YEAAAS=' '
      CALL CDXINI0
*
      ILUN=LUNINP
      ILUF=0
      INPL=' '
*
*   -- by default, call CDPAW
      INPL='CDPAW'
*
#if !defined(CERNLIB_IBMVM)
      FILNAME='cdex_init'
#endif
#if defined(CERNLIB_IBMVM)
      FILNAME='/CDEX INIT *'
#endif
      INQUIRE(FILE=FILNAME,EXIST=OK)
      IF(OK) THEN
        CALL CDX_MESS('*** "cdex_init" exists')
        INPL='EXEC '//FILNAME
      ELSE
        CALL CDX_MESS('*** "cdex_init" NOT FOUND')
      ENDIF
*
*   -- if there is a command, bypass the command reading
      JDIV=0
      JMAX=LNBLNK(INPL)
      IF(JMAX.GT.0) GOTO 11
*
   10 CONTINUE
      INPL=' '
      IF(ILUF.LE.0) THEN
        IF(ICDXDIV.LE.0) THEN
          CALL CDX_MESS('#####################')
          CALL CDX_MESS('Please call CDPAW !!!')
          CALL CDX_MESS('#####################')
        ENDIF
        CALL CDX_MESS('----')
        CODE='CDEX>'
        IF(YEAAAS.NE.' ') CODE(5:)='(Y)>'
        LL=LNBLNK(CODE)
#if !defined(CERNLIB_IBMVM)
        WRITE(*,'(A,$)') CODE(:LL)
#endif
#if defined(CERNLIB_IBMVM)
        WRITE(*,'( A )') CODE(:LL)
#endif
      ENDIF
      READ(ILUN,'(A)',END=77) INPL
      LTEX=MAX0(LNBLNK(INPL),1)
      IF(ILUF.GT.0) THEN
        CALL CDX_MESS('**>'//INPL(:LTEX)//'<**')
      ELSE
        CALL CDX_PROT(CODE(:LL)//INPL(:LTEX))
        IF(LTEX.EQ.1.AND.INPL(LTEX:LTEX).EQ.' ')
     +                        CALL CDX_MESS('Type: H -help,   Q -quit')
      ENDIF
      IF(INPL(1:1).EQ.'*'.OR.INPL(1:2).EQ.' *') GOTO 10
      IF(INPL(1:1).EQ.'#'.OR.INPL(1:2).EQ.' #') GOTO 10
      JMAX=INDEX(INPL,'!')-1
      IF(JMAX.LT.0) JMAX=LTEX
      IF(JMAX.LE.0) GOTO 10
      JDIV=0
*
   11 CONTINUE
      IF(JDIV.GE.JMAX) GOTO 10
      J=JSEANC(INPL(JDIV+1:),' ')
      IF(J.LE.0) GOTO 10
      JDIV=JDIV+J-1
      LLIN=JSEARC(INPL(JDIV+1:JMAX),';$')-1
      IF(LLIN.LT.0) LLIN=INDEX(INPL(JDIV+1:JMAX),'   ')-1
      IF(LLIN.LT.0) LLIN=JMAX-JDIV
      JDIV=JDIV+LLIN+1
      IF(LLIN.LE.0) GOTO 11
*
      LINE=INPL(JDIV-LLIN:JDIV-1)
      JPAK=LLIN
      IFTX=0
      IFOK=1
      NXL=0
*
   12 CONTINUE
      CALL TEXINS(LINE,JPAK,IFTX,FNAME,NUMB,FNUM)
      IF(IFTX.LE.0) GOTO 11
      NXL=JBYT(IFTX,1,2)
CCCC  IF(NXL.EQ.3) JPAK=JPAK-200
      CALL CLTOU(FNAME)
      LFN=INDEX(FNAME,' ')-1
      IF(LFN.LT.0) LFN=LEN(FNAME)
*
      IFEXE=0
      IF(IFSHEQC(FNAME(:LFN),'HELP').GT.0) THEN
        IFEXE=-1
c***                                1         2         3         4
c***                       1234567890123456789012345678901234567890
        CALL CDX_MESS(   '=============================== Help for'
     +                 // ' CDEX ================================')
*
*    call other routines to type their help for commands
      CALL CDEXE1(LINE,0,IFEXE)
*
        CALL CDX_MESS('> CDPAW [par/val ...] [-opt]')
        CALL CDX_MESS('> CDNEW [par/val ...] [-opt] [chtop filename]')
        CALL CDX_MESS('> CDPREF ; CDOPEN')
        CALL CDX_MESS('> CDMDIR ; CDDDIR')
        CALL CDX_MESS('> CDTEXT [par/val ...] [LBANK] [filename]')
        CALL CDX_MESS('> CDPURG')
        CALL CDX_MESS('> CDLIST')
        CALL CDX_MESS('> CDTM   ; CDTS')
        CALL CDX_MESS('> CDPKTM ; CDPKTS ; CDUPTM ; CDUPTS')
        CALL CDX_MESS('> CDERMS')
        CALL CDX_MESS('> CDSTAT ; CDEND ')
        CALL CDX_MESS('>')
        CALL CDX_MESS('> DBFiles    DBREAD')
        CALL CDX_MESS('>')
        CALL CDX_MESS('> RZCDIR ; RZLDIR ; RZNDIR')
        CALL CDX_MESS('> RZDELT')
        CALL CDX_MESS('> RZCLOS ; RZCLOS')
        CALL CDX_MESS('>')
        CALL CDX_MESS('> EXEC filename  ;  CHAIN filename')
        CALL CDX_MESS('> SETYES ; SETASK  ;  PRON ; PROFF')
*
*   type a concluding help for commands in the given routine
      CALL CDX_MESS('Help   Quit   EXit')
      CALL CDX_MESS('","=" " in commands. Use ";"'
     +  //' or 3 spaces to separate commands in a line')
        CALL CDX_MESS('========================================'
     +                      // '======================================')
*
      ELSE IF(FNAME.EQ.'CDPAW ') THEN
        IFEXE=1
        CALL CXPAW(MWPAWC,LINE,JPAK,IERR)
*
      ELSE IF(FNAME.EQ.'CDNEW ') THEN
        IFEXE=1
        CALL CXNEW(LINE,JPAK,IERR)
*
      ELSE IF(FNAME.EQ.'CDOPEN') THEN
        IFEXE=1
        CALL CXOPEN(LINE,JPAK,IERR)
*
      ELSE IF(FNAME.EQ.'CDMDIR') THEN
        IFEXE=1
        CALL CXMDIR(LINE,JPAK,IERR)
*
      ELSE IF(FNAME.EQ.'CDDDIR') THEN
        IFEXE=1
        CALL CXDDIR(LINE,JPAK,IERR)
*
      ELSE IF(FNAME.EQ.'CDTEXT') THEN
        IFEXE=1
        CALL CXTEXT(LINE,JPAK,IERR)
*
      ELSE IF(FNAME.EQ.'CDPKTM'.OR.FNAME.EQ.'CDPKTS'.OR.
     +        FNAME.EQ.'CDUPTM'.OR.FNAME.EQ.'CDUPTS'.OR.
     +        FNAME.EQ.'CDTM  '.OR.FNAME.EQ.'CDTS  ') THEN
        IFEXE=1
        CALL CXTIME(FNAME,LINE,JPAK,IERR)
*
      ELSE IF(FNAME.EQ.'CDSTAT') THEN
        IFEXE=1
        CALL CDSTAT(LUNOUT,IRC)
        CALL UXIRCM('CDSTAT',IRC)
*
      ELSE IF(FNAME.EQ.'CDERMS') THEN
        IFEXE=1
        CALL TEXINS(LINE,JPAK,IFTX,FNAME,NUMB,FNUM)
        CALL CDI_MESS('>>CALL CDERMS(0,errmes)',NUMB)
        CALL UXIRCM('errmes',NUMB)
*
      ELSE IF(FNAME.EQ.'CDPURG') THEN
        IFEXE=1
        CALL CXPURG(LINE,JPAK,IERR)
*
      ELSE IF(FNAME.EQ.'CDLIST') THEN
        IFEXE=1
        CALL CXLIST(LINE,JPAK,IERR)
*
*   ............... DB part .................
*
      ELSE IF(FNAME.EQ.'DBREAD') THEN
        IFEXE=1
        CALL DBREAD(LINE,JPAK,IERR)
*
      ELSE IF(IFSHEQC(FNAME(:LFN),'DBF*ILES').GT.0) THEN
        IFEXE=1
        CALL UXFILLS
*
      ELSE IF(IFSHEQC(FNAME(:LFN),'DBR*ECORD').GT.0) THEN
        IFEXE=1
CC\\    CALL CDXDBR(LINE,JPAK,IERR)
*
      ELSE IF(FNAME.EQ.'RZCDIR'.OR.
     +        FNAME.EQ.'RZNDIR'.OR.
     +        FNAME.EQ.'RZLDIR'.OR.
     +        FNAME.EQ.'RZDELT'.OR.
     +        FNAME.EQ.'RZEND '.OR.
     +        FNAME.EQ.'RZCLOS'    ) THEN
        IFEXE=1
        CALL RZXCHCH(FNAME,LINE,JPAK,IERR)
 
*
      ELSE IF(FNAME.EQ.'EXEC') THEN
        IFEXE=1
        CALL TEXSTR(LINE,JPAK,IFTX,FILNAME)
        INQUIRE(FILE=FILNAME,EXIST=OK)
        IF(OK) THEN
          ILUF=ILUF+1
          ILUN=LUNCMD+ILUF-1
          OPEN(ILUN,FILE=FILNAME,STATUS='OLD')
          INQUIRE(ILUN,NAME=FILNAME)
          JJ=INDEX(FILNAME,']')
          LL=INDEX(FILNAME,';')
          IF(JJ.GT.0.AND.LL.GT.JJ+1) FILNAME=FILNAME(JJ+1:LL-1)
          FILNM(ILUF)=FILNAME
          LFLNM=LNBLNK(FILNM(ILUF))
          CALL CDX_MESS('*** Execute: "'//FILNM(ILUF)(:LFLNM)//'"')
        ELSE
          CALL CDX_MESS('*** Exec file NOT FOUND')
          IERR=9990+ILUF
        ENDIF
*
      ELSE IF(FNAME.EQ.'CHAIN') THEN
        IFEXE=1
        CALL TEXSTR(LINE,JPAK,IFTX,FILNAME)
        INQUIRE(FILE=FILNAME,EXIST=OK)
        IF(OK) THEN
          LFLNM=LNBLNK(FILNM(ILUF))
          CALL CDX_MESS  ('*** Closing  "'//FILNM(ILUF)(:LFLNM)//'"')
          CLOSE(ILUN)
          OPEN(ILUN,FILE=FILNAME,STATUS='OLD')
          INQUIRE(ILUN,NAME=FILNAME)
          JJ=INDEX(FILNAME,']')
          LL=INDEX(FILNAME,';')
          IF(JJ.GT.0.AND.LL.GT.JJ+1) FILNAME=FILNAME(JJ+1:LL-1)
          FILNM(ILUF)=FILNAME
          LFLNM=LNBLNK(FILNM(ILUF))
          CALL CDX_MESS('*** Chain to "'//FILNM(ILUF)(:LFLNM)//'"')
        ELSE
          CALL CDX_MESS('*** Exec file NOT FOUND')
          IERR=9990+ILUF
        ENDIF
*
      ELSE IF(FNAME.EQ.'SETYES') THEN
        IFEXE=1
        YEAAAS='Y'
*
      ELSE IF(FNAME.EQ.'SETASK') THEN
        IFEXE=1
        YEAAAS=' '
*
      ELSE IF(FNAME.EQ.'PRON') THEN
        IFEXE=1
        CALL CDX_SETPR(1)
      ELSE IF(FNAME.EQ.'PROFF') THEN
        IFEXE=1
        CALL CDX_SETPR(0)
      ELSE IF(FNAME.EQ.'PROCL') THEN
        IFEXE=1
        CALL CDX_SETPR(-1)
      ELSE IF(IFSHEQC(FNAME(:LFN),'Q*UIT').GT.0
     +    .OR.IFSHEQC(FNAME(:LFN),'EX*IT').GT.0) THEN
        IFEXE=99
      ELSE
        CALL CDEXE1(LINE,0,IFEXE)
      ENDIF
      IF(IFEXE.EQ.0) CALL CDX_MESS
     +  ('>>'//FNAME(:LFN)//'<< ignored. Type: H -help, Q -quit')
*
      IF(ILUF.GT.0.AND.IERR.NE.0) THEN
*     -- stop file(s) execution (error) --
        CALL CDX_MESS('EXEC/CHAIN INTERRUPTED')
        DO I=ILUF,1,-1
          CLOSE(LUNCMD+I-1)
          LFLNM=LNBLNK(FILNM(I))
          CALL CDX_MESS('*** Closing  "'//FILNM(I)(:LFLNM)//'"')
        ENDDO
        ILUF=0
        ILUN=LUNINP
      ENDIF
*
      IF(IFEXE.EQ.99) GOTO 99
      GOTO 11
*
   77 CONTINUE
      IF(ILUF.GT.0) THEN
        LFLNM=LNBLNK(FILNM(ILUF))
        CALL CDX_MESS  ('*** End  of  "'//FILNM(ILUF)(:LFLNM)//'"')
        CLOSE(ILUN)
        FILNM(ILUF)='-'
        ILUF=ILUF-1
        IF(ILUF.GT.0) THEN
          ILUN=LUNCMD+ILUF-1
          LFLNM=LNBLNK(FILNM(ILUF))
          CALL CDX_MESS('*** Back  to "'//FILNM(ILUF)(:LFLNM))
        ELSE
          ILUF=0
          ILUN=LUNINP
        ENDIF
      ELSE
        CALL CDX_MESS('----End of TERM input----')
        ILUN=0
      ENDIF
      IF(ILUN.GT.0) GOTO 10
*
   99 CONTINUE
      CALL CDX_END
      CALL CDX_MESS('----- CDEX end -----')
      END
