*
* $Id: cxpaw.F,v 1.1.1.1 1996/02/28 16:23:40 mclareni Exp $
*
* $Log: cxpaw.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:40  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CXPAW(MWPAWC,LINE,JPAK,IFERR)
*     ---------------------- ------****-=====-
*                        -- Author :    Boris Khomenko   10/02/94
      IMPLICIT NONE
*
      INTEGER   MWPAWC,JPAK,IFERR
      CHARACTER LINE*(*)
*
#include "cdxdiv.inc"
*
#include "yeaaas.inc"
*
*  -- externals --
      INTEGER JBYT , LNBLNK
      INTEGER IFSHEQC
*
*  -- initialization flag --
      INTEGER IFINIT
*  -- default/set parameters
*                  for CXPAW  --
*
CC:   'NWPaw'   'NWHbk'   'UDNam'   'NWIni'   'NWMax'   'ChOpt'
CC:   LVNPAW    LVNHBK    LVUNAM    LVNINI    LVNMAX    LVOPTN
CC:   NDNPAW(2) NDNHBK(2) CDUNAM(2) NDNINI(2) NDNMAX(2) CDOPTN(2)
CC:   NWPAW     NWHBK     UDNAM     NWINI     NWMAX     CHOPT
*
*     .... NWPAW ....
      INTEGER   LVNPAW , NDNPAW(2) , NWPAW
*
*     .... NWHBK ....
      INTEGER   LVNHBK , NDNHBK(2) , NWHBK
*
*     .... UDNAM ....
      INTEGER   LVUNAM
      CHARACTER*8 CDUNAM(2) , UDNAM
*
*     .... NWINI ....
      INTEGER   LVNINI , NDNINI(2) , NWINI
*
*     .... NWMAX ....
      INTEGER   LVNMAX , NDNMAX(2) , NWMAX
*
*     .... CHOPT ....
      INTEGER   LVOPTN
      CHARACTER*6        CDOPTN(2) , CHOPT
*
#if !defined(CERNLIB_IBMVM)
      STATIC  IFINIT
      STATIC    LVNPAW , NDNPAW
      STATIC    LVNHBK , NDNHBK
      STATIC    LVUNAM , CDUNAM
      STATIC    LVNINI , NDNINI
      STATIC    LVNMAX , NDNMAX
      STATIC    LVOPTN , CDOPTN
#endif
*  -- other parameters for CDPAW  --
      INTEGER   IDIV , IRC
*  - - - - - - - - - - - - - -
      INTEGER   LVNEW
*  ----------------------------
*
*  -- Command string analyse variables --
      INTEGER IFTX , IFNX , LPRF
      CHARACTER PRFX*8 , PRF*8
      INTEGER NUMB
      REAL    FNUM
      INTEGER JPK1, JTX, JNM, JNX, JMX
*
*  -- Local variables --
*
      INTEGER   IFQUI
      CHARACTER CODE*11 , YES*1
      CHARACTER MESL*78
      INTEGER   JJ , LR , LL
*
      DATA IFINIT/0/
*
      IF(IFINIT.EQ.0) THEN
        IFINIT=1
*
        LVNPAW=1
        LVNHBK=1
        LVUNAM=1
        LVNINI=1
        LVNMAX=1
        LVOPTN=1
*
        NDNPAW(1)=MWPAWC
        NDNHBK(1)=MWPAWC/10
        CDUNAM(1)='DBUSER'
        NDNINI(1)=MWPAWC/10
        NDNMAX(1)=MWPAWC/2
        CDOPTN(1)='-ZPHU'
      ENDIF
*
      IFERR=0
      IFQUI=0
      YES=YEAAAS
*
   12 CONTINUE
*     _ save the next item's pointer
      JPK1=JPAK
      CALL TEXINS(LINE,JPAK,IFTX,PRFX,NUMB,FNUM)
      IFNX=JBYT(IFTX,1,2)
      CALL CLTOU(PRFX)
*
      IF(IFTX.LE.0) THEN
*     -- it is a call without parameters
*        -- to do a CALL...
        IFQUI=1
*
      ELSE IF(PRFX(1:1).EQ.'%') THEN
*     -- it is a Command Modificator --
*
      ELSE IF(PRFX(1:1).EQ.'?') THEN
*     -- it is an info/help request --
*            -- show the parameters setting --
        CALL CDX_MESS('>>CDPAW  --- parameters --- <<')
        CALL UXIPARM(' ','NWPaw',LVNPAW,NDNPAW(LVNPAW),0,  0,NDNPAW(1))
        CALL UXIPARM(' ','NWHbk',LVNHBK,NDNHBK(LVNHBK),0,  0,NDNHBK(1))
        CALL UXCPARM(' ','UDName',LVUNAM,CDUNAM(LVUNAM),0,' ',CDUNAM(1))
        CALL UXIPARM(' ','NWIni',LVNINI,NDNINI(LVNINI),0,  0,NDNINI(1))
        CALL UXIPARM(' ','NWMax',LVNMAX,NDNMAX(LVNMAX),0,  0,NDNMAX(1))
        CALL UXCPARM(' ','opt  ',LVOPTN,CDOPTN(LVOPTN),0,' ',CDOPTN(1))
*     -- show the call format --
        CALL CDX_MESS('Call format:')
        CALL CDX_MESS('> CDPAW [?] [par/val ...] [-opt]')
        IFQUI=99
*
      ELSE IF(PRFX.EQ.'YES') THEN
*     -- it is a "YES" --
        YES='Y'
        CALL CDX_MESS('>>CDPAW : "YES"')
*
      ELSE IF(PRFX.EQ.'ASK') THEN
*     -- it is a "ASK" --
        YES='-'
        CALL CDX_MESS('>>CDPAW : "ASK"')
*
      ELSE IF(PRFX(1:1).EQ.'-') THEN
*     -- it is an option --
        LVNEW=0
        IF(PRFX(1:2).EQ.'-=') LVNEW=1
        CALL UXNEWCP('CDPAW ','opt  ',LVNEW,PRFX,LVOPTN,CDOPTN)
*
      ELSE IF(PRFX.NE.' '.AND.IFNX.EQ.3) THEN
*       -- it seems to be a parameter (re)definition
*              NWPAW/n , NWHBK/n , UDNAM/cn NWINI/n , NWMAX/n
*
        CALL TEXINS(LINE,JPAK,IFTX,PRF,NUMB,FNUM)
        CALL CLTOU(PRF)
*
        LVNEW=0
        IF(PRF.EQ.'=') LVNEW=1
*
        IF     (IFSHEQC(PRFX,'NWP*aw').GT.0) THEN
          CALL UXNEWIP('CDPAW ','NWPaw',LVNEW,NUMB,LVNPAW,NDNPAW)
*
        ELSE IF(IFSHEQC(PRFX,'NWH*bk').GT.0) THEN
          CALL UXNEWIP('CDPAW ','NWHbk',LVNEW,NUMB,LVNHBK,NDNHBK)
*
        ELSE IF(IFSHEQC(PRFX,'UDN*am').GT.0) THEN
          CALL UXNEWCP('CDPAW ','UDNam',LVNEW, PRF,LVUNAM,CDUNAM)
*
        ELSE IF(IFSHEQC(PRFX,'NWI*ni').GT.0) THEN
          CALL UXNEWIP('CDPAW ','NWIni',LVNEW,NUMB,LVNINI,NDNINI)
*
        ELSE IF(IFSHEQC(PRFX,'NWM*ax').GT.0) THEN
          CALL UXNEWRP('CDPAW ','NWMax',LVNEW,NUMB,LVNMAX,NDNMAX)
*
        ELSE
*       -- signal an INVALID SET PARAMETER --
          CALL UXCPARM('CDPAW ',PRFX,0,' ',0,' ',' ')
          IFERR=1
        ENDIF
*
      ELSE
*     -- signal an INVALID item --
        IFERR=1
        CALL CDX_MESS('>>CDPAW : Neither "Par/Value" nor "-opt"')
      ENDIF
      IF(IFNX.GT.0.AND.IFQUI.EQ.0.AND.IFERR.EQ.0) GOTO 12
*
      IF(IFQUI.LE.1.AND.IFERR.EQ.0) THEN
*       -- call parameters --
        NWPAW=NDNPAW(LVNPAW)
        NWHBK=NDNHBK(LVNHBK)
        UDNAM=CDUNAM(LVUNAM)
        NWINI=NDNINI(LVNINI)
        NWMAX=NDNMAX(LVNMAX)
        CHOPT=CDOPTN(LVOPTN)
        IF(CHOPT(1:1).EQ.'-') CHOPT=CHOPT(2:)
*
        MESL='>>CALL CDPAW ('
        WRITE(MESL(15:),1111) NWPAW,NWHBK,UDNAM,NWINI,NWMAX,CHOPT
 1111   FORMAT(I6,',',I5,',IDIV,"',A8,'"',2(','I5),',"',A6)
        LL=LNBLNK(MESL)
        MESL(LL+1:LL+6)='",IRC)'
        LL=LL+6
        CALL CDX_MESS(MESL(:LL))
*
        MESL=' '
        MESL(15:)=' nwpaw nwhbk       udname    nwini nwmax  opt'
        CALL CDX_MESS(MESL(:LNBLNK(MESL)))
*
        IF(YES.NE.'Y') CALL CDX_ANSW('OK ? (y/n , <cr>=y )',YES)
        IF(YES.EQ.' ') YES='Y'
        CALL CLTOU(YES)
*
        IF(YES.EQ.'Y') THEN
          CALL CDX_MESS('>>....................................')
          CALL CDPAW(NWPAW,NWHBK,IDIV,UDNAM,NWINI,NWMAX,CHOPT,IRC)
          CALL CDX_MESS('>>....................................')
*
          ICDXDIV=IDIV
*         -- report the returned IDIV --
          CALL ENCODI(IDIV,-1,CODE,LR)
          CALL CDX_MESS('==CDPAW >>  IDIV='//CODE(:LR))
*         -- report the Return Code --
          CALL UXIRCM('CDPAW ',IRC)
          CALL CDX_MESS('>>........more of Z init..............')
          CALL CDXINIZ
CC        CALL CDX_MESS('>>....................................')
        ELSE
          CALL CDX_MESS('>>CDPAW  not called (NOK)')
        ENDIF
      ELSE IF(IFERR.NE.0) THEN
*     -- error end --
        CALL CDX_MESS('>>CDPAW not called (instruction error)')
      ENDIF
      END
