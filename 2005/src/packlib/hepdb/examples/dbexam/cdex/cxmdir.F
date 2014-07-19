*
* $Id: cxmdir.F,v 1.1.1.1 1996/02/28 16:23:40 mclareni Exp $
*
* $Log: cxmdir.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:40  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CXMDIR(LINE,JPAK,IFERR)
*     ---------------- ------****-====-
*                        -- Author :    Boris Khomenko   10/02/94
      IMPLICIT NONE
*
      CHARACTER LINE*(*)
      INTEGER   JPAK
*
#include "cdxfils.inc"
*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC+KEEP,CDXFILS.
CC*
CC      INTEGER MCDXFIL
CC      PARAMETER (MCDXFIL = 10)
CC      COMMON /CDXFILS/ NCDXFIL,KCDXFIL,
CC     +                 ICXFSTA(10),CDXFPRF(10),CDXFTOP(10),CDXFNAM(10)
CC      INTEGER   NCDXFIL, KCDXFIL, ICXFSTA
CC      CHARACTER CDXFPRF*2, CDXFTOP*8, CDXFNAM*40
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
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
*                  for CDMDIR --
*
CC:   'NKeys'   'MXObj'   'IPRec'   'DELta'   'ChOpt'
CC:   LVKEYS    LVMOBJ    LVPREC    LVDELT    LVOPTN
CC:   NKEYS     MPOBJ     IPREC     DELTA     CHOPT
CC:   NDKEYS(2) NDMOBJ(2) NDPREC(2) RDDELT(2) CHOPTN(2)
*
*     .... NKEYS ....
      INTEGER   LVKEYS , NDKEYS(2) , NKEYS
*
      CHARACTER*8 CHTAG(10) , CHFOR*10
*
*     .... MPOBJ ....
      INTEGER   LVMOBJ , NDMOBJ(2) , MPOBJ
*
*     .... IPREC ....
      INTEGER   LVPREC , NDPREC(2) , IPREC
*
*     .... DELTA ....
      INTEGER   LVDELT
      REAL               RDDELT(2) , DELTA
*
*     .... CHOPT ....
      INTEGER   LVOPTN
      CHARACTER*6        CHOPTN(2) , CHOPT
*
#if !defined(CERNLIB_IBMVM)
      STATIC  IFINIT
      STATIC    LVKEYS , NDKEYS
      STATIC    LVMOBJ , NDMOBJ
      STATIC    LVPREC , NDPREC
      STATIC    LVDELT , RDDELT
      STATIC    LVOPTN , CHOPTN
#endif
*  -- other parameters for CDMDIR --
      CHARACTER CHPATH*40
      INTEGER   LPATH
      INTEGER   LUNDB , LUNFZ , IDIV , IRC
*  - - - - - - - - - - - - - -
      INTEGER   LVNEW
*  ----------------------------
*
*  -- Command string analyse variables --
      INTEGER JPK1
      INTEGER IFTX , IFNX , LPRF
      CHARACTER PRFX*8 , PRF*8
      INTEGER NUMB
      REAL    FNUM
*
*  -- Local variables --
*
      INTEGER   IFERR , IFQUI
      CHARACTER YES*1
      CHARACTER MESL*78
      INTEGER   JJ , LL , LR
*
      DATA IFINIT/0/
*
      IF(IFINIT.EQ.0) THEN
        IFINIT=1
*
        LVKEYS=1
        LVMOBJ=1
        LVPREC=1
        LVDELT=1
        LVOPTN=1
*
        NDKEYS(1)=0
        NDMOBJ(1)=100
        NDPREC(1)=0
        RDDELT(1)=0.
        CHOPTN(1)='-'
*
        CHFOR=' '
        DO JJ=1,10
          CHTAG(JJ)='UsKey'//CHAR(ICHAR('0')+JJ)//'a'
        ENDDO
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
*            -- show the parameters setting --
        CALL CDX_MESS('>>CDMDIR --- parameters --- <<')
        CALL UXIPARM(' ','NKeys',LVKEYS,NDKEYS(LVKEYS),0,  0,NDKEYS(1))
        CALL UXIPARM(' ','MPObj',LVMOBJ,NDMOBJ(LVMOBJ),0,  0,NDMOBJ(1))
        CALL UXIPARM(' ','IPRec',LVPREC,NDPREC(LVPREC),0,  0,NDPREC(1))
        CALL UXRPARM(' ','DELta',LVDELT,RDDELT(LVDELT),0,  0,RDDELT(1))
        CALL UXCPARM(' ','opt  ',LVOPTN,CHOPTN(LVOPTN),0,' ',CHOPTN(1))
*     -- show the call format --
        CALL CDX_MESS('Call format:')
        CALL CDX_MESS('> CDMDIR [par/val -opt ...] [path]')
        IFQUI=99
*
      ELSE IF(PRFX.EQ.'YES') THEN
*     -- it is a "YES" --
        YES='Y'
        CALL CDX_MESS('>>CDMDIR: "YES"')
*
      ELSE IF(PRFX.EQ.'ASK') THEN
*     -- it is a "ASK" --
        YES='-'
        CALL CDX_MESS('>>CDMDIR: "ASK"')
*
      ELSE IF(PRFX(1:1).EQ.'-') THEN
*     -- it is an option --
        LVNEW=0
        IF(PRFX(1:2).EQ.'-=') LVNEW=1
        CALL UXNEWCP('CDMDIR','opt  ',LVNEW,PRFX,LVOPTN,CHOPTN)
*
      ELSE IF(PRFX.NE.' '.AND.IFNX.EQ.3) THEN
*       -- it seems to be a parameter (re)definition
*              NKEYS/n , MPOBJ/n , IPREC/n , DELTA/d
*
        CALL TEXINS(LINE,JPAK,IFTX,PRF,NUMB,FNUM)
        CALL CLTOU(PRF)
*
        LVNEW=0
        IF(PRF.EQ.'=') LVNEW=1
*
        IF     (IFSHEQC(PRFX,'NK*EYS').GT.0) THEN
          CALL UXNEWIP('CDMDIR','NKeys',LVNEW,NUMB,LVKEYS,NDKEYS)
*
        ELSE IF(IFSHEQC(PRFX,'MPO*BJ').GT.0) THEN
          CALL UXNEWIP('CDMDIR','MPObj',LVNEW,NUMB,LVMOBJ,NDMOBJ)
*
        ELSE IF(IFSHEQC(PRFX,'IPR*EC').GT.0) THEN
          CALL UXNEWIP('CDMDIR','IPRec',LVNEW,NUMB,LVPREC,NDPREC)
*
        ELSE IF(IFSHEQC(PRFX,'DEL*TA').GT.0) THEN
          CALL UXNEWRP('CDMDIR','DELta',LVNEW,FNUM,LVDELT,RDDELT)
*
        ELSE
*       -- signal an INVALID SET PARAMETER --
          CALL UXCPARM('CDMDIR',PRFX,0,' ',0,' ',' ')
          IFERR=1
        ENDIF
*
      ELSE IF(IFTX.EQ.3) THEN
*     -- it looks like a pathname (starts with "/")
        IFQUI=1
*       -- get pathname with previously saved pointers
        CALL TEXSTR(LINE,JPK1,IFTX,CHPATH)
        LPATH=LNBLNK(CHPATH)
        CALL CLTOU(CHPATH(:LPATH))
*
      ELSE
*       -- signal an INVALID item --
        IFERR=1
        CALL CDX_MESS('>>CDMDIR: Neither "Par/Value" nor Path "/..."')
      ENDIF
      IF(IFNX.GT.0.AND.IFQUI.EQ.0.AND.IFERR.EQ.0) GOTO 12
*
      IF(IFQUI.LE.1.AND.IFERR.EQ.0) THEN
*       -- call parameters --
        NKEYS=NDKEYS(LVKEYS)
        MPOBJ=NDMOBJ(LVMOBJ)
        IPREC=NDPREC(LVPREC)
        DELTA=RDDELT(LVDELT)
        CHOPT=CHOPTN(LVOPTN)
        IF(CHOPT(1:1).EQ.'-') CHOPT=CHOPT(2:)
*
        MESL='>>CALL CDMDIR('
        WRITE(MESL(15:),1111) NKEYS,MPOBJ,IPREC,CHOPT
 1111   FORMAT('CHPATH,',
     +         3(I5,','),'#      ,"',A5,'",IRC)')
        JJ=INDEX(MESL,'#')
        CALL ENCODR(DELTA,44,MESL(JJ:JJ+6),LR)
        CALL CDX_MESS(MESL(:LNBLNK(MESL)))
*
        MESL=' '
        MESL(15:)='       nkeys mpobj iprec   delta chopt'
        CALL CDX_MESS(MESL(:LNBLNK(MESL)))
*
        MESL(15:)=' chpath='//CHPATH(:LPATH)
        CALL CDX_MESS(MESL(:LNBLNK(MESL)))
*
        IF(CHPATH(:4).NE.'//CD'.OR.LPATH.LT.6.OR.
     +         LPATH.GT.6.AND.CHPATH(7:7).NE.'/')
     +  CALL CDX_MESS('>>CDMDIR WARNING! Not a HEPDB top //CDxx[/...]')
*
        IF(YES.NE.'Y') CALL CDX_ANSW('OK ? (y/n , <cr>=y )',YES)
        IF(YES.EQ.' ') YES='Y'
        CALL CLTOU(YES)
*
        IF(YES.EQ.'Y') THEN
          CALL CDMDIR
     +         (CHPATH,NKEYS,CHFOR,CHTAG,MPOBJ,IPREC,DELTA,CHOPT,IRC)
*         -- report the Return Code --
          CALL UXIRCM('CDMDIR',IRC)
        ELSE
          CALL CDX_MESS('>>CDMDIR not called (NOK)')
        ENDIF
      ELSE IF(IFERR.NE.0) THEN
*     -- error end --
        CALL CDX_MESS('>>CDMDIR not called (instruction error)')
      ENDIF
      END
