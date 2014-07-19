*
* $Id: cxlist.F,v 1.1.1.1 1996/02/28 16:23:41 mclareni Exp $
*
* $Log: cxlist.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:41  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CXLIST(LINE,JPAK,IFERR)
*     ---------------- ------****-====-
*                        -- Author :    Boris Khomenko   10/02/94
      IMPLICIT NONE
*
      CHARACTER LINE*(*)
      INTEGER   JPAK
*
*
#include "cdxdiv.inc"
*
#include "cdxfils.inc"
*
#include "yeaaas.inc"
*
*  -- externals --
      INTEGER JBYT , LNBLNK
      INTEGER IFSHEQC
*
*  -- initialization flag --
      INTEGER IFINIT
*
*  -- default/set parameters
*                  for CDLIST --
*
CC:   'KSN'     'ChBank'  'LNK1'    'LNK2'    'DAT1'    'DAT2'    'ChOpt'
CC:   LVKYSN    LVBANK    LVLNK1    LVLNK2    LVDAT1    LVDAT2    LVOPTN
CC:   KEYSN     CHBAN     LINK1     LINK2     IDAT1     IDAT2     CHOPT
CC:   NDKYSN(2) CHBANK(2) NDLNK1(2) NDLNK2(2) NDDAT1(2) NDDAT2(2) CHOPTN(2)
*
*     .... KSN ....
      INTEGER   LVKYSN , NDKYSN(2) , KEYSN
*
*     .... ChBank ....
      INTEGER   LVBANK
      CHARACTER*4  CHBANK(2) , CHBAN
*
*     .... LNK1 ....
      INTEGER   LVLNK1 , NDLNK1(2) , LINK1
*
*     .... LNK2 ....
      INTEGER   LVLNK2 , NDLNK2(2) , LINK2
*
*     .... DAT1 ....
      INTEGER   LVDAT1 , NDDAT1(2) , IDAT1
*
*     .... DAT2 ....
      INTEGER   LVDAT2 , NDDAT2(2) , IDAT2
*
*     .... CHOPT ....
      INTEGER   LVOPTN
      CHARACTER*6        CHOPTN(2) , CHOPT
*
#if !defined(CERNLIB_IBMVM)
      STATIC  IFINIT
      STATIC    LVKYSN , NDKYSN
      STATIC    LVBANK , CHBANK
      STATIC    LVLNK1 , NDLNK1
      STATIC    LVLNK2 , NDLNK2
      STATIC    LVDAT1 , NDDAT1
      STATIC    LVDAT2 , NDDAT2
      STATIC    LVOPTN , CHOPTN
#endif
*  -- other parameters for CDLIST --
      CHARACTER CHPAT*40
      INTEGER   LPAT , LOPT , LBAN
      INTEGER   IRC
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
      INTEGER   JJ , LR , LL
*
      DATA IFINIT/0/
*
      IF(IFINIT.EQ.0) THEN
        IFINIT=1
*
        LVKYSN=1
        LVBANK=1
        LVLNK1=1
        LVLNK2=1
        LVDAT1=1
        LVDAT2=1
        LVOPTN=1
*
        NDKYSN(1)=0
        CHBANK(1)='-'
        NDLNK1(1)=0
        NDLNK2(1)=0
        NDDAT1(1)=0
        NDDAT2(1)=0
        CHOPTN(1)='-V'
      ENDIF
*
      IFERR=0
      IFQUI=0
      YES=YEAAAS
*
      CHPAT=' '
*
   12 CONTINUE
*     _ save the next item's pointer
      JPK1=JPAK
      CALL TEXNME(LINE,JPAK,IFTX,PRFX)
      IFNX=MOD(IFTX,4)
      CALL CLTOU(PRFX)
*
      IF(IFTX.LE.0) THEN
*     -- it is a call without parameters
*            -- show the parameters setting --
        CALL CDX_MESS('>>CDLIST --- parameters --- <<')
        CALL UXIPARM(' ','KSN  ',LVKYSN,NDKYSN(LVKYSN),0,  0,NDKYSN(1))
        CALL UXCPARM(' ','ChBnk',LVBANK,CHBANK(LVBANK),0,' ',CHBANK(1))
        CALL UXIPARM(' ','L1   ',LVLNK1,NDLNK1(LVLNK1),0,  0,NDLNK1(1))
        CALL UXIPARM(' ','L2   ',LVLNK2,NDLNK2(LVLNK2),0,  0,NDLNK2(1))
        CALL UXIPARM(' ','D1   ',LVDAT1,NDDAT1(LVDAT1),0,  0,NDDAT1(1))
        CALL UXIPARM(' ','D2   ',LVDAT2,NDDAT2(LVDAT2),0,  0,NDDAT2(1))
        CALL UXCPARM(' ','opt  ',LVOPTN,CHOPTN(LVOPTN),0,' ',CHOPTN(1))
*     -- show the call format --
        CALL CDX_MESS('Call format:')
        CALL CDX_MESS('> CDLIST [par/val ...] [-opt] [path]')
        IFQUI=99
*
      ELSE IF(PRFX.EQ.'YES') THEN
*     -- it is a "YES" --
        YES='Y'
        CALL CDX_MESS('>>CDLIST: "YES"')
*
      ELSE IF(PRFX.EQ.'ASK') THEN
*     -- it is a "ASK" --
        YES='-'
        CALL CDX_MESS('>>CDLIST: "ASK"')
*
      ELSE IF(PRFX(1:1).EQ.'-') THEN
*     -- it is an option --
        LVNEW=0
        IF(PRFX(1:2).EQ.'-=') LVNEW=1
        CALL UXNEWCP('CDLIST','opt  ',LVNEW,PRFX,LVOPTN,CHOPTN)
*
      ELSE IF(PRFX.NE.' '.AND.IFNX.EQ.3) THEN
*       -- it seems to be a parameter (re)definition
*              KSN/n , CHBnk/c , L1/n , L2/n , D1/n , D2/n
*
        IF(PRFX(1:1).NE.'C') THEN
          CALL TEXINS(LINE,JPAK,IFTX,PRF,NUMB,FNUM)
        ELSE
          CALL TEXSTR(LINE,JPAK,IFTX,PRF)
        ENDIF
        IFNX=MOD(IFTX,4)
        CALL CLTOU(PRF)
*
        LVNEW=0
        IF(PRF.EQ.'=') LVNEW=1
*
        IF     (IFSHEQC(PRFX,'K*SN').GT.0) THEN
          CALL UXNEWIP('CDLIST','Ksn  ',LVNEW,NUMB,LVKYSN,NDKYSN)
*
        ELSE IF(IFSHEQC(PRFX,'CHB*nk').GT.0) THEN
          CALL UXNEWCP('CDLIST','CHBnk',LVNEW,PRF ,LVBANK,CHBANK)
*
        ELSE IF(PRFX.EQ.'L1') THEN
          CALL UXNEWIP('CDLIST','L1',LVNEW,NUMB,LVLNK1,NDLNK1)
*
        ELSE IF(PRFX.EQ.'L2') THEN
          CALL UXNEWIP('CDLIST','L2',LVNEW,NUMB,LVLNK2,NDLNK2)
*
        ELSE IF(PRFX.EQ.'D1') THEN
          CALL UXNEWIP('CDLIST','D1',LVNEW,NUMB,LVDAT1,NDDAT1)
*
        ELSE IF(PRFX.EQ.'D2') THEN
          CALL UXNEWIP('CDLIST','D2',LVNEW,NUMB,LVDAT2,NDDAT2)
*
        ELSE
*       -- signal an INVALID SET PARAMETER --
          CALL UXCPARM('CDLIST',PRFX,0,' ',0,' ',' ')
          IFERR=1
        ENDIF
*
      ELSE IF(PRFX.EQ.'*'.OR.PRFX.EQ.'.') THEN
*     -- it is the current directory ("*" or ".")
        CHPAT=' '
        LPAT=1
        IFQUI=1
*
      ELSE IF((PRFX.EQ.' '.AND.IFTX.EQ.3).OR.PRFX(1:1).EQ.'.') THEN
*     -- it looks like a pathname (starts with "/" or ".")
*       -- get pathname with previously saved pointers
        JPAK=JPK1
        CALL TEXSTR(LINE,JPAK,IFTX,CHPAT)
        IFNX=MOD(IFTX,4)
        LPAT=LNBLNK(CHPAT)
        CALL CLTOU(CHPAT(:LPAT))
        IFQUI=1
*
      ELSE
*       -- signal an INVALID item --
        IFERR=1
        CALL CDX_MESS('>>CDLIST: Unrecognized item')
      ENDIF
      IF(IFNX.GT.0.AND.IFQUI.EQ.0.AND.IFERR.EQ.0) GOTO 12
*
      IF(IFQUI.EQ.1.AND.IFERR.EQ.0) THEN
*       -- call parameters --
        KEYSN=NDKYSN(LVKYSN)
        CHBAN=CHBANK(LVBANK)
        LINK1=NDLNK1(LVLNK1)
        LINK2=NDLNK2(LVLNK2)
        IDAT1=NDDAT1(LVDAT1)
        IDAT2=NDDAT2(LVDAT2)
        CHOPT=CHOPTN(LVOPTN)
*
        IF(CHOPT(1:1).EQ.'-') CHOPT=CHOPT(2:)
        LOPT=MAX0(LNBLNK(CHOPT),1)
        LBAN=MAX0(LNBLNK(CHBAN),1)
*
        CALL CDI_MESS
     +  ('>>CALL CDLIST(chpath,ksn=0,b="'//CHBAN(:LBAN)//'",.>',KEYSN)
        CALL CDI_MESS('l1=0,.>',LINK1)
        CALL CDI_MESS('l2=0,.>',LINK2)
        CALL CDI_MESS('d1=0,.>',IDAT1)
        CALL CDI_MESS('d2=0,opt="'//CHOPT(:LOPT)//'",IRC)',IDAT2)
*
        CALL CDX_MESS('               chpath='//CHPAT(:LPAT))
*
        CALL CDLIST
     +       (CHPAT,KEYSN,CHBAN,LINK1,LINK2,IDAT1,IDAT2,CHOPT,IRC)
*       -- report the Return Code --
        IF(IRC.NE.0) CALL UXIRCM('CDLIST',IRC)
      ELSE IF(IFERR.NE.0) THEN
*     -- error end --
        CALL CDX_MESS('>>CDLIST not called (instruction error)')
      ENDIF
      END
