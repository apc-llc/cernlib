*
* $Id: cxtext.F,v 1.1.1.1 1996/02/28 16:23:40 mclareni Exp $
*
* $Log: cxtext.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:40  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CXTEXT(LINE,JPAK,IFERR)
*     ---------------- ------****-====-
*                        -- Author :    Boris Khomenko   10/02/94
      IMPLICIT NONE
*
      CHARACTER LINE*(*)
      INTEGER   JPAK
*
*  (ICDXDIV)
#include "cdxdiv.inc"
CC*
CC* - - - - - -/CDULNK/- - - - - - - - - - - - - - - - - -
CC      COMMON /CDULNK/ ICDXDIV , LCDXDIV,
CC     +                LDBRKY(10),LDBRDA(10),LDBRXX
CC      INTEGER         ICDXDIV , LCDXDIV
CC      INTEGER         LDBRKY,LDBRDA,LDBRXX
CC* - - - - - - - - - - - - - - - - - - - - - - - - - - - -
CC*
*
#include "dbrinf.inc"
CC* - - - - - -/DBRINFO/- - - - - - - - - - - - - - - - - -
CC      COMMON /DBRIFIL/ LUDBRIN,NLDBRIN,DBRIFIL
CC      INTEGER          LUDBRIN,NLDBRIN
CC      CHARACTER*60     DBRIFIL
CC*
CC      COMMON /DBRHEAD/ IDBRTYP,IDBRMOD,LENGDBR,DBRTYPE,DBRPATH
CC      INTEGER          IDBRTYP,IDBRMOD,LENGDBR
CC      CHARACTER        DBRTYPE*4,DBRPATH*40
CC*
CC      COMMON /DBRCMNT/ DBRCMNT
CC      CHARACTER        DBRCMNT*72
CC*
CC      COMMON /DBRINBF/ IDBRDIV,LDBRBUF,IDBRKEY(20),NDBRKEY
CC      INTEGER          IDBRDIV,LDBRBUF,IDBRKEY    ,NDBRKEY
CC*
CC      COMMON /DBRLINK/ LDBRKYS,LDBRDAT
CC      INTEGER          LDBRKYS,LDBRDAT
CC* - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
#include "cdxluns.inc"
*
#include "yeaaas.inc"
*
*  -- externals --
      INTEGER LNBLNK
      INTEGER IFSHEQC
*
*  -- initialization flag --
      INTEGER IFINIT
*  -- default/set parameters
*                  for CDTEXT --
*
CC:   'LUnit'   'ChOpt'
CC:   LVUNIT
CC:   NDUNIT(2)
CC:   LUNIT
*
*     .... LUNIT ....
      INTEGER   LVUNIT , NDUNIT(2) , LUNIT
*
#if !defined(CERNLIB_IBMVM)
      STATIC    IFINIT
      STATIC    LVUNIT , NDUNIT
#endif
*  -- other parameters for CDTEXT --
      CHARACTER CHFILE*40
      INTEGER   LBANK
      CHARACTER CHOPT*6
      INTEGER   IRC
*  - - - - - - - - - - - - - -
      INTEGER   LVNEW
*  ----------------------------
*
*  -- Command string analyse variables --
      INTEGER IFTX , IFNX , LPRF
      CHARACTER PRFX*8 , PRF*8
      INTEGER NUMB
      REAL    FNUM
      INTEGER JPK1
*
*  -- Local variables --
*
      INTEGER   IFERR , IFQUI
      INTEGER   LFILE , LOPT
      CHARACTER YES*1 , CH*1
      CHARACTER MESL*78
      INTEGER   JJ , LR , LL
      CHARACTER CODE*11
*
      DATA IFINIT/0/
*
      IF(IFINIT.EQ.0) THEN
        IFINIT=1
        LVUNIT=1
        NDUNIT(1)=2
      ENDIF
*
      IFERR=0
      IFQUI=0
      YES=YEAAAS
*
      CHFILE='-'
      LBANK=0
      CHOPT='-'
*
   12 CONTINUE
*     _ save the next item's pointer
      JPK1=JPAK
      CALL TEXINS(LINE,JPAK,IFTX,PRFX,NUMB,FNUM)
      IFNX=MOD(IFTX,4)
      CALL CLTOU(PRFX)
*
      IF(IFTX.LE.0) THEN
*     -- it is a call without parameters
*            -- show the parameters setting --
        CALL CDX_MESS('>>CDTEXT --- parameters --- <<')
        CALL UXIPARM(' ','LUnit',LVUNIT,NDUNIT(LVUNIT),0,  0,NDUNIT(1))
*     -- show the call format --
        CALL CDX_MESS('Call format:')
        CALL CDX_MESS('> CDTEXT [par/val] [-opt] [* or LBANK] [file]')
        IFQUI=99
*
      ELSE IF(PRFX.EQ.'YES') THEN
*     -- it is a "YES" --
        YES='Y'
        CALL CDX_MESS('>>CDTEXT: "YES"')
*
      ELSE IF(PRFX.EQ.'ASK') THEN
*     -- it is a "ASK" --
        YES='-'
        CALL CDX_MESS('>>CDTEXT: "ASK"')
*
      ELSE IF(PRFX(1:1).EQ.'-') THEN
*       -- it is an option set
        CHOPT=PRFX
*
      ELSE IF(PRFX.NE.' '.AND.IFNX.EQ.3) THEN
*       -- it seems to be a parameter set (not option)
*              LUNIT/n
*
        CALL TEXINS(LINE,JPAK,IFTX,PRF,NUMB,FNUM)
        CALL CLTOU(PRF)
*
        LVNEW=0
        IF(PRF.EQ.'=') LVNEW=1
*
        IF     (IFSHEQC(PRFX,'LU*NIT').GT.0) THEN
          CALL UXNEWIP('CDTEXT','LUnit',LVNEW,NUMB,LVUNIT,NDUNIT)
*
        ELSE
*       -- signal an INVALID SET PARAMETER --
          CALL UXCPARM('CDTEXT',PRFX,0,' ',0,' ',' ')
          IFERR=1
        ENDIF
*
      ELSE IF(IFTX/4.EQ.2) THEN
*     -- it is an alone integer: LBANK
        LBANK=NUMB
*
      ELSE IF(PRFX(1:1).EQ.'*') THEN
*     -- it is a reference to a current DB record --
        IF(LDBRDA.GT.0) THEN
          LBANK=LDBRDA
        ELSE
          CALL CDX_MESS('>>CDTEXT: No link to DB record !!!')
          IFERR=1
        ENDIF
*
      ELSE
*     -- otherwise it is a filename
*       -- get filename with previously saved pointers
        JPAK=JPK1
        CALL TEXSTR(LINE,JPAK,IFTX,CHFILE)
        IF(CHFILE.EQ.' ') CHFILE='-'
        LFILE=LNBLNK(CHFILE)
      ENDIF
*
      IF(IFNX.GT.0.AND.IFQUI.EQ.0.AND.IFERR.EQ.0) GOTO 12
*
      IF(IFQUI.LE.1.AND.IFERR.EQ.0) THEN
*     -- make a call -------------------
*       -- call parameters
        LUNIT=NDUNIT(LVUNIT)
        IF(CHOPT.EQ.'-') THEN
*       -- automatic option
          IF(LBANK.EQ.0) THEN
            CHOPT='R'
          ELSE IF(CHFILE.NE.'-') THEN
            CHOPT='W'
          ELSE
            CHOPT='P'
          ENDIF
          CALL CDX_MESS('>>CDTEXT Auto Option: "'//CHOPT(:1)//'"')
        ENDIF
        IF(CHOPT(1:1).EQ.'-') CHOPT=CHOPT(2:)
        LOPT=MAX0(LNBLNK(CHOPT),1)
*
        IF(CHOPT.EQ.'P') YES='Y'
*
        CALL ENCODI(LUNIT,1,CODE(:3),LR)
        MESL='>>CALL CDTEXT('//CODE(:3)//',CHFILE,"-",LBANK,"'
     +                            //CHOPT(:LOPT)//'",IRC)'
        CALL CDX_MESS(MESL(:LNBLNK(MESL)))
*
        MESL=' '
        MESL(15:)='lun chfile="'//CHFILE(:LFILE)//'"'
        CALL CDX_MESS(MESL(:LNBLNK(MESL)))
*
        IF(LBANK.GT.0.OR.INDEX(CHOPT,'R').LE.0) THEN
*       -- report initial LBANK value if given (or assumed)
          CALL ENCODI(LBANK,-1,CODE,LR)
          IF(LDBRDA.GT.0) CODE=CODE(:LR)//'<*'
          CALL CDX_MESS('                             LBank='//CODE)
        ENDIF
*
        IF(YES.NE.'Y') CALL CDX_ANSW('OK ? (y/n , <cr>=y )',YES)
        IF(YES.EQ.' ') YES='Y'
        CALL CLTOU(YES)
*
        IF(YES.EQ.'Y') THEN
          IF(INDEX(CHOPT,'P').GT.0)
     +      CALL CDX_MESS('>>....................................')
*
          CALL CDTEXT(LUNIT,CHFILE,'-',LBANK,CHOPT,IRC)
*
          IF(INDEX(CHOPT,'P').GT.0)
     +      CALL CDX_MESS('>>....................................')
*
          IF(INDEX(CHOPT,'R').GT.0) THEN
*         -- return and report final LBANK value if has to be so
            CALL ENCODI(LBANK,-1,CODE,LR)
            LDBRDA=LBANK
            CODE=CODE(:LR)//'>*'
            CALL CDX_MESS('==CDTEXT>>  Text is read into LBANK='//CODE)
          ENDIF
*         -- report the Return Code --
          CALL UXIRCM('CDTEXT',IRC)
          IF((INDEX(CHOPT,'R').GT.0.OR.
     +        INDEX(CHOPT,'W').GT.0).AND.
     +        INDEX(CHOPT,'A').LE.0) THEN
            CALL ENCODI(LUNIT,-1,CODE(:3),LR)
            CALL CDX_MESS('==CDTEXT>> Auto Close LUN='//CODE(:3))
            CLOSE(LUNIT)
          ENDIF
        ELSE
          CALL CDX_MESS('>>CDTEXT not called (NOK)')
        ENDIF
      ELSE IF(IFERR.NE.0) THEN
*     -- error end --
        CALL CDX_MESS('>>CDTEXT not called (instruction error)')
      ENDIF
      END
