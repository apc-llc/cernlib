*
* $Id: dbread.F,v 1.1.1.1 1996/02/28 16:23:41 mclareni Exp $
*
* $Log: dbread.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:41  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBREAD(LINE,JPAK,IFERR)
*     ---------------- ------****-====-
*                        -- Author :    Boris Khomenko   11/03/94
      IMPLICIT NONE
*
      CHARACTER LINE*(*)
      INTEGER   JPAK
*
#include "dbrinf.inc"
CC* - - - - - -/DBRINF/- - - - - - - - - - - - - - - - - -
CC* - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
#include "yeaaas.inc"
*
*  -- externals --
      INTEGER LNBLNK
      INTEGER IFSHEQC
*
*  -- NO default/set parameters
*                  for DBREAD  --
*
*  -- other parameters for DBREAD  --
      CHARACTER CHFILE*60
      INTEGER   LFILE
      CHARACTER CHPATH*40
      INTEGER   LPATH
*  ----------------------------
*
*  -- Command string analyse variables --
      INTEGER JPK1 , IFT1
      INTEGER IFTX , IFNX , LPRF
      CHARACTER PRFX*8 , PRF*8
      INTEGER NUMB
      REAL    FNUM
*
*  -- Local variables --
*
      INTEGER   IFERR , IFQUI
      CHARACTER YES*1
      CHARACTER CH*1
      CHARACTER MESL*78
      INTEGER   JJ , LR , LL
*
      IFERR=0
      IFQUI=0
      YES=YEAAAS
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
*        DBR status will be displayed
        CALL CDX_MESS('>>DBREAD  --- status --- <<')
*     -- show the call format --
        CALL CDX_MESS('Call format:')
        CALL CDX_MESS('> DBREAD [Close] [File filename]')
        CALL CDX_MESS('> DBREAD [All Rec Line/n Skip/n Put]')
        IFQUI=99
*
CCC   ELSE IF(PRFX(1:1).EQ.'-') THEN
CCC   -- it is an option --
*
      ELSE IF(PRFX.EQ.'YES') THEN
*     -- it is a "YES" --
        YES='Y'
        CALL CDX_MESS('>>DBREAD: "YES"')
*
      ELSE IF(PRFX.EQ.'ASK') THEN
*     -- it is a "ASK" --
        YES='-'
        CALL CDX_MESS('>>DBREAD: "ASK"')
*
      ELSE IF(IFSHEQC(PRFX,'C*lose').GT.0) THEN
*     -- it is a file closing command --
        IF(LUDBRIN.GT.0) THEN
          MESL=
     +    '>>DBREAD: closing LUN=000 "'//DBRIFIL(:LNBLNK(DBRIFIL))//'"'
          JJ=INDEX(MESL,'=')
          CALL ENCODI(LUDBRIN,-1,MESL(JJ+1:JJ+3),LR)
          CALL CDX_MESS(MESL(:LNBLNK(MESL)))
          CLOSE(LUDBRIN)
          LUDBRIN=0
        ELSE IF(DBRIFIL.NE.'-') THEN
          CALL CDX_MESS('>>DBREAD: already closed file "'
     +                                 //DBRIFIL(:LNBLNK(DBRIFIL))//'"')
        ELSE
          CALL CDX_MESS('>>DBREAD: Nothing to close')
        ENDIF
*
*
      ELSE IF(IFSHEQC(PRFX,'F*ile').GT.0) THEN
*     -- it is a file definition --
*
        CALL TEXSTR(LINE,JPAK,IFTX,CHFILE)
        IFNX=MOD(IFTX,4)
*
        IF(CHFILE.NE.' ') THEN
        ELSE
        ENDIF
*
      ELSE
*       -- signal an INVALID item --
        IFERR=1
        CALL CDX_MESS('>>DBREAD: Unrecognized item')
      ENDIF
      IF(IFNX.GT.0.AND.IFQUI.EQ.0.AND.IFERR.EQ.0) GOTO 12
*
      IF(IFQUI.LE.1.AND.IFERR.EQ.0) THEN
CCC     CALL  DBRSTAT(KDBR)
*
      ELSE IF(IFERR.NE.0) THEN
*     -- error end --
        CALL CDX_MESS('>>DBREAD instruction error')
      ENDIF
      END
