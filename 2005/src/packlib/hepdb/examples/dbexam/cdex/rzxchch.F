*
* $Id: rzxchch.F,v 1.1.1.1 1996/02/28 16:23:40 mclareni Exp $
*
* $Log: rzxchch.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:40  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE RZXCHCH(FNAME,LINE,JPAK,IFERR)
*     -----------------------------****-
*     ************************************
*     * This is an interface to call     *
*     * several RZ routiunes of the type *
*     *      RZxxxx(CHDIR,CHOPT)         *
*     *  or  RZxxxx(CHDIR)               *
*     *     -- Author :   Boris Khomenko *
*     ************************************
*
      IMPLICIT NONE
*
      CHARACTER FNAME*(*)
      CHARACTER LINE*(*)
      INTEGER   JPAK
*
*  -- general definitions --
***+CDE, QUEST.  " common /quest/ iquest(100) "
      COMMON /QUEST/ IQUEST(100)
      INTEGER        IQUEST
*
*  -- externals --
      INTEGER JBYT , LNBLNK
*
*  -- parameters for RZXXXX calls --
      CHARACTER FNAM*8
      CHARACTER CHDIR*40
      CHARACTER CHOPT*6
      INTEGER   LNAM , LDIR , LOPT
      INTEGER   IRC
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
      INTEGER   IFERR , IFOKY , IFCON
      CHARACTER YES*1
      CHARACTER MESL*78
      INTEGER   LMES
      INTEGER   JJ , LR , LL
*
      IFERR=0
      IFOKY=0
*
      FNAM=FNAME
      LNAM=MAX0(LNBLNK(FNAM),6)
      CHDIR=' '
      CHOPT=' '
*
*     -- take pointers (the DIR/PATH starts at JNX)
      CALL TEXUPJ(JPAK,JTX,JNM,JNX,JMX)
      IF(JNX.LE.JMX) THEN
        IFOKY=1
*       ------ get a directory (path) ------
        JJ=JNX+INDEX(LINE(JNX:JMX),' ')-1
        IF(JJ.LT.JNX) JJ=JMX
        CHDIR=LINE(JNX:JJ)
        LDIR=JJ-JNX+1
        CALL CLTOU(CHDIR(:LDIR))
*       -- change JPAK with new JNX
        JNX=JJ+1
        CALL TEXPKJ(JTX,JNM,JNX,JMX,JPAK)
*
*       ------ get options ------
        CALL TEXINS(LINE,JPAK,IFTX,PRFX,NUMB,FNUM)
        CALL CLTOU(PRFX)
      ELSE
*     -- it is a call without parameters
        IFOKY=1
        IF     (FNAM.EQ.'RZCDIR') THEN
          CHOPT='P'
        ELSE IF(FNAM.EQ.'RZNDIR') THEN
          CHOPT='P'
        ELSE IF(FNAM.EQ.'RZLDIR') THEN
          CHOPT='-'
        ELSE IF(FNAM.EQ.'RZCLOS') THEN
          IFOKY=-1
        ELSE IF(FNAM.EQ.'RZDELT') THEN
          IFOKY=-1
        ELSE IF(FNAM.EQ.'RZEND ') THEN
          IFOKY=-1
        ELSE
          IFOKY=-1
          CALL CDX_MESS('>>'//FNAM//' inside RZXCHCH ???')
        ENDIF
      ENDIF
*
      IF(IFOKY.GE.0.AND.IFERR.EQ.0) THEN
*       -- confirmation flag
        IFCON=0
        IF     (FNAM.EQ.'RZCDIR') THEN
          IF(INDEX(CHOPT,'-').GT.0) IFCON=1
          IF(INDEX(CHOPT,'U').GT.0) IFCON=1
        ELSE IF(FNAM.EQ.'RZNDIR') THEN
          IF(INDEX(CHOPT,'-').GT.0) IFCON=1
        ELSE IF(FNAM.EQ.'RZLDIR') THEN
*         (not at all)
        ELSE IF(FNAM.EQ.'RZCLOS') THEN
          IFCON=1
        ELSE IF(FNAM.EQ.'RZDELT') THEN
          IFCON=1
          CHOPT='#'
        ELSE IF(FNAM.EQ.'RZEND ') THEN
          IFCON=1
          CHOPT='#'
        ENDIF
*
        LDIR=MAX0(LNBLNK(CHDIR),1)
        LOPT=MAX0(LNBLNK(CHOPT),1)
*
        MESL='>> call '//FNAM//'("'//CHDIR(:LDIR)//'")'
        LMES=INDEX(MESL,')')
        IF(CHOPT(1:1).NE.'#') THEN
          MESL(LMES:)=',"'//CHOPT(:LOPT)//'")'
          LMES=INDEX(MESL,')')
        ENDIF
        CALL CDX_MESS(MESL(:LMES))
*
        IF(IFCON.GT.0) THEN
          CALL CDX_ANSW('OK ? (y/n , <cr>=y )',YES)
          IF(YES.EQ.' ') YES='Y'
          CALL CLTOU(YES)
          IF(YES.EQ.'Y') CALL CDX_MESS('>>...... OK, do it')
        ELSE
          YES='Y'
        ENDIF
*
        IF(YES.EQ.'Y') THEN
          IF(CHOPT.EQ.'-') CHOPT=' '
          IF     (FNAM.EQ.'RZCDIR') THEN
            CALL RZCDIR(CHDIR,CHOPT)
          ELSE IF(FNAM.EQ.'RZNDIR') THEN
            CALL RZNDIR(CHDIR,CHOPT)
          ELSE IF(FNAM.EQ.'RZLDIR') THEN
            CALL RZLDIR(CHDIR,CHOPT)
          ELSE IF(FNAM.EQ.'RZCLOS') THEN
            CALL RZCLOS(CHDIR,CHOPT)
          ELSE IF(FNAM.EQ.'RZDELT') THEN
            CALL RZDELT(CHDIR)
          ELSE IF(FNAM.EQ.'RZEND ') THEN
            CALL RZEND (CHDIR)
          ENDIF
*         -- report the Return Code --
          IRC=IQUEST(1)
          IF(IRC.NE.0) CALL UXIRCM(FNAM(:LNAM),IRC)
        ELSE
          CALL CDX_MESS('>>'//FNAM(:LNAM)//' not called ')
        ENDIF
      ENDIF
      END
