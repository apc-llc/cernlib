*
* $Id: zftpcd.F,v 1.1.1.1 1996/03/08 15:44:19 mclareni Exp $
*
* $Log: zftpcd.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:19  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE ZFTPCD
#include "cspack/czsock.inc"
#include "cspack/hcmail.inc"
#include "cspack/quest.inc"
      CHARACTER*80 PATH
      CHARACTER*80 CHPASS
      CHARACTER*20 CHOPT
*
#include "cspack/czopen.inc"
 
      CALL KUGETS(PATH,LPATH)
      IF(LPATH.EQ.0) THEN
         CALL ZFTPWD
         RETURN
      ENDIF
*
*     For remote connections to VM systems, cater for write links
*
      CALL KUGETC(CHPASS,LPASS)
      CALL KUGETC(CHOPT,LCHOPT)
 
      IF(LCHOPT.EQ.0) THEN
         CHOPT  = ' '
         LCHOPT = 1
      ENDIF
 
      IOPTC = INDEX(CHOPT(1:LCHOPT),'C')
      IOPTR = INDEX(CHOPT(1:LCHOPT),'R')
      IOPTW = INDEX(CHOPT(1:LCHOPT),'W')
 
      IF((IOPTR.NE.0).AND.(IOPTW.NE.0)) THEN
         PRINT *,'ZFTPCD. only one of R(ead) and W(rite)',
     +           'maybe specified'
         RETURN
      ENDIF
 
      IF((IOPTR+IOPTW.NE.0).AND.(LPASS.EQ.0)) THEN
         PRINT *,'ZFTPCD. password must be given with ',
     +           'R or W options'
         RETURN
      ENDIF
 
      IF(IOPTC.NE.0) THEN
         CHMAIL = 'XZIO :CD  '//PATH(1:LPATH) //' C'
      ELSEIF(IOPTR.NE.0) THEN
         CHMAIL = 'XZIO :CD  '//PATH(1:LPATH) // ' ' //
     +      CHPASS(1:LPASS)//' R'
      ELSEIF(IOPTW.NE.0) THEN
         CHMAIL = 'XZIO :CD  '//PATH(1:LPATH) // ' ' //
     +      CHPASS(1:LPASS)//' W'
      ELSE
         CHMAIL = 'XZIO :CD  '//PATH(1:LPATH)
      ENDIF
 
      CALL CZPUTA(CHMAIL,ISTAT)
#include "cspack/czmess.inc"
  99  END
 
