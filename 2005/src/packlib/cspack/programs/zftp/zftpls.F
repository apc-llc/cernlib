*
* $Id: zftpls.F,v 1.1.1.1 1996/03/08 15:44:19 mclareni Exp $
*
* $Log: zftpls.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:19  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE ZFTPLS
#include "cspack/czsock.inc"
#include "cspack/hcmail.inc"
#include "cspack/quest.inc"
      CHARACTER*80 PATH
      CHARACTER*4  CHOPT
*
#include "cspack/czopen.inc"
      CALL KUGETS(PATH,LPATH)
 
      CALL KUGETC(CHOPT,NCH)
 
      IF(LPATH.EQ.0) THEN
         PATH  = ' '
         LPATH = 1
      ENDIF
 
      IF(NCH.EQ.0) THEN
         CHOPT = ' '
         NCH   = 1
      ENDIF
 
      IF(INDEX(CHOPT,'C').EQ.0) CALL CLTOU(PATH)
*
      CHMAIL = 'XZIO :LS  '//PATH(1:LPATH) // '(' // CHOPT(1:NCH) // ')'
 
      CALL CZPUTA(CHMAIL,ISTAT)
#include "cspack/czmess.inc"
  99  END
 
