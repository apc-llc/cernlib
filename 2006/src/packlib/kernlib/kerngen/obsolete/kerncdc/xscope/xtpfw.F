*
* $Id: xtpfw.F,v 1.1.1.1 1996/02/15 17:51:18 mclareni Exp $
*
* $Log: xtpfw.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:18  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XTPFW (LUN,NFILE)
 
C CERN PROGLIB# Z200    XTPFW           .VERSION KERNCDC  0.1   750926
 
      LOGICAL XEOF,XRDIN
      DATA (MAXRED =5)
 
      NF=MAX0 (NFILE,1)
 
   10 NRED=0
   15 READ (LUN) X
      IF (XEOF (LUN)) GO TO 50
      IF (XRDIN (LUN)) GO TO 20
      GO TO 10
 
C--                PARITIES
   20 NRED=NRED+1
      IF (NRED.NE.MAXRED) GO TO 15
      CALL REMARK (40HXTPFW DOES NOT WORK FOR BCD FILES, EXIT.)
      CALL EXIT
 
C--                ONE E.O.F HAS BEEN CROSSED
   50 NF=NF-1
      IF (XRDIN (LUN)) CONTINUE
      IF (NF.NE.0) GO TO 10
      RETURN
      END
