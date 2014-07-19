*
* $Id: islbnk.F,v 1.10 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: islbnk.F,v $
* Revision 1.10  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE ISLBNK(QPART,QCAL,QLEP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Return flags indicating which banks to write
C-
C-   Outputs :
C-   QPART= value set by call to ISBKST
C-   QCAL =     "
C-   QLEP =     "
C-
C-   ENTRY ISBKST(QPART,QCAL,QLEP)
C-
C-   Created   7-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      LOGICAL QPART,QCAL,QLEP,QPS,QCS,QLS
      SAVE QPS,QCS,QLS
C----------------------------------------------------------------------
C
C
      QPART=QPS
      QCAL=QCS
      QLEP=QLS
      RETURN
C
      ENTRY ISBKST(QPART,QCAL,QLEP)
      QPS=QPART
      QCS=QCAL
      QLS=QLEP
  999 RETURN
C
      END
