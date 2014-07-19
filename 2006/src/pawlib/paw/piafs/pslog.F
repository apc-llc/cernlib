*
* $Id: pslog.F,v 1.1.1.1 1996/03/01 11:39:27 mclareni Exp $
*
* $Log: pslog.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:27  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/14 02/03/94  10.32.03  by  Fons Rademakers
*-- Author :    Fons Rademakers   18/02/94
      SUBROUTINE PSLOG
************************************************************************
*                                                                      *
* Write Ntuple command execution log to syslog.                        *
*                                                                      *
************************************************************************
*
#include "hbook/hcpiaf.inc"
#include "paw/quest.inc"
*
      CALL PNTLOG(2, 0, 0.)
      CALL PSTIME(2, 0, 0, A, RQUEST(98))
      IF (NSLAVE .EQ. 0) THEN
         RQUEST(97) = A
      ELSE
         RQUEST(97) = RQUEST(97) + A
      ENDIF
*
      CALL PSELOG(IQUEST(90), IQUEST(91), IQUEST(92), IQUEST(93),
     +            RQUEST(95), RQUEST(96), RQUEST(97), RQUEST(98),
     +            IQUEST(88), IQUEST(89), IQUEST(99), IQUEST(87))
*
      END
