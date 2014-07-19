*
* $Id: cdcdir.F,v 1.1.1.1 1996/02/28 16:24:43 mclareni Exp $
*
* $Log: cdcdir.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:43  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCDIR(CHPATH,CHOPT,IRC)
      CHARACTER*(*) CHPATH,CHOPT
#include "hepdb/cdcblk.inc"
#include "hepdb/cdkeys.inc"
#include "hepdb/ckkeys.inc"
      DATA NENTRY/0/
      SAVE NENTRY
 
      IF(NENTRY.EQ.0) THEN
         NSYSCD = NOFSCK
         NEXPCD = NOFECK
         NPAICD = NPARCD
         NENTRY = 1
      ENDIF
 
      CALL RZCDIR(CHPATH,CHOPT)
      IRC = IQUEST(1)
      IF(IRC.EQ.0) THEN
         NUSRCD = IQUEST(7) - NSYSCD - NEXPCD - NPAICD * 2
      ELSE
         NUSRCD = 0
      ENDIF
 
      END
