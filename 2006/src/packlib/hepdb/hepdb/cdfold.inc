*
* $Id: cdfold.inc,v 1.1.1.1 1996/02/28 16:24:03 mclareni Exp $
*
* $Log: cdfold.inc,v $
* Revision 1.1.1.1  1996/02/28 16:24:03  mclareni
* Hepdb, cdlib, etc
*
*
#ifndef CERNLIB_HEPDB_CDFOLD_INC
#define CERNLIB_HEPDB_CDFOLD_INC
*
*
* cdfold.inc
*
      IF(IOPTP.EQ.0) THEN
         CALL CUTOL(CHFILE(1:LFILE))
      ENDIF

#endif
