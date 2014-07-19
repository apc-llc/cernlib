*
* $Id: cdcrdr.F,v 1.1.1.1 1996/02/28 16:24:47 mclareni Exp $
*
* $Log: cdcrdr.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:47  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCRDR (PATHN, NKEX, CHOPT, IRC)
*     ===========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDCRDR (PATHN, NKEX, CHOPT, IRC*)                       *
*                                                                      *
*   Routine to create DB Directory interactively                       *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDACTI, CDAUXI                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =161 : Illegal path name                               *
*                                                                      *
************************************************************************
*
#include "hepdb/ckkeys.inc"
      CHARACTER       PATHN*(*), CHOPT*(*)
      CHARACTER       CTAG(MXDMCK)*8, CHKTY*100, CHPRO*32, KEYTP*1
*
*     ------------------------------------------------------------------
*
* *** Check if it is a partitioned directory
*
      IOPTC  = INDEX (CHOPT, 'C')
      IOPTP  = INDEX (CHOPT, 'P')
      IF (IOPTP.NE.0) THEN
        CALL KUPROI ('Maximum number of objects in a partition ', MXKP)
        IF (MXKP.LT.1.OR.MXKP.GT.MXKYCK) MXKP = MXKPCK
      ELSE
        MXKP   = 0
      ENDIF
*
* *** Precisions for packing
*
      IPREC  = 0
      DELTA  = 0.
      IF (IOPTC.NE.0) THEN
        CALL KUPROI ('IPREC : Precision for packing ',  IPREC)
        CALL KUPROR ('DELTA : zero suppression limit ', DELTA)
      ENDIF
*
*  ** Get the Users Keys (Name and Type)
*
      CHKTY  = ' '
      DO 20 I = 1, NKEX
        WRITE (CHPRO, 1001) I
        CALL KUPROC (CHPRO, CTAG(I), NCT)
   10   CALL KUPROC ('Key-Type ? (I,H,B or A)', KEYTP, LKTP)
        IF (KEYTP.NE.'I'.AND.KEYTP.NE.'H'.AND.KEYTP.NE.'B'.AND.
     +      KEYTP.NE.'A')      GO TO 10
        CHKTY(I:I) = KEYTP
   20 CONTINUE
*
*  ** Create the Directory
*
      CALL CDMDIR (PATHN, NKEX,CHKTY,CTAG, MXKP, IPREC,DELTA, CHOPT,IRC)
*
 1001 FORMAT ('Key Name for user key ',I2,' : ')
*                                                             END CDCRDR
  999 END
