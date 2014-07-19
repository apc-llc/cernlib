*
* $Id: pminit.F,v 1.2 1996/04/04 09:09:32 couet Exp $
*
* $Log: pminit.F,v $
* Revision 1.2  1996/04/04 09:09:32  couet
* The calls to KUTERM and KUGRFL have been moved into PAWINT3, after
* the IGINIT call (HIGZ initialisation).
*
* Revision 1.1.1.1  1996/03/01 11:39:06  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/08 16/08/95  15.34.10  by  O.Couet
*-- Author :    Fons Rademakers   09/05/93
      SUBROUTINE PMINIT
************************************************************************
*                                                                      *
*  Motif specific initialization routine called from PAWINTM.          *
*                                                                      *
************************************************************************
*
#include "paw/pcaddr.inc"
#include "paw/fpadr.inc"
*
      EXTERNAL      PAMLOC, PMHIST
      EXTERNAL      MATLAB, MATROW, SHOW_CHAIN, NTWINI, NTWCHK
      EXTERNAL      FPHSTART, FPVSTART
*
*           Initialize KUIP
*
      CALL KUMLOC(PAMLOC)
*
*           Create Motif specific command structure from definition file
*
      CALL KUIDFM
      CALL ZBRDEF
      CALL PAMDEF
*
*           Initialize jump addresses for Motif only routines
*
      JMLAB   = JUMPAD(MATLAB)
      JMROW   = JUMPAD(MATROW)
      JMCHAIN = JUMPAD(SHOW_CHAIN)
      JNTWINI = JUMPAD(NTWINI)
      JNTWCHK = JUMPAD(NTWCHK)
      JMHIST  = JUMPAD(PMHIST)
      JMFPH   = JUMPAD(FPHSTART)
      JMFPV   = JUMPAD(FPVSTART)
*
      END
