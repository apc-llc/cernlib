*
* $Id: cdcrfd.F,v 1.1.1.1 1996/02/28 16:23:45 mclareni Exp $
*
* $Log: cdcrfd.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:45  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCRFD
*     =================
*
************************************************************************
*                                                                      *
*        SUBR. CDCRFD                                                  *
*                                                                      *
*   Creates a the directory structure of a fictitious data base        *
*                                                                      *
*   Called by CEXAM03                                                  *
*                                                                      *
************************************************************************
*
      CHARACTER       CRATF*50, CRATT(50)*8
      DATA            NKEX  /50/
      DATA CRATF/'IIIHHHHHHHHHHHHHHHHHHHHBBBBBBBBBBBBBBBBBBBBIIIIIII'/
      DATA CRATT/'UINTEG01','UINTEG02','UINTEG03',
     +           'UHOLL_01','UHOLL_02','UHOLL_03','UHOLL_04','UHOLL_05',
     +           'UHOLL_06','UHOLL_07','UHOLL_08','UHOLL_09','UHOLL_10',
     +           'UHOLL_11','UHOLL_12','UHOLL_13','UHOLL_44','UHOLL_15',
     +           'UHOLL_16','UHOLL_17','UHOLL_18','UHOLL_19','UHOLL_20',
     +           'UBINR_01','UBINR_02','UBINR_03','UBINR_04','UBINR_05',
     +           'UBINR_06','UBINR_07','UBINR_08','UBINR_09','UBINR_10',
     +           'UBINR_11','UBINR_12','UBINR_13','UBINR_44','UBINR_15',
     +           'UBINR_16','UBINR_17','UBINR_18','UBINR_19','UBINR_20',
     +           'UINTEG04','UINTEG05','UINTEG06','UINTEG07','UINTEG08',
     +           'UINTEG09','UINTEG10'/
*
*     ------------------------------------------------------------------
*
      IPREC  = 0
      DELTA  = 0.
      CALL CDMDIR ('//DBL3/FAKE/CRATES', NKEX, CRATF, CRATT, 0,
     +             IPREC, DELTA, 'C', IRC)
*                                                             END CDCRFD
      END
