*
* $Id: datime.s,v 1.1.1.1 1996/02/15 17:53:42 mclareni Exp $
*
* $Log: datime.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:42  mclareni
* Kernlib
*
*
        TITLE DATIME
;#
; CERN PROGLIB# Z007    DATIME          .VERSION KERNPDP  1.02  761001
;#
        SEARCH HELP
#include "reentr.inc"
        INT=1
        TEMP=2
        RMDR=3
        T4==4
        HELLO(DATIME)
        DATE    TEMP,
        IDIVI   TEMP,^D31                       ;DAY
        MOVEI   INT,1(RMDR)
        IDIVI   TEMP,^D12                       ;MONTH
        IMULI   RMDR,^D100
        ADDI    INT,^D100(RMDR)
        ADDI    TEMP,^D64                       ;YEAR
        IMULI   TEMP,^D10000
        ADD     INT,TEMP
        MOVEM   INT,@(16)
        MSTIME  TEMP,                           ;TIME IN MILLI SECONDS
        IDIVI   TEMP,^D1000                     ;GET SECONDS
        IDIVI   TEMP,^D3600                     ;GET HOURS
        IDIVI   RMDR,^D60                       ;GET MINUTES
        IMULI   TEMP,^D100
        ADD     TEMP,RMDR                       ;ADD THE SECONDS
        MOVEM   TEMP,@1(16)
        GOODBY
        PRGEND
#ifdef CERNLIB_TCGEN_DATIME
#undef CERNLIB_TCGEN_DATIME
#endif
