*
* $Id: pntlog.F,v 1.1.1.1 1996/03/01 11:38:46 mclareni Exp $
*
* $Log: pntlog.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/15 08/03/94  12.15.19  by  Fons Rademakers
*-- Author :    Fons Rademakers   18/02/94
      SUBROUTINE PNTLOG(IACT, IVAL, RVAL)
************************************************************************
*                                                                      *
* Handles the statistics collection of the Ntuple commands.            *
* Depending on IACT the logging is initialized or values (integer or   *
* real) are set or the logging is terminated.                          *
*                                                                      *
* IACT = 0    begin (called by slaves or master in seq mode)           *
* IACT = 1    final collect                                            *
* IACT = 2    end (put in iquest for export to master)                 *
* IACT = 3    ival = number of events                                  *
* IACT = 4    ival = number of columns not in cache                    *
* IACT = 5    initialize counters to zero (called by master)           *
* IACT = 6    add partial results from slaves (called by master)       *
* IACT = 7    ival = total number of columns in Ntuple                 *
* IACT = 8    ival = 1 chain, else no chain                            *
* IACT = 9    ival = Ntuple id                                         *
* IACT = 10   ival = selection function type, =1 comis, =2 f77, =3 C   *
* IACT = 11   ival = number of columns in query                        *
************************************************************************
*
#include "paw/pntold.inc"
#include "paw/quest.inc"
#include "paw/rzcxio.inc"
*
*-- Log info.
*
*-- Ntuple type, 0=RWN, 1=CWN, number of columns and rows used in query,
*-- number of cache hits
      INTEGER        NTYPE, NOC, NOR, NHITS, NOCT, NCHAIN, ID, ISEL,
     +               NMISS
*-- Real and virtual I/O, CP time
      REAL           ORIO, RIO, OVIO, VIO, CPTIME
*
      SAVE
*
      IF (IACT .EQ. 0) THEN
         NTYPE = -1
         ISEL  = 0
         CALL HBVM(OVIO)
         ORIO = RZXIN + RZXOUT
      ELSEIF (IACT .EQ. 1) THEN
         IF (NTOLD) THEN
            NTYPE = 0
         ELSE
            NTYPE = 1
         ENDIF
         NHITS  = NOC - NMISS
         IF (NHITS .LT. 0) NHITS = 0
         CALL HBVM(VIO)
         VIO    = (VIO - OVIO) * 4.E-6
         RIO    = (RZXIN+RZXOUT - ORIO) * 4.E-6
         CPTIME = 0.0
      ELSEIF (IACT .EQ. 2) THEN
         IQUEST(87) = ISEL
         IQUEST(88) = ID
         IQUEST(89) = NOCT
         IQUEST(90) = NTYPE
         IQUEST(91) = NOC
         IQUEST(92) = NOR
         IQUEST(93) = NHITS
         RQUEST(95) = RIO
         RQUEST(96) = VIO
         RQUEST(97) = CPTIME
* 98 = real time
         IQUEST(99) = NCHAIN
      ELSEIF (IACT .EQ. 3) THEN
         NOR = IVAL
      ELSEIF (IACT .EQ. 4) THEN
         NMISS = IVAL
      ELSEIF (IACT .EQ. 5) THEN
         NOR    = 0
         NMISS  = 0
         RIO    = 0.0
         VIO    = 0.0
         CPTIME = 0.0
      ELSEIF (IACT .EQ. 6) THEN
         ISEL   = IQUEST(87)
         ID     = IQUEST(88)
         NOCT   = IQUEST(89)
         NTYPE  = IQUEST(90)
         NOC    = IQUEST(91)
         NOR    = NOR + IQUEST(92)
         NHITS  = IQUEST(93)
         RIO    = RIO + RQUEST(95)
         VIO    = VIO + RQUEST(96)
         CPTIME = CPTIME + RQUEST(97)
* 98 = real time
         NCHAIN = IQUEST(99)
      ELSEIF (IACT .EQ. 7) THEN
         NOCT = IVAL
      ELSEIF (IACT .EQ. 8) THEN
         NCHAIN = IVAL
      ELSEIF (IACT .EQ. 9) THEN
         ID = IVAL
      ELSEIF (IACT .EQ. 10) THEN
         ISEL = IVAL
      ELSEIF (IACT .EQ. 11) THEN
         NOC = IVAL
      ENDIF
*
      END
