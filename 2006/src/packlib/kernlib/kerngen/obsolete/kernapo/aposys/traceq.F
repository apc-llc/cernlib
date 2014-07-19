*
* $Id: traceq.F,v 1.1.1.1 1996/02/15 17:51:09 mclareni Exp $
*
* $Log: traceq.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:09  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE TRACEQ (LUN, NLEV)
C
C CERN PROGLIB# N105    TRACEQ          .VERSION KERNAPO  1.17  890626
C ORIG. 19/05/89  R. Wilhelm NIKHEF-H
C
C      Fork a new process to do a traceback on the current process.
C
C           LUN   -  Logical unit number of the print file
C                    0 = standard print file (unit 6)
C           NLEV  -  Maximum number of trace-back levels to print
C
%include '/sys/ins/pgm.ins.ftn'
%include '/sys/ins/base.ins.ftn'

      INTEGER*4 ADDRESS(2), OUTSTR, HANDLE
      INTEGER*2 ARGCOUNT,NSTREAM,CONVEC(4),NAME(2),ARG(25)
      INTEGER*2 MODE

      CHARACTER    CHPID*20, CHEDT*26
      EQUIVALENCE (CHPID,ARG(3))
      EQUIVALENCE (CHEDT,ARG(13))

      INTEGER GETPID

C--       Handle Logical output unit

      IF (LUN.EQ.0) THEN
        LOUT = 6
      ELSE
        LOUT = LUN
      ENDIF

      WRITE (LOUT, '(/,A,/)') '***** TRACEQ:  In-Line Trace-Back  *****'

      NAME(1) = 2
      NAME(2) = 'sh'
      ARG(1) = 48
      ARG(2) = 'tb'

C--       Get current UNIX process number

      IPID = GETPID()
      WRITE(CHPID,'(A1,I19)') ' ', IPID

C--       The first 5 routine names are system routines which
C--       the user should not care about. At maximum NLEV lines
C--       of traceback information are printed.

      ILAST = NLEV + 5
      IF (ILAST.LT.999) THEN
         WRITE(CHEDT, '(A,I3,A)') ' | edstr -e 1,5d -e ', ILAST, ',$d'
      ELSE
         CHEDT = ' | edstr -e 1,5d'
      ENDIF

C--       Get the STREAM identifier of FORTRAN unit LOUT

      INQUIRE(LOUT, STRID=OUTSTR, IOSTAT=IST)
      IF (IST.NE.STATUS_$OK) THEN
        OUTSTR = STREAM_$STDOUT
      ENDIF

C--       Set up the streams to be passed to the new process:
C--       Standard input, output; Error input, output.
C--       Note that we cannot use parameters for the last two
C--       differences between the SR9.7 and SR10 insert files.

      NSTREAM  = 4
      CONVEC(1) = STREAM_$STDIN
      CONVEC(2) = OUTSTR
      CONVEC(3) = 2
      CONVEC(4) = 3

      ARGCOUNT = 2
      ADDRESS(1) = IADDR(NAME)
      ADDRESS(2) = IADDR(ARG)

C--       Invoke command: /com/sh tb unix_pid | edstr -e 1,5d

      MODE = 0
      CALL PGM_$INVOKE('/com/sh', INT2(7), ARGCOUNT, ADDRESS,
     +                  NSTREAM, CONVEC, MODE, HANDLE, IST)
      CALL PGM_$PROC_WAIT(HANDLE, IST)
      RETURN
      END
#ifdef CERNLIB_CCGEN_TRACEQ
#undef CERNLIB_CCGEN_TRACEQ
#endif
#ifdef CERNLIB_TCGEN_TRACEQ
#undef CERNLIB_TCGEN_TRACEQ
#endif
