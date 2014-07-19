*
* $Id: pstate.F,v 1.1.1.1 1996/03/01 11:39:28 mclareni Exp $
*
* $Log: pstate.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:28  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/07 29/11/93  15.59.49  by  Fons Rademakers
*-- Author :    Fons Rademakers   12/08/93
      SUBROUTINE PSTATE(ISLAV, ISTAT)
************************************************************************
*                                                                      *
* Transfer the initial (i.e. current) state of the master to a slave.  *
* The initial state includes:                                          *
*      - the log level                                                 *
*      - the currently open files                                      *
*      - the current chain definition                                  *
*      - the current working directory                                 *
*                                                                      *
* Later it should also contain the currently defined cuts and whatever *
* I did forget at the moment.                                          *
*                                                                      *
************************************************************************
*
#include "hbook/hcdire.inc"
#include "hbook/hcpiaf.inc"
*
      ISTAT = 0
*
*-- Send the current log level
*
      WRITE(CHSMPF,'(A,I12)') 'LOGLEV', LOGLPF
      CALL CZPUTA(CHSMPF,ISTAT)
*
*-- Send the HCOPT words
*
      CALL HOPTPF
*
*-- Loop over all connected files and send them to the slave server
*
      CALL PFKUPN('fortran/close 0', ISLAV, ISTAT)
      DO 10 I = 1, NCHTOP
         IF (ICHTOP(I).GT.0 .AND. ICHTOP(I).LT.1000 .AND.
     +                            HFNAME(I)(1:6) .NE. 'Chain ') THEN
            WRITE(CHLMPF,'(A,I4,1X,A,1X,I8)') 'histo/file', ICHTOP(I),
     +                             HFNAME(I)(1:LENOCC(HFNAME(I))), 0
            CALL PFKUPN(CHLMPF, ISLAV, ISTAT)
            IF (ISTAT .NE. 0) GOTO 90
         ENDIF
10    CONTINUE
*
*-- If a chain has been defined send the chain definition
*
      CALL PCHSND(ISLAV, ISTAT)
      IF (ISTAT .NE. 0) GOTO 90
*
*-- Place slave in same CWD as master server
*
      CALL HCDIR(CHSMPF,'R')
      CHLMPF = 'cd '//CHSMPF(1:LENOCC(CHSMPF))
      CALL PFKUPN(CHLMPF, ISLAV, ISTAT)
*
90    RETURN
*
      END
