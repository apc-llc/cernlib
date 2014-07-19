*
* $Id: pslave.F,v 1.2 1996/09/10 09:20:43 dinofm Exp $
*
* $Log: pslave.F,v $
* Revision 1.2  1996/09/10 09:20:43  dinofm
* The set of configureded slaves can now be split in two halves, i.e. to
* allow to run PIAF only over 4 slaves out of the 8 configured.
* The choice of the first or second nodes subset depends on the master PID
* being odd or even.
*
* Revision 1.1.1.1  1996/03/01 11:39:27  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.06/03 04/11/94  15.43.36  by  Julian Bunn
*-- Author :    Fons Rademakers   11/08/93
      SUBROUTINE PSLAVE(NSLAV, IDIAL)
************************************************************************
*                                                                      *
* Go into parallel mode with possibly NSLAV slaves.                    *
* On output NSLAV is set to the actual number of active slaves (note   *
* that this is not the same as the number of running slaves).          *
* This routine reads the etc/piaf.conf file to see the available hosts *
* and tries to start a piafslave on each host (depending on NSLAV).    *
*                                                                      *
* If IDIAL=1 have a dialog with the client after startup of every      *
* slave.                                                               *
*                                                                      *
* For load balancing purposes we also could read the file              *
* etc/piaf.load which contains a list of nodes in order of increasing  *
* load.                                                                *
*                                                                      *
************************************************************************
*
#include "hbook/hcdire.inc"
#include "hbook/czsock.inc"
#include "hbook/hcpiaf.inc"
#include "paw/pchost.inc"
#include "paw/pcwk.inc"
*
      CHARACTER*80  CHFILE, LINE
      CHARACTER*16  CHPORT, CHCLUS, NODES(MXSLPF)
      INTEGER ISPLIT, ISPOFFS
*
*-- Trivial case is equivalent to sequential mode
*
      IF (NSLAV .EQ. 1) THEN
         NSLAV = 0
         RETURN
      ENDIF

      ISPLIT = 0
      ISPOFFS = 0
* 
*-- Send new initial state to already open slaves joining the group
*
      IF (NSLAV .GT. NSLAVE) THEN
         DO 5 I = NSLAVE+1, MIN(NSLAV, NSLVPF)
            CALL PFSOCK(I)
            CALL PSTATE(I, ISTAT)
    5    CONTINUE
         CALL PFSOCK(0)
      ENDIF
*
*-- Check if already enough slaves are running
*
      IF (NSLAV .LE. NSLVPF) RETURN
*
*-- We need more slaves
*
      NMORE = NSLAV - NSLVPF
*
*-- Open Piaf config file and read on which nodes slaves may be started
*
#if !defined(CERNLIB_VAX)
      CHFILE = CONDIR(1:LENOCC(CONDIR))//'/etc/piaf.conf'
      CALL PALUNF(1,3,ILUN)
      CALL KUOPEN(ILUN,CHFILE,'UNKNOWN',ISTAT)
#endif
#if defined(CERNLIB_VAX)
      chfile = condir(:lenocc(condir)) // ':piaf.conf'
      CALL PALUNF(1,3,ILUN)
      CALL KUOPEN(ILUN,CHFILE,'OLD',ISTAT)
#endif
      IF (ISTAT .NE. 0) GOTO 110
*
*-- We expect the lines to have the format 'slave <nodename>' (with one blank)
*
      NNODES = 0
   10 READ(ILUN, '(A)', END=20, ERR=110) LINE
*---- Do we have to split the set of nodes ?
      IF (LINE(1:5) .EQ. 'split') ISPLIT = NNODES

      IF (LINE(1:5) .EQ. 'slave') THEN
         NNODES = NNODES + 1
         NODES(NNODES) = LINE(7:)
      ENDIF
      GOTO 10
   20 CLOSE(ILUN)

*

*
*-- Remove the nodes that already run a slave from the list
*
      DO 50 I = 1, NSLVPF
         DO 40 J = 1, NNODES
            IF (SLHOPF(I) .EQ. NODES(J)) THEN
               DO 30 K = J+1, NNODES
                  NODES(K-1) = NODES(K)
   30          CONTINUE
               NNODES = NNODES - 1
               GOTO 50
            ENDIF
   40    CONTINUE
   50 CONTINUE
*
*-- Remove the bad nodes from the list
*
      DO 80 I = 1, NSLBAD
         DO 70 J = 1, NNODES
            IF (SLHBAD(I) .EQ. NODES(J)) THEN
               DO 60 K = J+1, NNODES
                  NODES(K-1) = NODES(K)
   60          CONTINUE
               NNODES = NNODES - 1
               GOTO 80
            ENDIF
   70    CONTINUE
   80 CONTINUE
*
*-- Now open, if possible, NMORE slaves
*
      NMORE = MIN(NMORE, NNODES)
      IF (NMORE .EQ. 0) GOTO 110
*
      IER = 0
      PRINT *, ' '
*
      CHPORT = 'piafslave'
      CHCLUS = 'piaf'
*
*-- If we try to start the whole bunch of slaves and the set of nodes has 
*-- to be split, start the first subset of nodes or the second
*-- depending on the master's PID being odd or even.
*-- Splitting is not even tried if we there are bad nodes or slaves already
*-- running.

      IF ((NMORE . EQ. NNODES).AND.(ISPLIT .GT. 0)) THEN
         IF (MOD(MASPID,2) .NE. 0) THEN
            NMORE = ISPLIT
         ELSE
            ISPOFFS = ISPLIT
            NMORE = NNODES - ISPLIT
         ENDIF
      ENDIF

      DO 100 I = 1, NMORE
*
         CALL PZOPEN(CHPORT,CHCLUS,NODES(I+ISPOFFS),CHVERS,PIAFRC,ISTAT)
         IF (ISTAT .NE. 0) GOTO 90
         CONNPF = .TRUE.
*
*-- Receive piafslave startup confirmation
*
         CALL CZGETA(CHSMPF,ISTAT)
         IF (ISTAT .NE. 0) GOTO 90
         L = LENOCC(CHSMPF)
         IF (CHSMPF(1:2) .EQ. 'OK') THEN
            LINE = ' '//CHSMPF(4:L)
         ELSE
            PRINT *, CHSMPF(1:L)
            ISTAT = 1
            GOTO 90
         ENDIF
*
*-- Send user account name, version id, workstation type, protocol level,
*-- master's pid and slave id
*
         WRITE(CHSMPF,'(2A16,4I12)') CHUSER, CHVERS, IWK, ISPROTO,
     +                               MASPID, NSLVPF+1
         CALL CZPUTA(CHSMPF,ISTAT)
         IF (ISTAT .NE. 0) GOTO 90
*
*-- Get motd message (no motd is send by the slaves, just get the empty log)
*
         CALL PFLOOP(ISTAT)
         IF (ISTAT .NE. 0) GOTO 90
*
         NCHTOP = NCHTOP + 1
         CHTOP(NCHTOP) = NODES(I)
         CALL CLTOU(CHTOP(NCHTOP))
         ICHTOP(NCHTOP) = ISKIN+10000*ISKOUT
         ICHLUN(NCHTOP) = 0
         HFNAME(NCHTOP) = 'Connection to '//NODES(I)
*
         NSLVPF = NSLVPF + 1
         SLHOPF(NSLVPF) = NODES(I)
         SLINPF(NSLVPF) = ISKIN
         SLUTPF(NSLVPF) = ISKOUT
*
*-- Transfer initial (i.e. current) state from master to slave server
*
         CALL PSTATE(NSLVPF, ISTAT)
         IF (ISTAT .NE. 0) THEN
            NCHTOP = NCHTOP - 1
            NSLVPF = NSLVPF - 1
            CALL CZCLOS(IST)
         ENDIF
*
*-- In case of error add host to bad host list
*
   90    IF (ISTAT .NE. 0) THEN
            IER = 1
            NSLBAD = NSLBAD + 1
            SLHBAD(NSLBAD) = NODES(I)
            LINE = '  **** Error starting slave server @ '//
     +             NODES(I)(1:LENOCC(NODES(I)))
         ENDIF
*
*-- Send result of startup for this slave to client
*
         CALL PFSOCK(0)
         IF (IDIAL .EQ. 1) THEN
            CALL CZPUTA(LINE, ISTAT)
         ELSE
            PRINT *, LINE(2:LENOCC(LINE))
         ENDIF
  100 CONTINUE
*
      IF (NSLVPF .EQ. 1) THEN
         PRINT *,' Could only create 1 slave server, will use',
     +           ' sequential mode'
         NCHTOP = NCHTOP - 1
         NSLVPF = NSLVPF - 1
         CALL PFSOCK(1)
         CALL CZCLOS(IST)
      ENDIF
*
      IF (IER.NE.0 .OR. IDIAL.EQ.0) PRINT *, ' '
*
*-- Reset ISKIN and ISKOUT to talk again from master to client
*
  110 CALL PFSOCK(0)
      NSLAV = MIN(NSLAV, NSLVPF)
*
      END
