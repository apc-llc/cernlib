#include "geant321/pilot.h"
*CMZ :          23/02/96  14.35.00  by  John Apostolakis CERN GP-MIMD 2
*-- Author: John Apostolakis CERN  (GP-MIMD 2 through April 1995)
c     subroutine MUXREAD( lunread )
c--------------------------------------------------------------------
c     This routine is used to read, on one node, the data for the
c     primary particles of events and distribute this data to 
c     requesting nodes to work on.
c--------------------------------------------------------------------
#if defined(CERNLIB_PARA)
      subroutine MUXREAD( lunread )
      implicit none
      integer lunread
c     lunread = unit number of event input file (stream)
c--------------------------------------------------------------------
c     Reads in primary particle id and 3-momenta and distributes them.
c
c     Although current version uses one node only to read the data and
c     all the others to simulate them, future versions may use other
c     method. 
c
c     Author:  J.Apostolakis CERN/CN      
c     First version:  October 20, 1994
c     Last modified:  March 12, 1996
c
c     Called by:  gukine (if an event input file exists) 
c--------------------------------------------------------------------
c
#include "geant321/mpifinc.inc"
#include "geant321/multiprox.inc"
#include "geant321/gcflag.inc"
 
C   
C   These variables that will hold the data to be input (program dependent)
C
#include "momentid.inc"
c    The arrays will hold the data read in (leader) or received (worker).
 
c   Constants: tags to distinguish different communications
      integer  tagnparts, tag3mom, tagidpart, tgrequest
      parameter  (tagnparts=2001, tag3mom=2002, tagidpart=2003)
      parameter  (tgrequest=2010)
 
C   The two following "includes" are for termination like in grun.f
#include "geant321/gcunit.inc"
#include "geant321/gctime.inc"
      REAL TIMNOW
C   End of declarations for termination 
 
      INTEGER LUNERR
c
      integer  nodeto        !  current node to send to
c                               !  \/ MOVED to gprun.f \/
      integer  locleader        !  node that acts as 'host' here and
      data     locleader / 0 /  !   does all the file reading
 
      integer  itr, j, ndfrom, ndcount, i
      integer  ierr, npstat(10)
 
c     Used for call to gpkeepbg - see below.                   !  ONLYBG  
      integer timesleep
      data  timesleep / 150 /

c     As stderr is not defined, use Geant's stdout
      lunerr= lout

      if ( (npleader .lt. 0) .or. (npleader .gt. npsize) ) then
          npleader= locleader         
      endif

      IF(nprank.eq.npleader)THEN
 
c          This is the host node that read the data and distributes it! 
c          ---------------------------------------------------------------
c             The host waits for a work request, reads input data and 
c            replies to the node sending it an event to process.
c          ---------------------------------------------------------------
 
c          do{ ----------->  the host loops here over all the events
c     
  50        CONTINUE
            IF(IEVENT.LE.NEVENT) THEN
               
c              Receive a request from a "worker" for an event to process
c              (with procid as data).
c     
               call mpi_recv(nodeto, 1, MPI_INTEGER, MPI_ANY_SOURCE,
     &                          tgrequest, MPI_COMM_WORLD, npstat, ierr)
               if( ierr .ne. 0)
     &              call gpmsgerr( 'muxread - tag request host', 
     &              'mpi_recv', ierr )

c              Check to ensure that message was sent by node "nodeto"
               if( nodeto .ne. npstat(MPI_SOURCE) ) then
                  write( LUNERR, *) ' WARNING: nodeto ', nodeto,
     $                 ' differs from ',' value indicated in status - '
     $                 ,npstat(MPI_SOURCE), '*After Receive tagreq #1 '
               endif

c 	       Read an event's data: This segment depends on the program/data!
c              ---------------------
c
 
               READ(lunread,100,END=499) NTR
  100          FORMAT(I5)
c     x        ( NOTE: that an "END=lineno" condition is recommended 
c     x          here, so that if the end of the file is encountered it can be 
c     x          dealt with for the parallel program. Otherwise the host
c     x          will fail and the nodes may wait forever - a badly
c     x          behaved parallelprogram .      )

c              start of if( ntr > 0 )  ========
c
               if( ntr .gt. 0 ) then

c                  This send can be asynchronous.
                   call MPI_send( ntr, 1, MPI_INTEGER, nodeto,
     &                            tagnparts, MPI_COMM_WORLD, ierr)
	           if(ierr .ne. 0) call gpmsgerr('mpi_send', ierr,
     &                              'muxread- sending ntr' )

                   DO ITR=1,NTR
                       READ(lunread,110) (p1s(i,itr),i=1,3),itypes(itr)
                   ENDDO
  110              FORMAT(3F8.4,I2)
c     
c                  Send all the data, in two sends
c                  -----------------
c
                   call MPI_send( P1S, 3*ntr, MPI_REAL,     nodeto,  
     &                        tag3mom, MPI_COMM_WORLD,   ierr)
	           if(ierr .ne. 0) call gpmsgerr('mpi_send', ierr,
     &                              'muxread- sending 3-momenta' )
 
                   call MPI_send( itypes, ntr, MPI_INTEGER, nodeto,  
     &                        tagidpart, MPI_COMM_WORLD, ierr)
	           if(ierr .ne. 0) call gpmsgerr('mpi_send', ierr,
     &                              'muxread - sending particle id' )
c 
c     x            NOTE: As a variable number of primary particles 
c     x              constitute an event, and the data is of two 
c     x              different datatypes (floating point and integer)
c     x              either several sends (as here) or derived datatypes
c     x              or packing of the data into another buffer is needed.
               else
		   IEORUN = 1
               endif
c              end   of if( NTR > 0 )  ========

c              Next lines have been copied from standard grun.f, to
c                ensure the correct termination of the program, due to 
c                the end of elapsed time.
c
	       IF(IEORUN.EQ.0) THEN
c                 Incrementing ievent, as "host" does not exit this
c                 routine until all events are distributed.
 	          IEVENT=IEVENT+1
c
c	           Check time left
c
		  IF(ITIME.LE.0)GO TO 50
		  IF(MOD(IEVENT,ITIME).NE.0)GO TO 50
		  CALL TIMEL(TIMNOW)
		  IF(TIMNOW.GT.TIMEND)GO TO 50
		  WRITE(CHMAIL,10000)TIMEND
10000             FORMAT(5X,'***** THE JOB STOPS NOW BECAUSE THE TIME ',
     +                   'LEFT IS LESS THAN     ',F8.3,' SECONDS *****')
		  CALL GMAIL(0,2)
		  IEORUN = 1
	       ENDIF
c	       End  of  lines taken from standard grun.f ....
  	    ENDIF
c           end of  if( ievent > nevent )
c           while { }  ----------->  the host should loop "forever"...
 
  499       continue
c
c           The input file has been exhausted (used up). 
c           Send a termination (ntr=0) message to all nodes.
c
            ntr=0
	    IEORUN = 1
c
c     x     Inform the workers that all events have been assigned.
c     x     ------------------
c
c           Sending the goodbyes in processor order.
	    nfirstworker=1
	    do ndcount= nfirstworker, npsize-1
               call    MPI_send( ntr,    1, MPI_INTEGER, ndcount,
     &                          tagnparts, MPI_COMM_WORLD,    ierr)
	       write (lunerr,*)  ' Node ', npleader, ' sent a ',
     &              ' goodbye (number of parts ntr=0) to node ',ndcount
	    enddo
 
c     x     Now receive all the outstanding requests, so that no
c     x       messages are left hanging (one node has been rcv above).
	    ndfrom= nodeto
	    nfirstworker=1
	    do ndcount= nfirstworker+1, npsize-1
               call MPI_recv(ndfrom, 1, MPI_INTEGER, MPI_ANY_SOURCE,
     &                       tgrequest, MPI_COMM_WORLD, npstat, ierr)

               if( ndfrom .ne. npstat(MPI_SOURCE) ) then
                     write( LUNERR, *)  ' WARNING: MPI or parallel ', 
     &                 ' program error: ndfrom ', ndfrom, ' differs ',
     &                 ' from value indicated in status ', 
     &                 npstat(MPI_SOURCE), 
     &                 ' - Location: Receive tagreq #1 '
               endif
            enddo	
c
      ELSE
c
c     x    'Worker': ---------------------------------------------------
c     x    --------- Receive data and store it in arrays P1S and itypes
c

c     x    If you want this program to use only unused CPU cycles, you
c     x    can uncomment the line below. It makes certain that a node is
c     x    not loaded before asking for work. 
c     x      The subroutine gpkeepbg only return when the load is low enough.
c
c          call gpkeepbg( timesleep )                           !  ONLYBG  

c          Send a request for data to process,
c
	   call     MPI_send( nprank, 1, MPI_INTEGER, npleader,
     &                          tgrequest, MPI_COMM_WORLD,         ierr)
           if(ierr .ne. 0) call gpmsgerr('mpi_recv', ierr,
     &                          'muxread - requsting work' )
     
           call     MPI_recv( ntr,  1, MPI_INTEGER, npleader,
     &                          tagnparts, MPI_COMM_WORLD, npstat, ierr)
           if(ierr .ne. 0) call gpmsgerr('mpi_recv', ierr,
     &                          'muxread - receiving num-parts' )
 
c	   The case where NTR = 0 should be handled by the calling routine ??
c
           if( NTR .gt. 0 ) then
c              Receive input data arrays
               call MPI_recv( P1S,   3*ntr,   MPI_REAL,    npleader, 
     &                        tag3mom,   MPI_COMM_WORLD, npstat, ierr  )
               if(ierr .ne. 0) call gpmsgerr('mpi_recv', ierr,
     &                          'muxread - receiving plund ' )
 
               call MPI_recv( itypes,   ntr,  MPI_INTEGER, npleader, 
     &                        tagidpart, MPI_COMM_WORLD, npstat, ierr  )
               if(ierr .ne. 0) call gpmsgerr('mpi_recv', ierr,
     &                          'muxread - receiving klund ' )
           else
               write(LUNERR,*) ' Node ', nprank, ' got No-part ntr=0'
     $              ,' message and is finishing. '
	       IEORUN = 1
           endif
      ENDIF
 
      RETURN
      END
#endif
