*
* $Id: gukine.F,v 1.4 1996/03/22 15:27:18 japost Exp $
*
* $Log: gukine.F,v $
* Revision 1.4  1996/03/22 15:27:18  japost
* Minor bug correction in a write statement.
*
* Revision 1.3  1996/03/15 15:50:05  japost
* Small corrections to Parallel code
*
* Revision 1.2  1996/03/13 17:30:13  ravndal
* Modifications for parallel version testing included
*
c Revision 1.1.1.1  95/10/24  10:22:13  cernlib
c Geant
c 
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE GUKINE
C.
C.    ******************************************************************
C.    *                                                                *
C.    *                                                                *
C.    *       Read or Generates Kinematics for primary tracks          *
C.    *                                                                *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gclist.inc"
#include "geant321/gckine.inc"
#include "geant321/gconsp.inc"
      CHARACTER*4 CHGET(20)
      DIMENSION XVERT(3)
      DIMENSION P1(3)
      DIMENSION RNDM(3)
      SAVE XVERT
C
      DATA XVERT/0.,0.,-100./
#if defined(CERNLIB_PARA)
#include "geant321/gcflag.inc"
#include "geant321/gcunit.inc"
#include "momentid.inc"
      integer  nsize, nrank, nleader 
*----   The types of local variables  ---  GKTYPES
      REAL      XVERT, P1, RNDM, pmom, th, ph
      INTEGER   i, ident, ier, ilook, nvtx, itr, nt1
      INTEGER   itype, imax, itry
#endif
C.
C.    ------------------------------------------------------------------
C.
      IF(NGET.LE.0)GO TO 10
      ITRY=0
  5   ITRY=ITRY+1
      IF(ITRY.GT.2)GO TO 10
      DO 6 I=1,NGET
         CALL UHTOC(LGET(I),4,CHGET(I),4)
   6  CONTINUE
      CALL GFIN(1,CHGET,NGET,IDENT,'K',IER)
      IF(IER.LT.0)GO TO 5
      CALL GLOOK('KINE',LGET,NGET,ILOOK)
      IF(ILOOK.NE.0)GO TO 99
C
 10   CALL GSVERT(XVERT,0,0,0,0,NVTX)
C
      IF(IKINE.EQ.0)THEN
#if !defined(CERNLIB_PARA)
           READ(4,100)NTR
  100      FORMAT(I5)
           DO 200 ITR=1,NTR
           READ(4,110)P1,ITYPE
  110      FORMAT(3F8.4,I2)
           CALL GSKINE(P1,ITYPE,NVTX,0,0,NT1)
#endif
#if  defined(CERNLIB_PARA)
c
c          Muxread reads a different event for each node.
c          The routine returns number of primary particles in the 
c          event "ntr", and for each of these particles sets the 
c          particle type "itypes(1:ntr)" and momenta "p1s(1:3,1:ntr)" 
c
           call muxread( 4 )      ! read from unit number 4
c                                
           DO  itr=1,ntr
               CALL GSKINE(p1s(1,itr),itypes(itr),NVTX,0,0,NT1)
           enddo
           if( ntr.eq.0) then
	       call gprocs( nsize, nrank, nleader )
               write(chmail,'(A,I4,A,A,A)') 'Node ', nrank, ' has ',
     &           'received an event with no primary particles (ntr=0)',
     &           '. This signals the end of the run for this node.'
               call gmail(1,1)
               ieorun = 1
           endif
#endif
  200      CONTINUE
      ENDIF
C
      IF(IKINE.EQ.1)THEN
           IF(PKINE(1).EQ.0.)PKINE(1)=25.
           IF(PKINE(2).EQ.0.)PKINE(2)=65.
           IF(PKINE(3).EQ.0.)PKINE(3)=-5.
           IF(PKINE(4).EQ.0.)PKINE(4)= 5.
           IF(PKINE(5).EQ.0.)PKINE(5)=  .2
           IF(PKINE(6).EQ.0.)PKINE(6)=  .8
           IF(PKINE(7).EQ.0.)PKINE(7)= 1.
           IMAX=INT(PKINE(7))
           IF(PKINE(8).EQ.0.)PKINE(8)=14.
           ITYPE=INT(PKINE(8))
           CALL GRNDM(RNDM,3)
           DO 300 I=1,IMAX
           TH=RNDM(1)*(PKINE(2)-PKINE(1))+PKINE(1)
           PH=RNDM(2)*(PKINE(4)-PKINE(3))+PKINE(3)
           PMOM=RNDM(3)*(PKINE(6)-PKINE(5))+PKINE(5)
           P1(1)=PMOM*SIN(TH*DEGRAD)*COS(PH*DEGRAD)
           P1(2)=PMOM*SIN(TH*DEGRAD)*SIN(PH*DEGRAD)
           P1(3)=PMOM*COS(TH*DEGRAD)
           CALL GSKINE(P1,ITYPE,NVTX,0,0,NT1)
  300      CONTINUE
      ENDIF
C
  99  RETURN
      END
