*
* $Id: gfinds.F,v 1.2 1999/01/14 15:30:30 japost Exp $
*
* $Log: gfinds.F,v $
* Revision 1.2  1999/01/14 15:30:30  japost
*   Fixed the problem described below in the exact manner given:
*
* Date: Wed, 21 Oct 1998 11:55:47 EDT
* From: "Shawn McKee - (734) 764-4395" <mckee@pooh.physics.lsa.umich.edu>
* Reply-To: cern-heplib@listbox1.cern.ch
* To: cern-heplib@listbox1.cern.ch
* Subject: Problem with using large memory size in GEANT
*
*   I think I have found a problem in the the gfinds routine in the GTRAK
*   area of GEANT.  When trying to use a ZEBRA memory space larger than
*   16777216 WORDS (4-byte words) my GEANT code would not work correctly.
*   The problem is illustrated with the following FORTRAN code.  The specific
*   point at which things fail is dependent on the machine specific
*   implementation of REAL*4.
*
*   The basic problem was that "pointers" to memory in ZEBRA should always
*   use INTEGER*4 values to prevent losing precision.  Adding 1 to a large
*   real number and taking the integer value is not the same as taking the
*   integer value of the real and adding 1!  All memory pointers I found
*   correctly used integers, except a code fragment in gfinds.F inside the
*   GTRAK area.  The faulty line was:
*
*          JAT = JFATTR(JVO)
*
*   where JFATTR was an inline function:
*
*      JATTF(JV) = JV + Q(JV+5) + 6
*
*   The problem is that the right-hand-side(RHS) of the inline function is first
*   "promoted" to REAL*4, added together and then converted back to an integer.
*   This causes an error when the RHS is larger than 16777216 on OpenVMS.
*
*   The fix is to simply encase the Q() inside INT():
*
*      JATTF(JV) = JV + INT(Q(JV+5)) + 6
*
*   Can this fix be added to the current gfinds.F source code?  Thank-you!
*
*       Shawn McKee
*                                 University of Michigan
*                                 smckee@umich.edu
*
* ------------------------------------------------------------------------
*
*   An earlier enquiry about this pinpointed the problem, but was missed
*
* > From: Bogdan Pawlik <bogdan@FNAL.GOV>
* > Subject: GEANT-3.21.04 : a bug in GFINDS
* > Date: Thursday, April 10, 1997 8:39 PM
* >
* > Dear Sir,
* >   I was having troubles running GEANT with very large
* >   memory ( 20 Mb ) , I was experiencing random crashes.
* >   What I found eventually when debugging problem, was
* >   that cause of this lies in GFINDS where in-line function:
* >
* >         JATTF(JV) = JV + Q(JV+5) + 6
* >   is used to get pointer in JSET structure.
* >    The problem with this instruction is that it is
* >   mixing types i.e.  int = int +float  . This
* >   procedure fails  on SGI IRIX5.3 and also
* >   at OVMS (DEC-APLHA)  for integers above 16777215.
* >   The result is as follow :
* >             int       float   int
* >     SGI    16777216 + 1.0 = 16777216
* >   ALPHA    16777216 + 1.0 = 16777218
* >
* >    instead 16777217 .
* >   The results of this bug are unpredictable ....
* >   from unreliable tracking to crashes.
* >
* >    Running Geant with really large memory becomes
* >   more and more often this days so I think fixing
* >   this problem in GFINDS and possibly in other
* >   places  is important.
*
* Revision 1.1.1.1  1995/10/24 10:21:41  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 06/07/94  18.26.03  by  S.Giani
*-- Author :
      SUBROUTINE GFINDS
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Returns the set/volume parameters corresponding to       *
C.    *       the current space point in /GCTRAK/                      *
C.    *       and fill common /GCSETS/                                 *
C.    *                                                                *
C.    *       IHSET  user set identifier                               *
C.    *       IHDET  user detector identifier                          *
C.    *       ISET set number in JSET                                  *
C.    *       IDET   detector number in JS=LQ(JSET-ISET)               *
C.    *       IDTYPE detector type (1,2)                               *
C.    *       NUMBV  detector volume numbers (array of length NVNAME)  *
C.    *       NVNAME number of volume levels                           *
C.    *                                                                *
C.    *    ==>Called by : GTRACK                                       *
C.    *       Author    R.Brun  *********                              *
C.    *       Modified  V.Perev                                        *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gcsets.inc"
#include "geant321/gcvolu.inc"
#include "geant321/gctmed.inc"
#if defined(CERNLIB_DEBUG)
      INTEGER        LNAM(15), LNUM(15)
#endif
      JATTF(JV) = JV + INT(Q(JV+5)) + 6
C.
C.    ------------------------------------------------------------------
C.
*
#if defined(CERNLIB_DEBUG)
      WRITE(CHMAIL,1000)NLEVEL
      CALL GMAIL (0, 0)
      DO 5 I    = 1,NLEVEL
        WRITE(CHMAIL,1001)NAMES(I),NUMBER(I),LVOLUM(I),LINDEX(I)
        CALL GMAIL (0, 0)
        WRITE(CHMAIL,1002)(GTRAN(J,I),J = 1,3),(GRMAT(J,I),J=1,10)
        CALL GMAIL (0, 0)
    5   CONTINUE
 1000 FORMAT (' DEBUG : GFINDS =',I3)
 1001 FORMAT (5(1X,A4,3I3))
 1002 FORMAT (1X,13F9.4)
      NLEV = NLEVEL
      CALL UCOPY (NAMES (1),LNAM(1),NLEV)
      CALL UCOPY (NUMBER(1),LNUM(1),NLEV)
      NLEVEL    = 0
      CALL GLVOLU (NLEV, LNAM, LNUM, IER)
      IF (IER.NE.0)             STOP
#endif
*
      IHSET = 0
      IHDET = 0
      ISET  = 0
      IDET  = 0
      IDTYPE = 0
      NVNAME = 0
*
      DO 10 NLEV = NLEVEL,1,-1
         JVO = LQ(JVOLUM-LVOLUM(NLEV))
         JAT = JATTF(JVO)
         IDET = Q(JAT+8)
         IF(IDET.NE.0) THEN
            NL = NLEV
            GO TO 15
         ENDIF
  10  CONTINUE
      GOTO 99
  15  ISET   = Q(JAT+7)
      IDTYPE = Q(JAT+9)
      IHSET  = IQ(JSET+ISET)
      JS     = LQ(JSET-ISET)
      IHDET  = IQ(JS+IDET)
      JD     = LQ(JS-IDET)
      NVNAME = IQ(JD+2)
      DO 40 I=1,NVNAME
            NAME=IQ(JD+2*I+9)
            NUMBV(I)=0
            DO 30 J=1,NLEVEL
               IF(NAMES(J).EQ.NAME)THEN
                  NUMBV(I)=NUMBER(J)
                  GO TO 40
               ENDIF
  30        CONTINUE
  40  CONTINUE
C
   99 END
