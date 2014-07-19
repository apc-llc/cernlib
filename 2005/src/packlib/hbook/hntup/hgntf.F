*
* $Id: hgntf.F,v 1.3 1999/03/05 15:42:35 couet Exp $
*
* $Log: hgntf.F,v $
* Revision 1.3  1999/03/05 15:42:35  couet
* - Bug fixed to handle properly the word swapping of REAL*8 machines on little
* endian machines
*
* Revision 1.2  1998/09/25 09:28:40  mclareni
* Modifications for the Mklinux port flagged by CERNLIB_PPC
*
* Revision 1.1.1.1  1996/01/16 17:07:57  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/14 06/10/94  23.19.37  by  Fons Rademakers
*-- Author :    Fons Rademakers   29/01/92
      SUBROUTINE HGNTF(IDD,IDNEVT,IERROR)
*.==========>
*.
*.           Return in the preset addresses (set by HBNAME)
*.           the values of the variables as used in the last call
*.           to HGNT, HGNTB, or HGNTV for the event (row) IDNEVT.
*.
*.  This routine does the actual reading of the variables in LTMP1.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcnt.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hntcur.inc"
#include "hbook/hcrecv.inc"
*
      INTEGER       ILOGIC
      LOGICAL       LOGIC, INDVAR, USEBUF
      EQUIVALENCE  (LOGIC, ILOGIC)
*
#include "hbook/jbyt.inc"
*
      IERROR = 0
*
*-- Minimal test for maximum speed. Just set the pointers to the
*-- buffer and tmp structures. The N-tuple header must already be in memory.
*-- No reading from disk will be done.
*
      IF (NTCUR.NE.IDD .OR. NTCUR.EQ.0) THEN
         CALL HNBUFF(IDD, .TRUE.)
         IF (IERR .NE. 0) THEN
            CALL HBUG('Cannot find Ntuple buffer structure',
     +                'HGNTF',IDD)
            GOTO 50
         ENDIF
         IF (IQ(LTMP1+1) .EQ. 0) THEN
            CALL HBUG('No variables selected for reading',
     +                'HGNTF',IDD)
            GOTO 50
         ENDIF
         NTCUR = IDD
      ENDIF
*
      IDLAST = IDD
*
      LCID = LQ(LTMP-2)
*
      LINT  = LQ(LCID-3)
      LREAL = LQ(LCID-4)
*
      NVAR = IQ(LTMP1+1)
*
      DO 40 I = 1, NVAR
*
         JTMP   = ZNTMP1*(I-1) + 2
         INDX   = IQ(LTMP1+JTMP)
         IOFF   = IQ(LTMP1+JTMP+1)
         IOFFST = IQ(LTMP1+JTMP+2)
         IF (IOFFST .EQ. 0) THEN
            USEBUF = .FALSE.
         ELSE
            USEBUF = .TRUE.
         ENDIF
         LBLOK  = LQ(LTMP1-I)
         LNAME  = LQ(LBLOK-1)
*
         NSUB  = JBYT(IQ(LNAME+IOFF+ZDESC), 18, 3)
         ITYPE = JBYT(IQ(LNAME+IOFF+ZDESC), 14, 4)
         ISIZE = JBYT(IQ(LNAME+IOFF+ZDESC), 8,  6)
         NBITS = JBYT(IQ(LNAME+IOFF+ZDESC), 1,  7)
*
         INDVAR = .FALSE.
         IF (JBIT(IQ(LNAME+IOFF+ZDESC),28) .EQ. 1) INDVAR = .TRUE.
*
         IF (.NOT.NRECOV .AND. IQ(LNAME+IOFF+ZNADDR).EQ.0) GOTO 40
*
*-- fix the NBITS for character variables (7 bits was not enough)
*
         IF (ITYPE .EQ. 5) THEN
            NBITS = IBIPB*ISIZE
*
*-- in case called from PAW destination is character*32 array and not the
*-- natural size (design mistake, but to late to make clean fix)
*
            MXBY  = ISHFT(ISIZE,-2)
            MXBY1 = MXBY
            IF (JBIT(IQ(LQ(LCID-1)),3) .NE. 0) MXBY1 = 8
         ENDIF
*
         IF (IQ(LNAME+IOFF+ZITMP) .EQ. 0) THEN
            IQ(LNAME+IOFF+ZITMP) = IQ(LCID+ZIFTMP)
            IQ(LCID+ZIFTMP) = IQ(LCID+ZIFTMP) + ZNTMP
         ENDIF
         ITMP = IQ(LNAME+IOFF+ZITMP)
*
         IEDIF = 0
         IELEM = 1
         NELEM = 1
         INEVT = IDNEVT
         DO 10 J = 1, NSUB
            LP = IQ(LINT+IQ(LNAME+IOFF+ZARIND)+(J-1))
            IF (LP .LT. 0) THEN
               IELEM = IELEM*(-LP)
               NELEM = IELEM
            ELSE
*
*-- for variable length arrays calculate the start address using the
*-- sum of the index variable which is stored in IQ(LTMP+IPTMP+4).
*-- The start address of the N'th event of an M element array is
*-- found by looking for the NxM'th event of a 1 element array
*
               IF (IQ(LNAME+LP-1+ZNADDR) .EQ. 0) THEN
                  CALL HBUG('Address of index variable not set',
     +                      'HGNT',ID)
                  GOTO 40
               ENDIF
               LL    = IQ(LNAME+LP-1+ZRANGE)
               IEMAX = IELEM*IQ(LINT+LL+1)
*
               IPTMP = IQ(LNAME+LP-1+ZITMP)
               INEVT = (IQ(LTMP+IPTMP+4) * IELEM) + 1
               IELEM = IELEM*IQ(LTMP+IPTMP+5)
               NELEM = 1
*
               IEDIF = IEMAX - IELEM
            ENDIF
   10    CONTINUE
*
*-- load bank containing the event
*
         LCIND = IQ(LNAME+IOFF+ZLCONT)
         LRECL = IABS(IQ(LCID+ZNPRIM)) - 1
*
         IF (IQ(LTMP+1).NE.0 .AND. IDNEVT.EQ.IQ(LTMP+1)+1) THEN
            IBANK  = IQ(LTMP+ITMP)
            IFIRST = IQ(LTMP+ITMP+1)
            NB     = IQ(LTMP+ITMP+2)
            NLEFT  = IQ(LTMP+ITMP+3)
         ELSE
*
*-- NW is number of full words occupied by one data item
*-- IPW is data Items Per Word
*-- NWRD is the Number of WoRDs occupied by IDNEVT events
*-- IBANK is the bank in which IDNEVT is stored
*-- IFIRST is the word offset in IBANK for the first word of IDNEVT
*-- NB is the bit offset in IFIRST for the first word of IDNEVT
*
            IB = NBITS
            NW = 1
            IF (ISIZE .GT. IBYPW) THEN
               NW = ISIZE/IBYPW
               IB = NBITS/NW
            ENDIF
            IPW = IBIPW/IB
            NWRD = (INEVT-1)*NELEM*NW/IPW
            IBANK = NWRD/LRECL + 1
            IFIRST = MOD(NWRD+2, LRECL)
            IF (IFIRST .EQ. 0) IFIRST = LRECL
            IF (IFIRST .EQ. 1) IFIRST = LRECL + 1
            NB = (INEVT-1)*NELEM*NW*IB - NWRD*IB*IPW
*
            NLEFT = LRECL - IFIRST + 2
            NLEFT = NLEFT*IBIPW - NB
*
         ENDIF
*
*-- let LR2 point to the correct contents bank in the buffer structure
*
         IF (IELEM .GT. 0) THEN
            IF (IQ(LNAME+IOFF+ZIBANK) .EQ. IBANK) THEN
               LR2 = LQ(LNAME-INDX)
            ELSE
               CALL HNTRD(INDX, IOFF, IBANK, IER)
               IF (IER .NE. 0) GOTO 50
            ENDIF
         ENDIF
*
         DO 30 J = 1, IELEM
***         IM = MOD(NB, IBIPW)
            IM = IAND(NB, IBIPW-1)
            IF (IM.NE.0 .AND. NBITS.GT.IBIPW-IM) THEN
               NB     = 0
               NLEFT  = NLEFT - IBIPW+IM
               IFIRST = IFIRST + 1
            ENDIF
*
*-- if next item is not in current bank load next bank
*
            IF (NBITS .GT. NLEFT) THEN
               IBANK = IBANK + 1
               CALL HNTRD(INDX, IOFF, IBANK, IER)
               IF (IER .NE. 0) GOTO 50
               NB     = 0
               NLEFT  = LRECL*IBIPW
               IFIRST = 2
            ENDIF
*
            IF (NRECOV .AND. .NOT.INDVAR) GOTO 25
*
*-- Restoring REALs
*
            IF (ITYPE .EQ. 1) THEN
               IF (ISIZE .EQ. 4) THEN
                  IF (NBITS .EQ. 32) THEN
                     IF (USEBUF) THEN
#if defined(CERNLIB_NO_IEEE)
                        CALL IE3TOS(Q(LR2+IFIRST),Q(IOFFST+1),1,K)
#endif
#if !defined(CERNLIB_NO_IEEE)
                        Q(IOFFST+1) = Q(LR2+IFIRST)
#endif
                     ELSE
#if defined(CERNLIB_NO_IEEE)
                        CALL IE3TOS(Q(LR2+IFIRST),
     +                              Q(IQ(LNAME+IOFF+ZNADDR)+J),1,K)
#endif
#if !defined(CERNLIB_NO_IEEE)
                        Q(IQ(LNAME+IOFF+ZNADDR)+J) = Q(LR2+IFIRST)
#endif
                     ENDIF
                  ELSE
                     RMIN  = Q(LREAL+IQ(LNAME+IOFF+ZRANGE))
                     RMAX  = Q(LREAL+IQ(LNAME+IOFF+ZRANGE)+1)
                     IPACK = JBYT(IQ(LR2+IFIRST), NB+1, NBITS)
                     IF (USEBUF) THEN
                        Q(IOFFST+1) = IPACK *
***  +                          (RMAX - RMIN)/(2**NBITS - 1) + RMIN
     +                          (RMAX - RMIN)/(ISHFT(1,NBITS)-1) + RMIN
                     ELSE
                        Q(IQ(LNAME+IOFF+ZNADDR)+J) = IPACK *
***  +                          (RMAX - RMIN)/(2**NBITS - 1) + RMIN
     +                          (RMAX - RMIN)/(ISHFT(1,NBITS)-1) + RMIN
                     ENDIF
                  ENDIF
               ELSEIF (ISIZE .EQ. 8) THEN
                  IF (NBITS .EQ. 64) THEN
                     IF (USEBUF) THEN
#if defined(CERNLIB_NO_IEEE)
                        CALL IE3TOD(Q(LR2+IFIRST),Q(IOFFST+1),1,K)
#endif
#if !defined(CERNLIB_NO_IEEE)
#if defined(CERNLIB_VAX)||defined(CERNLIB_DECS)||(defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC))||defined(CERNLIB_WINNT)
                        Q(IOFFST+1) = Q(LR2+IFIRST+1)
                        Q(IOFFST+2) = Q(LR2+IFIRST)
#else
                        Q(IOFFST+1) = Q(LR2+IFIRST)
                        Q(IOFFST+2) = Q(LR2+IFIRST+1)
#endif
#endif
                     ELSE
#if defined(CERNLIB_NO_IEEE)
                        CALL IE3TOD(Q(LR2+IFIRST),
     +                              Q(IQ(LNAME+IOFF+ZNADDR)+2*J-1),1,K)
#endif
#if !defined(CERNLIB_NO_IEEE)
#if defined(CERNLIB_VAX)||defined(CERNLIB_DECS)||(defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC))||defined(CERNLIB_WINNT)
                        Q(IQ(LNAME+IOFF+ZNADDR)+2*J-1) = Q(LR2+IFIRST+1)
                        Q(IQ(LNAME+IOFF+ZNADDR)+2*J)   = Q(LR2+IFIRST)
#else
                        Q(IQ(LNAME+IOFF+ZNADDR)+2*J-1) = Q(LR2+IFIRST)
                        Q(IQ(LNAME+IOFF+ZNADDR)+2*J)   = Q(LR2+IFIRST+1)
#endif
#endif
                     ENDIF
                  ELSE
*-- no double precision packed reals yet
                  ENDIF
               ENDIF
*
*-- Restoring INTEGERs
*
            ELSEIF (ITYPE .EQ. 2) THEN
               IF (ISIZE .EQ. 2) THEN
 
               ELSEIF (ISIZE .EQ. 4) THEN
                  IF (INDVAR) THEN
                     IF (USEBUF) THEN
                        IQ(IOFFST+1) = IQ(LR2+IFIRST) -
     +                                 IQ(LR2+IFIRST-1)
                        IQ(LTMP+ITMP+5) = IQ(IOFFST+1)
                     ELSE
                        IQ(IQ(LNAME+IOFF+ZNADDR)+J) = IQ(LR2+IFIRST) -
     +                                                IQ(LR2+IFIRST-1)
                        IQ(LTMP+ITMP+5) = IQ(IQ(LNAME+IOFF+ZNADDR)+J)
                     ENDIF
                     IQ(LTMP+ITMP+4) = IQ(LR2+IFIRST-1)
                  ELSEIF (NBITS .EQ. 32) THEN
                     IF (USEBUF) THEN
                        IQ(IOFFST+1) = IQ(LR2+IFIRST)
                     ELSE
                        IQ(IQ(LNAME+IOFF+ZNADDR)+J) = IQ(LR2+IFIRST)
                     ENDIF
                  ELSE
                     IF (JBIT(IQ(LR2+IFIRST), NB+NBITS) .EQ. 1) THEN
                        IF (USEBUF) THEN
                           IQ(IOFFST+1) =
     +                           -JBYT(IQ(LR2+IFIRST), NB+1, NBITS-1)
                        ELSE
                           IQ(IQ(LNAME+IOFF+ZNADDR)+J) =
     +                           -JBYT(IQ(LR2+IFIRST), NB+1, NBITS-1)
                        ENDIF
                     ELSE
                        IF (USEBUF) THEN
                           IQ(IOFFST+1) =
     +                            JBYT(IQ(LR2+IFIRST), NB+1, NBITS-1)
                        ELSE
                           IQ(IQ(LNAME+IOFF+ZNADDR)+J) =
     +                            JBYT(IQ(LR2+IFIRST), NB+1, NBITS-1)
                        ENDIF
                     ENDIF
                  ENDIF
               ELSEIF (ISIZE .EQ. 8) THEN
                  IF (NBITS .EQ. 64) THEN
                     IF (USEBUF) THEN
                        IQ(IOFFST+1) = IQ(LR2+IFIRST)
                        IQ(IOFFST+2) = IQ(LR2+IFIRST+1)
                     ELSE
                        IQ(IQ(LNAME+IOFF+ZNADDR)+2*J-1)=IQ(LR2+IFIRST)
                        IQ(IQ(LNAME+IOFF+ZNADDR)+2*J)=IQ(LR2+IFIRST+1)
                     ENDIF
                  ELSE
*-- no double precision packed integers yet
                  ENDIF
               ENDIF
*
*-- Restoring UNSIGNED INTEGERs
*
            ELSEIF (ITYPE .EQ. 3) THEN
               IF (ISIZE .EQ. 2) THEN
 
               ELSEIF (ISIZE .EQ. 4) THEN
                  IF (NBITS .EQ. 32) THEN
                     IF (USEBUF) THEN
                        IQ(IOFFST+1) = IQ(LR2+IFIRST)
                     ELSE
                        IQ(IQ(LNAME+IOFF+ZNADDR)+J) = IQ(LR2+IFIRST)
                     ENDIF
                  ELSE
                     IF (USEBUF) THEN
                        IQ(IOFFST+1) =
     +                         JBYT(IQ(LR2+IFIRST), NB+1, NBITS)
                     ELSE
                        IQ(IQ(LNAME+IOFF+ZNADDR)+J) =
     +                         JBYT(IQ(LR2+IFIRST), NB+1, NBITS)
                     ENDIF
                  ENDIF
               ELSEIF (ISIZE .EQ. 8) THEN
                  IF (NBITS .EQ. 64) THEN
                     IF (USEBUF) THEN
                        IQ(IOFFST+1)=IQ(LR2+IFIRST)
                        IQ(IOFFST+2)=IQ(LR2+IFIRST+1)
                     ELSE
                        IQ(IQ(LNAME+IOFF+ZNADDR)+2*J-1)=IQ(LR2+IFIRST)
                        IQ(IQ(LNAME+IOFF+ZNADDR)+2*J)=IQ(LR2+IFIRST+1)
                     ENDIF
                  ELSE
*-- no double precision packed unsigned integers yet
                  ENDIF
               ENDIF
*
*-- Restoring LOGICALs
*
            ELSEIF (ITYPE .EQ. 4) THEN
               IF (ISIZE .EQ. 1) THEN
 
               ELSEIF (ISIZE .EQ. 2) THEN
 
               ELSEIF (ISIZE .EQ. 4) THEN
                  ILOGI = JBYT(IQ(LR2+IFIRST), NB+1, NBITS)
                  IF (ILOGI .EQ. 1) THEN
                     LOGIC = .TRUE.
                  ELSE
                     LOGIC = .FALSE.
                  ENDIF
                  IF (USEBUF) THEN
                     IQ(IOFFST+1) = ILOGIC
                  ELSE
                     IQ(IQ(LNAME+IOFF+ZNADDR)+J) = ILOGIC
                  ENDIF
               ENDIF
*
*-- Restoring CHARACTERs
*
            ELSEIF (ITYPE .EQ. 5) THEN
#if defined(CERNLIB_IBM)||defined(CERNLIB_VAX)||defined(CERNLIB_DECS)||defined(CERNLIB_MSDOS)||(defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC))||defined(CERNLIB_WINNT)
               IF (USEBUF) THEN
                  CALL HRZFRA(IQ(LR2+IFIRST),IQ(IOFFST+1),MXBY)
               ELSE
                  CALL HRZFRA(IQ(LR2+IFIRST),
     +                        IQ(IQ(LNAME+IOFF+ZNADDR)+MXBY1*(J-1)+1),
     +                        MXBY)
               ENDIF
#endif
#if (!defined(CERNLIB_IBM))&&(!defined(CERNLIB_VAX))&&(!defined(CERNLIB_DECS))&&(!defined(CERNLIB_MSDOS))&&(!(defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC)))&&(!defined(CERNLIB_WINNT))
               DO 20  K = 1, MXBY
                  IF (USEBUF) THEN
                     IQ(IOFFST+K) = IQ(LR2+IFIRST+K-1)
                  ELSE
                     IQ(IQ(LNAME+IOFF+ZNADDR)+MXBY1*(J-1)+K) =
     +                                          IQ(LR2+IFIRST+K-1)
                  ENDIF
   20          CONTINUE
#endif
            ENDIF
*
   25       NB = NB + NBITS
            IF (ISHBIT .NE. 0) THEN
               IFIRST = IFIRST + ISHFT(NB,-ISHBIT)
            ELSE
               IFIRST = IFIRST + NB/IBIPW
            ENDIF
***         NB     = MOD(NB, IBIPW)
            NB     = IAND(NB, IBIPW-1)
            NLEFT  = NLEFT - NBITS
            IF (USEBUF) IOFFST = IOFFST + ISHFT(ISIZE,-2)
   30    CONTINUE
*
         IQ(LTMP+ITMP)   = IBANK
         IQ(LTMP+ITMP+1) = IFIRST
         IQ(LTMP+ITMP+2) = NB
         IQ(LTMP+ITMP+3) = NLEFT
*
         IF (USEBUF) THEN
            IF (IEDIF .EQ. 0) THEN
               IQ(LTMP1+JTMP+2) = IOFFST
            ELSE
               IQ(LTMP1+JTMP+2) = IOFFST + (IEDIF*ISHFT(ISIZE,-2))
            ENDIF
         ELSE
            IQ(LTMP1+JTMP+2) = 0
         ENDIF
*
   40 CONTINUE
*
      IQ(LTMP+1) = IDNEVT
*
      RETURN
*
*-- error
*
   50 IQ(LTMP+1) = 0
      IERROR = 1
*
      END
