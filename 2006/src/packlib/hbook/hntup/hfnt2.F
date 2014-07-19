*
* $Id: hfnt2.F,v 1.8 1999/03/23 14:34:23 couet Exp $
*
* $Log: hfnt2.F,v $
* Revision 1.8  1999/03/23 14:34:23  couet
* - With some CWNs containing REAL*8, an error message saying that
*   the number was invalid (NaN) was displayed. This was a side
*   effect of the word swapping problem fixed previously.
*
* Revision 1.7  1999/03/05 15:42:32  couet
* - Bug fixed to handle properly the word swapping of REAL*8 machines on little
* endian machines
*
* Revision 1.6  1998/12/02 09:05:30  couet
* - clean up
*
* Revision 1.5  1998/09/25 09:28:34  mclareni
* Modifications for the Mklinux port flagged by CERNLIB_PPC
*
* Revision 1.4  1998/03/20 09:43:59  couet
* - When filling a CWNtuple and using a 24bit packing of a real, for each entry
*   equal to the maximum value XMAX the value actually stored into the CWN is
*   XMIN. This only seems to happen with 24 to 30 bit packing. This error occured
*   with the CERNLIB 98 and 96a on hpux and irix (at least).
*
* Revision 1.3  1997/07/07 09:34:43  couet
* - Error message changed in case of NaN filling to make it more clear.
*
* Revision 1.2  1996/07/11 15:44:14  couet
* - Bug fix from: Rainer Bartoldus <bartoldu@opalr6.physik.uni-bonn.de>
*
*   The problem occurred with a job that reads in a list of CW ntuples,
* corrects some entries and writes out a list of new ntuples.
*
* After we observed problems with the output files, we discovered that
* even without any correction the output was different from the input.
*
* The symptom affected only signed integer variables. It looked as if
* their sign had been written randomly.
*
* However, this happened only after a certain number of input files
* depending on the size of the PAWC common block (The larger the common
* was the later the files were corrupted).
*
* Revision 1.1.1.1  1996/01/16 17:07:57  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :          07/08/95  13.11.08  by  Julian Bunn
*-- Author :    Fons Rademakers   06/01/92
      SUBROUTINE HFNT2
*.==========>
*.
*.           Filling of a new (variable row length) n-tuple.
*.           For the data-structure description see routine HBNT.
*.
*. This routine does the actual filling of the variables in block LBLOK.
*.
*. The first word of every physical record is, for the time being, only
*. used for the storage of intermadiate results of index variables.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcnt.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
*
 
      CHARACTER*80 MSG
 
      INTEGER      ILOGIC
      LOGICAL      LOGIC, INDVAR, RANGE
      CHARACTER*32 NAME
      CHARACTER*96 MESS
      EQUIVALENCE (LOGIC, ILOGIC)
*-* FMIN,FMAX must be saved on HP (compiler bug)
      SAVE         FMIN, FMAX
*
#include "hbook/jbyt.inc"
*
      LNAME  = LQ(LBLOK-1)
*
      NDIM   = IQ(LBLOK+ZNDIM)
*
*-- when this routine is called for the first time for a specific block
*-- create a reference link area in the LNAME bank that will contain
*-- pointers to the corresponding contents banks in the LBUF structure
*
      IF (IQ(LBLOK+ZNOENT).EQ.0 .AND. IQ(LNAME-3).EQ.1) THEN
         CALL MZPUSH(IHDIV, LNAME, NDIM, 0, ' ')
      ENDIF
*
      IQ(LBLOK+ZNOENT) = IQ(LBLOK+ZNOENT) + 1
*
      IOFF = 0
*
      DO 50 I = 1, NDIM
         NSUB  = JBYT(IQ(LNAME+IOFF+ZDESC), 18, 3)
         ITYPE = JBYT(IQ(LNAME+IOFF+ZDESC), 14, 4)
         ISIZE = JBYT(IQ(LNAME+IOFF+ZDESC), 8,  6)
         NBITS = JBYT(IQ(LNAME+IOFF+ZDESC), 1,  7)
*
         INDVAR = .FALSE.
         IF (JBIT(IQ(LNAME+IOFF+ZDESC),28) .EQ. 1) INDVAR = .TRUE.
*
         IF (IQ(LNAME+IOFF+ZNADDR) .EQ. 0) GOTO 40
*
*-- fix the NBITS for character variables (7 bits was not enough)
*
         IF (ITYPE .EQ. 5) NBITS = IBIPB*ISIZE
*
*-- when a range has been specified enable range checking
*
         RANGE = .FALSE.
         LRAN = IQ(LNAME+IOFF+ZRANGE)
         IF (LRAN .NE. 0) THEN
            RANGE = .TRUE.
            IF (ITYPE .EQ. 1) THEN
               FMIN = Q(LREAL+LRAN)
               FMAX = Q(LREAL+LRAN+1)
            ELSEIF (ITYPE.EQ.2 .OR. ITYPE.EQ.3) THEN
               IMIN = IQ(LINT+LRAN)
               IMAX = IQ(LINT+LRAN+1)
            ELSE
               CALL HBUG('Type can not have range specification',
     +                   'HFNT',ID)
               GOTO 40
            ENDIF
         ENDIF
*
         LCIND = IQ(LNAME+IOFF+ZLCONT)
         IF (LQ(LNAME-I) .EQ. 0) THEN
            LQ(LNAME-I) = LQ(LBUF-LCIND)
         ENDIF
*
*-- make sure that LQ(LNAME-I) points to the right (i.e. last) bank
*-- when filling resumes (after, e.g., a call to HGNT)
*
         IF (IQ(LNAME+IOFF+ZIBANK) .NE. IQ(LNAME+IOFF+ZNRZB)) THEN
            CALL HNTRD(I, IOFF, IQ(LNAME+IOFF+ZNRZB), IER)
            IF (IER .NE. 0) THEN
               CALL HBUG('Error resuming filling','HFNT',ID)
               GOTO 40
            ENDIF
         ENDIF
*
         LR2    = LQ(LNAME-I)
         IFIRST = IQ(LNAME+IOFF+ZIFCON)
         NB     = IQ(LNAME+IOFF+ZIFBIT) - 1
*
         LRECL = IQ(LR2-1) - 1
         NLEFT = LRECL - IFIRST + 2
         NLEFT = NLEFT*IBIPW - NB
*
         IELEM = 1
         DO 10 J = 1, NSUB
            LP = IQ(LINT+IQ(LNAME+IOFF+ZARIND)+(J-1))
            IF (LP .LT. 0) THEN
               IE = -LP
            ELSE
               IF (IQ(LNAME+LP-1+ZNADDR) .EQ. 0) THEN
                  CALL HBUG('Address of index variable not set',
     +                      'HFNT',ID)
                  GOTO 40
               ENDIF
               IE = IQ(IQ(LNAME+LP-1+ZNADDR)+1)
            ENDIF
            IELEM = IELEM*IE
   10    CONTINUE
*
         DO 30 J = 1, IELEM
*
*-- if next item does not fit in the left over bits start with new word
*
***         IM = MOD(NB, IBIPW)
            IM = IAND(NB, IBIPW-1)
            IF (IM.NE.0 .AND. NBITS.GT.IBIPW-IM) THEN
               NB     = 0
               NLEFT  = NLEFT - IBIPW+IM
               IFIRST = IFIRST + 1
            ENDIF
*
*-- if next item does not fit in current buffer, write buffer
*
            IF (NBITS .GT. NLEFT) THEN
               IF (INDVAR) ISAVE = IQ(LR2+LRECL+1)
               CALL HNTWRT(I, IOFF, IER)
               IF (IER .NE. 0) GOTO 60
               IF (INDVAR) IQ(LR2+1) = ISAVE
               CALL SBIT1(IQ(LQ(LBUF-LCIND)),1)
               NB     = 0
               NLEFT  = LRECL*IBIPW
               IFIRST = 2
            ENDIF
*
*-- store REAL
*
            IF (ITYPE .EQ. 1) THEN
               IF (ISIZE .EQ. 4) THEN
                  IF (RANGE) THEN
                     QVAL = Q(IQ(LNAME+IOFF+ZNADDR)+J)
 
#if defined(CERNLIB_SUN)||defined(CERNLIB_SOLARIS)||defined(CERNLIB_SGI)||defined(CERNLIB_DECS)||defined(CERNLIB_ALPHA)||defined(CERNLIB_HPUX)||defined(CERNLIB_IBMRT)
*     test the floating point
                     JX=IFPS(QVAL)
                     IF(JX .EQ. 0) THEN
		         WRITE(MSG,'(A,I10,A,I10)')
     + 'HFNT bad float in column:',IOFF/ZNADDR,' line:',J
                         CALL HFPBUG(QVAL,MSG,ID)
                     ENDIF
*
#endif
 
                     IF (QVAL .LT. FMIN .OR. QVAL .GT. FMAX) THEN
                        LL = IQ(LNAME+IOFF+ZLNAME)
                        LV = IQ(LNAME+IOFF+ZNAME)
                        CALL UHTOC(IQ(LCHAR+LV), 4, NAME, LL)
                        WRITE(MESS,1000) NAME(1:LL), IQ(LBLOK+ZNOENT),
     +                                   QVAL
                        CALL HBUG(MESS,'HFNT',ID)
                     ENDIF
                     IF (QVAL .LT. FMIN) QVAL = FMIN
                     IF (QVAL .GT. FMAX) QVAL = FMAX
                  ENDIF
                  IF (NBITS .EQ. 32) THEN
#if defined(CERNLIB_NO_IEEE)
                     CALL IE3FOS(Q(IQ(LNAME+IOFF+ZNADDR)+J),
     +                           Q(LR2+IFIRST),1,K)
#endif
#if !defined(CERNLIB_NO_IEEE)
                     Q(LR2+IFIRST) = Q(IQ(LNAME+IOFF+ZNADDR)+J)
 
*     test the floating point
                     JX=IFPS(Q(LR2+IFIRST))
                     IF(JX .EQ. 0) THEN
	                 WRITE(MSG,'(A,I10,A,I10)')
     + 'HFNT bad float in column:',IOFF/ZNADDR,' line:',J
                         CALL HFPBUG(Q(LR2+IFIRST),MSG,ID)
                     ENDIF
*
#endif
 
                  ELSE
                     IBITF = ISHFT(1,NBITS)-1
                     IF (QVAL.EQ.FMAX) THEN
                        IPACK = IBITF
                     ELSE
                        IPACK = (QVAL-FMIN)/(FMAX-FMIN)*IBITF+0.5
                     ENDIF
                     CALL SBYT(IPACK, IQ(LR2+IFIRST), NB+1, NBITS)
                  ENDIF
               ELSEIF (ISIZE .EQ. 8) THEN
                  IF (NBITS .EQ. 64) THEN
#if defined(CERNLIB_NO_IEEE)
                     CALL IE3FOD(Q(IQ(LNAME+IOFF+ZNADDR)+2*J-1),
     +                           Q(LR2+IFIRST),1,K)
#endif
#if !defined(CERNLIB_NO_IEEE)
#if defined(CERNLIB_VAX)||defined(CERNLIB_DECS)||(defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC))||defined(CERNLIB_WINNT)
                     Q(LR2+IFIRST)   = Q(IQ(LNAME+IOFF+ZNADDR)+2*J)
                     Q(LR2+IFIRST+1) = Q(IQ(LNAME+IOFF+ZNADDR)+2*J-1)
                     JX=IFPD(Q(LR2+IFIRST+1))
#else
                     Q(LR2+IFIRST)   = Q(IQ(LNAME+IOFF+ZNADDR)+2*J-1)
                     Q(LR2+IFIRST+1) = Q(IQ(LNAME+IOFF+ZNADDR)+2*J)
                     JX=IFPD(Q(LR2+IFIRST))
#endif
 
*     test the floating point
                     IF(JX .EQ. 0) THEN
	                WRITE(MSG,'(A,I10,A,I10)')
     + 'HFNT bad float in column:',IOFF/ZNADDR,' line:',2*J-1
                        CALL HFPBUG(Q(LR2+IFIRST),'HFNT',ID)
                     ENDIF
#endif
 
                  ELSE
*-- no double precision packed reals yet
                  ENDIF
               ENDIF
*
*-- store INTEGER
*
            ELSEIF (ITYPE .EQ. 2) THEN
               IF (ISIZE .EQ. 2) THEN
 
               ELSEIF (ISIZE .EQ. 4) THEN
                  IF (.NOT.RANGE .AND. NBITS.NE.32) THEN
***                  IMAX  = 2**(NBITS-1) - 1
                     IMAX  = ISHFT(1,NBITS-1) - 1
                     IMIN  = -IMAX
                     RANGE = .TRUE.
                  ENDIF
                  IF (RANGE) THEN
                     IVAL = IQ(IQ(LNAME+IOFF+ZNADDR)+J)
                     IF (IVAL .LT. IMIN .OR. IVAL .GT. IMAX) THEN
                        LL = IQ(LNAME+IOFF+ZLNAME)
                        LV = IQ(LNAME+IOFF+ZNAME)
                        CALL UHTOC(IQ(LCHAR+LV), 4, NAME, LL)
                        WRITE(MESS,1010) NAME(1:LL), IQ(LBLOK+ZNOENT),
     +                                   IVAL
                        CALL HBUG(MESS,'HFNT',ID)
                     ENDIF
                     IF (IVAL .LT. IMIN) IVAL = IMIN
                     IF (IVAL .GT. IMAX) IVAL = IMAX
                  ENDIF
                  IF (INDVAR) THEN
                     IQ(LR2+IFIRST) = IQ(LR2+IFIRST-1) +
     +                                IQ(IQ(LNAME+IOFF+ZNADDR)+J)
                  ELSEIF (NBITS .EQ. 32) THEN
                     IQ(LR2+IFIRST) = IQ(IQ(LNAME+IOFF+ZNADDR)+J)
                  ELSE
                     IF (IVAL .LT. 0) THEN
                        CALL SBIT1(IQ(LR2+IFIRST), NB+NBITS)
                        CALL SBYT(-IVAL, IQ(LR2+IFIRST), NB+1, NBITS-1)
                     ELSE
*
*   Mods from Rainer Bartoldus
*   The previous version was:
*                          CALL SBYT(IVAL, IQ(LR2+IFIRST), NB+1, NBITS-1)
*
                        CALL SBYT(IVAL, IQ(LR2+IFIRST), NB+1, NBITS)
*
                     ENDIF
                  ENDIF
               ELSEIF (ISIZE .EQ. 8) THEN
                  IF (NBITS .EQ. 64) THEN
                     IQ(LR2+IFIRST)  =IQ(IQ(LNAME+IOFF+ZNADDR)+2*J-1)
                     IQ(LR2+IFIRST+1)=IQ(IQ(LNAME+IOFF+ZNADDR)+2*J)
                  ELSE
*-- no double precision packed integers yet
                  ENDIF
               ENDIF
*
*-- store UNSIGNED INTEGER
*
            ELSEIF (ITYPE .EQ. 3) THEN
               IF (ISIZE .EQ. 2) THEN
 
               ELSEIF (ISIZE .EQ. 4) THEN
                  IF (.NOT.RANGE .AND. NBITS.NE.32) THEN
***                  IMAX  = 2**(NBITS) - 1
                     IMAX  = ISHFT(1,NBITS) - 1
                     IMIN  = 0
                     RANGE = .TRUE.
                  ENDIF
                  IF (RANGE) THEN
                     IVAL = IQ(IQ(LNAME+IOFF+ZNADDR)+J)
                     IF (IVAL .LT. IMIN .OR. IVAL .GT. IMAX) THEN
                        LL = IQ(LNAME+IOFF+ZLNAME)
                        LV = IQ(LNAME+IOFF+ZNAME)
                        CALL UHTOC(IQ(LCHAR+LV), 4, NAME, LL)
                        WRITE(MESS,1010) NAME(1:LL), IQ(LBLOK+ZNOENT),
     +                                   IVAL
                        CALL HBUG(MESS,'HFNT',ID)
                     ENDIF
                     IF (IVAL .LT. IMIN) IVAL = IMIN
                     IF (IVAL .GT. IMAX) IVAL = IMAX
                  ENDIF
                  IF (NBITS .EQ. 32) THEN
                     IQ(LR2+IFIRST) = IQ(IQ(LNAME+IOFF+ZNADDR)+J)
                  ELSE
                     CALL SBYT(IVAL, IQ(LR2+IFIRST), NB+1, NBITS)
                  ENDIF
               ELSEIF (ISIZE .EQ. 8) THEN
                  IF (NBITS .EQ. 64) THEN
                     IQ(LR2+IFIRST)  =IQ(IQ(LNAME+IOFF+ZNADDR)+2*J-1)
                     IQ(LR2+IFIRST+1)=IQ(IQ(LNAME+IOFF+ZNADDR)+2*J)
                  ELSE
*-- no double precision packed unsigned integers yet
                  ENDIF
               ENDIF
*
*-- store LOGICAL
*
            ELSEIF (ITYPE .EQ. 4) THEN
               IF (ISIZE .EQ. 1) THEN
 
               ELSEIF (ISIZE .EQ. 2) THEN
 
               ELSEIF (ISIZE .EQ. 4) THEN
                  ILOGIC = IQ(IQ(LNAME+IOFF+ZNADDR)+J)
                  IF (LOGIC) THEN
                     CALL SBYT(1, IQ(LR2+IFIRST), NB+1, NBITS)
                  ELSE
                     CALL SBYT(0, IQ(LR2+IFIRST), NB+1, NBITS)
                  ENDIF
               ENDIF
*
*-- store CHARACTER string
*
            ELSEIF (ITYPE .EQ. 5) THEN
               MXBY = ISHFT(ISIZE,-2)
               DO 20  K = 1, MXBY
                  IQ(LR2+IFIRST+K-1) =
     +                     IQ(IQ(LNAME+IOFF+ZNADDR)+MXBY*(J-1)+K)
   20          CONTINUE
#if defined(CERNLIB_IBM)||defined(CERNLIB_VAX)||defined(CERNLIB_DECS)||defined(CERNLIB_MSDOS)||(defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC))||defined(CERNLIB_WINNT)
               CALL HRZTOA(IQ(LR2+IFIRST),MXBY)
#endif
            ENDIF
*
            NB = NB + NBITS
            IF (ISHBIT .NE. 0) THEN
               IFIRST = IFIRST + ISHFT(NB,-ISHBIT)
            ELSE
               IFIRST = IFIRST + NB/IBIPW
            ENDIF
***         NB     = MOD(NB, IBIPW)
            NB     = IAND(NB, IBIPW-1)
            NLEFT  = NLEFT - NBITS
            IF (J .EQ. 1) CALL SBIT1(IQ(LQ(LBUF-LCIND)),1)
   30    CONTINUE
*
*-- update pointers
*
         IQ(LNAME+IOFF+ZIFCON) = IFIRST
         IQ(LNAME+IOFF+ZIFBIT) = NB + 1
*
   40    IOFF = IOFF + ZNADDR
   50 CONTINUE
*
   60 RETURN
*
1000  FORMAT(A,': Value out of range, event ',I10,' value ',G14.7)
1010  FORMAT(A,': Value out of range, event ',I10,' value ',I11)
*
      END
