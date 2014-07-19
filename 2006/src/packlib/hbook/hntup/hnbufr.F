*
* $Id: hnbufr.F,v 1.2 2002/01/30 15:03:04 couet Exp $
*
* $Log: hnbufr.F,v $
* Revision 1.2  2002/01/30 15:03:04  couet
* - typo fixed
*
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/08 28/01/94  15.14.11  by  Fons Rademakers
*-- Author :    Fons Rademakers   28/04/92
      SUBROUTINE HNBUFR(IDD)
*.==========>
*.
*.           Buffer manager for new (variable row length) read/only N-tuples.
*.           This routine creates buffers for variables that have
*.           an address (ZNADDR) set, and deletes the buffers for variables
*.           where the address is zero. In case of memory resident Ntuples
*.           read the buffers from disk (?!?!?!).
*.           For the data-structure description see routine HBNT.
*.
*.  This routine restores the contents buffers for a memory resident
*.  N-tuple ID.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
*
      COMMON/QUEST/IQUEST(100)
      CHARACTER*128 CHWOLD, CHDIR, CWDRZ
      INTEGER       KEYS(2)
      LOGICAL       MEMORY
*
      IERR   = 0
      ICYCLE = 9999
*
*-- create LBUF structure and the contents banks for N-tuple IDD
*
      NDIM = IQ(LCID+ZNDIM)
      NWP  = IABS(IQ(LCID+ZNPRIM))
*
      IF (LQ(LCDIR-4) .EQ. 0) THEN
*
*-- enough space left for buffer header structure?
*
         NTOT = NDIM+2+33
         CALL HSPACE(NTOT,'HNBUFR',IDD)
         IF (IERR.NE.0) GOTO 50
*
         CALL MZBOOK(IHDIV,LBUFM,LCDIR,-4,'HBUF',NDIM,NDIM,2,2,0)
         LBUF = LBUFM
         IQ(LBUF-5) = IDD
*
      ELSEIF (IQ(LBUF-5) .NE. IDD) THEN
*
*-- find buffer bank for ntuple IDD
*
         LBUF = LQ(LCDIR-4)
   10    IF (IQ(LBUF-5) .EQ. IDD) GOTO 20
         IF (LQ(LBUF) .NE. 0) THEN
            LBUF = LQ(LBUF)
            GOTO 10
         ENDIF
*
*-- no buffer structure for this ntuple; create it but
*-- first check if there is enough space left?
*
         NTOT = NDIM+2+33
         CALL HSPACE(NTOT,'HNBUFR',IDD)
         IF (IERR.NE.0) GOTO 50
*
         CALL MZBOOK(IHDIV,LBUF,LBUF,0,'HBUF',NDIM,NDIM,2,2,0)
         IQ(LBUF-5) = IDD
      ENDIF
*
*-- Create contents banks for the variables that will be retrieved
*-- (i.e. for all variables of which the restore address is set).
*--
*-- If the Ntuple is memory resident read the whole column from disk
*-- (memory resident means that the Ntuple columns were stored completely
*-- in memory and then written in one piece to disk).
*
   20 MEMORY = IQ(LCID+ZNPRIM) .LE. 0
*
*-- if memory resident goto correct RZ directory
*
      IF (MEMORY) THEN
         NCHRZ = IQ(LCID+ZNCHRZ)
         CALL RZCDIR(CWDRZ,'R')
         CALL HCDIR(CHWOLD,'R')
         CHDIR = ' '
         CALL UHTOC(IQ(LCID+ZNCHRZ+1),4,CHDIR,NCHRZ)
         IF (CHDIR.NE.CWDRZ) THEN
            CALL HCDIR(CHDIR,' ')
         ENDIF
         KEYS(1) = IQ(LCID+ZID)
      ENDIF
*
*-- loop over all blocks
*
      LBLOK = LQ(LCID-1)
      LCHAR = LQ(LCID-2)
      LINT  = LQ(LCID-3)
      LREAL = LQ(LCID-4)
*
   30 LNAME  = LQ(LBLOK-1)
*
      IOFF = 0
      NDIM = IQ(LBLOK+ZNDIM)
*
*-- loop over all variables in every block
*
      DO 40 I = 1, NDIM
         LCIND = IQ(LNAME+IOFF+ZLCONT)
         IADD  = IQ(LNAME+IOFF+ZNADDR)
         LB    = LQ(LBUF-LCIND)
*
         IF (IADD .EQ. 0) THEN
*
*-- drop existing bank at LB, only when modify bit is not set
*
            IF (LB .NE. 0) THEN
               IF (JBIT(IQ(LB),1) .EQ. 0) THEN
                  CALL MZDROP(IHDIV,LB,' ')
                  LQ(LBUF-LCIND) = 0
               ENDIF
            ENDIF
*
         ELSEIF (MEMORY .AND. LB.EQ.0) THEN
            KEYS(2) = IQ(LNAME+IOFF+ZNRZB)*10000 +
     +                IQ(LNAME+IOFF+ZLCONT)
*
*-- check if there is enough space to read in the entire structure
*
            CALL HRZIN(IHDIV,0,0,KEYS,ICYCLE,'C')
            IF (IQUEST(1) .NE. 0) THEN
               CALL HBUG('Error reading contents bank', 'HNBUFR', IDD)
               IERR = 1
               GOTO 50
            ENDIF
            NWORDS = IQUEST(12)
            CALL HSPACE(NWORDS+1000,'HNBUFR',IDD)
            IF (IERR .NE. 0) GOTO 50
*
*-- read in contents
*
            CALL HRZIN(IHDIV,LBUF,-LCIND,KEYS,ICYCLE,' ')
*
         ELSEIF (LB .EQ. 0) THEN
            NTOT = NWP+33
            CALL HSPACE(NTOT,'HNBUFR',IDD)
            IF (IERR.NE.0) GOTO 50
*
            CALL MZBOOK(IHDIV,L,LBUF,-LCIND,'HCON',0,0,NWP,1,-1)
*
         ENDIF
*
         IOFF = IOFF + ZNADDR
   40 CONTINUE
*
      LBLOK = LQ(LBLOK)
      IF (LBLOK .NE. 0) GOTO 30
*
      IF (MEMORY) THEN
         IF (CHDIR.NE.CWDRZ) THEN
            CALL HCDIR(CHWOLD,' ')
            IF (CHWOLD .NE. CWDRZ) THEN
               CALL RZCDIR(CWDRZ,' ')
            ENDIF
         ENDIF
      ENDIF
*
*-- create and set TMP buffers
*
      CALL HNTMP(IDD)
*
   50 RETURN
      END
 
