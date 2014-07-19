*
* $Id: fmedit.F,v 1.1.1.1 1996/03/07 15:17:42 mclareni Exp $
*
* $Log: fmedit.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:42  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMEDIT
#include "fatmen/fmpath.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatsys.inc"
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA)
      CHARACTER*8   HNAME,HTYPE,HSYS,HDISK
      CHARACTER*8   USER,ADDR
      CHARACTER*4   MODE
      CHARACTER*256 DSN
      CHARACTER*20  CHFILE
      CHARACTER*80  LINE
      INTEGER       FMHOST,FMNODE
#include "fatmen/fatinit.inc"
      CALL KUGETC(PATH,LPATH)
      IF(LPATH.EQ.0) RETURN
      CALL FMFIXF(PATH,PATH)
      LPATH = LENOCC(PATH)
      CALL KUGETI(KSN)
      IF(KSN .EQ. 0) THEN
        CALL VZERO(KEYS,10)
        ELSE
        KEYS(1) = KSN
        ENDIF
 
      CALL FMGETK(PATH(1:LPATH),L,KEYS,IRC)
      IF (IRC .NE. 0) PRINT *,'Return code ',IRC,' from FMGETK'
*
*     Disk files ...
*
      IF (IQ(L+MMTPFA) .EQ. 1) THEN
*
*     File is on disk. Check if it is on this node.
*
         IC = FMHOST(HNAME,HTYPE,HSYS)
         CALL UHTOC(IQ(L+MHSNFA),4,HDISK,8)
         LDISK = LENOCC(HDISK)
 
         IF (FMNODE(HDISK(1:LDISK)).NE.0) THEN
            IF(IDEBFA.GE.2) WRITE (LPRTFA,9014) HDISK,HNAME
9014  FORMAT(' FMEDIT. Disk file is on host ',A8,
     +       ' current host = ',A8)
            IRC = 1
            RETURN
         ENDIF
      CALL UHTOC(IQ(L+MFQNFA),4,DSN,NFQNFA)
      LDSN = LENOCC(DSN)
      LBLANK = INDEX(DSN,' ')
      IF(LBLANK.NE.0) LDSN = LBLANK
#if defined(CERNLIB_IBMVM)
*
*     Get disk name and link to it
*
 
         LSTA = INDEX(DSN,'<')
         IF (LSTA .NE. 0) THEN
*
*     Format of DSN is <user.address>filename.filetype on VM
*
            LDOT = INDEX(DSN,'.')
            LBRA = INDEX(DSN,'>')
 
            IF ((LDOT .NE. 0) .AND. (LDOT .LE. LBRA)) THEN
               LEND = LDOT
               ELSE
               LEND = LBRA
               ENDIF
 
            USER = DSN(LSTA+1:LEND-1)
            LUSR = LEND - LSTA + 1
            ADDR = '    '
 
            IF ((LDOT .NE. 0) .AND. (LDOT .LE. LBRA)) THEN
              ADDR= DSN(LDOT+1:LBRA-1)
              ENDIF
 
            CALL VMCMS('EXEC GIME '//USER(1:LUSR)//ADDR//
     +      '(QUIET NONOTICE STACK)',IRC)
            CALL VMRTRM(LINE,LENGTH)
            MODE = LINE(1:1)
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9015) USER,MODE
9015   FORMAT(' FMEDIT. linked to ',A8,' mode ',A4)
            ELSE
            MODE = '*'
            ENDIF
         CHFILE = DSN(LBRA+1:LDSN) // ' ' // MODE(1:2)
 
         CALL KUEDIT(CHFILE,ISTAT)
#endif
#if !defined(CERNLIB_IBMVM)
         CALL KUEDIT(DSN(1:LDSN),ISTAT)
#endif
         IF(ISTAT.NE.0) PRINT *,'Return code ',ISTAT,' from KUEDIT'
         ELSE
         PRINT *,'FMEDIT. Only local disk files currently supported'
         IRC = 3
         ENDIF
 
      END
