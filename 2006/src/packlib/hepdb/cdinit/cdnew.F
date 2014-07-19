*
* $Id: cdnew.F,v 1.1.1.1 1996/02/28 16:24:14 mclareni Exp $
*
* $Log: cdnew.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:14  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDNEW(LUNCD,CHTOP,CHFILE,IDIV,NPAIR,NQUO,NPRE,NTOP,
     +                 LRECL,CHOPT,IRC)
 
#include "hepdb/cduscm.inc"
 
#if defined(CERNLIB_IBMVM)
      CHARACTER*80  CHLINE,CHNAME
      CHARACTER*20  WORD
      CHARACTER*6   CHBLOK,CHFREE
      DIMENSION     IBUF(8192)
#endif
      CHARACTER*(*) CHTOP,CHFILE,CHOPT
      CHARACTER*20  CHRZOP,CHCDOP
 
      CHARACTER*10  RZOPTS
      CHARACTER*14  CDOPTS
 
      DIMENSION     IOPTRZ(10)
      DIMENSION     IOPTCD(14)
 
      DATA          RZOPTS/'LNSU1WYCXP'/
      DATA          CDOPTS/'SU1LDCXMZQEFA7'/
 
      IRC = 0
*
*     Default record length is 1024 words
*
      IF(LRECL.EQ.0) LRECL = 1024
*
*     Build options for RZOPEN: X, N default
*
      CALL UOPTC(CHOPT,RZOPTS,IOPTRZ)
      IOPTN = IOPTRZ(2)
      IF(IOPTN.EQ.0) IOPTRZ(9) = 1
      IOPTRZ(2) = 1
 
      CHRZOP = ' '
      J      = 0
 
      DO 10 I=1,LEN(RZOPTS)
         IF(IOPTRZ(I).NE.0) THEN
            J = J + 1
            CHRZOP(J:J) = RZOPTS(I:I)
         ENDIF
   10 CONTINUE
*
*     Open the database file
*
#if defined(CERNLIB_IBMVM)
      LFILE = LENOCC(CHFILE)
      CHNAME = CHFILE(1:LFILE)
      CALL CTRANS('.',' ',CHNAME,1,LFILE)
      IF(INDEX(CHNAME(1:LFILE),' ').EQ.
     +   INDEXB(CHNAME(1:LFILE),' ')) THEN
         CHNAME(LFILE+1:LFILE+3) = ' A6'
         LFILE = LFILE + 3
      ENDIF
      CALL RZOPEN(LUNCD,CHTOP,CHNAME,CHRZOP,LRECL,IRC)
#endif
#if !defined(CERNLIB_IBMVM)
      CALL RZOPEN(LUNCD,CHTOP,CHFILE,CHRZOP,LRECL,IRC)
#endif
      IF(IRC.NE.0) GOTO 999
*
*     Build options for CDINIT
*
      CALL UOPTC(CHOPT,CDOPTS,IOPTCD)
      IF(IOPTN.EQ.0) IOPTCD(7) = 1
      IOPTCD(9) = 1
 
      CHCDOP = ' '
      J      = 0
      DO 30 I=1,LEN(CDOPTS)
         IF(IOPTCD(I).NE.0) THEN
            J = J + 1
            CHCDOP(J:J) = CDOPTS(I:I)
         ENDIF
   30 CONTINUE
#if defined(CERNLIB_IBMVM)
*
*     Preformat NPRE records
*
      IF(IOPTCD(12).NE.0) THEN
         CHNAME = ' '
         INQUIRE(LUNCD,NAME=CHNAME)
         LNAME = LENOCC(CHNAME) -1
         CALL VMCMS('QUERY DISK '//CHNAME(LNAME:LNAME)//' (LIFO',IRC)
*
*     Get number of free blocks, blocksize
*
         CALL VMRTRM(CHLINE,NCH)
         CALL WORDSEP(' ')
         CHFREE = WORD(CHLINE(1:NCH),7)
         CHBLOK = WORD(CHLINE(1:NCH),11)
         LBLOCK = ICDECI(CHBLOK,1,6)
         LFREE  = ICDECI(CHFREE,1,6)
*
*     Clear stack
*
         CALL VMRTRM(CHLINE,NCH)
*
*     Check if we have the room to write NPRE records
*
         NBYTES = LRECL * 4 * NPRE
         NFREE  = LBLOCK * LFREE
         IF(NBYTES.GT.NFREE) THEN
            NDO = NFREE/(LRECL*4)
            IF(IDEBCD.GE.0) THEN
               PRINT *,'CDNEW. disk ',CHNAME(LNAME:LNAME),
     +         ' has ',LFREE,' free blocks of size ',LBLOCK,' bytes'
               PRINT *,'CDNEW. preformatting of ',NPRE,
     +         ' records of length ',LRECL*4,' bytes requested'
               PRINT *,'CDNEW. request reduced to ',NDO,' records'
            ENDIF
         ELSE
            NDO = NPRE
         ENDIF
*
*     Write the records
*
         DO 20 IREC=1,NDO
            CALL RZOPHD(LUNCD,LRECL,IREC,IBUF,IRC)
   20    CONTINUE
      ENDIF
#endif
*
*     Initialise HEPDB for this file
*
      CALL CDINIT(IDIV,LUNCD,0,CHTOP,NPAIR,NQUO,NTOP,CHCDOP,IRC)
 
  999 END
