*
* $Id: hbnam.F,v 1.1.1.1 1996/01/16 17:07:56 mclareni Exp $
*
* $Log: hbnam.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:56  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/02 10/04/95  14.47.02  by  Julian Bunn
*-- Author :    Fons Rademakers   17/12/91
      SUBROUTINE HBNAM(IDD, BLKNA1, ADDRES, FORM1, ISCHAR)
*.==========>
*.
*.           Describing the variables to be stored in the new
*.           (variable row length) n-tuple.
*.           For the data-structure description see routine HBNT.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcform.inc"
*
      INTEGER        IDD, ADDRES, HNBPTR
      CHARACTER*(*)  BLKNA1, FORM1
      PARAMETER     (MAXTOK = 50)
      CHARACTER*8    BLKNAM
      CHARACTER*40   SFORM
      CHARACTER*80   TOK(MAXTOK)
      CHARACTER*1300 FORM
      LOGICAL        ISCHAR
*
*-- check if ID already in table, if not HBNT should be called first
*
      IF (IDD .NE. IDLAST) THEN
         ID    = IDD
         IDPOS = LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
         IF (IDPOS .LE. 0) THEN
            CALL HBUG('nTuple does not exist.','HBNAME',IDD)
            RETURN
         ENDIF
         IDLAST = ID
         LCID   = LQ(LTAB-IDPOS)
         I4 = JBIT(IQ(LCID+KBITS),4)
         IF (I4 .EQ. 0) RETURN
         IF (IQ(LCID-2) .NE. ZLINK) THEN
            CALL HBUG('HBNAME cannot be used for Row-wise nTuples',
     +                'HBNAME',IDD)
            RETURN
         ENDIF
      ENDIF
*
*-- check length of BLKNAM
*
      BLKNAM = BLKNA1
      IF (LENOCC(BLKNA1) .GT. LEN(BLKNAM)) THEN
         PRINT *, '*** Warning: Block name truncated to: ', BLKNAM
      ENDIF
      CALL CLTOU(BLKNAM)
*
*-- check length of FORM
*
      IF (LENOCC(FORM1) .GT. LEN(FORM)) THEN
         CALL HBUG('CHFORM string too long','HBNAME',IDD)
         RETURN
      ENDIF
      FORM = FORM1
*
      IADD = ADDRES
*
*-- set pointers
*
      LBLOK  = LQ(LCID-1)
      LCHAR  = LQ(LCID-2)
      LINT   = LQ(LCID-3)
      LREAL  = LQ(LCID-4)
*
      SFORM = FORM
      CALL CLTOU(SFORM)
      IF (SFORM(1:6) .EQ. '$CLEAR') THEN
         CALL HNMSET(IDD, ZNADDR, 0)
         CALL SBIT0(IQ(LBLOK),3)
         RETURN
      ELSEIF (SFORM(1:4).EQ.'$SET' .OR. SFORM(1:4).EQ.'!SET') THEN
*
*-- !SET: Super dirty trick to tell HBOOK that we access the Ntuple from PAW.
*-- HBOOK needs to know this to restore character array columns in
*-- the correct way (i.e. as an array of character*32 variables instead of
*-- an array of the defined length). All due to an early design error.
*
         IF (SFORM(1:1) .EQ. '!') CALL SBIT1(IQ(LBLOK),3)
         LBLOK = HNBPTR(BLKNAM)
         IF (LBLOK .EQ. 0) THEN
            CALL HBUG('Unknown block '//BLKNAM,'HBNAME',IDD)
            RETURN
         ENDIF
         LNAME = LQ(LBLOK-1)
*
         LSF = LENOCC(SFORM)
         I = INDEX(SFORM,':')
         IF (I.GT.0 .AND. LSF.GT.5) THEN
            CALL HNMADR(SFORM(I+1:LSF), IADD, ISCHAR)
         ELSE
            CALL HNMADR('*', IADD, ISCHAR)
         ENDIF
         RETURN
      ENDIF
*
      LNAME = LQ(LBLOK-1)
*
      NWID = IQ(LBLOK-1)
      NWN  = IQ(LNAME-1)
*
*-- find the pointer to the block with BLKNAM
*-- if the BLKNAM was not found create new block data structure
*-- (the data structure for the first block has already been created
*-- in HBNT)
*
      IF (IQ(LBLOK+ZIBLOK) .NE. 0) THEN
         LBLOK = HNBPTR(BLKNAM)
         IF (LBLOK .EQ. 0) THEN
            NTOT = NWID+NWN+2*33
            CALL HSPACE(NTOT+1000,'HBNAME',IDD)
            IF (IERR .NE. 0) GOTO 99
            IDLAST = IDD
            LLBLK = LQ(LCID-7)
            CALL MZBOOK(IHDIV,LBLOK,LLBLK, 0, 'HBLK',ZLINK,ZLINK-2,
     +                  NWID,IOBL,0)
            LQ(LCID-7) = LBLOK
*
            CALL MZBOOK(IHDIV,LNAME,LBLOK,-1,'HNAM',1,0,NWN,2,0)
*
            IQ(LBLOK+ZIFNAM) = 1
*
            CALL UCTOH(BLKNAM,IQ(LBLOK+ZIBLOK),4,8)
            IQ(LCID+ZNBLOK) = IQ(LCID+ZNBLOK) + 1
         ELSE
            LNAME = LQ(LBLOK-1)
         ENDIF
      ELSE
         CALL UCTOH(BLKNAM,IQ(LBLOK+ZIBLOK),4,8)
         IQ(LCID+ZNBLOK) = IQ(LCID+ZNBLOK) + 1
      ENDIF
*
*-- HBNAME may not be called anymore once filling of a block has started
*
      IF (IQ(LBLOK+ZNOENT) .NE. 0) THEN
         CALL HBUG('Filling started, cannot extend block definition',
     +             'HBNAME',IDD)
         RETURN
      ENDIF
*
*-- parse FORM to find all tokens
*
      CALL HNTTOK(FORM, TOK, NTOK, MAXTOK, IERR)
      IF (IERR .EQ. 1) THEN
         CALL HBUG('Incomplete format description','HBNAME',IDD)
         RETURN
      ELSEIF (IERR .EQ. 2) THEN
         CALL HBUG('Too many variables in format (>50)',
     +             'HBNAME',IDD)
         RETURN
      ENDIF
*
*-- make sure there is at least enough space for NTOK variables in LNAME
*
      INEED = ZNADDR*NTOK
      IMAX  = IQ(LBLOK+ZIFNAM)-1 + INEED
      IF (IMAX .GT. IQ(LNAME-1)) THEN
         CALL MZPUSH(IHDIV, LNAME, 0, INEED, 'I')
      ENDIF
*
*-- loop over all tokens and fill the LNAME structure
*
      DO 10 I = 1, NTOK
         CALL HNTNAM(TOK(I), IADD)
         IF (IERR .EQ. 1) THEN
            PRINT *, 'Format: ', FORM(1:LENOCC(FORM))
            IF (I .EQ. 1) THEN
               CALL HBUG('Error in first token of format description',
     +                   'HBNAME', IDD)
            ELSE
               PRINT *, 'Token ', TOK(I-1)(1:LENOCC(TOK(I-1)))
               CALL HBUG('Error after above token in format '//
     +                   'description','HBNAME',IDD)
            ENDIF
            RETURN
         ENDIF
10    CONTINUE
*
*-- Ntuple structure has been changed.
*-  Bit 1 is set for any change and bit 2 for a change in the number of columns
*
      LBLOK = LQ(LCID-1)
      CALL SBIT1(IQ(LBLOK),1)
      CALL SBIT1(IQ(LBLOK),2)
*
99    RETURN
*
      END
