*
* $Id: zftlls.F,v 1.1.1.1 1996/03/08 15:44:20 mclareni Exp $
*
* $Log: zftlls.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:20  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE ZFTLLS
#if defined(CERNLIB_VAXVMS)
#include "cspack/vmsinf.inc"
      EXTERNAL       XZFINF
      INCLUDE       '($RMSDEF)'
      INTEGER       SYS$GETMSG
      CHARACTER*255 CHDIR,CHDEF
      CHARACTER*10  CBLOCKS
#endif
      CHARACTER*255 CHFILE
      CHARACTER*80 COMM
      CHARACTER*80 PATH
      CHARACTER*4  CHOPT,CHOPTT
*
      CALL KUGETS(PATH,LPATH)
      CALL KUGETC(CHOPT,NCH)
 
      IF(NCH.EQ.0) THEN
         CHOPT = ' '
         NCH   = 1
      ENDIF
 
      IF(INDEX(CHOPT,'C').EQ.0) CALL CLTOU(PATH)
 
      NFOUND = 0
      ICONT  = 0
      IOPTL  = 0
      CHOPTT = '  '
      COMM   = ' '
      IF(INDEX(CHOPT,'L').NE.0) THEN
         CHOPTT = '-l'
         IOPTL  = 1
      ENDIF
#if defined(CERNLIB_UNIX)
 
      IF(LPATH.EQ.0) THEN
         PATH  = ' '
         LPATH = 1
      ENDIF
 
      IF(INDEX(CHOPT,'C').EQ.0) CALL CUTOL(PATH)
 
      WRITE(COMM,9001) CHOPTT,PATH(1:LPATH)
 9001 FORMAT('ls ',A,' ',A)
      IC = SYSTEMF(COMM)
#endif
#if defined(CERNLIB_VAXVMS)
      IF(LPATH.EQ.0) THEN
         LPATH = 3
         PATH  = '*.*'
      ENDIF
10    CONTINUE
      CHFILE = ' '
      CHDEF  = ' '
      CALL GETDEF(CHDEF)
      LDEF = LENOCC(CHDEF)
      IF ((LDEF.NE.0) .AND.
     +   (INDEX(PATH(1:LPATH),':').EQ.0).AND.
     +   (INDEX(PATH(1:LPATH),'>').EQ.0).AND.
     +   (INDEX(PATH(1:LPATH),'-').EQ.0)) THEN
         ISTAT = LIB$FIND_FILE(CHDEF(1:LDEF)//
     +                         PATH(1:LPATH),CHFILE,ICONT)
      ELSE
         ISTAT = LIB$FIND_FILE(PATH(1:LPATH),CHFILE,ICONT)
      ENDIF
*
*     Check for RMS$_DNF - directory not found
*               RMS$_FNF - file      not found
*               RMS$_NMF - no more files
*
      IF(ISTAT.EQ.RMS$_NMF) GOTO 99
      IF(.NOT.ISTAT) THEN
         IRC = SYS$GETMSG(%VAL(ISTAT),LFILE,CHFILE,,)
         PRINT *,CHFILE(1:LFILE)
         IRC = ISTAT
         ISTAT = LIB$FIND_FILE_END(ICONT)
         GOTO 99
      ELSE
      LFILE = LENOCC(CHFILE)
*
*     Get recfm, nblocks, creation date, if IOPTL
*
         IF(IOPTL.NE.0) THEN
            OPEN(99,FILE=CHFILE(1:LFILE),STATUS='OLD',READONLY,
     +           IOSTAT=IRC,USEROPEN=XZFINF)
            ISTART = INDEXB(CHFILE(1:LFILE),']')+1
            IF(NFOUND.EQ.0) THEN
               PRINT *,'Directory: '//CHFILE(1:ISTART-1)
               PRINT *
            ENDIF
            WRITE(CBLOCKS,'(I10)') IBLK
            PRINT *,CHFILE(ISTART:LFILE)//' '//CBLOCKS//' '//
     +         CREDAT(1:17)//' '//
     +         CHUIC(1:LENOCC(CHUIC))//' '//CHPROT
         ELSE
            PRINT *,CHFILE(1:LFILE)
         ENDIF
         NFOUND = NFOUND + 1
         GOTO 10
 
       ENDIF
#endif
#if defined(CERNLIB_IBMVM)
 
      IF(LPATH.EQ.0) THEN
         PATH  = ' '
         LPATH = 1
      ENDIF
 
      CALL CTRANS('.',' ',PATH,1,LPATH)
      IF(IOPTL.EQ.0) THEN
         CALL VMCMS('LISTFILE '//PATH(1:LPATH)//'(STACK',IRC)
      ELSE
         CALL VMCMS('LISTFILE '//PATH(1:LPATH)//'(STACK L',IRC)
      ENDIF
      CALL VMCMS('SENTRIES',NFOUND)
      DO 10 I=1,NFOUND
      CALL VMRTRM(CHFILE,LFILE)
      PRINT *,CHFILE(1:LENOCC(CHFILE))
10    CONTINUE
#endif
 
  99  END
 
