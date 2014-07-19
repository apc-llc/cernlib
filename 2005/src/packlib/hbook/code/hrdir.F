*
* $Id: hrdir.F,v 1.1.1.1 1996/01/16 17:07:47 mclareni Exp $
*
* $Log: hrdir.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:47  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/07 20/12/93  13.00.08  by  Rene Brun
*-- Author :
      SUBROUTINE HRDIR(MAXDIR,CHDIR,NDIR)
*.==========>
*.       Returns the list of subdirectories of the CWD
*.    Input:
*. MAXDIR  Length of the character array CHDIR
*.    Output:
*. CHDIR*  Character  array  which  will  contain  the  directory  names
*.         attached to the CWD.  If the  length of the directory name is
*.         greater then the length of one  element of CHDIR (as obtained
*.         by the LEN function),  only as many characters as will fit in
*.         the array element are returned.
*. NDIR*   Actual number of subdirectories attached to the CWD
*.         If this number is greater than MAXDIR,  only the first MAXDIR
*.         directory names will be returned in CHDIR.
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcdire.inc"
#include "hbook/hcpiaf.inc"
#include "hbook/czsock.inc"
#include "hbook/czbuff.inc"
#include "hbook/czcbuf.inc"
*
      COMMON/QUEST/IQUEST(100)
#if defined(CERNLIB_VAX)
      DIMENSION LQQ(50),IQQ(50),IHDIR(4)
      EQUIVALENCE  (LQQ(1),IQUEST(11)),(IQQ(1),LQQ(9))
#endif
      CHARACTER*(*) CHDIR(*)
*.___________________________________________
*
*           Current directory refers to a RZ file.
*
      IF(ICHTOP(ICDIR).GT.0)THEN
         IF (ICHTOP(ICDIR).GT.1000 .AND.
     +       ICHLUN(ICDIR).NE.0) THEN
#if defined(CERNLIB_CZ)
*
*--- remote file on Piaf server
*
            ISKIN  = MOD(ICHTOP(ICDIR),10000)
            ISKOUT = ICHTOP(ICDIR)/10000
            IF(IZCBUF.EQ.0)GO TO 60
            CHSMPF = 'HRDIR:'
            WRITE(CHSMPF(7:),'(I4)') MAXDIR
            CALL CZPUTA(CHSMPF,IRC)
            CALL HLOGPF(' ',IRC)
            IF (IQUEST(1) .EQ. 0) THEN
               N = 4
               CALL JUMPST(IZCBUF)
               CALL JUMPX2(N,ISTAT)
               IF (ISTAT .NE. 0) GOTO 60
               READ(CHBUF,'(I4)') NDIR
               DO 5 I = 1, NDIR
                  N = 16
                  CALL JUMPX2(N,ISTAT)
                  IF (ISTAT .NE. 0) GOTO 60
                  CHDIR(I) = CHBUF
  5            CONTINUE
            ENDIF
#endif
#if !defined(CERNLIB_CZ)
            CALL HBUG('CZ option not active','HRDIR',0)
            NDIR = 0
#endif
         ELSE
            CALL RZRDIR(MAXDIR,CHDIR,NDIR)
         ENDIF
         GO TO 60
      ENDIF
*
*           Current directory in a GLOBAL section
*
      NCHMAX= LEN(CHDIR(1))
      NCH   = MIN(NCHMAX,16)
#if defined(CERNLIB_VAX)
      IF(ICHTOP(ICDIR).LT.0)THEN
         LOCQ=-LOCF(IQUEST(1))-ICHTOP(ICDIR)
         NDIR=0
         JCDIR=LQQ(LOCQ+1)
         IF(NLPAT.GT.1)THEN
            DO 30 IL=2,NLPAT
               CALL UCTOH(CHPAT(IL),IHDIR,4,16)
               JCDIR=LQQ(LOCQ+JCDIR-1)
   10          IF(JCDIR.EQ.0)GO TO 60
               DO 20 I=1,4
                  IF(IHDIR(I).NE.IQQ(LOCQ+JCDIR+I))THEN
                     JCDIR=LQQ(LOCQ+JCDIR)
                     GO TO 10
                  ENDIF
   20          CONTINUE
   30       CONTINUE
         ENDIF
*
         LF=LQQ(LOCQ+JCDIR-1)
         IF(LF.NE.0)THEN
   40       IF(NDIR.GE.MAXDIR)GO TO 60
            NDIR=NDIR+1
            CHDIR(NDIR)=' '
            CALL UHTOC(IQQ(LOCQ+LF+1),4,CHDIR(NDIR),NCH)
            LF=LQQ(LOCQ+LF)
            IF(LF.NE.0)GO TO 40
         ENDIF
         GO TO 60
      ENDIF
#endif
#if defined(CERNLIB_HMMAP)
      IF(ICHTOP(ICDIR).LT.0)THEN
         IGOFF=-LOCF(LQ(1))-ICHTOP(ICDIR)
         CALL HRDIRM(LQ(IGOFF+1),MAXDIR,CHDIR,NDIR)
         RETURN
      ENDIF
#endif
*
*           Directory is in //PAWC
*
      LF=LQ(LCDIR-1)
      NDIR=0
      IF(LF.NE.0)THEN
   50    IF(NDIR.GE.MAXDIR)GO TO 60
         NDIR=NDIR+1
         CHDIR(NDIR)=' '
         CALL UHTOC(IQ(LF+1),4,CHDIR(NDIR),NCH)
         LF=LQ(LF)
         IF(LF.NE.0)GO TO 50
      ENDIF
*
   60 RETURN
      END
