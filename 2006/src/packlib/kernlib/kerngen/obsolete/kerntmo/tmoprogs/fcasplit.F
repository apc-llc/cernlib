*
* $Id: fcasplit.F,v 1.1.1.1 1996/02/15 17:53:49 mclareni Exp $
*
* $Log: fcasplit.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:49  mclareni
* Kernlib
*
*
#include "kerntmo/pilot.h"
      PROGRAM FCASPLIT
 
      PARAMETER    (MXLENA=64)
      PARAMETER    (MXLENO=24)
 
      DIMENSION    NOPTV(3)
      EQUIVALENCE (NOPTF,NOPTV(1))
      EQUIVALENCE (NOPTC,NOPTV(2))
      EQUIVALENCE (NOPTA,NOPTV(3))
 
      CHARACTER    CHFIN*(MXLENA), CHFSH*(MXLENA)
      CHARACTER    CHLINE*80, CHNAME*80
      CHARACTER    CHIDF*8, CHIDC*8, CHIDA*8, CHIDX*4
 
      PARAMETER    (CHIDF = 'CDECK  I' )
      PARAMETER    (CHIDC = '/*DECK I' )
      PARAMETER    (CHIDA = '|DECK  I' )
      PARAMETER    (CHIDX =         'D>, ' )
 
      CHARACTER    CHOPT(3)*(MXLENO)
      CHARACTER    CHUSE*1
 
      DATA  CHOPT(1) / '-c -Og -w'  /
      DATA  CHOPT(2) / '-c' /
      DATA  CHOPT(3) / ' ' /
 
 
      PRINT 9001
 9001 FORMAT (' FCASPLIT executing.')
      NARGS = IARGC()
      IF  (NARGS.EQ.0)  THEN
          PRINT 9002
 9002 FORMAT (1X
     +/' FCASPLIT x.y optf optc opts   splits a file x.y containing'
     +/' a mixture of Fortran/C/assembler routines into separate'
     +/' files n.f or n.c or n.s, n being the name of each routine,'
     +/' creating at the same time a shell script x.sh which can'
     +/' be executed to compile all routines separately.'
     +/' Parameters 2,3,4 are the option-strings to be used for'
     +/' the calls to the Fortran/C/assembly compilers.'
     +/1X
     +/' Each routine must start with an identifying line :'
     +/ '    "CDECK  ID>, "   in cols.  1-12  for Fortran'
     +/ '    "/*DECK ID>, "   in cols.  1-12  for C'
     +/ '    "|DECK  ID>, "   in cols.  1-12  for assembler'
     +/ '    "n"              in cols. 13-40  gives the name'
     +/ '                     symbols . | < # ! all terminate'
     +/ '                     symbol  */        also  terminates'
     +/1X)
          STOP 'no execution'
        ENDIF
 
C------            Acquire the parameters
 
      CALL GETARG (1,CHFIN)
 
      N  = LNBLNK (CHFIN)
      JA = 1
      JE = N
      J  = N
   16 IF (CHFIN(J:J).EQ.'/')  THEN
          JA = J + 1
          GO TO 17
        ELSEIF (CHFIN(J:J).EQ.'.')  THEN
          IF (JE.EQ.N)  JE = J - 1
        ENDIF
      J = J - 1
      IF (J.NE.0)              GO TO 16
   17 CHFSH = CHFIN(JA:JE) // '.sh'
 
      DO  18  JJ=2,NARGS
      CALL GETARG (JJ,CHOPT(JJ-1))
   18 CONTINUE
 
      PRINT 9014, CHFIN(1:LNBLNK(CHFIN))
      PRINT 9015, CHFSH(1:LNBLNK(CHFSH))
      PRINT 9016, CHOPT(1)(1:LNBLNK(CHOPT(1)))
      PRINT 9017, CHOPT(2)(1:LNBLNK(CHOPT(2)))
      PRINT 9018, CHOPT(3)(1:LNBLNK(CHOPT(3)))
 9014 FORMAT (5X,'       Input file : ',A)
 9015 FORMAT (5X,'     Shell script : ',A)
 9016 FORMAT (5X,'  Fortran options : ',A)
 9017 FORMAT (5X,'       cc options : ',A)
 9018 FORMAT (5X,' Assembly options : ',A)
 
      DO  24  JJ=1,3
      NOPTV(JJ) = LNBLNK (CHOPT(JJ)) + 1
   24 CONTINUE
 
C--                Open input and .sh file
 
   28 OPEN (11, FILE=CHFIN, STATUS='OLD')
      OPEN (21, FILE=CHFSH, STATUS='UNKNOWN')
      REWIND 11
      REWIND 21
 
C--                Read next line
      ISTART = -12
      NROUT  = 0
      NLINES = 0
   31 READ (11,8031,END=71) CHLINE
 8031 FORMAT (A)
 
      NCH = LNBLNK (CHLINE)
      NCH = MAX (NCH,1)
 
   34 IF (CHLINE(9:12).NE.CHIDX)  GO TO 35
          JTYPE = 1
          IF (CHLINE(1:8).EQ.CHIDF)  GO TO 41
          IF (CHLINE(1:8).EQ.CHIDC)  GO TO 42
          IF (CHLINE(1:8).EQ.CHIDA)  GO TO 43
   35 IF (ISTART.NE.0)       GO TO 37
   36 WRITE (22,8031) CHLINE(1:NCH)
      NLINES = NLINES + 1
      GO TO 31
 
C--                Leading unheaded lines
 
   37 IF (ISTART.EQ.-1)      GO TO 31
      PRINT 9037, CHLINE(1:NCH)
 9037 FORMAT (' ignored: ',A)
      ISTART = ISTART + 1
      GO TO 31
 
C----              Start new routine
 
   43 JTYPE = JTYPE + 1
   42 JTYPE = JTYPE + 1
   41 ISTART = 0
      NROUT  = NROUT + 1
 
      JPUT = 0
      NUS  = MIN(NCH,40)
      DO  44  JTK=13,NUS
      CHUSE = CHLINE(JTK:JTK)
      JV = ICHAR(CHUSE)
      IF (JV.EQ.32)          GO TO 44
      IF (JV.EQ.46)          GO TO 51
      IF (JV.EQ.124)         GO TO 51
      IF (JV.EQ.60)          GO TO 51
      IF (JV.EQ.35)          GO TO 51
      IF (JV.EQ.33)          GO TO 51
      IF (JV.EQ.42)  THEN
          JN = ICHAR (CHLINE(JTK+1:JTK+1))
          IF (JN.EQ.47)      GO TO 51
        ENDIF
      IF (JV.LT.91)  THEN
          IF (JV.GE.65)  CHUSE = CHAR(JV+32)
        ENDIF
      JPUT = JPUT + 1
      CHNAME(JPUT:JPUT) = CHUSE
   44 CONTINUE
 
   51 NPUT = JPUT + 2
 
      IF       (JTYPE.EQ.1)  THEN
          CHNAME(NPUT-1:NPUT) = '.f'
          WRITE (21,9052)  'f77',CHOPT(1)(1:NOPTF),CHNAME(1:NPUT)
        ELSEIF (JTYPE.EQ.2)  THEN
          CHNAME(NPUT-1:NPUT) = '.c'
          WRITE (21,9052)  'cc',CHOPT(2)(1:NOPTC),CHNAME(1:NPUT)
        ELSEIF (JTYPE.EQ.3)  THEN
          CHNAME(NPUT-1:NPUT) = '.s'
          WRITE (21,9052)  'as',CHOPT(3)(1:NOPTA),CHNAME(1:NPUT)
        ENDIF
 9052 FORMAT (A,2X,A,2X,A)
 
      CLOSE (22)
      OPEN  (22, FILE=CHNAME(1:NPUT),STATUS='UNKNOWN')
      REWIND 22
      PRINT 9054, NROUT,CHNAME(1:NPUT)
 9054 FORMAT (' make',I4,1X,A)
      GO TO 36
 
C----              Done
 
   71 PRINT 9071, NLINES
 9071 FORMAT (1X,I6,' lines written.')
      STOP
      END
