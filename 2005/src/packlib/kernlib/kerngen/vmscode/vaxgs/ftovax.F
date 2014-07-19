*
* $Id: ftovax.F,v 1.1.1.1 1996/02/15 17:50:25 mclareni Exp $
*
* $Log: ftovax.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:25  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      SUBROUTINE FTOVAX (CHFILE,NN)
C
C CERN PROGLIB# Z267    FTOVAX          .VERSION KERNVAX  2.46  940913
C ORIG. 20/08/90 JZ, modif. 4/5/94 JZ change of specs:
C
C-    Convert file-name syntax UNIX -> VAX :
C-    UNIX:   //node/=log/dir/a/b/c/file.ext;cy
C-       or    //node/log/dir/a/b/c/file.ext;cy
C-    gives:   node::log:[dir.a.b.c]file.ext;cy

C-    UNIX:   /=log/dir/a/b/c/file.ext;cy
C-       or    /log/dir/a/b/c/file.ext;cy
C-    gives:   log:[dir.a.b.c]file.ext;cy

C-    UNIX:   /(dir/a/b/c/file.ext;cy
C-    gives:   [dir.a.b.c]file.ext;cy

C-    UNIX:      a/b/c/file.ext;cy
C-    gives:   [.a.b.c]file.ext;cy

C-    UNIX:      ../a/b/c/file.ext;cy
C-    gives:     [-.a.b.c]file.ext;cy

C-    UNIX:           ~/a/b/c/f.e    or          ~/f.e
C-    gives   log:[name.a.b.c]f.e        log:[name]f.e

C-    return NDSLAT = 1  converted
C-                    0  no conversion
C-                   -1  error

      CHARACTER       CHFILE*(*)
      CHARACTER       CHNAME*512,     CHWK*512
      COMMON /CHSLAT/ CHNAME, CHWK
      CHARACTER       COLNA(512)*1,   COLWK(512)*1
      EQUIVALENCE    (COLNA,CHNAME), (COLWK,CHWK)
      COMMON /SLATE/ NDSLAT, ISLATE(39)

      NU = NN
      IF (NU.LE.0)                 GO TO 91
      IF (NU.GE.500)               GO TO 91
      CHNAME(1:NU) = CHFILE(1:NU)
      CALL CLEFT (CHNAME,1,NU)
      NU = NDSLAT

      JSLA  = ICFIND ('/', CHNAME,1,NU)

      IF (COLNA(1).NE.'~')  THEN
          IF (JSLA.GT.NU)          GO TO 90
        ENDIF

      NP = 0
      JT = 1
      IF (COLNA(1).EQ.'~')         GO TO 31
      IF (COLNA(1).EQ.'.')         GO TO 51
      IF (COLNA(1).NE.'/')         GO TO 61
      IF (COLNA(2).EQ.'/')         GO TO 24
      IF (COLNA(2).EQ.'(')         GO TO 41
      GO TO 26

C----         Handle  //node

   24 JSLA = ICFIND ('/', CHNAME,3,NU)
      N    = JSLA - 1
      IF (N.LE.2)                  GO TO 91

      CHWK(1:N) = CHNAME(3:N) // '::'
      NP = N
      JT = JSLA
      IF (JT.GE.NU)                GO TO 29

C----         Handle  /=logical  or  /logical

   26 JT = JT + 1
      IF (COLNA(JT).EQ.'=')  JT= JT + 1
      JSLA = ICFIND ('/', CHNAME,JT,NU)
      N    = JSLA - JT
      IF (N.LE.0)                  GO TO 91

      CHWK(NP+1:NP+N) = CHNAME(JT:JT+N-1)
      NP = NP + N + 1
      COLWK(NP) = ':'

      JT = JSLA
      IF (JT.GE.NU)                GO TO 29

      JSLN = ICFIND ('/', CHNAME,JT+1,NU)
      IF (JSLN.GT.NU)  JT= JT + 1

      N = NU+1 - JT
      IF (N.GT.0)  THEN
          CHWK(NP+1:NP+N) = CHNAME(JT:JT+N-1)
          NP = NP + N
        ENDIF

   29 CHNAME(1:NP) = CHWK(1:NP)
      NU = NP
      JSLA  = ICFIND ('/', CHNAME,1,NU)
      IF (JSLA.GT.NU)              GO TO 69
      GO TO 67

C----         Handle ~/something

   31 IF (COLNA(2).EQ.'/')  THEN
          JT = 2
        ELSE
          COLNA(1) = '/'
          JT = 1
        ENDIF

      CALL GETENVF ('HOME',CHWK(1:64))
      NP = NDSLAT

      JSLA  = ICFIND ('[', CHWK,1,NP)
      IF (JSLA.GE.NP)              GO TO 91
      NP = LNBLNK (CHWK(1:NP))
      IF (COLWK(NP-1).EQ.'.')  NP= NP - 1
      N  = NU+1 - JT
      CHWK(NP:NP+N) = CHNAME(JT:JT+N-1)
      NU = NP + N
      CHNAME(1:NU) = CHWK(1:NU)
      GO TO 67

C----         Handle  /(dir/a/b/f.e

   41 JSLA = JT
      COLNA(JT+1) = ' '
      GO TO 67

C----         Handle  .something

   51 COLWK(1) = '/'
      CALL CFILL ('-', CHWK,2,NU)

      NP = 1
      JT = 1
   53 JSLN  = ICFIND ('/', CHNAME,JT,NU)
      N = JSLN - JT - 1
      IF (N.EQ.0)                  GO TO 56

      DO 54  J=JT+1,JT+N
      IF (COLNA(J).NE.'.')         GO TO 91
   54 CONTINUE

      NP = NP + N
   56 JT = JT + N + 2
      IF (JT.LT.NU)  THEN
          IF (COLNA(JT).EQ.'.')    GO TO 53
        ENDIF

      JT = JT - 1
      N  = NU+1 - JT
      IF (N.GT.0)  THEN
          CHWK(NP+1:NP+N) = CHNAME(JT:JT+N-1)
          NP = NP + N
        ENDIF
      IF (NP.LT.3)                 GO TO 91

      CHNAME(1:NP) = CHWK(1:NP)
      NU   = NP
      JSLA = 1
      GO TO 67

C----         Handle  a/b/c/f.e

   61 CHWK(1:NU+2) = '[.' // CHNAME(1:NU)
      NU = NU + 2
      CHNAME(1:NU) = CHWK(1:NU)
      JSLA  = 1

C--       translate / to .

   67 JSLE = ICFILA ('/', CHNAME,JSLA+1,NU)
      IF (JSLE.GT.NU)              GO TO 91
      CALL CTRANS ('/', '.', CHNAME,JSLA+1,JSLE-1)
      COLNA(JSLA) = '['
      COLNA(JSLE) = ']'
      IF (CHNAME(1:2).EQ.'[]')  CHNAME(1:2)= '  '
   69 CALL CLEFT (CHNAME,1,NU)
      NU = NDSLAT
      IF (NU.EQ.0)                 GO TO 91
      IF (NU.GT.LEN(CHFILE))       GO TO 91
C     PRINT 9869, CHNAME(1:NU)
C9869 FORMAT (' TO_VAX delivers >',A,'<')
      NN = NU
      CHFILE(1:NU) = CHNAME(1:NU)
      NDSLAT = 1
      RETURN

   90 NDSLAT = 0
      RETURN

   91 NDSLAT = -1
      RETURN
      END
