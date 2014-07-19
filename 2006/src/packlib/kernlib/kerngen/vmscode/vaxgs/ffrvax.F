*
* $Id: ffrvax.F,v 1.1.1.1 1996/02/15 17:50:25 mclareni Exp $
*
* $Log: ffrvax.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:25  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      SUBROUTINE FFRVAX (CHFILE,NN)
C
C CERN PROGLIB# Z267    FFRVAX          .VERSION KERNVAX  2.46  940913
C ORIG. 20/08/90 JZ, modif. 4/5/94 JZ change of specs:
C     disk:[dir...  gives now  /disk/dir/.. and no longer /=disk/dir/..
C-    [dir...       gives now  /(dir/..     and not       /dir/..
C
C-    Convert file-name syntax VAX -> UNIX :
C-    VAX:     node::disk:[dir.a.b.c]file.ext;cy
C-    gives:   //node/disk/dir/a/b/c/file.ext;cy

C-    return NDSLAT = 1  converted
C-                    0  no conversion
C-                   -1  unknown syntax

      CHARACTER       CHFILE*(*)
      CHARACTER       CHNAME*512,     CHWK*512
      COMMON /CHSLAT/ CHNAME, CHWK
      CHARACTER       COLNA(512)*1,   COLWK(512)*1
      EQUIVALENCE    (COLNA,CHNAME), (COLWK,CHWK)
      COMMON /SLATE/ NDSLAT, ISLATE(39)

      NU  = NN
      IF (NU.LE.0)                 GO TO 91
      IF (NU.GE.500)               GO TO 91
      CHNAME(1:NU) = CHFILE(1:NU)

      J  = ICFIND ('/', CHNAME,1,NU)
      IF (J.LE.NU)                 GO TO 90

C--       replace .][  or ][

   16 J = INDEX (CHNAME(1:NU), '.][')
      IF (J.NE.0)  THEN
          CHNAME(J+1:J+2)= '  '
          GO TO 16
        ENDIF

   17 J = INDEX (CHNAME(1:NU), '][')
      IF (J.NE.0)  THEN
          CHNAME(J:J+1)= '. '
          GO TO 17
        ENDIF

      CALL CLEFT (CHNAME,1,NU)
      NU = NDSLAT

C--       find [ and ], they must both be absent or present

      JCOLN = ICFIND (':', CHNAME,1,NU)
      JOPEN = ICFIND ('[', CHNAME,1,NU)
      IF (JOPEN.LE.NU)  THEN
          JCLOS = ICFIND (']', CHNAME,JOPEN,NU)
        ELSE
          JOPEN = ICFIND ('<', CHNAME,1,NU)
          JCLOS = ICFIND ('>', CHNAME,JOPEN,NU)
        ENDIF

      IF     (JOPEN.GT.NU)  THEN
          IF (JCLOS.LE.NU)         GO TO 91
        ELSE
          IF (JCLOS.GT.NU)         GO TO 91
        ENDIF

      NP = 0
      JT = 1
      IF (JCOLN.GE.JOPEN)          GO TO 41

C--           Handle  node::  -> //node

      IF (COLNA(JCOLN+1).NE.':')   GO TO 24
      NP = JCOLN + 1
      IF (NP.LE.2)                 GO TO 91
      CHWK(1:NP) = '//' // CHNAME(1:NP-2)

      JT  = JCOLN + 2
      IF (JT.GT.NU)                GO TO 28
      JCOLN = ICFIND (':', CHNAME,JT,NU)
      IF (JCOLN.GE.JOPEN)          GO TO 91

C--           Handle  logical:  -> /logical

   24 NP = NP + 1
      COLWK(NP) = '/'

      N = JCOLN - JT
      IF (N.LE.0)                  GO TO 91
      CHWK(NP+1:NP+N) = CHNAME(JT:JT+N-1)
      NP = NP + N
      JT = JCOLN + 1
      IF (JT.GT.NU)                GO TO 28

      IF (JOPEN.GT.NU)             GO TO 28
      IF (JOPEN.NE.JT)             GO TO 91
      JT = JT + 1
      NP = NP + 1
      COLWK(NP) = '/'
      GO TO 63

C--       copy the trailing text without [ and ]

   28 N  = NU+1 - JT
      IF (N.GT.0)  THEN
          NP = NP + 1
          COLWK(NP) = '/'
          CHWK(NP+1:NP+N) = CHNAME(JT:JT+N-1)
        ENDIF
      NU = NP + N
      CHNAME(1:NU) = CHWK(1:NU)
      GO TO 69

C----         Neither node nor disk

   41 IF (JOPEN.GT.NU)             GO TO 90
      IF (JOPEN.NE.1)              GO TO 91
      IF (COLNA(2).EQ.']')         GO TO 91
      IF (COLNA(2).EQ.'-')         GO TO 51
      IF (COLNA(2).EQ.'.')         GO TO 61

C--           Handle  [a.b.c]f.e  -> /(a/b/c/f.e

      CHWK(1:2) = '/('
      NP = 2
      JT = 2
      GO TO 63

C--           Handle  [---] or [--.a.b]f.e  -> ../../a/b/f.e

   51 CHWK(1:2) = '..'
      NP = 2
      JT = 3

   54 IF (COLNA(JT).EQ.']')        GO TO 63
      IF (COLNA(JT).EQ.'.')        GO TO 63
      IF (COLNA(JT).NE.'-')        GO TO 91
      CHWK(NP+1:NP+3) = '/..'
      NP = NP + 3
      JT = JT + 1
      IF (JT.GT.NU)                GO TO 91
      GO TO 54

C--           Handle  [.a.b]f.e  -> a/b/f.e

   61 JT = 3

C--       copy the trailing text

   63 N  = NU+1 - JT
      IF (N.GT.0)  CHWK(NP+1:NP+N) = CHNAME(JT:JT+N-1)
      NU = NP + N
      CHNAME(1:NU) = CHWK(1:NU)
      JR = NP

C--       translate  . or ]  to /

   66 JR = JR + 1
      IF (COLNA(JR).EQ.'.')   COLNA(JR) = '/'
      IF (COLNA(JR).NE.']')        GO TO 66
      COLNA(JR) = '/'

   69 IF (NU.GT.LEN(CHFILE))       GO TO 91
C     PRINT 9869, CHNAME(1:NU)
C9869 FORMAT (' FR_VAX delivers >',A,'<')
      NN = NU
      CHFILE(1:NU) = CHNAME(1:NU)
      NDSLAT = 1
      RETURN

   90 NDSLAT = 0
      RETURN

   91 NDSLAT = -1
      RETURN
      END
