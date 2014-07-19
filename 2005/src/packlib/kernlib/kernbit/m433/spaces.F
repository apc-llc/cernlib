*
* $Id: spaces.F,v 1.2 2002/03/04 16:20:46 mclareni Exp $
*
* $Log: spaces.F,v $
* Revision 1.2  2002/03/04 16:20:46  mclareni
* Tidy a dubious construct which fails with Sun WS6U1 f77 at opt 3
*
* Revision 1.1.1.1  1996/02/15 17:47:49  mclareni
* Kernlib
*
*
      CHARACTER*(*) FUNCTION SPACES(STR,MSPACE)
C
C CERN PROGLIB# M433    SPACES          .VERSION KERNFOR  4.14  860211
C ORIG.  6/05/86 M.GOOSSENS/DD
C
C-    The function value SPACES returns the character string STR with
C-    leading blanks removed and each occurence of one or more blanks
C-    replaced by NSPACE blanks inside the string STR
C
      CHARACTER*(*) STR
C
      NSPACE = MSPACE
      LENSPA = LEN(SPACES)
      SPACES = ' '
      IF (NSPACE.LT.0) NSPACE = 0
      IBLANK = 1
      ISPACE = 1
  100 INONBL = INDEXC(STR(IBLANK:),' ')
      IF (INONBL.EQ.0) THEN
          SPACES(ISPACE:) = STR(IBLANK:)
                                                    GO TO 999
      ENDIF
      INONBL = INONBL + IBLANK - 1
      IBLANK = INDEX(STR(INONBL:),' ')
      IF (IBLANK.EQ.0) THEN
          SPACES(ISPACE:) = STR(INONBL:)
                                                    GO TO 999
      ENDIF
      IBLANK = IBLANK + INONBL - 1
      SPACES(ISPACE:) = STR(INONBL:IBLANK-1)
      ISPACE = ISPACE + IBLANK - INONBL + NSPACE
      IF (ISPACE.LE.LENSPA)                         GO TO 100
  999 END
