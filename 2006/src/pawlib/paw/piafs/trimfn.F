*-- Author :    Dino F.M. 25/11/96
      SUBROUTINE TRIMFN(CHCMD)
************************************************************************
*                                                                      *
* Remove the path part from filenames enclosed in double quotes.       *
* This accounts for the PIAF Fortran files being always pushed in      *
* the home directory.                                                  *
*                                                                      *
************************************************************************

      CHARACTER*(*) CHCMD
      CHARACTER*256 CHCMD2
      CHARACTER CHA1
      INTEGER ISOURCE,IDEST,INQUOTE,ISKIP,LENCMD

      IF (INDEX (CHCMD,'"').NE.0) THEN
*
*------- At least a double quote in the command...
*
         INQUOTE = 0
         ISKIP = 0
         IDEST = 256
         LENCMD = LENOCC (CHCMD)
         DO 100 ISOURCE =  LENCMD , 1, -1
            CHA1 = CHCMD (ISOURCE:ISOURCE)
            IF (CHA1.NE.'"') THEN
               IF (ISKIP.EQ.0) THEN
                  IF (INQUOTE.EQ.0) THEN
                     CHCMD2 (IDEST:IDEST) = CHA1
                     IDEST = IDEST - 1
                  ELSE
                     IF ((CHA1.EQ.'/').OR.(CHA1.EQ.']')) THEN
                        ISKIP = 1
                     ELSE
                        CHCMD2 (IDEST:IDEST) = CHA1
                        IDEST = IDEST - 1
                     ENDIF
                  ENDIF
               ENDIF
            ELSE
*------------- Quote found. Can be an Open Quote or a Close Quote
               IF (INQUOTE.EQ.0) THEN
                  INQUOTE = 1
               ELSE
                  INQUOTE = 0
                  ISKIP = 0
               ENDIF
            ENDIF
 100     CONTINUE
         CHCMD = CHCMD2 (IDEST+1:)
      ENDIF
      END

      SUBROUTINE RMQUOTE(CHCMD)
************************************************************************
*                                                                      *
* If the NT command  contain a '!' parameter, suppress the quotes.     *
* This is a legacy of PAW V2.06 that puts quotes around one of the     *
* default parameters.                                                  *
*                                                                      *
************************************************************************

      CHARACTER*(*) CHCMD
      CHARACTER*256 CHCMD2
      INTEGER ISOURCE

      ISOURCE = INDEX (CHCMD,'''!''')
      IF (ISOURCE.NE.0) THEN
         CHCMD2=CHCMD(:ISOURCE-1)//' ! '//CHCMD(ISOURCE+3:)
         CHCMD = CHCMD2
      ENDIF
      END
