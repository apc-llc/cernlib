CDECK  ID>, FLNAME.
      SUBROUTINE FLNAME (IXNAME, IFLAPP)

C-    Ready the file name for use in OPEN or UNLINKF
C-
C-        IXNAME = -1 use file name pointed to by IXFLUN
C-                  0 use file name as is in CHLIFI
C-                 >0 use file name pointed to by IXNAME
C-
C-    return IFLAPP = 0/1 if no/yes append mode
C.    started 16-dec-93

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      PARAMETER   (NSLINE=256, NSTXKR=192)
      CHARACTER    IDPROG*8, LINECC*(NSLINE)
      CHARACTER    CHTERM*4, TXKR*(NSTXKR), TXKEY*8, CHWORK*4
      COMMON /FLKRAC/NLCUM, JLINC,NLINT, IXHOME, JFAULT,IFLLIT
     +,              IDPROG, LINECC, CHTERM, TXKR,TXKEY, CHWORK
      PARAMETER     (NSLIFI=128)
      CHARACTER      CHLIFI*(NSLIFI), CHLIEX*8
      COMMON /FLINKC/LUNOP,LUNFD,LUNOLD,LUNSIZ
     +,              IXFLUN, NLIFI,CHLIFI,CHLIEX
C--------------    End CDE              --------------------------------


      IF (IXNAME.GE.0)  IXFLUN= IXNAME
      IF (IXFLUN.GT.0)  THEN
          CHLIFI = ' '
          CALL NA_GET (IXFLUN, CHLIFI, 1)
          NLIFI = NDSLAT
        ENDIF

      IFLAG = 0
      IF (CHLIFI(1:1).EQ.' ')      GO TO 23
   21 IF (CHLIFI(1:1).EQ.'=')      GO TO 22
      IF (CHLIFI(1:1).EQ.':')      GO TO 22
      IF (CHLIFI(1:2).EQ.'/:')     GO TO 31
      IF (CHLIFI(1:1).NE.'+')      GO TO 41
      IFLAG = 1

   22 CHLIFI(1:1) = ' '
   23 CALL CLEFT  (CHLIFI,1,NLIFI)
      NLIFI = NDSLAT
      GO TO 21

C--           file name as is

   31 CHLIFI(1:2) = '  '
      CALL CLEFT  (CHLIFI,1,NLIFI)
      NLIFI = NDSLAT
      GO TO 47

   41 IF (CHLIFI(1:2).EQ.'~/')  THEN
          CALL NA_GET (IXHOME, TXKR, 1)
          N = NDSLAT
          TXKR(N+1:N+NLIFI-2) = CHLIFI(3:NLIFI)
          NLIFI = N + NLIFI - 2
          CHLIFI(1:NLIFI) = TXKR(1:NLIFI)
        ENDIF
   47 IFLAPP = IFLAG
      RETURN
      END
