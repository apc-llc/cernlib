CDECK  ID>, NSYNOPT.
      PROGRAM NSYNOPT


      PARAMETER   (NCNAME=32)
      PARAMETER   (NFILES=4)
      CHARACTER    NAME(NFILES)*(NCNAME)


      DATA NAME/ 'PAM     .car      9   2   0  !ff'
     +,          'opt               3  11   0  !ff'
     +,          'read    .cra      1   1   2  !ff'
     +,          'print   .lis      2   4   0  !ff' /
C-                _:.=+=.:_1_:.=+=.:_2_:.=+=.:_3_:
C-                                 (1) (2) (3)

C-        (1) LUNUSE =  1  read,    2  print,
C-                      3  option,  4  cch subst,    >4  file

C-        (2) LUNDES =  1  cradle input
C-                      2  PAM input
C-                      4  printed output
C-                      5  Fortran output
C-                      6  ASM output write-only (T=ATT)
C-                      7  ASM input-output (T=MODIF)
C-                     11  option parameter
C-                     12  control-character substitution parameter

C-        (3) LUNFLG = 0/>0 parameter value EOF not/yes allowed
C-                     2   the cradle file need not exist


      CALL MQINIT

      CALL FLPARA (NFILES,NAME,
     +     'Nysynopt Help,Eject,Start, Individual,Missing,' //
     +     ' Xact,Ycde,Zseq,0->4 pg size')

      CALL EXSYNOPT

      END
