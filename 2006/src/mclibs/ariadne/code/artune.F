C***********************************************************************
C $Id: artune.F,v 1.2 1996/04/10 12:33:42 mclareni Exp $

      SUBROUTINE ARTUNE(SET)

C...ARiadne subroutine TUNE

C...Sets parameters and switches in Ariadne and other programs which
C...Ariadne runs with.

#include "arimpl.f"
#include "ardat1.f"
#include "arhide.f"
#include "ludat1.f"
#include "leptou.f"
#include "pypars.f"

      CHARACTER SET*(*)


      PARA(1)=0.22
      PARA(2)=0.2
      PARA(3)=0.6
      PARA(4)=1.0/137.0
      PARA(5)=0.6
      PARA(6)=-1.0
      PARA(7)=0.0
      PARA(8)=0.0
      PARA(9)=0.0
      PARA(10)=1.0
      PARA(11)=0.6
      PARA(12)=0.6
      PARA(13)=0.6
      PARA(14)=1.0
      PARA(15)=1.0
      PARA(16)=-1.0
      PARA(17)=2.0
      PARA(18)=1.0
      PARA(19)=0.001
      PARA(20)=0.0
      PARA(21)=1.0
      PARA(22)=0.0
      PARA(23)=0.0
      PARA(24)=0.0
      PARA(25)=2.0
      PARA(26)=9.0
      PARA(27)=0.6
      PARA(28)=0.0
      PARA(29)=0.0
      PARA(30)=0.0
      PARA(31)=25.0
      PARA(32)=0.0
      PARA(33)=0.0
      PARA(34)=0.0
      PARA(35)=0.0
      PARA(36)=0.0
      PARA(37)=0.0
      PARA(38)=0.0
      PARA(39)=0.001
      PARA(40)=1.0E32

      MSTA(3)=0
      MSTA(6)=-1
      MSTA(11)=0
      MSTA(12)=1
      MSTA(14)=1
      MSTA(15)=5
      MSTA(16)=2
      MSTA(17)=3
      MSTA(18)=3
      MSTA(19)=2
      MSTA(20)=0
      MSTA(21)=0
      MSTA(22)=1
      MSTA(23)=1
      MSTA(24)=2
      MSTA(25)=1
      MSTA(26)=0
      MSTA(27)=0
      MSTA(28)=0
      MSTA(29)=0
      MSTA(30)=3
      MSTA(31)=1
      MSTA(32)=2
      MSTA(33)=1
      MSTA(34)=2
      MSTA(35)=0
      MSTA(36)=2
      MSTA(37)=1

      PHAR(101)=-1.0
      PHAR(102)=-1.0
      PHAR(103)=1.0
      PHAR(104)=1.0
      PHAR(105)=0.0
      PHAR(107)=-1.0
      PHAR(109)=2.0

      MHAR(101)=2
      MHAR(102)=2
      MHAR(103)=1
      MHAR(104)=0
      MHAR(106)=0
      MHAR(107)=0
      MHAR(108)=0
      MHAR(109)=0
      MHAR(110)=0
      MHAR(111)=1
      MHAR(112)=-1
      MHAR(113)=1
      MHAR(115)=0
      MHAR(116)=0
      MHAR(117)=0
      MHAR(118)=0
      MHAR(120)=1
      MHAR(123)=0
      MHAR(128)=1
      MHAR(130)=1
      MHAR(131)=1
      MHAR(132)=1
      MHAR(133)=9
      MHAR(134)=1

      PHAR(101)=-1.0
      PHAR(102)=-1.0
      PHAR(103)=1.0
      PHAR(104)=1.0
      PHAR(105)=0.0
      PHAR(107)=-1.0

      PARP(85)=0.0

      IF (SET.EQ.'4.04') THEN
        PARA(19)=0.0
        PARA(25)=0.0
        MSTA(19)=1
        MSTA(22)=0
        MSTA(23)=0
        MSTA(25)=0
        MSTA(26)=0
        MSTA(30)=1
        MSTA(32)=0
        MSTA(33)=0
        MSTA(36)=0
        MSTA(37)=0
        MSTA(38)=0
        MSTA(39)=0
        MSTA(40)=0
        MHAR(101)=0
        MHAR(103)=0
        MHAR(111)=0
        MHAR(120)=0
        MHAR(128)=0
        MHAR(130)=0
        MHAR(131)=0
        MHAR(132)=0
        MHAR(133)=0
        MHAR(134)=0
        MSTJ(11)=1
        PARJ(41)=0.23
        PARJ(42)=0.34
        PARJ(21)=0.405
        PARL(3)=0.47
        WRITE(MSTA(7),1000) 'version 4.04 of the EMC/DELPHI'
      ELSEIF (SET.EQ.'4.05') THEN
        PARA(19)=-1.0
        PARA(21)=0.25
        PARA(25)=0.0
        MSTA(19)=2
        MSTA(30)=1
        MSTA(32)=1
        MSTA(36)=0
        MSTA(37)=0
        MHAR(101)=0
        MHAR(103)=0
        MHAR(111)=0
        MHAR(120)=0
        MHAR(128)=0
        MHAR(130)=0
        MHAR(131)=0
        MHAR(132)=0
        MHAR(133)=0
        MHAR(134)=0

        MSTJ(11)=1
        PARJ(41)=0.23
        PARJ(42)=0.34
        PARJ(21)=0.405
        PARL(3)=0.6
        WRITE(MSTA(7),1000) 'version 4.05 of the EMC/DELPHI'
      ELSEIF (SET.EQ.'4.06') THEN
        PARA(21)=0.25
        PARA(25)=0.0
        MSTA(19)=2
        MSTA(30)=1
        MSTA(36)=0
        MSTA(37)=0
        MHAR(101)=1
        MHAR(120)=0
        MHAR(128)=0
        MHAR(130)=0
        MHAR(131)=0
        MHAR(132)=0
        MHAR(133)=0

        MSTJ(11)=1
        PARJ(41)=0.23
        PARJ(42)=0.34
        PARJ(21)=0.405
        PARL(3)=0.6
        WRITE(MSTA(7),1000) 'version 4.06 of the EMC/DELPHI'
      ELSEIF (SET.EQ.'ALEPH'.OR.SET.EQ.'aleph') THEN
        PARA(1)=0.218
        PARA(3)=0.58
        PARA(5)=0.58
        PARA(25)=0.0
        MSTA(19)=1
        MSTA(20)=1
        MSTA(30)=1
        MSTA(36)=0
        MSTA(37)=0
        MHAR(101)=1
        MHAR(111)=0
        MHAR(120)=0
        MHAR(128)=0
        MHAR(130)=0
        MHAR(131)=0
        MHAR(132)=0
        MHAR(133)=0
        MHAR(134)=0
        MSTJ(11)=3
        PARJ(41)=0.5
        PARJ(42)=0.81
        PARJ(21)=0.354
        PARJ(54)=-0.05
        PARJ(55)=-0.006
        WRITE(MSTA(7),1000) SET
      ELSEIF (SET.EQ.'DELPHI'.OR.SET.EQ.'delphi') THEN
        PARA(25)=0.0
        MSTA(19)=1
        MSTA(30)=1
        MSTA(36)=0
        MSTA(37)=0
        MHAR(101)=1
        MHAR(120)=0
        MHAR(128)=0
        MHAR(130)=0
        MHAR(132)=0
        MHAR(133)=0
        MHAR(134)=0
        MSTJ(11)=1
        PARJ(41)=0.23
        PARJ(42)=0.34
        PARJ(21)=0.405
        WRITE(MSTA(7),1000) SET
      ELSEIF (SET.EQ.'OPAL'.OR.SET.EQ.'opal') THEN
        PARA(1)=0.20
        PARA(3)=1.0
        PARA(5)=1.0
        PARA(25)=0.0
        MSTA(19)=1
        MSTA(30)=1
        MSTA(36)=0
        MSTA(37)=0
        MHAR(101)=1
        MHAR(111)=0
        MHAR(120)=0
        MHAR(128)=0
        MHAR(130)=0
        MHAR(131)=0
        MHAR(132)=0
        MHAR(133)=0
        MHAR(134)=0
        PARJ(41)=0.18
        PARJ(42)=0.34
        PARJ(21)=0.37
        WRITE(MSTA(7),1000) SET
      ELSEIF (SET.EQ.'A406P04'.OR.SET.EQ.'a406p04'.OR.
     $        SET.EQ.'A406P05'.OR.SET.EQ.'a406p05'.OR.
     $        SET.EQ.'A406P07'.OR.SET.EQ.'a406p07'.OR.
     $        SET.EQ.'A406P09'.OR.SET.EQ.'a406p09'.OR.
     $        SET.EQ.'LEP2GG'.OR.SET.EQ.'lep2gg') THEN
        PARA(21)=1.0
        PARA(25)=0.0
        PARA(27)=0.6
        MSTA(19)=2
        MSTA(30)=3
        MSTA(36)=2
        MSTA(37)=1

        PHAR(109)=2.0
        MHAR(101)=1
        MHAR(111)=0
        MHAR(118)=0
        MHAR(120)=1
        MHAR(123)=1
        MHAR(128)=0
        MHAR(130)=0
        MHAR(131)=0
        MHAR(132)=0
        MHAR(133)=0
        MHAR(134)=0

        MSTJ(11)=1
        PARJ(41)=0.23
        PARJ(42)=0.34
        PARJ(21)=0.405
        PARL(3)=0.6
        WRITE(MSTA(7),1000) 'PRELIMINARY 4.06'
      ELSEIF (SET.EQ.'EMC'.OR.SET.EQ.'emc'.OR.
     $       SET.EQ.'4.07'.OR.SET.EQ.'4.08') THEN
        PARA(21)=1.0
        PARA(25)=2.0
        PARA(26)=9.0
        PARA(27)=0.6
        PARA(28)=0.0
        MSTA(19)=2
        MSTA(22)=1
        MSTA(30)=3
        MSTA(35)=0
        MSTA(36)=2
        MSTA(37)=1

        PHAR(109)=2.0
        MHAR(101)=2
        MHAR(111)=1
        MHAR(118)=0
        MHAR(120)=1
        MHAR(123)=0
        MHAR(128)=1
        MHAR(130)=1
        MHAR(131)=1
        MHAR(132)=1
        MHAR(133)=9
        MHAR(134)=1

        MSTJ(11)=1
        PARJ(41)=0.23
        PARJ(42)=0.34
        PARJ(21)=0.405
        PARL(3)=0.6
        PARP(85)=0.0
        WRITE(MSTA(7),1000) 'EMC/DELPHI'
      ELSE
        WRITE(MSTA(7),1010) SET
      ENDIF

 1000 FORMAT(7x,'Parameters and switches initialized using the "',A,
     $     '" tuning set')
 1010 FORMAT('Tuning set "',A,'" does not exist. Parameters and',
     $     ' switches retains their default value')

      RETURN

C**** END OF ARTUNE ****************************************************
      END
