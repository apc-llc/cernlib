$! ***************************************************************
$! *                                                             *
$! * DZEDIT.COM v1.03                                            *
$! *                                                             *
$! * User Interface to DZEDIT                                    *
$! * Author: M.Marquina 92/02/11                                 *
$! *                                                             *
$! * Mods       Date   Comments                                  *
$! * MARQUINA 93/12/02 Add /DISPLAY, fix /TCPIP                  *
$! * MARQUINA 93/10/25 New location of GKS-based modules         *
$! * MARQUINA 92/07/22 There is no TCP/IP version                *
$! *                                                             *
$! *************************************************************** 
$ SAY :== WRITE/SYMBOL SYS$OUTPUT
$ blk=" "
$                                        ver="PRO"
$      If F$TYPE(CERN_LEVEL).nes."" Then ver =CERN_LEVEL
$! --- Find out if GKS is available to choose correct default driver
$ RDIR  =F$TRNLNM("CERN",,0)
$ gkslib=RDIR-"]"+"gks."+ver+".lib]gks.olb"
$                                 drv="GKS"
$ If F$SEARCH(gkslib).eqs."" Then drv="X11"
$
$      popt=" "
$      do  =" "
$
$ SHNAME=F$ENVIRONMENT("PROCEDURE")
$ SHDIR=F$PARSE(SHNAME,,,"DEVICE")+F$PARSE(SHNAME,,,"DIRECTORY")
$
$ If F$SEARCH("''SHDIR'F$TCPIP.COM").eqs.""
$ Then Say "You need the new function F$TCPIP.COM from CERN"
$      Exit
$ Endif
$ @'SHDIR'F$TCPIP
$ tcpvs=F$ELEMENT(0," ",TCPIP_ENV)
$ trs  =F$ELEMENT(1," ",TCPIP_ENV)
$
$
$ If p2.nes.""
$ Then no=0
$      O_DO:
$      no=no+1
$      opt=F$ELEMENT(no,"/",p2)
$      If opt.eqs."/" Then Goto O_END
$           add=1
$      If F$LOCATE(opt,"PRO NEW OLD").lt.11
$      Then ver=opt
$           add=0
$      Endif
$      If F$LOCATE(opt,"GKS DGKS X11").lt.12
$      Then drv=opt
$           add=0
$      Endif  
$      If F$EXTRACT(0,4,opt).eqs."HOST"
$      Then node=F$EXTRACT(5,F$LENGTH(opt)-5,opt)
$           add=0
$      Endif
$      If F$EXTRACT(0,7,opt).eqs."DISPLAY"
$      Then node=F$EXTRACT(8,F$LENGTH(opt)-8,opt)
$           add=0
$      Endif
$      If opt.eqs."DECNET"
$      Then trs="DECNET"
$           add=0
$      Endif 
$      If opt.eqs."TCPIP" 
$      Then add=0
$           trs="TCPIP"
$      Endif   
$      If opt.eqs."TEST"
$      Then do ="Say "
$           add=0
$      Endif
$      If add.eq.1 Then popt=popt+"/"+opt
$      Goto O_DO
$      O_END:
$ Endif
$ DISPLAY_ON=0
$ If drv.eqs."X11".or.drv.eqs."PP"
$ Then If F$TYPE(node).eqs."".and.F$TRNLNM("DECW$DISPLAY").eqs."" 
$      Then node=F$ELEMENT(3," ",TCPIP_ENV)
$           trs =F$ELEMENT(4," ",TCPIP_ENV)
$      Endif
$      If F$TYPE(node).nes."" Then If node.nes."-" Then DISPLAY_ON=1
       If DISPLAY_ON Then set display/create/transport='trs'/node='node'
$      If trs.eqs."DECNET" Then tcpvs=trs
$ Endif
$                                             tcppg=""
$!If tcpvs.nes."DECNET".and.drv.nes."PP" Then tcppg="_"+F$EXTRACT(0,1,tcpvs)
$
$                       pgm=RDIR-"]"+ver+"."
$ If drv.eqs."GKS" Then pgm=pgm+"gks"
$ pgm=pgm+"exe]dze"+drv+tcppg+".exe"
$ If F$SEARCH(pgm).eqs.""
$ Then Say blk
$      Say blk,"The ",ver," version of DZEDIT-",drv,"/",tcpvs
$      Say blk,"is not available on this system"
$      Say blk 
$      Exit
$ Else If ver.nes."PRO"
$ Then Say blk
$      Say blk,"Calling ",ver," version of DZEDIT-",drv,"/",tcpvs,"..."
$      Say blk
$ Endif
$ Endif
$ pawpgm :== "$"'pgm''popt'
$ deassign sys$input
$ 'do' pawpgm
$ deletex/sym/global pawpgm
$ Exit
