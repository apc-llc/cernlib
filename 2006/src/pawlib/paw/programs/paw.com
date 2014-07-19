$!========================================================================
$!
$! $Id: paw.com,v 1.1 1996/04/25 09:39:34 cernlib Exp $
$! Name      : PAW
$!
$! Purpose   : Invokes the required version of the PAW executable
$!
$! Arguments : See PAW /HELP
$!
$! Created  25-NOV-1994   Julian J. Bunn
$! Mods      7-MAR-1995   Roger Ruber (for UCX)
$!          24-APR-1995   Julian J. Bunn (corrections for batch mode)
$! $Log: paw.com,v $
$! Revision 1.1  1996/04/25 09:39:34  cernlib
$! The paw.com file for VMS
$!
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   all_opts = "''p1' ''p2' ''p3' ''p4' ''p5' ''p6' ''p7' ''p8'"
$!
$! If a site paw_options.com exists, then take override symbols from that ...
$!
$   if f$search("cern_root:[exe]paw_options.com") .nes. "" then @cern_root:[exe]paw_options
$!
$! Otherwise, set them here ...
$!
$   if f$type(root_dir) .eqs. "" then root_dir = f$trnlnm("CERN")-"]"  ! The root of the CERN Program Library Tree, minus it's appendage
$   if f$type(driver_override) .eqs. "" then driver_override = ""      ! "X11" for X, "DGKS" for DEC GKS, "GKS" for GTS-GRAL
$   if f$type(TCP_override) .eqs. "" then TCP_override = ""            ! "_U" for UCX, "_M" for Multinet, "" for not linked with IP
$!                                                                     ! (no IP means no NETWORK menu in PAW, and, e.g. no PIAF access)
$   if f$type(level_override) .eqs. "" then level_override = ""        ! NEW or OLD or PRO or blank
$   if f$type(plusplus_override) .eqs. "" then plusplus_override = ""  ! ++ or blank
$   if f$type(transport_override) .eqs. "" then transport_override = ""! blank or TCPIP or DECNET or WINTCP (to create remote displays)
$   set symbol/scope=local
$!
$! Other symbols used in the procedure
$   paw_choice = ""      ! will be the selected PAW exe
$   paw_available = ""   ! will be list of PAW exe locally available
$   paw_options = ""     ! the options to be passed to the PAW exe
$   node = ""            ! will be the remote node name for remote display
$   transport_type = ""  ! will be transport to remote node
$   display_was_created = "no"
$   display_exists = "no"
$   create_display = "no"
$   help = "no"
$   paw_batch = "no"
$!
$! First extract any overrides set by the user on the command line
$   lopts = f$length(all_opts)
$   lbatch= f$locate("/B",all_opts)
$   lpp   = f$locate("++",all_opts)
$   lip   = f$locate("=TCPIP",all_opts)
$   lde   = f$locate("=DECNET",all_opts)
$   lwi   = f$locate("=WINTCP",all_opts)
$   lnode = f$locate("/NOD",all_opts)
$   lx11  = f$locate("/X",all_opts)
$   ldgks = f$locate("/D",all_opts)
$   lgks  = f$locate("/G",all_opts)
$   lpro  = f$locate("/P",all_opts)
$   lold  = f$locate("/O",all_opts)
$   lnew  = f$locate("/NE",all_opts)
$   lquer = f$locate("/Q",all_opts)
$   lhelp = f$locate("HELP",all_opts)
$   lmult = f$locate("/M",all_opts)
$   lucx  = f$locate("/U",all_opts)
$   lbare = f$locate("/NOT",all_opts)
$   llist = f$locate("/L",all_opts)
$   if lbatch.ne. lopts then paw_batch = "yes"
$   if lx11  .ne. lopts then driver_override = "X11"
$   if ldgks .ne. lopts then driver_override = "DGKS"
$   if lgks  .ne. lopts then driver_override = "GKS"
$   if lnew  .ne. lopts then level_override = "NEW"
$   if lold  .ne. lopts then level_override = "OLD"
$   if lpro  .ne. lopts then level_override = "PRO"
$   if lpp   .ne. lopts then plusplus_override = "PP"
$   if lpp   .ne. lopts then driver_override = ""                           ! driver IS Motif
$   if lpp   .ne. lopts .and. lbare .ne. lopts then plusplus_override = "P" ! temporary fix for PAWP.EXE !
$   if lip   .ne. lopts then transport_override = "TCPIP"
$   if lde   .ne. lopts then transport_override = "DECNET"
$   if lwi   .ne. lopts then transport_override = "WINTCP"
$   if lquer .ne. lopts then help = "yes"
$   if lhelp .ne. lopts then help = "yes"
$   if lmult .ne. lopts then TCP_override = "_M"
$   if lucx  .ne. lopts then TCP_override = "_U"
$   if lbare .ne. lopts then TCP_override = ""
$   if lnode .ne. lopts then create_display = "yes"
$   if lip   .ne. lopts then create_display = "yes"
$   if lde   .ne. lopts then create_display = "yes"
$   if lwi   .ne. lopts then create_display = "yes"
$   if f$mode() .eqs. "BATCH" .or. paw_batch .eqs. "yes"
$   then
$      all_opts = all_opts + "-w 0"
$      create_display = "no"
$   endif
$   if lnode .ne. lopts
$   then
$       topts = f$extract(lnode,999,all_opts)
$       node = f$element(1,"=",topts)
$       node = f$element(0,"/",node)
$   endif
$   cernlib_version = "pro"
$   if f$type(cern_level) .nes. "" then cernlib_version = cern_level
$   if level_override .nes. "" then cernlib_version = level_override
$!
$   if help .eqs. "yes" 
$   then 
$      gosub help_text
$      goto EXIT
$   endif
$!
$   if llist .ne. lopts then gosub select_paw
$!
$get_connection_type:
$   if transport_override .nes. "" then transport_type = transport_override
$   if f$trnlnm("DECW$DISPLAY") .nes. "" then display_exists = "yes"
$   if display_exists .eqs. "yes" .and. create_display .eqs. "no" then goto get_paw_type
$!
$!If the user has not specified /NODE or decnet,tcpip,wintcp transport, then use port settings
$   if create_display .eqs. "yes" then goto set_display
$   if f$mode() .eqs. "BATCH" then goto set_display
$   create_display = "yes"
$   port = f$getdvi("TT","TT_ACCPORNAM")
$   lport = f$length(port)
$   if f$locate("/",port) .ne. lport 
$   then
$!Terminal cannot be used to get remote machine on user's behalf
$      goto get_paw_type
$   endif
$   if f$locate("::",port) .ne. lport
$   then
$!A DECnet connection
$      node = f$extract(0,f$locate("::",port),port)
$      transport_type = "DECNET"
$      goto set_display
$   endif
$   If F$LOCATE("Port:",port).ne.lport    !   UCX connection
$   Then   
$!A UCX connection   
$     port=F$EXTRACT(0,F$LOCATE("Port:",port),port)
$     If F$LOCATE("Host:",port).ne.lport
$     Then 
$         port=F$EDIT(F$EXTRACT(F$LOCATE(":",port)+1,lport,port),"TRIM,COMPRESS")
$         node=port
$     Endif
$     transport_type = "TCPIP"
$     goto set_display
$   Endif
$   If F$TRNLNM("TWG$ETC").nes."".and.F$LOCATE(".",port).nes.lport ! Wollongong
$   Then 
$!A Wollongong connection
$     tmp =F$EXTRACT(0,F$LOCATE(".",port)+1,port)
$     tmp2=F$EXTRACT(F$LOCATE(".",port)+1,lport,port)
$     tmp =tmp + F$EXTRACT(0,F$LOCATE(".",tmp2)+1,tmp2)
$     tmp2=F$EXTRACT(F$LOCATE(".",tmp2)+1,F$LENGTH(tmp2),tmp2)
$     tmp =tmp + F$EXTRACT(0,F$LOCATE(".",tmp2)+1,tmp2)
$     tmp2=F$EXTRACT(F$LOCATE(".",tmp2)+1,F$LENGTH(tmp2),tmp2)
$     tmp =tmp + F$EXTRACT(0,F$LOCATE(".",tmp2),tmp2)
$     node=tmp
$     transport_type = "WINTCP"
$     goto set_display
$   Endif
$   if f$locate(".",port) .ne. lport
$   then
$      node = port
$      transport_type = "TCPIP"
$      goto set_display
$   endif
$!
$set_display:
$!In this section we set a display if required, prompting for missing information
$   if create_display .eqs. "yes"
$   then 
$      if transport_type .eqs. ""  
$      then
$          write sys$output " "
$          write sys$output "You can display PAW output locally (LOCAL), on a remote VMS workstation 
$          write sys$output "via DECNET (DECNET) or to a remote workstation via TCP/IP." 
$          write sys$output " "
$loop2:
$          inquire/nopun transport_type "Select one of DECNET TCPIP WINTCP LOCAL : "
$          if f$locate(transport_type,"DECNET TCPIP WINTCP LOCAL") .eq. f$length("DECNET TCPIP WINTCP LOCAL") then goto loop2
$          if transport_type .eqs. "LOCAL" 
$          then
$             create_display = "no"
$             goto get_paw_type
$          endif
$      endif
$loop3:
$      if node .eqs. ""
$      then
$          write sys$output " "
$          if transport_type .eqs. "DECNET" then inquire/nopun node "Enter the DECnet node name : "
$          if transport_type .eqs. "TCPIP" then inquire/nopun node "Enter the IP name or number of the remote node : "
$          if transport_type .eqs. "WINTCP" then inquire/nopun node "Enter the IP name or number of the remote node : "
$          write sys$output " "
$          goto loop3
$      endif    
$      comm = "set display/create/transport=''transport_type'/node=''node'"
$      write sys$output "Creating DISPLAY with : ""''comm'"""
$      'comm
$      if .not. $severity then goto exit
$      display_was_created = "yes"
$      display_exists = "yes"
$   endif 
$get_paw_type:
$   pp = plusplus_override
$   if driver_override .nes. "" then driver = driver_override
$   tcp = TCP_override
$   write sys$output "The ''cernlib_version' version of the PAW''pp' ''driver' driver for PAW is selected."
$   paw_choice = "PAW''pp'''driver'''tcp'"
$got_paw_choice:
$   pawfile = f$search(root_dir+cernlib_version+".*...]"+paw_choice+".EXE")
$   if pawfile .eqs. "" 
$   then
$      write sys$output "The PAW exe called ''paw_choice' is not available."
$      paw_choice = ""
$      paw_available = ""
$      gosub select_paw
$      goto got_paw_choice
$   endif
$   pawcomm = "$''pawfile'''all_opts'"
$   set symbol/scope=global
$   kxterm:=="CERN:[''cernlib_version'.exe]kxterm"
$   write sys$output "Executing ''pawfile'"
$   deassign sys$input 
$   pawcomm 'paw_options'
$EXIT:
$   if display_was_created .eqs. "yes" then set display/delete/node='node'
$   EXIT
$HELP_TEXT: 
$   type sys$input
$   DECK

  The PAW command is invoked as follows:

      $ PAW[++] [/option...]

  where "++" invokes the PAW version with the Motif user interface.
  The allowed qualifiers are as follows:

  /OLD         Selects an executable from the OLD CERN Library
  /PRO         Selects an executable from the PRO CERN Library
  /NEW         Selects an executable from the NEW CERN Library

  /X11         Selects the X11 driver for X Window displays
  /DGKS        Selects the DEC GKS driver
  /GKS         Selects the GTS-GRAL GKS driver
  
  /MULTINET    Selects an executable linked with Multinet TCP/IP
  /UCX         Selects an executable linked with DEC TCP/IP (UCX)
  /NOTCPIP     Selects an executable without IP (no NETWORK commands)

  /TRANSPORT=  These qualifiers are used to set the display to a
  /NODE=       remote DECNET or IP connected workstation.

  /LIST        Lists the available PAW EXE files on the system
  /HELP        Displays this help

  There are some defaults that have already been set for you. They are:

$   EOD
$   if level_override .nes. "" then -
    write sys$output "   The ''level_override' versions of PAW"
$   if plusplus_override .eqs. "PP" then -
    write sys$output "   the PAW++ Motif user interface"
$   if driver_override .nes. "" then -
    write sys$output "   The driver for ''driver_override'"
$   if TCP_override .eqs. "_M" then -
    write sys$output "   Linked with Multinet"
$   if TCP_override .eqs. "_U" then -
    write sys$output "   Linked with UCX"
$   if TCP_override .eqs. "" then -
    write sys$output "   Linked without a TCP/IP package"
$   if transport_override .nes. "" then -
    write sys$output "   Displays to be created over ''transport_override'"
$   write sys$output " "         
$   return
$GET_PAW_LIST: 
$   if paw_available .nes. "" then goto end_get_paw_list
$   write sys$output " "
$   write sys$output "Listing available versions of PAW in ''cernlib_version' ..."
$   write sys$output " "
$loop1:
$   pawfile = f$search(root_dir+cernlib_version+".*...]PAW*.exe")
$   if pawfile .eqs. "" then goto end_get_paw_list
$   if f$locate("PAWSERV",pawfile) .ne. f$length(pawfile) then goto loop1
$   is = f$locate("]PAW",pawfile) + 1
$   pawfile = f$extract(is,999,pawfile)
$   pawfile = f$extract(0,f$locate(".EXE",pawfile),pawfile)
$   paw_type = "Unknown"
$   if pawfile .eqs. "PAWPP" then paw_type = "PAW++ with no TCP/IP"
$   if pawfile .eqs. "PAWPP_M" then paw_type = "PAW++ linked with Multinet"
$   if pawfile .eqs. "PAWPP_U" then paw_type = "PAW++ linked with UCX"
$   if pawfile .eqs. "PAWPP_W" then paw_type = "PAW++ linked with Wollongong"
$   if pawfile .eqs. "PAWX11" then paw_type = "PAW for X11 with no TCP/IP"
$   if pawfile .eqs. "PAWX11_M" then paw_type = "PAW for X11 linked with Multinet"
$   if pawfile .eqs. "PAWX11_U" then paw_type = "PAW for X11 linked with UCX"
$   if pawfile .eqs. "PAWX11_W" then paw_type = "PAW for X11 linked with Wollongong"
$   if pawfile .eqs. "PAWDGKS" then paw_type = "PAW with DEC GKS and no TCP/IP"
$   if pawfile .eqs. "PAWDGKS_M" then paw_type = "PAW with DEC GKS and linked with Multinet"
$   if pawfile .eqs. "PAWDGKS_U" then paw_type = "PAW with DEC GKS and linked with UCX"
$   if pawfile .eqs. "PAWDGKS_W" then paw_type = "PAW with DEC GKS and linked with Wollongong"
$   write sys$output "*** File ''pawfile' which is ''paw_type'"
$   paw_available = paw_available + " " + pawfile
$   goto loop1
$end_get_paw_list:
$   return
$SELECT_PAW: 
$start_select_paw:
$   gosub get_paw_list
$   write sys$output " "
$   if f$mode() .eqs. "BATCH" then goto batch
$   inquire/nopun paw_choice "Select the PAW version you require from the above list (or QUIT) : "
$   if f$extract(0,1,paw_choice) .eqs. "Q" then goto EXIT
$   if f$locate(paw_choice,paw_available) .eq. f$length(paw_available)
$   then
$      write sys$output "''paw_choice' is not in the above list !"
$      goto start_select_paw
$   endif
$end_select_paw:  
$   driver_override = ""
$   tcp_override = ""
$   plusplus_override = ""
$   lpawc = f$length(paw_choice)
$   if f$locate("X11",paw_choice) .ne. lpawc then driver_override = "X11"
$   if f$locate("GKS",paw_choice) .ne. lpawc then driver_override = "GKS"
$   if f$locate("DGKS",paw_choice) .ne. lpawc then driver_override = "DGKS"
$   if f$locate("_U",paw_choice) .ne. lpawc then tcp_override = "_U"
$   if f$locate("_M",paw_choice) .ne. lpawc then tcp_override = "_M"
$   if f$locate("_W",paw_choice) .ne. lpawc then tcp_override = "_W"
$   if f$locate("_",paw_choice) .eq. lpawc then tcp_override = ""
$   if f$locate("PP",paw_choice) .ne. lpawc then plusplus_override = "PP"
$   if f$locate("PP",paw_choice) .ne. lpawc then driver_override = ""
$   if paw_choice .eqs. "PAWP" ! temporary fix for bad naming convention
$   then
$      plusplus_override = "P"
$      driver_override = ""
$      tcp_override = ""
$   endif
$   return
$
$BATCH:
$ copy sys$input sys$output
  PAW requires additional information on the command line before it can be
  run in batch. For example, you may need to specify the remote display
  device with /NODE and /TRANSPORT (or use SET DISPLAY/CREATE prior to calling
  PAW in the batch job).
$ exit
