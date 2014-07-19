$! ***********************************************************************
$! *                                                                     *
$! * YEXPAND.COM v1.05                                                   *
$! *                                                                     *
$! * Translator of Unix cradles to Vax/VMS                               *
$! * Call   : yexpand [ -f -v ] ifile [ofile]                            *
$! * Options:  -f    Substitute Unix filenames by VMS equivalents in     *
$! *                 +ASM,+PAM                                           *
$! *           -v    verbose mode, reporting on substituted variables    *
$! *                                                                     *
$! * Author: M.Marquina 92/02/21                                         *
$! *                                                                     *
$! * Mods       Date   Comments                                          *
$! * MARQUINA 92/03/05 Cosmetic changes (single exit point,etc)          *
$! *          92/03/08 Translate Unix directories to VMS equivalent      *
$! *                   bin=EXE,etc                                       *
$! *                   Translate filenames only in +PAM,+ASM             *
$! *          92/07/14 Correct bug when an env.variable ends up the line *
$! *                   and when +PAM lines do not have T=A.              *
$! * JAMIE    94/03/08 Only translate control lines                      *
$! * JAMIE    94/04/22 Support also variables in {}, e.g. ${PLINAME}     *
$! * JAMIE    94/05/20 Only translate variables that are defined         *
$! * JAMIE    94/09/16 Trim 'uxfile'                                     *
$! *                                                                     *
$! *********************************************************************** 
$ SHNAME=F$ENVIRONMENT("PROCEDURE")
$ SHNAME=F$PARSE(SHNAME,,,"NAME")
$!
$ sav_mess=F$ENVIRONMENT("MESSAGE")
$!On ERROR Then Goto EXIT
$!
$ SAY :== WRITE/SYMBOL SYS$OUTPUT
$ blk=" "
$ lfn="        "
$ b20="                    "
$!
$ Gosub NARG
$ If p0.eq.0 Then Call SHEXIT 2 "usage: yexpand [-f] [-v] input_file [output_file]"
$!                           
$ args=s0
$ getopt "DFV" 'args'     ! Invoke the Unix parser
$ args='getopt_string'
$!
$ Gosub PARSE_OPTIONS
$ If ifile.eqs." "          Then Call SHEXIT 2 "input file missing"
$ itype=F$PARSE(ifile,,,"TYPE")
$ If itype.eqs."." Then ifile=ifile+".CRA"
$ If F$SEARCH(ifile).eqs."" Then Call SHEXIT 2 "input file "'ifile'" not found"
$ If ofile.eqs." " 
$ Then ip=F$LOCATE("]",ifile)
$      lp=F$LENGTH(ifile)
$      If ip.eq.lp Then Call SHEXIT 2 "output=input ; it cannot overwrite"
$      ofile=F$EXTRACT(ip+1,lp-ip,ifile)
$ Endif
$!
$ If _v.eq.1 Then Say blk,"yexpand: translator of Unix cradles to Vax/VMS"
$ open/read  input  'ifile'
$ open/write output 'ofile'
$!
$ nl=0
$ R_DO:
$ read/end=EOF input line
$ nl=nl+1
$ If _d.eq.1 Then Say line
$!
$ If f$extract(0,1,line).nes."+" then goto f_end
$!
$ ll=F$LENGTH(line)
$!
$! Check first for ${var}
$!
$ X_DO:
$ l_var = f$locate("${",line)
$ if l_var .eq. ll then goto X_END
$ left  = f$extract(0,l_var,line)
$ right = f$extract(l_var+2,ll,line)
$ lr    = ll - l_var - 2
$ e_var = f$locate("}",right)
$ if e_var .lt. lr 
$ then
$    var = f$extract(0,e_var,right)
$    right = f$extract(e_var+1,lr,right)
$    if f$type('var').eqs."" 
$       then 
$       else var='var'
$    endif
$    line=left+var+right
$    ll=f$length(line)
$    goto X_DO
$ endif
$ X_END:
$!
$! Old style
$!
$ _p=F$LOCATE("$",line)
$!
$ D_DO:
$ If _p.eq.ll Then Goto D_END
$! --- Parse the line=lp1"$"lp2
$ lp1=F$EXTRACT(0,_p,line)
$ lp2=F$EXTRACT(_p+1,ll-_p,line)
$! Find out where does the environment variable finish
$  ip=-1
$  ll2=F$LENGTH(lp2)
$  D__DO:
$  ip=ip+1
$  If ip.gt.ll2 Then Goto D_DO
$  is=F$EXTRACT(ip,1,lp2)
$  If F$LOCATE(is,".,/$*[]").lt.7 
$  Then nvar=F$EXTRACT(0,ip,lp2)
$            vvar="?"+nvar
$       If F$TYPE('nvar').nes."" 
$       Then vvar='nvar'
$            nvar=F$EXTRACT(0,25,nvar+b20)
$         If _v.eq.1 Then Say lfn,"- env. variable ",nvar," -> ",vvar
$       Endif
$       line=lp1+vvar+F$EXTRACT(ip,ll2-ip,lp2)
$       _p=F$LOCATE("$",line)
$       ll=F$LENGTH(line)
$       Goto D_DO
$  Endif
$  Goto D__DO
$ D_END:
$! check the line for Unix filenames
$ uline=F$EDIT(line,"UPCASE")
$ pcmd =F$EXTRACT(0,4,uline)
$ _sf=F$LOCATE(pcmd,"+PAM +ASM").lt.9.and.F$LOCATE("T=",uline).lt.ll
$!
$ _sf= _f.eq.1.and._sf.eq.1
$ If _sf.eq.0 Then Goto F_END
$ F_DO:
$ _ita=F$LOCATE(".",uline)
$ lux =ll-_ita-1
$ If lux.eq.0 Then Goto F_END
$ uxfile0=F$EXTRACT(_ita+1,lux,line)
$ uxfile =uxfile0
$ If F$EXTRACT(0,1,uxfile).eqs.":" Then uxfile=F$EXTRACT(1,lux-1,uxfile)
$ _r=F$EXTRACT(0,1,uxfile).eqs."=" 
$ If _r.eq.1 Then uxfile=F$EXTRACT(1,lux-1,uxfile)
$ vxfile=uxfile
$ uxfile=f$edit(uxfile,"TRIM") ! remove leading and trailing blanks
$ If F$EXTRACT(0,1,uxfile).eqs."/"
$ Then nft=0
$      FT_DO:
$      nft=nft+1
$      sft1=F$ELEMENT(nft  ,"/",uxfile)
$      sft2=F$ELEMENT(nft+1,"/",uxfile)
$      If nft.eq.1 
$      Then vxfile=sft1+":["
$           Goto FT_DO
$      Endif 
$      If sft2.nes."/" 
$      Then If sft1.eqs."bin" Then sft1="exe"
$           vxfile=vxfile+sft1+"."
$           Goto FT_DO
$      Endif 
$      vxfile=vxfile+"]"-".]"+"]"+sft1
$ Endif
$ vxtype=F$PARSE(vxfile,,,"TYPE")
$ If vxtype.eqs.".F" Then vxfile=vxfile+"ZZ"-".fZZ"+".FOR"
$ If _r.eq.1 Then vxfile="="+vxfile
$ line=F$EXTRACT(0,_ita+1,line)+vxfile
$ If _v.eq.1 
$ Then uxfile0=F$EXTRACT(0,30,uxfile0+b20)
$      Say lfn,"- filename ",uxfile0," -> ",vxfile
$ Endif
$ F_END:
$ If _d.eq.1 Then Say line
$ write output line
$ Goto R_DO
$!
$ EOF:
$ close input
$ close output
$!
$ EXIT:
$ set message'sav_mess'
$ Exit
$!--------
$ NARG:
$!--------
$ p0=0
$ s0=""
$ N_DO:
$ p0=p0+1
$ If p0.lt.9
$ Then s0=s0+p'p0'+" "
$      If p'p0'.nes."" Then Goto N_DO
$ Endif
$ p0=p0-1
$ Return
$!--------
$ PARSE_OPTIONS:
$!--------
$ _d=0    ! debug mode
$ _f=0    ! translate filenames Unix -> VMS syntax
$ _v=0    ! verbose mode
$ np=-1
$ O_DO:
$ np=np+1
$ arg=F$ELEMENT(np," ",args)
$ If arg.nes."--"
$ Then If arg.eqs."-D" Then _d=1
$      If arg.eqs."-F" Then _f=1
$      If arg.eqs."-V" Then _v=1
$      Goto O_DO
$ Endif
$ ifile=F$ELEMENT(np+1," ",args)
$ ofile=F$ELEMENT(np+2," ",args)
$ Return
$!--------
$ SHEXIT: SUBROUTINE
$!--------
$!set message/nof/noi/nos/not
$                 SHERR="%DCL-F-"
$ If p1.lt.4 Then SHERR="%DCL-E-"
$ If p1.lt.2 Then SHERR="%DCL-W-"
$ Say SHERR,SHNAME,", ",p2
$ Exit 'p1'
$      ENDSUBROUTINE
