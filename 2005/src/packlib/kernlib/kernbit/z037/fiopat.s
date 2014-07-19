;
; $Id: fiopat.s,v 1.1.1.1 1996/02/15 17:47:52 mclareni Exp $
;
; $Log: fiopat.s,v $
; Revision 1.1.1.1  1996/02/15 17:47:52  mclareni
; Kernlib
;
;
;Copyright (C) 1988 CAJ Mekenkamp. All Rights Reserved.
;Carlo Mekenkamp, President Krugerstraat 42, 1975 EH  IJmuiden, Holland
;Date:  14-MAR-1988
;The author of this program does not accept any responsibilities for
;damage caused by use or ill-use of this program.
;
;These routines may be used with the AST-routines found in VAXAST.FOR
;written by the same author. They are meant to give a user a way
;of interrupting too long calculations, without stopping it.
;See VAXAST.FOR for more information.
;
;Usage: MACRO FIOPAT and then LINK it before the fortran library
;You get 29 warnings from the linker of the double defined symbols
;Restrictions: It does not work with recursive I/O
;		as can be done by TYPE ;,IFUNC
;		with IFUNC a function which uses any I/O
;Function:	Overwriting FORTRAN I/O library-routines
;		FOR$READ_DO, _DF, _DU, _IF, _IL, _IO, _KF, _KO, _KU,
;			_SF, _SN, _SL, _SO and _SU
;		FOR$REWRITE_SF, _SO and _SU
;		FOR$WRITE_DF, _DO, _DU, _IF, _IL, _IO
;			 _SF, _SN, _SL, _SO and _SU
;			Called upon start of a Fortran I/O statement
;			They establish another condition handler
;			to prevent unwinding when control-c is pressed
;			and messing up fortran I/O
;			Then alter the return address to get control
;			when the original routines jumped to (!) return
;			and in the end jump to the original return address
;		FOR$IO_END
;			Called upon end of Fortran I/O statement
;			Alters the return address to get control
;			when the original routine jumped to (!) returns
;			and in the end calls a routine in which the
;			original return address is set back,
;			so that upon a RET program continues.
;			Afterwards it checks if control-c is pressed
;			and signals the condition, in which case
;			stack frames are as they should be.
;		If the fortran program uses the obsolete ENCODE and/or DECODE
;		The comment marks in the routine activate_forrtl
;		in front of the ENCODE/DECODE symbols should be removed
;		also to define the symbols
;		FOR$DECODE_MF and _MO
;		FOR$ENCODE_MF and _MO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.TITLE  "FIOPAT"
	.IDENT  "1.16"
	.SUBTITLE	"Fortran I/O Patch"
	$SSDEF	;Define system errors
	$STSDEF ;Define severity codes

	.MACRO  .SYMBOL VALUE
	.SAVE_PSECT	LOCAL_BLOCK
	.PSECT  FIOPAT_DATA
FIOPAT_D_'VALUE:	.ASCID  "VALUE"
FIOPAT_A_'VALUE:	.BLKA   1
FIOPAT_J_'VALUE:	.BLKA   1
	.RESTORE_PSECT
	.ENDM

	.MACRO  check   ?label
	blbs	r0,label
	pushl   r0
	calls   #1,g^lib$stop
label:  .ENDM	check

	.MACRO  LIB$FIND_IMAGE_SYMBOL_S file_name,symbol_desc,symbol_val
	pushal  symbol_val
	pushaq  symbol_desc
	pushaq  file_name
	calls   #3,g^lib$find_image_symbol
	.endm

	.MACRO  GET_SYMBOL	SYMBOL, -
				MASK=<^M<R2,R3,R4,R5,R11>>, -
				ROUTINE=<YES>
	.IIF	IDENTICAL <ROUTINE> <YES>,	.ROUTINE SYMBOL,<MASK>
	.SYMBOL SYMBOL
	LIB$FIND_IMAGE_SYMBOL_S fiopat_d_forrtl, -
				fiopat_d_'symbol, -
				fiopat_a_'symbol
	addl3   #2,fiopat_a_'symbol,fiopat_j_'symbol
	CHECK
	.ENDM   GET_SYMBOL

	.macro  .routine	SYMBOL,MASK
	.SAVE_PSECT	LOCAL_BLOCK
	.PSECT  FIOPAT_REDEF_FOR,EXE,NOWRT
	.entry  SYMBOL,MASK	;overwrites SYMBOL
;++
;	Functional description:
;
;		Overwrites SYMBOL supplied by fortran
;		If called first time, fill in addresses
;		Save SYMBOL address to jmp too
;
;--
	blbs	fiopat_forrtl_active,10$		;Is forrtl active?
	calls   #0,fiopat_activate_forrtl		;No-> activate forrtl
;Introduction message not printed
	pushaq  fiopat_d_intro
	calls   #1,g^lib$put_output
10$:	movl	fiopat_j_'SYMBOL,fiopat_routine
	jmp	FIOPAT_COMMON_ROUTINE
	.RESTORE_PSECT
	.endm   .routine

	.psect  fiopat_data,noexe,wrt,noshr
fiopat_cc_interrupt:	.long   0
fiopat_routine: .long	0
fiopat_forrtl_active:	.long   0
fiopat_d_forrtl:	.ascid  "FORRTL"
fiopat_aa_condition_handler:	.blka   1
fiopat_a_old_condition_handler: .blka	1
fiopat_j_old_condition_handler: .blka	1
fiopat_old_pc:	.long   0
fiopat_d_intro: .ascid  "Press CONTROL-C to interrupt too long calculations"

	.psect  fiopat_code,exe,nowrt

FIOPAT_COMMON_ROUTINE:
;++
;		Saves old handler and establishes handler
;		to prevent unwinding over fortran-OUTPUT
;		and calls original routine from fortran
;--
	tstl	fiopat_old_pc	;test for presence old handler
	beql	50$
;if non-zero there has been no call to for$io_end
;This can be the case with read(...,end=lab)
;Search now for the place of the old condition handler
;First look to see handler established still on same place
	cmpl	@fiopat_aa_condition_handler,#fiopat_condition_handler
	bneq	40$	;if not set fiopat_old_pc to zero
;If so, now look if it is on the frame-list
	pushl	r0	;save register r0
	movl	fp,r0	;Start with frame pointer in r0
10$:	beql	30$	;if pointer 0 then end of list
	cmpl	r0,fiopat_aa_condition_handler
	bgequ	20$	;if lower address
	movl	12(r0),r0	;get next frame pointer
	brb	10$	;loop till frame pointer found
20$:	bnequ	30$	;if not equal, no handler present anymore
	movl	fiopat_a_old_condition_handler,(r0)	;restore handler
;If control-c is pressed here, a direct stack-unwind will happen
;and so fiopat_old_pc isn't cleared. This does not matter, since
;it is cleared next time.
30$:	popl	r0	;Restore R0
40$:	clrl	fiopat_old_pc	;clear old_pc to inform no handler present
50$:	blbc	fiopat_cc_interrupt,60$
;Maybe interrupt pending after a read (...,end=label)...
;Then give now a new signal to unwind the stack
	clrl	fiopat_cc_interrupt	;clear interrupt flag
	pushl	#STS$K_ERROR!SS$_CONTROLC&<-<STS$M_SEVERITY+1>>
	calls   #1,G^LIB$SIGNAL ;and signal ss$_controlc condition as error
60$:	movl	12(fp),fiopat_aa_condition_handler	;save pointer
	movl	@12(fp),fiopat_a_old_condition_handler  ;save old handler
	movaw   fiopat_condition_handler,@12(fp)	;and establish one
	movl	16(fp),fiopat_old_pc	;save old pc
	moval   70$,16(fp)				;enter new pc
	jmp	@fiopat_routine		;jmp to called routine
;Necessary because it assumes (Just as I do) the parent's stack
;will stay.
;You could leave out the moval  70$,16(fp), but i did it for debugging
;purposes, and it's now possible to add some code, since control
;is back in this routine.
;Remember stack is now in proper state for caller, so no ret may be
;executed here, but a jmp.
70$:								;and return here
	jmp	@fiopat_old_pc  ;jump then to original return address

fiopat_activate_forrtl: .word	^M<>
;++
;	Functional description:
;
;		Gets the addresses of the symbols FOR$WRITE_xy, FOR$READ_xy
;		Symbols in order from SYS$SHARE:FORRTL.EXE
;		If fortran program uses the obsolete ENCODE/DECODE
;		Remove the double comment marks.
;		and FOR$IO_END, which are overwritten in this program
;
;--
	GET_SYMBOL	FOR$WRITE_SN
	GET_SYMBOL	FOR$WRITE_SL
	GET_SYMBOL	FOR$WRITE_IL
	GET_SYMBOL	FOR$READ_IL
	GET_SYMBOL	FOR$READ_SL
	GET_SYMBOL	FOR$READ_SN
;;	GET_SYMBOL	FOR$DECODE_MO
	GET_SYMBOL	FOR$READ_KO
	GET_SYMBOL	FOR$READ_IO
	GET_SYMBOL	FOR$READ_DO
	GET_SYMBOL	FOR$READ_SO
	GET_SYMBOL	FOR$WRITE_SO
	GET_SYMBOL	FOR$WRITE_DO
	GET_SYMBOL	FOR$WRITE_IO
	GET_SYMBOL	FOR$REWRITE_SO
;;	GET_SYMBOL	FOR$ENCODE_MO
;;	GET_SYMBOL	FOR$ENCODE_MF
	GET_SYMBOL	FOR$REWRITE_SF
	GET_SYMBOL	FOR$WRITE_IF
	GET_SYMBOL	FOR$WRITE_DF
	GET_SYMBOL	FOR$WRITE_SF
	GET_SYMBOL	FOR$READ_SF
	GET_SYMBOL	FOR$READ_IF
	GET_SYMBOL	FOR$READ_DF
;;	GET_SYMBOL	FOR$DECODE_MF
	GET_SYMBOL	FOR$READ_KF
	GET_SYMBOL	FOR$REWRITE_SU
	GET_SYMBOL	FOR$WRITE_DU
	GET_SYMBOL	FOR$WRITE_SU
	GET_SYMBOL	FOR$READ_SU
	GET_SYMBOL	FOR$READ_DU
	GET_SYMBOL	FOR$READ_KU
	GET_SYMBOL	FOR$IO_END,ROUTINE=<NO>
	clrl	fiopat_cc_interrupt
	movl	#1,fiopat_forrtl_active
	ret

fiopat_condition_handler:	.word   ^M<r2,r3,r4,r5,r6,r7,r8,r9,r10,r11>
;++
;	Functional description:
;
;		Condition handler to handle ss$_controlc signal
;		If signal=ss$_controlc then a flag is set
;		otherwise if a condition handler existed,
;		control is given to that handler
;		otherwise the condition is resignalled
;
;--
	movl	4(ap),r0		;get signal array
	pushal  #SS$_CONTROLC
	pushal  4(r0)	;signal name
        calls   #2,G^LIB$MATCH_COND     ;check it to be the control-c signal
	cmpl	r0,#1
	bneq	20$				;if so
	movl	#1,fiopat_cc_interrupt	;flag cc_interrupt occurred
	movl	#ss$_continue,r0		;and continue
	ret
20$:	tstl	fiopat_a_old_condition_handler  ;If old handler present
	beql	30$
	addl3   fiopat_a_old_condition_handler, #2, -
		fiopat_j_old_condition_handler
	jmp	@fiopat_j_old_condition_handler ;jmp to the old handler
30$:	movl	#ss$_resignal,r0		;otherwise resignal
	ret

	.entry  for$io_end,^M<R2,R3,R4,R5,R11>  ;overwrites for$io_end
;++
;	Functional description:
;
;		Overwrites FOR$IO_END supplied by fortran
;		Calls FOR$IO_END from fortran
;		Reestablish old handler (or zero if not present)
;		Check cc interrupt flag and signal SS$_CONTROLC
;
;--
	blbs	fiopat_forrtl_active,10$		;Is symbol active?
	calls   #0,fiopat_activate_forrtl		;No-> activate symbol
10$:	movl	16(fp),fiopat_old_pc	;save old pc
	moval   20$,16(fp)				;enter new pc
	jmp	@fiopat_j_for$io_end	;jmp to for$io_end
;Here I have to come back to restore the condition handler
;and check for occurrance of control-c interrupt
20$:                                                    ;and return here
;Call here is necessary to get stack in proper state io case of an unwind
;when signalling SS$_CONTROLC
	callg   (ap),30$
30$:	.word   ^M<>		;no registers saved here
	movl	fiopat_old_pc,16(fp)    ;first restore original address
	clrl	fiopat_old_pc
	movl	fiopat_a_old_condition_handler,@12(fp)  ;restore old handler
;If no interrupt has happened here, it is possible a control-c interrupt
;will take place and stack unwind will occur. It does not matter
;because state is normal.
	blbc	fiopat_cc_interrupt,40$ ;if interrupt occurred
	clrl	fiopat_cc_interrupt		;clear interrupt flag
;Signal with Error-status
;So a condition handler should catch it.
	pushl   #STS$K_ERROR!SS$_CONTROLC&<-<STS$M_SEVERITY+1>>
	calls   #1,G^LIB$SIGNAL ;and signal ss$_controlc condition as error
40$:	ret		;return to original return address
	.end
