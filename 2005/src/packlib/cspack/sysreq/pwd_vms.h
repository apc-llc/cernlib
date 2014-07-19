/*
 * $Id: pwd_vms.h,v 1.1.1.1 1996/03/08 15:44:26 mclareni Exp $
 *
 * $Log: pwd_vms.h,v $
 * Revision 1.1.1.1  1996/03/08 15:44:26  mclareni
 * Cspack
 *
 */
#if defined(CERNLIB_VAXVMS)
/*
 * Copyright (C) 1989 by Frederic Hemmer
 * All rights reserved
 */
 
/* pwd.h VMS "Password" file entry structure   */
 
struct passwd {
 char *pw_account; /* User's account name   */
 short pw_astlm; /* AST queue limit   */
 int pw_b_acc_p; /* Batch access primary days  */
 int pw_b_acc_s; /* Batch access secondary days  */
 short pw_biolm; /* Bufferred I/O count limit  */
 int pw_bytlm; /* Buffer I/O byte limit  */
 char *pw_clitab; /* User defined CLI table  */
 int pw_cputim; /* Maximum CPU time limit (session) */
 char *pw_shell; /* Command language Interpreter  */
 char *pw_defdev; /* Default device name   */
 char *pw_defdir; /* Default Directory name  */
 char *pw_dir; /* Default Directory full spec  */
 char *pw_defpriv; /* Default privileges   */
 int pw_dfwscnt; /* Default working set size  */
 short pw_diolm; /* Direct I/O count limit  */
 int pw_d_acc_p; /* Dialup access primary days  */
 int pw_d_acc_s; /* Dialup access secondary days  */
 char pw_encrypt; /* Primary password encrypt algorithm */
 char pw_encrypt2; /* Secondary password encrypt algorithm */
 short pw_enqlm; /* Lock queue limit   */
 char *pw_expir; /* Account expiration date  */
 short pw_fillm; /* Open file limit   */
 int pw_flags; /* Various login flags   */
 int pw_jtquota; /* Jobwide log. nam. table init. byte q.*/
 char *pw_lastl_i; /* Last interactive login date  */
 char *pw_lastl_n; /* Last non-interactive login date */
 char *pw_lgicmd; /* Default login command file  */
 int pw_l_acc_p; /* Local access primary days  */
 int pw_l_acc_s; /* Local access secondary days  */
 short pw_logfail; /* Login failure count   */
 short pw_maxacctj; /* Max # of bat,int and det jobs */
 short pw_maxdet; /* Detached process limit  */
 short pw_maxjobs; /* Active process limit   */
 int *pw_n_acc_p; /* Network access primary days  */
 int *pw_n_acc_s; /* Network access secondary days */
 char *pw_owner; /* Account owner   */
 int pw_pbytlm; /* Paged buffer I/O byte count limit */
 int pw_pgflquota; /* Paging file quota   */
 int pw_prccnt; /* Subprocess creation limit  */
 char pw_pri;  /* Default priority   */
 int pw_primedays; /* Primary and secondary days  */
 char *pw_priv; /* Privileges hold by user  */
 char *pw_passwd; /* Hashed primary password  */
 char *pw_pwd_date; /* Last primary password change date */
 char pw_pwd_length; /* Minimum password length  */
 char *pw_pwd_life; /* Password lifetime   */
 char *pw_pwd2; /* Hashed secondary password  */
 char *pw_pwd2_date; /* Last secondary password change date */
 char pw_quepri; /* Maximum job queue priority  */
 int pw_r_acc_p; /* Remote interactive primary days */
 int pw_r_acc_s; /* Remote interactive secondary days */
 short pw_salt; /* Random password salt   */
 short pw_shrfillm; /* Shared file limit   */
 short pw_tqcnt; /* Time queue entry limit  */
 int pw_uic;  /* UIC     */
 short pw_uid;  /* UIC member number   */
 short pw_gid;  /* UIC group number   */
 char *pw_name; /* User name    */
 int pw_wsextent; /* Working set extent   */
 int pw_wsquota; /* Working set quota   */
};
 
struct passwd *getpwent(), *getpwuid(), *getpwnam();
#endif
